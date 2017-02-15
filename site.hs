{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts #-}
import           Control.Applicative ((<$>), (<*>), empty)
import           Control.Monad
import           Control.Monad.Writer
import           Data.Monoid (mappend, mempty)
import           Data.Aeson
import           Data.Ord
import           Hakyll
import           Hakyll.Core.Util.String
import           Hakyll.Core.Metadata

import           Text.Pandoc.Options
import           Text.Pandoc.Walk
import           Text.Pandoc

import qualified Data.Set as S
import qualified Data.Map as M
import           Data.List hiding (span)
import           Data.Ord
import           Data.String
import           Data.Function
import           Data.Maybe
import           System.FilePath

import           Text.Blaze.Html5 hiding (map, main)
import           Text.Blaze.Html5.Attributes hiding (span)
import           Text.Blaze.Html.Renderer.String
import           Text.EditDistance

import           Prelude hiding (div, span)

import Debug.Trace

-- Defaults

-- | How many tags to display on the front page
tagCount = 10

-- | How many posts to show on the home page
maxPostCount = 5

data ImageAttribute = ImageCentered
                    | ImageFlow
                    | ImageAlignRight
                      deriving (Eq, Ord, Show, Read)

suggestNames max name names = take max sortedSuggestions
    where sortedSuggestions = map snd (sortBy (comparing fst) suggestions)
          suggestions = map (\s -> (distance s, s)) names
          distance s = levenshteinDistance defaultEditCosts s name

--------------------------------------------------------------------------------
pandocMathCompiler' allPosts customTransform =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros, Ext_implicit_figures]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }

        generateThumbnailsAndPostLinks = walk (useThumbnails . interpostLinks) . -- This walks inlines
                                         walk handleLargeQuotes -- This walks blocks...

        allPostsFixed = map (\s -> take (length s - length (".markdown" :: String)) s) $ map (drop (length ("posts/" :: String))) allPosts
        allPostsSet = S.fromList allPostsFixed

        -- The following function handles large quotes. These are Markdown quotes which begin with the strikeout text "~~LARGE~~".
        -- Large quotes can also be attributed to a source, which should come after a line saying exactly "~~SOURCE~~"
        handleLargeQuotes (BlockQuote (Para (Strikeout [Str "LARGE"]:afterLarge):largeQuoteBlocks)) =
          let largeQuoteBlocks' = Plain afterLarge:largeQuoteBlocks

              source (Para (Strikeout [Str "SOURCE"]:_)) = True
              source _ = False

              beforeSource = takeWhile (not . source) largeQuoteBlocks'
              afterSource  = dropWhile (not . source) $ largeQuoteBlocks'
              fixedSource = case afterSource of
                              Para (_:x):xs -> Plain x:xs
                              _             -> afterSource

              compiledContent = writeHtmlString writerOptions (Pandoc mempty beforeSource)
              compiledSource = writeHtmlString writerOptions (Pandoc mempty fixedSource)

              raw = div ! class_ "large-quote" $ do
                span ! class_ "quote-content" $ preEscapedToHtml compiledContent
                span ! class_ "quote-source" $ preEscapedToHtml compiledSource
          in RawBlock (Format "html") (renderHtml raw)
        handleLargeQuotes x = x

        interpostLinks (Link attrs t (url, title))
            | "post:" `isPrefixOf` url = let postName = drop 5 url
                                             postURL = "/posts/" ++ postName ++ ".html"
                                         in if postName `S.member` allPostsSet -- take the trailing '/' off
                                            then Link attrs t (postURL, title)
                                            else error (concat ["Unknown post referenced: ", postName, "\nPerhaps you meant:\n", intercalate "\n" (map ("    - " ++) (suggestNames 5 postName allPostsFixed))])
            | "image:" `isPrefixOf` url = let imgName = drop 6 url
                                          in Link attrs t ("/images/" ++ imgName, title)
        interpostLinks (Image attrs t (url, title))
            | "image:" `isPrefixOf` url = let imgName = drop 6 url
                                          in Image attrs t ("/images/" ++ imgName, title)
        interpostLinks x = x

        collectImageAttributes inlines = filterM selectAttributes inlines
        selectAttributes (Strikeout [Str "CENTERED"]) = tell (S.singleton ImageCentered) >> return False
        selectAttributes (Strikeout [Str "FLOWED"]) = tell (S.singleton ImageFlow) >> return False
        selectAttributes (Strikeout [Str "ALIGNRIGHT"]) = tell (S.singleton ImageAlignRight) >> return False
        selectAttributes _ = return True

        useThumbnails (Image attrs caption target) =
            let (url, title) = target
                (urlFp, urlExt) = splitExtension url
                url' = addExtension (urlFp ++ "-small") urlExt
                url'' = if "/images/" `isPrefixOf` url' then url' else url

                (caption', attributes) = runWriter (collectImageAttributes caption)
                compiledCaption = writeHtmlString writerOptions (Pandoc mempty [Plain caption'])

                isCentered = ImageCentered   `S.member` attributes
                isFlow     = ImageFlow       `S.member` attributes
                isRight    = ImageAlignRight `S.member` attributes

                htmlClasses = execWriter $
                              do when isCentered $ tell ["figure-centered"]
                                 when isFlow     $ tell ["figure-flow"]
                                 when isRight    $ tell ["figure-right"]

                raw = div ! class_ (fromString $ intercalate " " ("figure":htmlClasses)) $ do
                        a ! href (fromString url) $ do
                          img ! src (fromString url'') ! alt (fromString compiledCaption)
                        p ! class_ "caption" $ preEscapedToHtml compiledCaption
            in RawInline (Format "html") (renderHtml raw)
        useThumbnails x = x
    in pandocCompilerWithTransform defaultHakyllReaderOptions writerOptions (generateThumbnailsAndPostLinks . customTransform)

myHakyllConfig = defaultConfiguration {
                   deployCommand = "s3cmd sync --acl-public --delete-removed ./_site/ s3://travis.athougies.net/"
                 }

thumbnailMaxSize = 200
imageMaxSize = 1500

main :: IO ()
main = hakyllWith myHakyllConfig $ do
    match "js/*.js" $ do
      route idRoute
      compile copyFileCompiler

    match ("images/**" .&&. complement "images/stock/**") $ do
        route   idRoute
        compile $ do getResourceLBS >>= withItemBody (unixFilterLBS "python" ["./generate-thumbnail.py", show imageMaxSize])

    match ("images/**" .&&. complement "images/stock/**") $ version "small" $ do
      route $ customRoute $ \i -> let (path, ext) = splitExtension $ toFilePath i
                                      fp = addExtension (path ++ "-small") ext
                                  in fp
      compile $ do getResourceLBS >>= withItemBody (unixFilterLBS "python" ["./generate-thumbnail.py", show thumbnailMaxSize])

    match "images/stock/**" $ do
      route idRoute
      compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "css/*.hs" $ do
        route   $ setExtension "css"
        compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

    tags <- buildTagsWith getTagsQuoted "posts/*" (fromCapture "tags/*.html")
    posts <- map toFilePath <$> getMatches "posts/*"
    -- Project pages! :D
    projectIdentifiers <- getProjects
    projects <- sortBy (comparing projectPriority) .
                mapMaybe (\x -> case x of { Success x -> Just x; _ -> Nothing }) <$>
                mapM (fmap (fromJSON . Object) . getMetadata) projectIdentifiers

    let travisContext = listField "globalTags" tagCtx tagsAsItems `mappend`
                        listField "projects" projectAsItem (mapM makeItem projects) `mappend`
                        defaultContext

        tagsAsItems = mapM tagAsItem
                      (-- take tagCount .
                       reverse .
                       sortBy (comparing (length . snd)) .
                       tagsMap $ tags)
        tagAsItem (tag, posts) = makeItem (tag, posts)

        tagCtx        = field "tag" (return . fst . itemBody) `mappend`
                        field "postCount" (return . show . length . snd . itemBody)

        projectDependencies = map IdentifierDependency projectIdentifiers

        pandocMathCompiler = pandocMathCompiler' posts Prelude.id

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" travisContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
          underlying <- getUnderlying
          myTags <- getTagsQuoted underlying
          pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx `mappend` myTagsCtx myTags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx `mappend` travisContext)
            >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        route idRoute
        compile $ do
            list <- recentFirst =<< onlyPublished =<< loadAll pattern
            let context = constField "title" title `mappend`
                          listField "posts" postCtx (return list) `mappend`
                          travisContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" context
                >>= loadAndApplyTemplate "templates/default.html" context
                >>= relativizeUrls

    rulesExtraDependencies projectDependencies $
     create ["projects.html"] $ do
      let projectsContext = constField "title" "Projects" `mappend`
                            travisContext
      route idRoute
      compile $ makeItem ""
            >>= loadAndApplyTemplate "templates/projects.html" projectsContext
            >>= loadAndApplyTemplate "templates/default.html" projectsContext
            >>= relativizeUrls

    forM projects $ \project -> do
      let projectContext = projectCtx project `mappend` travisContext
      match (fromString ("projects/" ++ projectSlug project ++ ".markdown")) $ do
          route $ setExtension "html"
          compile $ pandocMathCompiler
                  >>= loadAndApplyTemplate "templates/project.html" projectContext
                  >>= loadAndApplyTemplate "templates/default.html" projectContext
                  >>= relativizeUrls

      match (fromString ("projects/" ++ projectSlug project ++ "/*")) $ do
          route $ setExtension "html"
          compile $ pandocMathCompiler
                >>= loadAndApplyTemplate "templates/project.html" projectContext
                >>= loadAndApplyTemplate "templates/default.html" projectContext
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< onlyPublished =<< loadAll "posts/*"
            let archiveCtx =
                    listField  "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    travisContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- take maxPostCount <$> (recentFirst =<< onlyPublished =<< loadAll "posts/*")
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    travisContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
data ProjectDescription = ProjectDescription {
      projectTitle    :: String,
      projectSlug     :: String,
      projectSummary  :: String,
      projectLanguage :: String,
      projectStatus   :: Maybe String,
      projectVersion  :: String,
      projectPriority :: Int,
      projectGithub   :: Maybe String
    } deriving Show

instance FromJSON ProjectDescription where
    parseJSON = withObject "ProjectDescription" $ \v ->
                ProjectDescription <$> v .: "title"
                                   <*> v .: "slug"
                                   <*> v .: "summary"
                                   <*> v .: "language"
                                   <*> v .:? "status"
                                   <*> v .: "version"
                                   <*> v .: "priority"
                                   <*> v .:? "github"

projectCtx :: ProjectDescription -> Context String
projectCtx (ProjectDescription { .. }) =
    constField "name" projectTitle `mappend`
    constField "slug" projectSlug `mappend`
    constField "summary" projectSummary `mappend`
    constField "version" projectVersion `mappend`
    constField "language" projectLanguage `mappend`
    maybe mempty (constField "status") projectStatus `mappend`
    maybe mempty (constField "github") projectGithub

projectAsItem :: Context ProjectDescription
projectAsItem =
    field "name" (return . projectTitle . itemBody) `mappend`
    field "slug" (return . projectSlug . itemBody) `mappend`
    field "summary" (return . projectSummary . itemBody) `mappend`
    field "version" (return . projectVersion . itemBody) `mappend`
    field "language" (return . projectLanguage . itemBody) `mappend`
    field "status" (maybe empty return . projectStatus . itemBody) `mappend`
    field "github" (maybe empty return . projectGithub . itemBody)

getMetadataMaybe f transform key _ item
    | key == f = do
       metadata <- getMetadata $ itemIdentifier item
       maybe empty (return . StringField . transform) $ lookupString f metadata
    | otherwise = empty

headerImageField :: Context String
headerImageField = headerImageF `mappend` headerImageCaptionF
  where headerImageF = Context (getMetadataMaybe "header-image" mkImgLink)
        headerImageCaptionF = Context (getMetadataMaybe "header-image-caption" Prelude.id)

        mkImgLink link
          | "image:" `isPrefixOf` link = "/images/" ++ drop 6 link
          | otherwise = link

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    headerImageField `mappend`
    Context (getMetadataMaybe "title" unQuote) `mappend`
    defaultContext

myTagsCtx :: [String] -> Context String
myTagsCtx tags = listField "myTags" simpleTagCtx (mapM makeItem tags)
  where simpleTagCtx = field "tag" (return . itemBody)

getProjects :: MonadMetadata m => m [Identifier]
getProjects = getMatches "projects/*.markdown"

--------------------------------------------------------------------------------
-- | Obtain tags from a page in an almost default way: parse them from the @tags@
-- metadata field, removing any surrounding quotes
getTagsQuoted :: MonadMetadata m => Identifier -> m [String]
getTagsQuoted identifier = do
    metadata <- getMetadata identifier
    return $ maybe [] (map trim . splitAll "," . unQuote) $ lookupString "tags" metadata

-- Takes a string possibly wrapped in qutoes and removes them
unQuote :: String -> String
unQuote s = let s' = if Prelude.head s == '"' then Prelude.tail s else s
                s'' = if last s' == '"' then init s' else s'
            in s''

onlyPublished :: (MonadMetadata m, Functor m) => [Item a] -> m [Item a]
onlyPublished = filterM (\item -> maybe True parseBool <$> getMetadataField (itemIdentifier item) "published")
    where parseBool "false" = False
          parseBool _ = True
