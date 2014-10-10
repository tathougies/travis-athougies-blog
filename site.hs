--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import           Control.Applicative ((<$>), empty)
import           Control.Monad
import           Control.Monad.Writer
import           Data.Monoid (mappend, mempty)
import           Data.Ord
import           Hakyll
import           Hakyll.Core.Util.String

import           Text.Pandoc.Options
import           Text.Pandoc.Walk
import           Text.Pandoc

import qualified Data.Set as S
import qualified Data.Map as M
import           Data.List
import           Data.Ord
import           Data.String
import           Data.Function
import           Data.Maybe
import           System.FilePath

import           Text.Blaze.Html5 hiding (map)
import           Text.Blaze.Html5.Attributes
import           Text.Blaze.Html.Renderer.String
import           Text.EditDistance

import           Prelude hiding (div)

import Debug.Trace

-- Defaults

-- | How many tags to display on the front page
tagCount = 10

data ImageAttribute = ImageCentered
                    | ImageFlow
                    | ImageAlignRight
                      deriving (Eq, Ord, Show, Read)

suggestNames max name names = take max sortedSuggestions
    where sortedSuggestions = map snd (sortBy (comparing fst) suggestions)
          suggestions = map (\s -> (distance s, s)) names
          distance s = levenshteinDistance defaultEditCosts s name

--------------------------------------------------------------------------------
pandocMathCompiler' allPosts =
    let mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                          Ext_latex_macros, Ext_implicit_figures]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr S.insert defaultExtensions mathExtensions
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }

        generateThumbnailsAndPostLinks = walk (useThumbnails . interpostLinks)

        allPostsFixed = map (\s -> take (length s - length (".markdown" :: String)) s) $ map (drop (length ("posts/" :: String))) allPosts
        allPostsSet = S.fromList allPostsFixed

        interpostLinks (Link t (url, title))
            | "post:" `isPrefixOf` url = let postName = drop 5 url
                                             postURL = "/posts/" ++ postName ++ ".html"
                                         in if postName `S.member` allPostsSet -- take the trailing '/' off
                                            then Link t (postURL, title)
                                            else error (concat ["Unknown post referenced: ", postName, "\nPerhaps you meant:\n", intercalate "\n" (map ("    - " ++) (suggestNames 5 postName allPostsFixed))])
            | "image:" `isPrefixOf` url = let imgName = drop 6 url
                                          in Link t ("/images/" ++ imgName, title)
        interpostLinks (Image t (url, title))
            | "image:" `isPrefixOf` url = let imgName = drop 6 url
                                          in Image t ("/images/" ++ imgName, title)
        interpostLinks x = x

        collectImageAttributes inlines = filterM selectAttributes inlines
        selectAttributes (Strikeout [Str "CENTERED"]) = tell (S.singleton ImageCentered) >> return False
        selectAttributes (Strikeout [Str "FLOWED"]) = tell (S.singleton ImageFlow) >> return False
        selectAttributes (Strikeout [Str "ALIGNRIGHT"]) = tell (S.singleton ImageAlignRight) >> return False
        selectAttributes _ = return True

        useThumbnails (Image caption target) = let (url, title) = target
                                                   (urlFp, urlExt) = splitExtension url
                                                   url' = addExtension (urlFp ++ "-small") urlExt

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
                                                             img ! src (fromString url') ! alt (fromString compiledCaption)
                                                           p ! class_ "caption" $ preEscapedToHtml compiledCaption
                                               in RawInline (Format "html") (renderHtml raw)
        useThumbnails x = x
    in pandocCompilerWithTransform defaultHakyllReaderOptions writerOptions generateThumbnailsAndPostLinks

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
    projects <- sortBy (comparing projectPriority) <$>
                mapM (liftM mkProjectDescription . getMetadata) projectIdentifiers

    let travisContext = listField "globalTags" tagCtx tagsAsItems `mappend`
                        listField "projects" projectAsItem (mapM makeItem projects) `mappend`
                        defaultContext

        tagsAsItems = mapM tagAsItem
                      (take tagCount .
                       reverse .
                       sortBy (comparing (length . snd)) .
                       tagsMap $ tags)
        tagAsItem (tag, posts) = makeItem (tag, posts)

        tagCtx        = field "tag" (return . fst . itemBody) `mappend`
                        field "postCount" (return . show . length . snd . itemBody)

        projectDependencies = map IdentifierDependency projectIdentifiers

        pandocMathCompiler = pandocMathCompiler' posts

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" travisContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
          underlying <- getUnderlying
          myTags <- getTags underlying
          pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx `mappend` myTagsCtx myTags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx `mappend` travisContext)
            >>= relativizeUrls

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag

        route idRoute
        compile $ do
            list <- loadAll pattern
            let context = constField "title" title `mappend`
                          listField "posts" postCtx (return (sortBy (comparing (Down . itemIdentifier)) list))  `mappend`
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
            posts <- recentFirst =<< loadAll "posts/*"
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
            posts <- recentFirst =<< loadAll "posts/*"
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
      projectVersion  :: String,
      projectPriority :: Int,
      projectGithub   :: Maybe String
    } deriving Show

mkProjectDescription :: M.Map String String -> ProjectDescription
mkProjectDescription metadata =
    let Just projectTitle    = M.lookup "title"    metadata
        Just projectSlug     = M.lookup "slug"     metadata
        Just projectSummary  = M.lookup "summary"  metadata
        Just projectLanguage = M.lookup "language" metadata
        Just projectVersion  = M.lookup "version"  metadata
        projectPriority      = read . fromJust $ M.lookup "priority" metadata
        projectGithub        = M.lookup "github"  metadata
    in ProjectDescription { .. }

projectCtx :: ProjectDescription -> Context String
projectCtx (ProjectDescription { .. }) =
    constField "name" projectTitle `mappend`
    constField "slug" projectSlug `mappend`
    constField "summary" projectSummary `mappend`
    constField "version" projectVersion `mappend`
    constField "language" projectLanguage `mappend`
    maybe mempty (constField "github") projectGithub

projectAsItem :: Context ProjectDescription
projectAsItem =
    field "name" (return . projectTitle . itemBody) `mappend`
    field "slug" (return . projectSlug . itemBody) `mappend`
    field "summary" (return . projectSummary . itemBody) `mappend`
    field "version" (return . projectVersion . itemBody) `mappend`
    field "language" (return . projectLanguage . itemBody) `mappend`
    field "github" (maybe empty return . projectGithub . itemBody)

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

myTagsCtx :: [String] -> Context String
myTagsCtx tags = listField "myTags" simpleTagCtx (mapM makeItem tags)
  where simpleTagCtx = field "tag" (return . itemBody)

getProjects :: MonadMetadata m => m [Identifier]
getProjects = getMatches "projects/*.markdown"

--------------------------------------------------------------------------------
-- | Obtain tags from a page in an almost default way: parse them from the @tags@
-- metadata field, removing any surrounding quotes
getTags :: MonadMetadata m => Identifier -> m [String]
getTags identifier = do
    metadata <- getMetadata identifier
    return $ maybe [] (map trim . splitAll "," . unQuote) $ M.lookup "tags" metadata
    
-- Takes a string possibly wrapped in qutoes and removes them
unQuote :: String -> String
unQuote s = let s' = if Prelude.head s == '"' then Prelude.tail s else s
                s'' = if last s' == '"' then init s' else s'
            in s''
