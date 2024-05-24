var figures = [];
var curFigure = -1;

var images = [];

var isHandheld = false;

var headerWidth = 280,
    headerRightSpace = 20,
    contentMinWidth = 300,
    contentPaddingLeft = 75,
    normalBodyPadding = 190;

var siteMinWidth = headerWidth - headerRightSpace + contentMinWidth + contentPaddingLeft + normalBodyPadding * 2;

$(document.body).ready(function () {
    $(".gallery").each(function () {
        var that = $(this);
        $(this).imagesLoaded(function () {
            that.isotope({itemSelector: ".figure",
                          layoutMode: "masonry",
                          masonry: { gutter: 10 }});
        });
    });

    figures = $("div.figure").get();
    $("div.figure").each(function (i) {
        var a = $(this).children("a");
        var origImg = a.attr("href");
        var caption = $(this).children("p.caption").html();
        a.attr("href", "#");
        images.push(new PreloadingImage(origImg));
        a.click(function (e) {
            e.preventDefault();
            makeOverlay(i);
        });
    });

    $("#gallery-prev").css("opacity", "0")
        .click(function (e) { e.preventDefault(); updateOverlay(Math.max(0, curFigure - 1)); })
        .hover(function () {
            if (curFigure != 0)
                $(this).fadeTo(100, 0.6);
        }, function () {
                $(this).fadeTo(100, 0);
        });
    $("#gallery-next").css("opacity", "0")
        .click(function (e) { e.preventDefault(); updateOverlay(Math.min(figures.length - 1, curFigure + 1)); })
        .hover(function () {
            if (curFigure != figures.length - 1)
                $(this).fadeTo(100, 0.6);
        }, function () {
                $(this).fadeTo(100, 0);
        });
    $("#gallery-modal").on($.modal.CLOSE, function () { curFigure = -1; });

    $("#gallery-image,#gallery-caption,.modal").click(function (e) { if (isHandheld) { e.preventDefault(); $.modal.close(); } });

    $("#show-header").click( /* On  phones, the header is hidden. This displays it */
        function (e)
        {
            $("#header").css("height", $(window).height() - $("#mini-header-bar").height());
            $("#header").css("margin-top", "");
            if ( $("#header").css("display") == "block" ) {
                $("#header").slideUp(150);
            } else {
                $("#header").slideDown(150);
            }
            e.preventDefault();
        });

    // $(window).scroll(function () {
    //     if ( $("#header").css("position") != "fixed" ) {
    //         var height = $(document).height() + $("#header").height();
    //         $("#header").css("margin-top", Math.min(Math.max(0, $(window).scrollTop()), height) + "px");
    //     }
    // });
    $(window).resize(function () {
        if ( $(window).width() < siteMinWidth ) {
            $("#header").css("height", $(window).height() - $("#mini-header-bar").height());
        } else {
            $("#header").removeAttr("style");
        }
    });
    if ( /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent) )
        isHandheld = true;
    else if ( $("#header").css("position") == "fixed" )
        isHandheld = true;
});

function updateOverlay(i)
{
    if (i == curFigure)
        return;

    if ( curFigure >= 0 )
        images[curFigure].unset();

    var img = $("#gallery-image");

    var figure = $(figures[i]);
    var captionHTML = figure.children("p.caption").html();

    images[i].setAsGallery();

    $("#gallery-caption #caption-text").html(captionHTML);
    $("#gallery-caption #figure-number").html(i + 1);

    if (i == 0)
        $("#gallery-prev").css("cursor", "default");
    else
        $("#gallery-prev").css("cursor", "pointer");

    if (i == figures.length - 1)
        $("#gallery-next").css("cursor", "default");
    else
        $("#gallery-next").css("cursor", "pointer");

    curFigure = i;

}

function makeOverlay(i)
{
    updateOverlay(i);
    var opts = { fadeDuration: 150 };
    if ( isHandheld )
        opts.opacity = 1;
    $("#gallery-modal").modal( opts );

    curFigure = i;
}

function PreloadingImage(origImg)
{
    var that = this;
    this.origImg = origImg;
    this.loaded = false;
    this.img = new Image();
    this.width = 100;
    this.height = 100;
    this.whenLoaded = false;

    this.img.onload = function () {
        that.loaded = true;
        that.width = this.naturalWidth;
        that.height = this.naturalHeight;
        if ( this.whenLoaded )
            this.whenLoaded();
    };

    this.img.src = origImg;

    this.resizeFigure = function (width, height)
    {
        var winHeight = $(window).height() - (isHandheld ? 30 : 150);
        var winWidth = $(window).width() - (isHandheld ? 30 : 150);

        var ratio = Math.min(winWidth/width, winHeight/height, 1);
        width = width * ratio;
        height = height * ratio;
        console.log(ratio);

        $("#gallery-image").css({height: height + "px", width: width + "px"});
        $("#gallery-next,#gallery-prev").css("height", height + "px");
        $("#gallery-caption").css(isHandheld ? "width" : "max-width", width - 10 + "px");
        if ( isHandheld ) {
            var realWinHeight = $(window).height();
            var realWinWidth = $(window).width();
            var borderVertical = realWinHeight - height;
            var borderHorizontal = realWinWidth - width;
            $("#gallery-caption").css("bottom", (borderVertical / 2) + "px");
            $(".modal").css({"padding-left": (borderHorizontal/2) + "px",
                             "padding-right": (borderHorizontal/2) + "px",
                             "padding-top": (borderVertical/2) + "px",
                             "padding-bottom": (borderVertical/2) + "px",
                             width: (realWinWidth - borderHorizontal) + "px",
                             height: (realWinHeight - borderVertical) + "px"});
        } else $.modal.resize();
    };

    this.unset = function () {
        if ( this.whenLoaded )
            this.whenLoaded = false;
    };

    this.setAsGallery = function () {
        if ( this.loaded ) {
            $("#gallery-image").attr("src", this.origImg);
            this.resizeFigure(this.width, this.height);
        } else {
            this.whenLoaded = this.setAsGallery;
            $("#gallery-image").attr("src", "/images/stock/spinner.gif");
            this.resizeFigure(this.width, this.height)
        }
    }
}
