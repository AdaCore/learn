$( function() {

    // make carousel divs clickable
    $( ".slider-element" ).click( function () {
        window.open( $( this ).find( "a:first" ).attr( "href" ));
        return false;
    });

    // create slider
    $('.responsiveCarousel').slick({
              dots: true,
              infinite: false,
              speed: 300,
              slidesToShow: 2,
              slidesToScroll: 1,
              autoplay: true,
              autoplaySpeed: 2000,
              responsive: [
                {
                  breakpoint: 968,
                  settings: {
                    slidesToShow: 1,
                    slidesToScroll: 1,
                    infinite: true,
                    dots: true
                  }
                },
                {
                  breakpoint: 768,
                  settings: {
                    slidesToShow: 2,
                    slidesToScroll: 2
                  }
                },
                {
                  breakpoint: 650,
                  settings: {
                    slidesToShow: 1,
                    slidesToScroll: 1
                  }
                }
                // You can unslick at a given breakpoint now by adding:
                // settings: "unslick"
                // instead of a settings object
              ]
            });

});