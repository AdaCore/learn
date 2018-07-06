$( function() {

    // make carousel divs clickable
    $( ".slider-element" ).click( function () {
        window.open( $( this ).find( "a:first" ).attr( "href" ));
        return false;
    });

    // create slider
    $(".slider").slick({

        dots: true,
        infinite: true,
        slidesToShow: 1
/*
        responsive: [{

            breakpoint: 1024,
            settings: {
                slidesToShow: 3,
                infinite: true
            }

        }, {

            breakpoint: 600,
            settings: {
                slidesToShow: 2,
                dots: true
            }

        }, {

            breakpoint: 300,
            settings: "unslick" // destroys slick

        }]
        */
    });

});