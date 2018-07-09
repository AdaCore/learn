$( function() {
    var new_node_string ="<div class='responsiveCarousel courses'>";

    // Get each top level node which will be a carousel if it has inside nodes
    $( "div.content-blocks li.toctree-l1" ).each( function() {
        if( $( this ).has ( "ul" ).length > 0 ) {
            var name = $( this ).children( "a" ).text();
            var type = $( this ).children( "a" ).text().toLowerCase();

            $( this ).find( "li.toctree-l2" ).each( function() {
                new_node_string += "<div class='" + type + "'>";
                
                var title = $( this ).children( "a" ).text();
                var link = $( this ).children( "a" ).attr( "href" );

                new_node_string += "<a href=" + link + ">";
                new_node_string += "<div class='inner'>"
                new_node_string += "<em>" + name + "</em>";
                new_node_string += "<strong>" + title + "</strong>";
                new_node_string += "<p></p>";
                new_node_string += "</div></a></div>";
            });
        }

        
    });

    new_node_string += "</div>";

    // remove all elements inside top div
    $( "div.content-blocks" ).empty();

    $( "div.content-blocks" ).append( new_node_string );

    // get content descriptions from other pages and populate description section of carousel divs
    $( "div.responsiveCarousel > div" ).each( function() {
        var link = $( this ).children( "a" ).attr( "href" );
        var desc = $( this ).find( "p" );
        desc.load( link + " div.content-description.docutils.container", function() {
            $( this ).find( "div" ).removeClass( "content-description docutils container" ).addClass( "carousel-description" );
        });
    });

});