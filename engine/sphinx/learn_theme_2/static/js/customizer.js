$( function() {
    var new_node_string ="";

    // Get each top level node which will be a carousel if it has inside nodes
    $( "div.content-blocks li.toctree-l1" ).each( function() {
        if( $( this ).has ( "ul" ).length > 0 ) {
            var name = $( this ).children( "a" ).text();
            var type = $( this ).children( "a" ).text().toLowerCase();

            new_node_string += "<h3>" + name + "</h3><section class='slider " + type + "'>";

            $( this ).find( "li.toctree-l2" ).each( function() {
                new_node_string += "<div class='slider-element'>";
                
                var title = $( this ).children( "a" ).text();
                var link = $( this ).children( "a" ).attr( "href" );

                new_node_string += "<a href=" + link + "></a>";
                new_node_string += "<span>" + title + "</span>";
                new_node_string += "</div>";
            });

            new_node_string += "</section>";
        }

        
    });


    // remove all elements inside top div
    $( "div.content-blocks" ).empty();

    $( "div.content-blocks" ).append( new_node_string );

});