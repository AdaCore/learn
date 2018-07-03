$( function() {

    $( "div.content-blocks li.toctree-l1" ).each( function() {
        var type = $( this ).children( "a" ).text().toLowerCase();

        $( this ).find( "li.toctree-l2" ).each( function() {
            $( this ).addClass( type )
        });
    });

});