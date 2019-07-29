import 'slick-carousel';

$(function() {

  const carouselNode = $('<div>').addClass('responsive-carousel');

  // Get each top level node which will be a carousel if it has inside nodes
  $('div.content-blocks li.toctree-l1').each(function() {
    if($(this).has('ul').length > 0 ) {
      const name = $(this).children('a').text();
      const type = $(this).children('a').text().toLowerCase();

      $(this).find('li.toctree-l2').each(function() {
        const title = $(this).children('a').text();
        const link = $(this).children('a').attr('href');

        carouselNode.append(
          $('<div>').addClass(type).append(
            $('<a>').attr('href', link).append(
              $('<div>').addClass('inner').append([
                $('<em>').text(name),
                $('<strong>').text(title),
                $('<p>')
              ])
            )
          )
        );
      });
    }
  });


  // remove all elements inside top div
  $('div.content-blocks').empty().append(carouselNode);


  // get content descriptions from other pages and populate description section of carousel divs
  $('div.responsive-carousel > div').each(function() {
      var link = $(this).children('a').attr('href');
      var desc = $(this).find('p');
      desc.load(link + ' div.content-description.docutils.container', function() {
        $(this).find('div').removeClass('content-description docutils container').addClass('carousel-description');
      });
  });

  // make carousel divs clickable
  $('.slider-element').click(function () {
    window.open( $(this).find('a:first').attr('href'));
    return false;
  });

  // create slider
  $('.responsive-carousel').slick({
    dots: true,
    infinite: false,
    speed: 300,
    slidesToShow: 2,
    slidesToScroll: 1,
    autoplay: false,
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
    ]
  });
});
