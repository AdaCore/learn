import $ from 'jquery';
import 'slick-carousel';

/** Class for carousel */
export class Carousel {
  private container: JQuery;

  /**
   * Constructs a carousel
   */
  constructor() {
    this.container = $('<div>')
        .addClass('responsive-carousel');
  }

  /**
   * Renders the carousel
   */
  public render(): void {
    // Get each top level node which will be a carousel if it has inside nodes
    $('div.content-blocks li.toctree-l1').each((i, elem) => {
      if ($(elem).has('ul').length > 0 ) {
        const name: string = $(elem).children('a').text();
        const type: string = $(elem).children('a').text().toLowerCase();

        $(elem).find('li.toctree-l2').each((j, subelem) => {
          const title: string = $(subelem).children('a').text();
          const link: string = $(subelem).children('a').attr('href');

          this.container.append(
              $('<div>').addClass(type).append(
                  $('<a>').attr('href', link).append(
                      $('<div>').addClass('inner').append([
                        $('<em>').text(name),
                        $('<strong>').text(title),
                        $('<p>'),
                      ])
                  )
              )
          );
        });
      }
    });

    // remove all elements inside top div
    $('div.content-blocks').empty().append(this.container);

    // get content descriptions from other pages and
    // populate description section of carousel divs
    $('div.responsive-carousel > div').each((i, elem) => {
      const link: string = $(elem).children('a').attr('href');
      const desc: JQuery = $(elem).find('p');
      desc.load(link + ' div.content-description.docutils.container', () => {
        desc.find('div')
            .removeClass('content-description docutils container')
            .addClass('carousel-description');
      });
    });

    // make carousel divs clickable
    $('.slider-element').on('click', (event: JQuery.ClickEvent) => {
      window.open(event.target.find('a:first').attr('href'));
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
            dots: true,
          },
        },
        {
          breakpoint: 768,
          settings: {
            slidesToShow: 2,
            slidesToScroll: 2,
          },
        },
        {
          breakpoint: 650,
          settings: {
            slidesToShow: 1,
            slidesToScroll: 1,
          },
        },
      ],
    });
  }
}
