import './styles/learn.scss';
import './ts/theme';
import {Carousel} from './ts/carousel';
import {Widget} from './ts/widget';


(function(): void {
  $(window).scroll(function() {
    if ($(window).scrollTop() > 300) {
      $('#scrollToTopBtn').fadeIn('slow');
    } else {
      $('#scrollToTopBtn').fadeOut('slow');
    }
  });

  $('#scrollToTopBtn').click(() => {
    $('html, body').animate({
      scrollTop: 0,
    }, 300);
    return false;
  });

  // widget entry point
  $(document).ready(() => {
    $('div.widget_editor').each((index: number, element: HTMLElement) => {
      let exampleServer = $(element).attr('example_server');
      if (exampleServer) {
        const widget = new Widget($(element), exampleServer);
        widget.render();
      } else {
        exampleServer = '';
      }
    });

    // carousel entry point
    const carousel: Carousel = new Carousel();
    carousel.render();
  });
}());
