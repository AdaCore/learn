import './styles/learn.scss';
import './ts/theme';
import {Carousel} from './ts/carousel';
import {ContactForm} from './ts/contact';
import {Widget, LabWidget} from './ts/widget';


(function(): void {
  $(window).on('scroll', () => {
    if ($(window).scrollTop() > 300) {
      $('#scrollToTopBtn').fadeIn('slow');
    } else {
      $('#scrollToTopBtn').fadeOut('slow');
    }
  });

  $('#scrollToTopBtn').on('click', () => {
    $('html, body').animate({
      scrollTop: 0,
    }, 300);
    return false;
  });

  // widget entry point
  // The on version doesn't work for some, probably, ridiculous reason...
  // $(document).on('ready', () => {
  $(document).ready(() => {
    $('div.widget_editor').each((index: number, element: HTMLElement) => {
      const exampleServer = $(element).attr('example_server');

      if (exampleServer) {
        const isLab = $(element).attr('lab');
        const widget =
          isLab ? new LabWidget($(element), exampleServer) :
            new Widget($(element), exampleServer);

        widget.render();
      } else {
        throw Error('Malformed widget! No server address specified.');
      }
    });

    // carousel entry point
    const carousel: Carousel = new Carousel();
    carousel.render();

    // contact form entry point
    $('div.contact-form').each((index: number, element: HTMLElement) => {
      const exampleServer = $(element).attr('example_server');

      if (exampleServer) {
        new ContactForm($(element), exampleServer);
      } else {
        throw Error('Malformed contact form. No server address specified.');
      }
    });
  });
}());
