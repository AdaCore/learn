// #if STAGING
import Cookies from 'js-cookie';
// #endif
import './styles/learn.scss';
import './ts/theme';
import {Carousel} from './ts/carousel';
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

    // #if STAGING
    if (!Cookies.get('AdaCore_staff')) {
      const msg = 'You have reached learn-staging, the learn testing site. ' +
      'This is reserved for testers only. You will be directed to the main ' +
      'learn.adacore.com site after pressing OK.';
      alert(msg);
      window.location.href = 'http://learn.adacore.com';
    }
    // #endif
  });
}());
