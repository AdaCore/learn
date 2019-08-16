import './styles/learn.scss';
import './ts/theme';
import {Carousel} from './ts/carousel';
import {Widget} from './ts/widget';
import './ts/scroll_to_top';


// widget entry point
$(document).ready(() => {
  $('div.widget_editor').each((index : number, element : HTMLElement) => {
    let exampleServer = $(element).attr('example_server');
    if (exampleServer) {
      const widget = new Widget($(element), exampleServer);
      widget.render();
    } else {
      exampleServer = '';
    }
  });

  const carousel : Carousel = new Carousel();
});
