import './styles/learn.scss';
import './ts/theme';
import './ts/carousel';
import {Widget} from './ts/widget';
import './ts/scroll_to_top';
import './ts/theme';

// widget entry point
$(document).ready(() => {
  $('<div>').each((index : number, element : HTMLElement) => {
    let exampleServer = $(this).attr('example_server');
    if (exampleServer) {
      const widget = new Widget($(element), exampleServer);
      widget.render();
    } else {
      exampleServer = '';
    }
  });
});
