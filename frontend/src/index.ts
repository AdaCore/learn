import './styles/learn.scss';
import {widgetFactory} from './ts/widget';
import {scrollTop} from './ts/scrolltop';

// #if STAGING
import {stagingRedirect} from './ts/staging';
// #endif

/**
 * Entrypoint
 *  The main entrypoint for the application
 */
function entrypoint(): void {
  const we = document.getElementsByClassName('widget_editor');
  widgetFactory(we);

  const btn = document.getElementById('scrollToTopBtn');
  scrollTop(btn as HTMLButtonElement);

  // #if STAGING
  stagingRedirect();
  // #endif
}

(function(): void {
  if (document.readyState != 'loading') {
    entrypoint();
  } else {
    document.addEventListener('DOMContentLoaded', entrypoint);
  }
}());
