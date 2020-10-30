import './styles/learn.scss';
import {widgetFactory, WidgetMap} from './ts/widget';
import {scrollTop} from './ts/scrolltop';

// #if STAGING
import {stagingRedirect} from './ts/staging';
// #endif

/**
 * This allows us to attach variables to the global window
 */
declare global {
  interface Window {
    widgetMap: WidgetMap;
  }
}

/**
 * Entrypoint
 *  The main entrypoint for the application
 */
function entrypoint(): void {
  // get list of all widgets on the page
  const we = document.getElementsByClassName('widget_editor');
  // store WidgetMap on window to keep reference alive
  window.widgetMap = widgetFactory(we);

  // register scroll to top btn functionality
  const btn = document.getElementById('scrollToTopBtn');
  scrollTop(btn as HTMLButtonElement);

  // #if STAGING
  // This is used to redirect non AdaCore staff to the main site if
  //  the staging site is accidentally reached
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
