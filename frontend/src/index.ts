import './styles/learn.scss';
import './styles/learn-dark.scss';
import {getElemsByClass, getElemById} from './ts/dom-utils';
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
  // get list of all widgets on the page
  const we = getElemsByClass(document, 'widget');

  widgetFactory(we as Array<HTMLDivElement>);

  // register scroll to top btn functionality
  const btn = getElemById('scrollToTopBtn');
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
