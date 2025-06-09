import './styles/learn.scss';
import {getElemsByClass} from './ts/dom-utils';
import {widgetFactory} from './ts/widget';

// #if SANDBOX
import {sandboxRedirect} from './ts/sandbox-redirect';
// #endif

/**
 * Entrypoint
 *  The main entrypoint for the application
 */
function entrypoint(): void {
  // get list of all widgets on the page
  const we = getElemsByClass(document, 'widget');

  widgetFactory(we as Array<HTMLDivElement>);

  // #if SANDBOX
  // This is used to redirect non AdaCore staff to the main site if
  //  the staging site is accidentally reached
  sandboxRedirect();
  // #endif
}

(function(): void {
  if (document.readyState != 'loading') {
    entrypoint();
  } else {
    document.addEventListener('DOMContentLoaded', entrypoint);
  }
}());
