// #if STAGING
import Cookies from 'js-cookie';
// #endif
import './styles/learn.scss';
import {Widget, LabWidget} from './ts/widget';

/**
 * Entrypoint
 *  The main entrypoint for the application
 */
function entrypoint(): void {
  const we = document.getElementsByClassName('widget_editor');

  for (let i = 0; i < we.length; i++) {
    const element = we[i];
    const exampleServer = element.getAttribute('example_server');

    if (exampleServer) {
      const isLab = element.getAttribute('lab');
      const widget =
        isLab ? new LabWidget((element as HTMLElement), exampleServer) :
          new Widget((element as HTMLElement), exampleServer);

      widget.render();
    } else {
      throw Error('Malformed widget! No server address specified.');
    }
  }

  const btn = document.getElementById('scrollToTopBtn');
  window.addEventListener('scroll', () => {
    if (document.body.scrollTop > 300) {
      btn.classList.remove('hide');
      btn.classList.add('show');
    } else {
      btn.classList.remove('show');
      btn.classList.add('hide');
    }
  });

  btn.addEventListener('click', () => {
    window.scrollTo(0, 0);
  });

  // #if STAGING
  if (!Cookies.get('AdaCore_staff')) {
    const msg = 'You have reached learn-staging, the learn testing site. ' +
    'This is reserved for testers only. You will be directed to the main ' +
    'learn.adacore.com site after pressing OK.';
    alert(msg);
    window.location.href = 'http://learn.adacore.com';
  }
  // #endif
}

(function(): void {
  if (document.readyState != 'loading') {
    entrypoint();
  } else {
    document.addEventListener('DOMContentLoaded', entrypoint);
  }
}());
