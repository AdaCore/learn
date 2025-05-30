import Cookies from 'js-cookie';

/**
 * Redirects the user to main learn site if not AdaCore employee
 *
 * @export
 */
export function sandboxRedirect(): void {
  /* istanbul ignore next */
  if (!Cookies.get('AdaCore_staff')) {
    const msg = 'You have reached learn-sandbox, the learn testing site. ' +
    'This is reserved for testers only. You will be directed to the main ' +
    'learn.adacore.com site after pressing OK.';
    alert(msg);
    window.location.href = 'http://learn.adacore.com';
  }
}
