import {Cookies} from 'typescript-cookies'

const cookies = new Cookies({
  path: '/',
  secure: true,
  samesite: 'none',
})

/**
 * Redirects the user to main learn site if not authenticated
 */
export function sandboxRedirect(): void {
  /* istanbul ignore next */
  const cookieName = "Learn_Sandbox_Authenticated";
  const cookieValue = cookies.get(cookieName) as string;
  const cookieReferenceValue = "true";

  if (cookieValue != cookieReferenceValue) {
    const passw = prompt("Enter site password:")

    if (passw != "Ada") {
      const msg = 'You have reached learn-sandbox, the learn testing site. ' +
      'This is reserved for testers only. You will be directed to the main ' +
      'learn.adacore.com site after pressing OK.';
      alert(msg);
      window.location.href = 'http://learn.adacore.com';
    }
    else
    {
      cookies.set(cookieName, cookieReferenceValue, {expires: 3650});
    }
  }
}
