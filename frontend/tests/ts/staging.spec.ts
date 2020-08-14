// Import testing libs
import chai, {expect} from 'chai';
import chaiDom from 'chai-dom';

chai.use(chaiDom);

import Cookies from 'js-cookie';

import {stagingRedirect} from '../../src/ts/staging';

describe('stagingRedirect()', () => {
  const jsdomAlert = window.alert;
  const startLoc = window.location.href;

  before(() => {
    window.alert = (msg): void => {
      console.log(msg);
    };
  });

  after(() => {
    window.alert = jsdomAlert;
  });

  Cookies.set('AdaCore_staff', 'true');
  it('should not redirect us with the cookie set', () => {
    stagingRedirect();
    expect(window.location.href).to.equal(startLoc);
  });

  // TODO: JSDOM doesn't support navigation
  // Cookies.remove('AdaCore_staff');
  // it('should redirect without the cookie set', () => {
  //   stagingRedirect();
  //   expect(window.location.href).to.equal('http://learn.adacore.com');
  // });
});
