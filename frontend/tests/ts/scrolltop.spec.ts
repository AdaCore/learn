// Import testing libs
import chai, {expect} from 'chai';
chai.use(require('chai-dom'));

import {scrollTop} from '../../src/ts/scrolltop';

describe('scrollTop()', () => {
  const btn = document.createElement('button') as HTMLButtonElement;
  scrollTop(btn);

  it('should have the btn hidden to start', () => {
    expect(btn).to.have.class('hide');
    expect(btn).not.to.have.class('show');
  });

  it('should start at scrollTop = 0', () => {
    expect(document.body.scrollTop).to.equal(0);
  });

  // TODO: JSDOM can't do scrolling events currently and we cannot test this
  // it('should show the btn after scrolling below threshold', () => {
  //   window.scrollTo(0, 500);
  //   expect(document.body.scrollTop).to.equal(500);
  //   expect(btn).to.have.class('show');
  //   expect(btn).not.to.have.class('hide');
  // });

  it('should scroll back to the top after btn click', () => {
    btn.click();
    expect(document.body.scrollTop).to.equal(0);
  });
});