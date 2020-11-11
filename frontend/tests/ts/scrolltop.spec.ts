// Import testing libs
import chai, {expect} from 'chai';
import chaiDom from 'chai-dom';

chai.use(chaiDom);

import {scrollTop} from '../../src/ts/scrolltop';

function triggerEvent(element: Window, eventName: string): void {
  var event = document.createEvent("HTMLEvents");
  event.initEvent(eventName, false, true);
  element.dispatchEvent(event);
}

function scrollTo (x: number, y: number): void {
  document.body.scrollTop = y;
  triggerEvent(window, 'scroll');
}

describe('scrollTop()', () => {
  let btn: HTMLButtonElement;

  before(() => {
    btn = document.createElement('button') as HTMLButtonElement;
    scrollTop(btn);

    // override window.scrollTo because JSDOM doesn't have it
    window.scrollTo = scrollTo as any;
  });

  it('should have the btn hidden to start', () => {
    expect(btn).to.have.class('hide');
    expect(btn).not.to.have.class('show');
  });

  it('should start at scrollTop = 0', () => {
    expect(document.body.scrollTop).to.equal(0);
  });

  it('should show the btn after scrolling below threshold', () => {
    window.scrollTo(0, 500);
    expect(document.body.scrollTop).to.equal(500);
    expect(btn).to.have.class('show');
    expect(btn).not.to.have.class('hide');
  });

  it('should scroll back to the top after btn click', () => {
    btn.click();
    expect(document.body.scrollTop).to.equal(0);
  });
});
