// Import testing libs
import { expect } from 'chai';
import 'mocha';

// Import package under test
import * as comp from '../../src/ts/components.ts';


describe('Button', () => {

  const myBut = new comp.Button(['myClass'], 'myBut', 'Click Me');
  let flag = false;

  myBut.registerEvent('click', (event: JQuery.ClickEvent) => {
    flag = true;
  });

  it('should call my callback function and set my flag to true', () => {
    myBut.render().trigger("click");
    expect(flag).to.equal(true);
  });

  it('should have myClass as an HTML class attribute', () => {
    expect(myBut.render().hasClass('myClass')).to.equal(true);
  });
});