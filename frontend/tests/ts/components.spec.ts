// Import testing libs
import {expect} from 'chai';
import 'mocha';

// Import package under test
import {Button} from 'ts/components';


describe('Button', () => {
  const myBut = new Button(['myClass'], 'myBut', 'Click Me');
  let flag = false;

  myBut.registerEvent('click', () => {
    flag = true;
  });

  it('should call my callback function and set my flag to true', () => {
    myBut.render().trigger('click');
    expect(flag).to.be.true;
  });

  it('should have myClass as an HTML class attribute', () => {
    expect(myBut.render().hasClass('myClass')).to.be.true;
  });
});
