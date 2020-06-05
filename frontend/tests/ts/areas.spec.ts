// Import testing libs
import {expect} from 'chai';
import 'mocha';

// Import package under test
import {Area} from 'ts/areas';

/**
 * Mock Area for testing
 *
 * @class MockArea
 * @extends {Area}
 */
class MockArea extends Area {
  /**
   * Creates an instance of MockArea.
   */
  constructor() {
    super();

    this.container = $('<div>');
  }

  /**
   * Implements the abstract render function
   *
   * @return {JQuery}
   */
  public render(): JQuery {
    return this.container;
  }
}

describe('Area', () => {
  describe('#add', () => {
    const classList = ['classA', 'classB'];
    const lineText = 'Line Text';
    const inTest = new MockArea();

    let flag = false;

    inTest.add(classList, lineText, () => {
      flag = true;
    });

    const obj = inTest.render().children('div');
    it('should add a single area to the container', () => {
      expect(obj.length).to.equal(1);
    });

    it('should have the classes and text specified', () => {
      expect(obj.attr('class').split(/\s+/)).to.deep.equal(classList);
      expect(obj.text()).to.equal(lineText);
    });

    it('should be a clickable div', () => {
      expect(flag).to.be.false;
      obj.trigger('click');
      expect(flag).to.be.true;
    });
  });

  describe('#reset', () => {
    const inTest = new MockArea();
    inTest.add([], 'test area 1');
    inTest.add([], 'test area 2');

    inTest.errorCount = 4;

    it('should have two areas in the container', () => {
      const obj = inTest.render().children('div');
      expect(obj.length).to.equal(2);
    });

    it('should have 0 areas in the container after reset and 0 errors', () => {
      inTest.reset();
      const obj = inTest.render().children('div');
      expect(obj.length).to.equal(0);
      expect(inTest.errorCount).to.equal(0);
    });
  });

  // describe('#addConsole', () => {
  //   const text = 'console text';
  //   const inTest = new MockArea();
  //   inTest.addConsole(text, )
  // });
});
