// Import testing libs
import {expect} from 'chai';

// Import package under test
import {Tabs, Button,
  ButtonGroup, CheckBox, ActionState} from '../../src/ts/components';

describe('Tabs', () => {
  const inTest = new Tabs();
  const parent = document.createElement('div');
  const name1 = 'My tab 1';
  const name2 = 'My tab 2';
  const content1 = document.createElement('div');
  content1.textContent = 'content 1';
  const content2 = document.createElement('div');
  content2.textContent = 'content 2';

  let flag1 = false;
  let flag2 = false;

  const header1 = inTest.addTab(name1, content1, () => {
    flag1 = true;
  });

  const header2 = inTest.addTab(name2, content2, () => {
    flag2 = true;
  });

  inTest.render(parent);

  const contentList = parent.querySelectorAll('div.tab-content');

  describe('#addTab()', () => {
    const c1 = contentList[0].querySelector('div');
    const c2 = contentList[1].querySelector('div');

    it('should have header1 with layout', () => {
      expect(header1).to.have.tagName('button');
      expect(header1).to.have.class('tab-links');
      expect(header1).to.have.text(name1);
    });

    it('should have header2 with layout', () => {
      expect(header2).to.have.tagName('button');
      expect(header2).to.have.class('tab-links');
      expect(header2).to.have.text(name2);
    });

    it('should have triggered click on header1', () => {
      expect(flag1).to.be.true;
      expect(flag2).to.be.false;
    });

    it('should have header 1 with active class', () => {
      expect(header1).to.have.class('active');
      expect(header2).not.to.have.class('active');
    });

    it('should have content1 shown and content2 hidden', () => {
      expect(parent).to.have.descendants('div.tab-content').and.have.length(2);
      expect(contentList[0]).to.have.class('active');
      expect(contentList[0]).to.have.descendants('div').and.have.length(1);
      expect(c1).to.have.text(content1.textContent);
      expect(contentList[0]).to.be.displayed;

      expect(contentList[1]).not.to.have.class('active');
      expect(contentList[1]).to.have.descendants('div').and.have.length(1);
      expect(c2).to.have.text(content2.textContent);
      expect(contentList[1]).not.to.be.displayed;
    });

    it('should have a cb on the header2 click', () => {
      header2.click();
      expect(flag2).to.be.true;

      expect(header1).not.to.have.class('active');
      expect(header2).to.have.class('active');
      expect(contentList[0]).not.to.have.class('active');
      expect(c1).to.have.text(content1.textContent);
      expect(contentList[0]).not.to.be.displayed;

      expect(contentList[1]).to.have.class('active');
      expect(c2).to.have.text(content2.textContent);
      expect(contentList[1]).to.be.displayed;
    });
  });

  describe('#show()', () => {
    it('should hide the tabs', () => {
      inTest.show(false);

      expect(header1).not.to.be.displayed;
      expect(header2).not.to.be.displayed;

      expect(contentList[0]).to.be.displayed;
      expect(contentList[1]).to.be.displayed;
    });

    it('should show the tabs', () => {
      inTest.show(true);

      expect(header1).to.be.displayed;
      expect(header2).to.be.displayed;
      expect(contentList[0]).not.to.be.displayed;
      expect(contentList[1]).to.be.displayed;
    });
  });
});

describe('Button', () => {
  const myBut = new Button(['myClass1', 'myClass2'], 'myBut', 'Click Me');
  const parent = myBut.render();

  describe('#constructor()', () => {
    it('should be a button with classes and attr', () => {
      expect(parent).to.have.tagName('button');
      expect(parent).to.have.attr('type', 'button');
      expect(parent).to.have.class('myClass1');
      expect(parent).to.have.class('myClass2');
      expect(parent).to.have.class('btn');
      expect(parent).to.have.class('btn-primary');
      expect(parent).to.have.attr('title', 'myBut');
      expect(parent).to.have.text('Click Me');
    });
  });

  describe('#registerEvent', () => {
    let flag = false;
    myBut.registerEvent('click', () => {
      flag = true;
    });

    it('should call my callback function when enabled', () => {
      myBut.enable();
      expect(myBut.getActionState()).to.equal(ActionState.Enabled);
      parent.click();
      expect(flag).to.be.true;
    });

    it('should not call my callback function when disbaled', () => {
      flag = false;
      myBut.disable();
      expect(myBut.getActionState()).to.equal(ActionState.Disabled);
      parent.click();
      expect(flag).to.be.false;
    });
  });
});

describe('ButtonGroup', () => {
  let inTest: ButtonGroup;

  beforeEach(() => {
    inTest =new ButtonGroup();
  });

  afterEach(() => {
    inTest = null;
  });

  describe('#render()', () => {
    it('should render a button group', () => {
      const parent = inTest.render();
      expect(parent).to.have.tagName('div');
      expect(parent).to.have.class('col-md-3');
    });
  });
  describe('#addButton()', () => {
    it('should add a button to the group with callback', () => {
      let flag = false;

      inTest.addButton(['test'], 'title', 'text', 'click', () => {
        flag = true;
      });

      const parent = inTest.render();

      expect(parent).to.have.descendants('button').and.have.length(1);
      const but = parent.querySelector('button');
      expect(but).to.have.class('test');
      expect(but).to.have.attr('title', 'title');
      expect(but).to.have.text('text');

      expect(flag).to.be.false;
      but.click();
      expect(flag).to.be.true;
    });

    it('should add two buttons to the group', () => {
      inTest.addButton(['test1'], 'title1', 'text1', 'click1', () => {
        return;
      });
      inTest.addButton(['test2'], 'title2', 'text2', 'click2', () => {
        return;
      });

      const parent = inTest.render();
      expect(parent).to.have.descendants('button').and.have.length(2);
    });

    it('should not allow two buttons to run at the same time', async () => {
      let flag1 = false;
      let flag2 = false;
      inTest.addButton(['test1'], 'title1', 'text1', 'click', async () => {
        setTimeout(() => {
          flag1 = true;
          expect(flag1).to.be.true;
          expect(flag2).to.be.false;
        }, 10);
      });
      inTest.addButton(['test2'], 'title2', 'text2', 'click', () => {
        flag2 = true;
      });
      const parent = inTest.render();
      const buttons = parent.querySelectorAll('button');
      expect(flag1).to.be.false;
      expect(flag2).to.be.false;
      buttons[0].click();
      buttons[1].click();
    });
  });

  describe('#length()', () => {
    it('should add one button to the group', () => {
      inTest.addButton(['test1'], 'title1', 'text1', 'click1', () => {
        return;
      });

      const parent = inTest.render();
      expect(parent).to.have.descendants('button').and.have.length(1);
      expect(inTest.length()).to.equal(1);
    });
    it('should add two buttons to the group', () => {
      inTest.addButton(['test1'], 'title1', 'text1', 'click1', () => {
        return;
      });
      inTest.addButton(['test2'], 'title2', 'text2', 'click2', () => {
        return;
      });

      const parent = inTest.render();
      expect(parent).to.have.descendants('button').and.have.length(2);
      expect(inTest.length()).to.equal(2);
    });
  });
});

describe('CheckBox', () => {
  describe('#constructor()', () => {
    it('should construct a simple checkbox', () => {
      const inTest = new CheckBox('label');
      const parent = inTest.render();
      const input = inTest.getCheckBox();

      expect(parent).to.have.tagName('div');
      expect(input).to.have.tagName('input');
      expect(input).to.have.attr('type', 'checkbox');
      expect(input).to.have.class('checkbox');
      expect(parent).to.contain(input);
      expect(input).not.to.have.attr('title');

      expect(parent).to.have.descendants('label').and.have.length(1);
      const label = parent.querySelector('label');
      expect(label).to.have.attr('for', input.id);
      expect(label).to.have.text('label');
    });

    it('should construct a complex checkbox', () => {
      const parent = document.createElement('div');
      const inTest = new CheckBox('label', parent, [
        'class1',
        'class2'],
      'my title');
      const input = inTest.getCheckBox();

      expect(parent).to.contain(input);
      expect(parent).to.have.class('class1');
      expect(parent).to.have.class('class2');
      expect(input).to.have.attr('title', 'my title');
    });
  });

  describe('#checked()', () => {
    const inTest = new CheckBox('label');
    const input = inTest.getCheckBox();

    it('should start unchecked', () => {
      expect(inTest.checked()).to.be.false;
    });

    it('should be true after check action', () => {
      input.checked = true;
      expect(inTest.checked()).to.be.true;
    });

    it('should be false after check action again', () => {
      input.checked = false;
      expect(inTest.checked()).to.be.false;
    });
  });

  describe('#setChecked()', () => {
    const inTest = new CheckBox('label');

    it('should be true after check action', () => {
      inTest.setChecked(true);
      expect(inTest.checked()).to.be.true;
    });

    it('should be false after check action again', () => {
      inTest.setChecked(false);
      expect(inTest.checked()).to.be.false;
    });
  });
});
