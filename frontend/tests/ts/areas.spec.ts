// Import testing libs
import chai, {expect} from 'chai';
import chaiDom from 'chai-dom';
chai.use(chaiDom);

// Import package under test
import {Area, OutputArea, LabArea, makeLabArea, LabContainer}
  from '../../src/ts/areas.ts';
import * as Strings from '../../src/ts/strings.ts';
import {CheckOutput} from '../../src/ts/server-types.ts';

describe('Area', () => {
  let parent: HTMLDivElement;
  let inTest: Area;

  describe('#add()', () => {
    let testdiv: HTMLDivElement;
    const classList = ['classA', 'classB'];
    const lineText = 'Line Text';

    let flag = false;

    before(() => {
      parent = document.createElement('div');
      inTest = new Area(parent);
      inTest.add(classList, lineText, () => {
        flag = true;
      });
      testdiv = parent.querySelector('div');
    });

    after(() => {
      parent = null;
      inTest = null;
    });

    it('should add a single area to the container', () => {
      expect(parent).to.have.descendants('div').and.have.length(1);
    });

    it('should have the classes and text specified', () => {
      for (const c of classList) {
        expect(testdiv).to.have.class(c);
      }
      expect(testdiv).to.have.text(lineText);
    });

    it('should be a clickable div', () => {
      expect(flag).to.be.false;
      testdiv.click();
      expect(flag).to.be.true;
    });
  });

  describe('#reset()', () => {
    before(() => {
      parent = document.createElement('div');
      inTest = new Area(parent);
      inTest.add([], 'test area 1');
      inTest.add([], 'test area 2');
    });

    after(() => {
      parent = null;
      inTest = null;
    });


    it('should have two areas in the container', () => {
      expect(parent).to.have.descendants('div').and.have.length(2);
    });

    it('should have 0 areas in the container after reset', () => {
      inTest.reset();
      expect(parent).not.to.have.descendants('div');
    });
  });

  describe('#addConsole()', () => {
    const text = 'my text';
    before(() => {
      parent = document.createElement('div');
      inTest = new Area(parent);
      inTest.addConsole(text);
    });

    after(() => {
      parent = null;
      inTest = null;
    });

    it('should have text that resembles console output', () => {
      expect(parent).to.have.descendants('div.output_console').
          and.have.length(1);
      const testdiv = parent.querySelector('div.output_console');
      expect(testdiv).to.have.text('$ ' + text);
    });
  });

  describe('#addInfo()', () => {
    const text = 'my text';
    before(() => {
      parent = document.createElement('div');
      inTest = new Area(parent);
      inTest.addInfo(text);
    });

    after(() => {
      parent = null;
      inTest = null;
    });

    it('should have specified text', () => {
      expect(parent).to.have.descendants('div.output_msg_info').
          and.have.length(1);
      const testdiv = parent.querySelector('div.output_msg_info');
      expect(testdiv).to.have.text(text);
    });
  });

  describe('#addMsg()', () => {
    const text = 'my text';
    before(() => {
      parent = document.createElement('div');
      inTest = new Area(parent);
      inTest.addMsg(text);
    });

    after(() => {
      parent = null;
      inTest = null;
    });

    it('should have specified text', () => {
      expect(parent).to.have.descendants('div.output_msg').and.have.length(1);
      const testdiv = parent.querySelector('div.output_msg');
      expect(testdiv).to.have.text(text);
    });
  });

  describe('#addLine()', () => {
    const text = 'my text';
    before(() => {
      parent = document.createElement('div');
      inTest = new Area(parent);
      inTest.addLine(text);
    });

    after(() => {
      parent = null;
      inTest = null;
    });

    it('should have specified text', () => {
      expect(parent).to.have.descendants('div.output_line').and.have.length(1);
      const testdiv = parent.querySelector('div.output_line');
      expect(testdiv).to.have.text(text);
    });
  });
});

describe('OutputArea()', () => {
  let parent: HTMLDivElement;
  let inTest: OutputArea;

  beforeEach(() => {
    parent = document.createElement('div');
    inTest = new OutputArea(parent);
  });

  afterEach(() => {
    parent = null;
    inTest = null;
  });

  describe('#addError()', () => {
    const msg = 'my message';

    it('should add an error to the output area', () => {
      inTest.addError(msg);
      expect(parent).to.have.descendants('div.output_error').and.have.length(1);
      const testdiv = parent.querySelector('div.output_error');
      expect(testdiv).to.have.text(msg);
    });
  });

  describe('#addLabStatus()', () => {
    it('should add a pass lab status to the area', () => {
      inTest.addLabStatus(true);

      expect(parent).to.have.descendants('div.lab_status').and.have.length(1);
      const testdiv = parent.querySelector('div.lab_status');
      expect(testdiv).to.have.text(Strings.LAB_COMPLETE_LABEL);
    });

    it('should add a fail lab status to the area', () => {
      inTest.addLabStatus(false);

      expect(parent).to.have.descendants('div.lab_status').and.have.length(1);
      const testdiv = parent.querySelector('div.lab_status');
      expect(testdiv).to.have.text(Strings.LAB_FAILED_LABEL);
    });
  });

  describe('#showSpinner()', () => {
    it('should not remove anything yet', () => {
      expect(parent).not.to.have.descendants('div.spinner');

      inTest.showSpinner(false);
      expect(parent).not.to.have.descendants('div.spinner');
    });

    it('should add a spinner to the container', () => {
      inTest.showSpinner(true);
      expect(parent).to.have.descendants('div.spinner').and.have.length(1);
    });

    it('should remove a spinner from the container', () => {
      inTest.showSpinner(false);
      expect(parent).not.to.have.descendants('div.spinner');
    });
  });
});

describe('LabArea', () => {
  const ref = 5;
  let wrapper: HTMLDivElement;
  let inTest: LabArea;
  describe('#constructor()', () => {
    let container: HTMLDivElement;
    let button: HTMLButtonElement;

    before(() => {
      inTest = makeLabArea(ref);
      wrapper = inTest.render();
      container = wrapper.querySelector('div.lab_test_case');
      button = wrapper.querySelector('button.accordion');
    });

    after(() => {
      wrapper = null;
      inTest = null;
    });

    it('should have classes and a container and button with classes', () => {
      expect(wrapper).to.have.tagName('div');
      expect(wrapper).to.have.descendants('div.lab_test_case').
          and.have.length(1);
      expect(wrapper).to.have.class('acc_wrapper');

      expect(wrapper).to.have.descendants('button.accordion').
          and.have.length(1);
    });

    it('should have a button wo active and container hidden', () => {
      expect(button).not.to.have.class('active');
      expect(container).not.to.be.displayed;
    });

    it('should have a button with text', () => {
      expect(button).to.have.descendants('span').and.have.length(1);
      const span = button.querySelector('span');
      expect(span).to.have.text(Strings.TEST_CASE_LABEL + ' #' + ref);
    });

    it('should have a button that adds active and shows container', () => {
      (button as HTMLElement).click();
      expect(button).to.have.class('active');
      expect(container).to.be.displayed;
    });

    it('should remove active and hide container when clicked again', () => {
      (button as HTMLElement).click();
      expect(button).not.to.have.class('active');
      expect(container).not.to.be.displayed;
    });
  });

  describe('#addResults()', () => {
    const passResult = {
      status: 'Success',
      out: 'output',
      actual: 'actual',
      in: 'input',
    };

    const failResult = {
      status: 'Fail',
      out: 'output',
      actual: 'actual',
      in: 'input',
    };

    beforeEach(() => {
      inTest = makeLabArea(ref);
      wrapper = inTest.render();
    });

    afterEach(() => {
      wrapper = null;
      inTest = null;
    });

    it('should add success class and have 1 result div', () => {
      inTest.addResults(passResult);
      const container = wrapper.querySelector('div.lab_test_case');
      const button = wrapper.querySelector('button.accordion');
      expect(button).to.have.class('lab_test_success');
      expect(container).to.have.class('lab_test_success');
      expect(container).to.have.descendants('div.lab_results').
          and.have.length(1);
    });

    it('should add fail classes to button and container', () => {
      inTest.addResults(failResult);
      const container = wrapper.querySelector('div.lab_test_case');
      const button = wrapper.querySelector('button.accordion');
      expect(button).to.have.class('lab_test_failed');
      expect(container).to.have.class('lab_test_failed');
    });

    it('should have the input results in the container', () => {
      inTest.addResults(passResult);
      const container = wrapper.querySelector('div.lab_test_case');
      expect(container).to.have.descendants('div.lab_results').
          and.have.length(1);
      const cased = container.querySelector('div.lab_results');
      expect(cased).to.have.descendants('div.lab_test_msg.lab_test_input').
          and.have.length(1);
      const label = cased.querySelector('div.lab_test_msg.lab_test_input');
      expect(label).to.have.descendants('span.lab_test_msg_title').
          and.have.length(1);
      const labelMsg = label.querySelector('span.lab_test_msg_title');
      expect(labelMsg).to.have.text(Strings.LAB_TEST_INPUT_LABEL + ':');
      expect(label).to.have.descendants('code').and.have.length(1);
      const labelCode = label.querySelector('code');
      expect(labelCode).to.have.text(passResult.in);
    });

    it('should have the output results in the container', () => {
      inTest.addResults(passResult);
      const container = wrapper.querySelector('div.lab_test_case');
      expect(container).to.have.descendants('div.lab_results').
          and.have.length(1);
      const cased = container.querySelector('div.lab_results');
      expect(cased).to.have.descendants('div.lab_test_msg.lab_test_output').
          and.have.length(1);
      const label = cased.querySelector('div.lab_test_msg.lab_test_output');
      expect(label).to.have.descendants('span.lab_test_msg_title').
          and.have.length(1);
      const labelMsg = label.querySelector('span.lab_test_msg_title');
      expect(labelMsg).to.have.text(Strings.LAB_TEST_OUTPUT_LABEL + ':');
      expect(label).to.have.descendants('code').and.have.length(1);
      const labelCode = label.querySelector('code');
      expect(labelCode).to.have.text(passResult.out);
    });

    it('should have the actual results in the container', () => {
      inTest.addResults(passResult);
      const container = wrapper.querySelector('div.lab_test_case');
      expect(container).to.have.descendants('div.lab_results').
          and.have.length(1);
      const cased = container.querySelector('div.lab_results');
      expect(cased).to.have.descendants('div.lab_test_msg.lab_test_actual').
          and.have.length(1);
      const label = cased.querySelector('div.lab_test_msg.lab_test_actual');
      expect(label).to.have.descendants('span.lab_test_msg_title').
          and.have.length(1);
      const labelMsg = label.querySelector('span.lab_test_msg_title');
      expect(labelMsg).to.have.text(Strings.LAB_TEST_ACTUAL_LABEL + ':');
      expect(label).to.have.descendants('code').and.have.length(1);
      const labelCode = label.querySelector('code');
      expect(labelCode).to.have.text(passResult.actual);
    });

    it('should have the status results in the container', () => {
      inTest.addResults(passResult);
      const container = wrapper.querySelector('div.lab_test_case');
      expect(container).to.have.descendants('div.lab_results').
          and.have.length(1);
      const cased = container.querySelector('div.lab_results');
      expect(cased).to.have.descendants('div.lab_test_msg.lab_test_status').
          and.have.length(1);
      const label = cased.querySelector('div.lab_test_msg.lab_test_status');
      expect(label).to.have.descendants('span.lab_test_msg_title').
          and.have.length(1);
      const labelMsg = label.querySelector('span.lab_test_msg_title');
      expect(labelMsg).to.have.text(Strings.LAB_TEST_STATUS_LABEL + ':');
      expect(label).to.have.descendants('code').and.have.length(1);
      const labelCode = label.querySelector('code');
      expect(labelCode).to.have.text(passResult.status);
    });
  });

  describe('#getRef()', () => {
    before(() => {
      inTest = makeLabArea(ref);
      wrapper = inTest.render();
    });

    after(() => {
      wrapper = null;
      inTest = null;
    });

    it('should return ref', () => {
      expect(inTest.getRef()).to.be.equal(ref);
    });
  });
});

describe('LabContainer', () => {
  describe('#getLabArea()', () => {
    const parent = document.createElement('div');
    const inTest = new LabContainer(parent);
    const labArea1 = inTest.getLabArea(1);
    const labArea2 = inTest.getLabArea(2);

    it('should create a new lab area for a new ref', () => {
      expect(labArea1).to.not.equal(labArea2);
    });

    it('should return the already created lab area with the same ref', () => {
      const tempArea1 = inTest.getLabArea(1);
      const tempArea2 = inTest.getLabArea(2);

      expect(tempArea1).to.equal(labArea1);
      expect(tempArea2).to.equal(labArea2);
    });
  });

  describe('#processResults()', () => {
    const parent = document.createElement('div');
    const inTest = new LabContainer(parent);
    const success = true;
    const testCases: CheckOutput.LabOutput = {
      success: success,
      cases: [
        {
          '1': {
            status: 'Success',
            out: 'output1',
            actual: 'actual1',
            in: 'input1',
          },
        },
        {
          '2': {
            status: 'Fail',
            out: 'output2',
            actual: 'actual2',
            in: 'input2',
          },
        },
      ],
    };

    it('should return the test status', () => {
      expect(inTest.processResults(testCases)).to.be.true;
    });
  });

  describe('#reset()', () => {
    const parent = document.createElement('div');
    const inTest = new LabContainer(parent);
    const testCases: CheckOutput.LabOutput = {
      success: false,
      cases: [
        {
          '1': {
            status: 'Success',
            out: 'output1',
            actual: 'actual1',
            in: 'input1',
          },
        },
        {
          '2': {
            status: 'Fail',
            out: 'output2',
            actual: 'actual2',
            in: 'input2',
          },
        },
      ],
    };

    it('should start empty', () => {
      expect(parent).to.be.empty;
    });

    it('should not be empty after adding results', () => {
      inTest.processResults(testCases);
      inTest.sort();

      expect(parent).not.to.be.empty;
    });

    it('should be empty after reset', () => {
      inTest.reset();
      expect(parent).to.be.empty;
    });
  });
});
