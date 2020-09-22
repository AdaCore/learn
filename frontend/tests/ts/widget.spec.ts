// Import testing libs
import chai, {expect} from 'chai';
import chaiDom from 'chai-dom';
import chaiAsPromised from 'chai-as-promised';

chai.use(chaiDom);
chai.use(chaiAsPromised);

import ace from 'brace';

import fetchMock from 'fetch-mock';
import {OutputArea} from '../../src/ts/areas';
import * as Strings from '../../src/ts/strings';

import {widgetFactory, Widget, LabWidget} from '../../src/ts/widget';
import {delay} from '../../src/ts/utilities';

describe('widgetFactory()', () => {
  let htmlList: HTMLCollectionOf<Element>;
  let inTestList: Array<Widget | LabWidget>;
  let pageWidget: HTMLElement;
  let file: HTMLElement;

  beforeEach(() => {
    pageWidget = document.createElement('div');
    pageWidget.classList.add('widget_editor');
    pageWidget.setAttribute('example_server', 'http://example.com');
    pageWidget.setAttribute('run_button', 'True');
    pageWidget.setAttribute('inline', 'True');
    document.body.appendChild(pageWidget);

    const shadowFile = document.createElement('div');
    shadowFile.classList.add('shadow_file');
    shadowFile.setAttribute('basename', 'shadow.txt');
    shadowFile.textContent = 'Hello shadow';
    pageWidget.appendChild(shadowFile);

    file = document.createElement('div');
    file.classList.add('file');
    file.setAttribute('basename', 'test.adb');
    file.textContent = 'Hello world';
    pageWidget.appendChild(file);
  });

  afterEach(() => {
    for (const inTest of inTestList) {
      inTest.destructor();
    }

    document.body.innerHTML = '';
  });

  it('should be a list with 2 elements', () => {
    const labWidget = document.createElement('div');
    labWidget.classList.add('widget_editor');
    labWidget.setAttribute('example_server', 'http://example.com');
    labWidget.setAttribute('run_button', 'True');
    labWidget.setAttribute('inline', 'True');
    labWidget.setAttribute('lab', 'True');
    document.body.appendChild(labWidget);
    labWidget.appendChild(file.cloneNode(true));

    htmlList = document.getElementsByClassName('widget_editor');
    inTestList = widgetFactory(htmlList);

    expect(inTestList).to.have.lengthOf(2);
    expect(inTestList[0]).to.be.an.instanceof(Widget);
    expect(inTestList[1]).to.be.an.instanceof(LabWidget);
  });

  it('should create div with error when no server addr is found', () => {
    pageWidget.removeAttribute('example_server');
    htmlList = document.getElementsByClassName('widget_editor');
    inTestList = widgetFactory(htmlList);

    expect(inTestList).to.have.length(0);
    expect(pageWidget).to.have.descendants('div').with.length(1);
    const errorDiv = pageWidget.querySelector('div');
    expect(errorDiv).to.have.html(
        '<p>An error has occured processing this widget. ' +
        Strings.INTERNAL_ERROR_MESSAGE + '</p>');
  });

  it('should create div with error when no files in widget', () => {
    pageWidget.removeChild(file);
    htmlList = document.getElementsByClassName('widget_editor');
    inTestList = widgetFactory(htmlList);

    expect(inTestList).to.have.length(0);
    expect(pageWidget).to.have.descendants('div').with.length(1);
    const errorDiv = pageWidget.querySelector('div');
    expect(errorDiv).to.have.html(
        '<p>An error has occured processing this widget. ' +
        Strings.INTERNAL_ERROR_MESSAGE + '</p>');
  });
});

describe('Widget', () => {
  const baseURL = 'http://example.com';
  let htmlList: HTMLCollectionOf<Element>;
  let inTestList: Array<Widget | LabWidget>;

  const pageWidget = document.createElement('div');
  pageWidget.classList.add('widget_editor');
  pageWidget.setAttribute('example_server', baseURL);
  pageWidget.setAttribute('name', 'pageWidget');
  pageWidget.setAttribute('run_button', 'True');
  pageWidget.setAttribute('inline', 'True');

  const file1 = document.createElement('div');
  file1.classList.add('file');
  file1.setAttribute('basename', 'test1.adb');
  file1.textContent = 'Hello world #1';
  pageWidget.appendChild(file1);

  const file2 = document.createElement('div');
  file2.classList.add('file');
  file2.setAttribute('basename', 'test2.adb');
  file2.textContent = 'Hello world \n#2';
  pageWidget.appendChild(file2);

  const shadowFile = document.createElement('div');
  shadowFile.classList.add('shadow_file');
  shadowFile.setAttribute('basename', 'shadow.txt');
  shadowFile.textContent = 'Hello shadow';
  pageWidget.appendChild(shadowFile);

  before(() => {
    document.body.appendChild(pageWidget);
    htmlList = document.getElementsByClassName('widget_editor');
    inTestList = widgetFactory(htmlList);
  });

  after(() => {
    for (const inTest of inTestList) {
      inTest.destructor();
    }

    document.body.innerHTML = '';
  });

  it('should have two editors', () => {
    expect(pageWidget).to.have.descendants('div.editor-container').
        and.have.length(2);
  });

  describe('test run button click', () => {
    let button: HTMLElement;
    const identifier = 123;

    it('should have a run button in the output row', () => {
      expect(pageWidget).to.have.descendants('div.output_row').
          and.have.length(1);
      const outputRow = pageWidget.querySelector('div.output_row');
      expect(outputRow).to.have.descendants('button').and.have.length(1);
      button = outputRow.querySelector('button');
    });

    describe('trigger run sequence', () => {
      const consoleMsg = 'This is a console message';
      const clickableInfoMsg = 'test1.adb:1:2: info: This is an info message';
      const clickableStdoutMsg = 'test1.adb:1:3: Clickable stdout message';
      const raisedMsg = 'raised TEST_ERROR : test2.adb:2 explicit raise';

      const outputArea = new OutputArea();
      outputArea.add(['output_info', 'console_output'],
          Strings.CONSOLE_OUTPUT_LABEL + ':');
      outputArea.addConsole(consoleMsg);
      outputArea.addInfo(clickableInfoMsg);
      outputArea.addMsg(clickableStdoutMsg);
      outputArea.addMsg(raisedMsg);

      before(async () => {
        fetchMock.post(baseURL + '/run_program/', {
          body: {
            'identifier': identifier.toString(),
            'message': 'Pending',
          },
        });
        fetchMock.post(baseURL + '/check_output/', {
          body: {
            'completed': false,
            'message': 'PENDING',
            'output': [],
            'status': 0,
          },
        }, {
          repeat: 1,
        });
        fetchMock.post(baseURL + '/check_output/', {
          body: {
            'completed': false,
            'message': 'PENDING',
            'output': [
              {
                'msg': {
                  'data': consoleMsg,
                  'type': 'console',
                },
              },
            ],
            'status': 0,
          },
        }, {
          repeat: 1,
          overwriteRoutes: false,
        });
        fetchMock.post(baseURL + '/check_output/', {
          body: {
            'completed': false,
            'message': 'PENDING',
            'output': [
              {
                'msg': {
                  'data': clickableInfoMsg,
                  'type': 'stdout',
                },
              },
            ],
            'status': 0,
          },
        }, {
          repeat: 1,
          overwriteRoutes: false,
        });
        fetchMock.post(baseURL + '/check_output/', {
          body: {
            'completed': false,
            'message': 'PENDING',
            'output': [
              {
                'msg': {
                  'data': clickableStdoutMsg,
                  'type': 'stdout',
                },
              }, {
                'msg': {
                  'data': raisedMsg,
                  'type': 'stderr',
                },
              },
            ],
            'status': 0,
          },
        }, {
          repeat: 1,
          overwriteRoutes: false,
        });
        fetchMock.post(baseURL + '/check_output/', {
          body: {
            'completed': true,
            'message': 'SUCCESS',
            'output': [],
            'status': 0,
          },
        }, {
          repeat: 1,
          overwriteRoutes: false,
        });

        button.click();
        await delay(5 * 250);
        await fetchMock.flush(true);
      });

      after(() => {
        fetchMock.reset();
      });

      it('should trigger a run program post when clicked', () => {
        const runProgram = fetchMock.calls(baseURL + '/run_program/');
        expect(runProgram).to.have.length(1);
        expect(runProgram[0][1]['body']).to.equal(JSON.stringify({
          'files': [
            {
              'basename': 'test1.adb',
              'contents': 'Hello world #1',
            },
            {
              'basename': 'test2.adb',
              'contents': 'Hello world \n#2',
            },
            {
              'basename': 'shadow.txt',
              'contents': 'Hello shadow',
            },
          ],
          'mode': 'run',
          'name': 'pageWidget',
          'lab': false,
        }));
      });

      it('should stop after 5 calls to check output', () => {
        const checkOutput = fetchMock.calls(baseURL + '/check_output/');
        expect(checkOutput).to.have.length(5);
      });

      it('should send back how many lines it has read', () => {
        const checkOutput = fetchMock.calls(baseURL + '/check_output/');

        expect(checkOutput[0][1]['body']).to.equal(JSON.stringify({
          'identifier': identifier.toString(),
          'read': 0,
        }));
        expect(checkOutput[1][1]['body']).to.equal(JSON.stringify({
          'identifier': identifier.toString(),
          'read': 0,
        }));
        expect(checkOutput[2][1]['body']).to.equal(JSON.stringify({
          'identifier': identifier.toString(),
          'read': 1,
        }));
        expect(checkOutput[3][1]['body']).to.equal(JSON.stringify({
          'identifier': identifier.toString(),
          'read': 2,
        }));
        expect(checkOutput[4][1]['body']).to.equal(JSON.stringify({
          'identifier': identifier.toString(),
          'read': 4,
        }));
      });

      it('should have an output area with the received data', () => {
        expect(pageWidget).to.have.descendants('div.output_area').
            and.have.length(1);
        const outputAreaHTML = pageWidget.querySelector('div.output_area');
        expect(outputAreaHTML).to.have.html(outputArea.render().innerHTML);
      });

      it('should have three clickable divs', () => {
        const outputMsgs = pageWidget.querySelectorAll('div.output_msg');
        expect(outputMsgs).to.have.length(2);
        expect(pageWidget).to.have.descendants('div.output_msg_info').
            and.have.length(1);
        const infoMsg = pageWidget.querySelector('div.output_msg_info');

        expect(infoMsg).to.have.text(clickableInfoMsg);
        expect(outputMsgs[0]).to.have.text(clickableStdoutMsg);
        expect(outputMsgs[1]).to.have.text(raisedMsg);

        const HTMLeditors = pageWidget.querySelectorAll('div.editor-container');
        const ed1 = ace.edit(HTMLeditors[0] as HTMLElement);
        const ed2 = ace.edit(HTMLeditors[1] as HTMLElement);

        expect(pageWidget).to.have.descendants('div.tab').and.have.length(1);
        const headers = pageWidget.querySelector('div.tab');
        expect(headers).to.have.descendants('button').and.have.length(2);
        const tabs = headers.querySelectorAll('button');

        expect(tabs[1]).not.to.have.class('active');
        expect(ed2.getCursorPosition()).not.to.deep.equal({row: 1, column: 0});
        (outputMsgs[1] as HTMLElement).click();
        expect(ed2.getCursorPosition()).to.deep.equal({row: 1, column: 0});
        expect(tabs[1]).to.have.class('active');

        expect(tabs[0]).not.to.have.class('active');
        expect(ed1.getCursorPosition()).not.to.deep.equal({row: 0, column: 2});
        (outputMsgs[0] as HTMLElement).click();
        expect(ed1.getCursorPosition()).to.deep.equal({row: 0, column: 2});
        expect(tabs[0]).to.have.class('active');

        expect(ed1.getCursorPosition()).not.to.deep.equal({row: 0, column: 1});
        (infoMsg as HTMLElement).click();
        expect(ed1.getCursorPosition()).to.deep.equal({row: 0, column: 1});
      });
    });

    describe('test server down handling', ()=> {
      before(() => {
        fetchMock.mock(baseURL + '/run_program/', 500);
      });

      after(() => {
        fetchMock.reset();
      });

      it('should catch the error and add text to the output area', async () => {
        const outputArea = new OutputArea();
        outputArea.add(['output_info', 'console_output'],
            Strings.CONSOLE_OUTPUT_LABEL + ':');
        outputArea.addError(Strings.MACHINE_BUSY_LABEL);
        button.click();
        await fetchMock.flush(true);
        const outputAreaHTML = pageWidget.querySelector('div.output_area');
        expect(outputAreaHTML).to.have.html(outputArea.render().innerHTML);
      });
    });

    describe('test broken server handling', ()=> {
      afterEach(() => {
        fetchMock.reset();
      });

      it('should catch an error if identifier is blank', async () => {
        fetchMock.post(baseURL + '/run_program/', {
          body: {
            'identifier': '',
            'message': 'Pending',
          },
        });
        const outputArea = new OutputArea();
        outputArea.add(['output_info', 'console_output'],
            Strings.CONSOLE_OUTPUT_LABEL + ':');
        outputArea.addError(Strings.MACHINE_BUSY_LABEL);
        button.click();
        await fetchMock.flush(true);
        const outputAreaHTML = pageWidget.querySelector('div.output_area');
        expect(outputAreaHTML).to.have.html(outputArea.render().innerHTML);
      });

      it('should timeout requests to check output', async () => {
        fetchMock.post(baseURL + '/run_program/', {
          body: {
            'identifier': '1234',
            'message': 'Pending',
          },
        });
        fetchMock.post(baseURL + '/check_output/', {
          body: {
            'completed': false,
            'message': 'PENDING',
            'output': [],
            'status': 0,
          },
        });

        const outputArea = new OutputArea();
        outputArea.add(['output_info', 'console_output'],
            Strings.CONSOLE_OUTPUT_LABEL + ':');
        outputArea.addError(Strings.MACHINE_BUSY_LABEL);

        button.click();
        await delay(205 * 250);
        await fetchMock.flush(true);

        expect(fetchMock.calls(baseURL + '/run_program/')).to.have.length(1);
        expect(fetchMock.calls(baseURL + '/check_output/')).to.have.length(200);
        const outputAreaHTML = pageWidget.querySelector('div.output_area');
        expect(outputAreaHTML).to.have.html(outputArea.render().innerHTML);
      });

      it('should throw an error if response has lab ref', async () => {
        fetchMock.post(baseURL + '/run_program/', {
          body: {
            'identifier': '1234',
            'message': 'Pending',
          },
        });
        fetchMock.post(baseURL + '/check_output/', {
          body: {
            'completed': false,
            'message': 'PENDING',
            'output': [
              {
                'msg': {
                  'data': 'test data',
                  'type': 'console',
                },
                'ref': '0',
              },
            ],
            'status': 0,
          },
        });

        const outputArea = new OutputArea();
        outputArea.add(['output_info', 'console_output'],
            Strings.CONSOLE_OUTPUT_LABEL + ':');
        outputArea.addError(Strings.MACHINE_BUSY_LABEL);

        button.click();
        await delay(2 * 250);
        await fetchMock.flush(true);

        const outputAreaHTML = pageWidget.querySelector('div.output_area');
        expect(outputAreaHTML).to.have.html(outputArea.render().innerHTML);
      });

      it('should report internal errors normally', async () => {
        fetchMock.post(baseURL + '/run_program/', {
          body: {
            'identifier': '1234',
            'message': 'Pending',
          },
        });
        fetchMock.post(baseURL + '/check_output/', {
          body: {
            'completed': true,
            'message': 'PENDING',
            'output': [
              {
                'msg': {
                  'data': 'There was an error.',
                  'type': 'internal_error',
                },
              },
            ],
            'status': -1,
          },
        });

        const outputArea = new OutputArea();
        outputArea.add(['output_info', 'console_output'],
            Strings.CONSOLE_OUTPUT_LABEL + ':');
        outputArea.addLine(
            'There was an error. ' + Strings.INTERNAL_ERROR_MESSAGE);
        outputArea.addError(Strings.EXIT_STATUS_LABEL +
          ': ' + -1);

        button.click();
        await delay(2 * 250);
        await fetchMock.flush(true);

        const outputAreaHTML = pageWidget.querySelector('div.output_area');
        expect(outputAreaHTML).to.have.html(outputArea.render().innerHTML);
      });

      it('should throw an error when msg has a bad type', async () => {
        fetchMock.post(baseURL + '/run_program/', {
          body: {
            'identifier': '1234',
            'message': 'Pending',
          },
        });
        fetchMock.post(baseURL + '/check_output/', {
          body: {
            'completed': false,
            'message': 'PENDING',
            'output': [
              {
                'msg': {
                  'data': 'Test message',
                  'type': 'blahblahblah',
                },
              },
            ],
            'status': 0,
          },
        });

        const outputArea = new OutputArea();
        outputArea.add(['output_info', 'console_output'],
            Strings.CONSOLE_OUTPUT_LABEL + ':');
        outputArea.addLine('Test message');
        outputArea.addError(Strings.MACHINE_BUSY_LABEL);

        button.click();
        await delay(2 * 250);
        await fetchMock.flush(true);

        const outputAreaHTML = pageWidget.querySelector('div.output_area');
        expect(outputAreaHTML).to.have.html(outputArea.render().innerHTML);
      });
    });
  });

  // TODO: test the settingsbar HTML
});
