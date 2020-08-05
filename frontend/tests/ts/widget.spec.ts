// Import testing libs
import chai, {expect} from 'chai';
chai.use(require('chai-dom'));
chai.use(require('chai-as-promised'));

import ace from 'brace';

import fetchMock from 'fetch-mock';
import { OutputArea } from '../../src/ts/areas';
import { CONSOLE_OUTPUT_LABEL } from '../../src/ts/strings';

import {widgetFactory, Widget, LabWidget} from '../../src/ts/widget';

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
    for(let inTest of inTestList) {
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
    labWidget.appendChild(file);

    htmlList = document.getElementsByClassName('widget_editor')
    inTestList = widgetFactory(htmlList);

    expect(inTestList).to.have.lengthOf(2);
    expect(inTestList[0]).to.be.an.instanceof(Widget);
    expect(inTestList[1]).to.be.an.instanceof(LabWidget);
  });

  it('should be throw an exception with malformed widget', () => {
    pageWidget.removeAttribute('example_server');
    htmlList = document.getElementsByClassName('widget_editor')
    const fn = () => {
      inTestList = widgetFactory(htmlList);
    };

    expect(fn).to.throw(Error, 'Malformed widget! No server address specified.');
  });
});

describe('Widget', () => {
  const baseURL = 'http://example.com'
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

  before(() => {
    document.body.appendChild(pageWidget);
    htmlList = document.getElementsByClassName('widget_editor')
    inTestList = widgetFactory(htmlList);
  });

  after(() => {
    for(let inTest of inTestList) {
      inTest.destructor();
    }

    document.body.innerHTML = '';
  });

  it('should have two editors', () => {
    expect(pageWidget).to.have.descendants('div.editor-container').and.have.length(2);
  });

  describe('test run button click', () => {
    let button: HTMLElement;
    const identifier = 123;

    it('should have a run button in the output row', () => {
      expect(pageWidget).to.have.descendants('div.output_row').and.have.length(1);
      const outputRow = pageWidget.querySelector('div.output_row');
      expect(outputRow).to.have.descendants('button').and.have.length(1);
      button = outputRow.querySelector('button');
    });

    describe('trigger run sequence', () => {
      const consoleMsg = 'This is a console message';
      const stdoutMsg = 'This is a stdout message';
      const clickableStdoutMsg = 'test1.adb:1:3: Clickable stdout message';
      const raisedMsg = 'raised TEST_ERROR : test2.adb:2 explicit raise';

      const outputArea = new OutputArea();
      outputArea.add(['output_info', 'console_output'],
          CONSOLE_OUTPUT_LABEL + ':');
      outputArea.addConsole(consoleMsg);
      outputArea.addLine(stdoutMsg);
      outputArea.addMsg(clickableStdoutMsg);
      outputArea.addMsg(raisedMsg);

      before(async () => {
        fetchMock.post(baseURL + '/run_program/', {
          body: {
            'identifier': identifier.toString(),
            'message' : 'Pending',
          },
        });
        fetchMock.post(baseURL + '/check_output/', {
          body: {
            'completed': false,
            'message': 'PENDING',
            'output': [],
            'status': 0
          },
        }, {
          repeat: 1,
        });
        fetchMock.post(baseURL + '/check_output/', {
          body: {
            'completed': false,
            'message':'PENDING',
            'output': [
              {
                'msg': {
                  'data': consoleMsg,
                  'type': 'console'
                }
              }
            ],
            'status': 0
          },
        }, {
          repeat: 1,
          overwriteRoutes: false,
        });
        fetchMock.post(baseURL + '/check_output/', {
          body: {
            'completed': false,
            'message':'PENDING',
            'output': [
              {
                'msg': {
                  'data': stdoutMsg,
                  'type': 'stdout'
                }
              }
            ],
            'status': 0
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
                'msg':{
                  'data': clickableStdoutMsg,
                  'type': 'stdout'
                }
              }, {
                'msg': {
                  'data': raisedMsg,
                  'type': 'stderr'
                }
              }
            ],
            'status': 0
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
            'status': 0
          },
        }, {
          repeat: 1,
          overwriteRoutes: false,
        });

        button.click();
        await new Promise((resolve) => setTimeout(resolve, 5 * 250));
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
            }
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
          'read': 0
        }));
        expect(checkOutput[1][1]['body']).to.equal(JSON.stringify({
          'identifier': identifier.toString(),
          'read': 0
        }));
        expect(checkOutput[2][1]['body']).to.equal(JSON.stringify({
          'identifier': identifier.toString(),
          'read': 1
        }));
        expect(checkOutput[3][1]['body']).to.equal(JSON.stringify({
          'identifier': identifier.toString(),
          'read': 2
        }));
        expect(checkOutput[4][1]['body']).to.equal(JSON.stringify({
          'identifier': identifier.toString(),
          'read': 4
        }));
      });

      it('should have an output area with the received data', () => {
        expect(pageWidget).to.have.descendants('div.output_area').and.have.length(1);
        const outputAreaHTML = pageWidget.querySelector('div.output_area');
        expect(outputAreaHTML).to.have.html(outputArea.render().innerHTML);
      });

      it('should have two clickable divs', () => {
        const outputMsgs = pageWidget.querySelectorAll('div.output_msg');
        expect(outputMsgs).to.have.length(2);
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
      });
    });
  });
});