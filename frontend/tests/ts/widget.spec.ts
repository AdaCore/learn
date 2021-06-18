// Import testing libs
import chai, {expect} from 'chai';
import chaiAsPromised from 'chai-as-promised';
import chaiDom from 'chai-dom';
chai.use(chaiDom);
chai.use(chaiAsPromised);

import {readFileSync} from 'fs';
import {resolve} from 'path';

import ace from 'brace';

import fetchMock from 'fetch-mock';
import {OutputArea} from '../../src/ts/areas';
import * as Strings from '../../src/ts/strings';

import {DownloadRequest} from '../../src/ts/comms';
import {widgetFactory} from '../../src/ts/widget';
import {ServerWorker} from '../../src/ts/server';
import {RunProgram} from '../../src/ts/server-types';
import {getElemsByTag, getElemById, getElemsByClass}
  from '../../src/ts/dom-utils';

/**
 * Helper function to fill DOM from a file
 *
 * @param {string} filename - The filename to use
 */
function fillDOM(filename: string): void {
  global.document = window.document;

  const htmlString = readFileSync(resolve(__dirname, '../html/html/' +
      filename), 'utf8');
  document.documentElement.innerHTML = htmlString;
}

/**
 * Clears the DOM
 *
 */
function clearDOM(): void {
  document.documentElement.innerHTML = '';
}

/**
 * Helper function to trigger an event
 *
 * @param {HTMLElement} element - The element to trigger the event on
 * @param {string} eventName - The event name to trigger
 */
function triggerEvent(element: HTMLElement, eventName: string): void {
  const event = document.createEvent('HTMLEvents');
  event.initEvent(eventName, false, true);
  element.dispatchEvent(event);
}

describe('Widget', () => {
  let inTest: Array<HTMLElement>;
  let root: HTMLElement;

  const baseURL = 'https://cloudchecker-staging.learn.r53.adacore.com';
  describe('Single Widget', () => {
    before(() => {
      fillDOM('single.html');
      inTest = getElemsByTag(document, 'widget');
      widgetFactory(inTest as Array<HTMLDivElement>);
      root = inTest[0];
    });

    after(() => {
      clearDOM();
    });

    it('should have a single widget on the page', () => {
      expect(inTest).to.have.length(1);
    });

    it('should have one tab with active status', () => {
      const tabs = getElemById(root.id + '.tab');
      const headers = getElemsByTag(tabs, 'button');
      expect(headers).to.have.length(1);
      expect(headers[0]).to.have.class('active');
    });

    it('should have lab setting false', () => {
      const lab = root.dataset.lab;
      expect(lab).to.equal('False');
    });

    it('should not have a lab area', () => {
      expect(() => {
        getElemById(root.id + '.lab-area');
      }).to.throw();
    });

    it('should not have a cli area', () => {
      expect(() => {
        getElemById(root.id + '.cli');
      }).to.throw();
    });

    describe('Action Buttons', () => {
      let buttonGroup: HTMLElement;
      let outputDiv: HTMLElement;
      let runButton: HTMLButtonElement;
      const identifier = 123;

      before(() => {
        buttonGroup = getElemById(root.id + '.button-group');
        outputDiv = getElemById(root.id + '.output-area');

        // stub scrollIntoView function beacuse JSDOM doesn't have it
        // eslint-disable-next-line max-len
        // eslint-disable-next-line @typescript-eslint/no-explicit-any, @typescript-eslint/no-unused-vars, @typescript-eslint/no-empty-function
        window.HTMLElement.prototype.scrollIntoView = (arg: any): void => {};
      });

      it('should have a single run button', () => {
        const buttons = getElemsByTag(buttonGroup, 'button');
        expect(buttons).to.have.length(1);
        runButton = buttons[0] as HTMLButtonElement;
        const mode = runButton.dataset.mode;
        expect(mode).to.equal('run');
      });

      describe('Normal Behavior', () => {
        let editor: ace.Editor;
        const consoleMsg = 'This is a console message';
        const clickableInfoMsg = 'test.adb:1:2: info: This is an info message';
        const clickableStdoutMsg = 'test.adb:2:3: Clickable stdout message';
        const raisedMsg = 'raised TEST_ERROR : test.adb:3 explicit raise';

        before(async () => {
          const editorDiv = getElemById(root.id + '.editor');
          editor = ace.edit(editorDiv);

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
              'completed': true,
              'message': 'SUCCESS',
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

          runButton.click();
          await ServerWorker.delay(3 * 250);
          await fetchMock.flush(true);
        });

        after(() => {
          fetchMock.reset();
        });

        it('should trigger a run program post when clicked', () => {
          const runProgram = fetchMock.calls(baseURL + '/run_program/');
          expect(runProgram).to.have.length(1);

          const expectSwitches = {
            'Builder': ['-test'],
            'Compiler': ['-test'],
          };

          const request =
              JSON.parse(runProgram[0][1]['body'] as string) as RunProgram.TS;

          expect(request.files).to.have.length(1);
          expect(request.mode).to.equal('run');
          expect(request.switches).to.deep.equal(expectSwitches);
          expect(request.name).to.equal('Test.Single');
          expect(request.lab).to.be.false;
        });

        it('should have an output area with the received data', () => {
          const fakeOutputAreaDiv = document.createElement('div');
          const fakeOutputArea = new OutputArea(fakeOutputAreaDiv);
          fakeOutputArea.add(['output_info', 'console_output'],
              Strings.CONSOLE_OUTPUT_LABEL + ':');
          fakeOutputArea.addConsole(consoleMsg);
          fakeOutputArea.addInfo(clickableInfoMsg);
          fakeOutputArea.addMsg(clickableStdoutMsg);
          fakeOutputArea.addMsg(raisedMsg);

          expect(outputDiv).to.have.html(fakeOutputAreaDiv.innerHTML);
        });

        it('should have a clickable info div', () => {
          const infoMsg = getElemsByClass(outputDiv, 'output_msg_info');

          // remove active class from header before click to see if click cb
          //  worked properly
          const header = getElemById(root.id + '.tab.test.adb');
          header.classList.remove('active');
          infoMsg[0].click();
          expect(header).to.have.class('active');
          expect(editor.getCursorPosition()).to.deep.equal({
            row: 0, column: 1});
        });
      });

      describe('Error Behavior', () => {
        let fakeDiv: HTMLDivElement;
        let fakeOA: OutputArea;
        let realDiv: HTMLElement;

        beforeEach(() => {
          fakeDiv = document.createElement('div') as HTMLDivElement;
          fakeOA = new OutputArea(fakeDiv);
          fakeOA.add(['output_info', 'console_output'],
              Strings.CONSOLE_OUTPUT_LABEL + ':');
          realDiv = getElemById(root.id + '.output-area');
        });

        afterEach(() => {
          fetchMock.reset();
        });

        it('should handle an error if ServerWorker throws', async () => {
          fetchMock.post(baseURL + '/run_program/', {
            body: {
              'identifier': '',
              'message': 'Pending',
            },
          });

          fakeOA.addError(Strings.MACHINE_BUSY_LABEL);
          runButton.click();
          await fetchMock.flush(true);

          expect(fakeDiv.innerHTML).to.equal(realDiv.innerHTML);
        });

        it('should handle an error if response has lab ref', async () => {
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

          fakeOA.addError(Strings.MACHINE_BUSY_LABEL);

          runButton.click();
          await ServerWorker.delay(2 * 250);
          await fetchMock.flush(true);

          expect(fakeDiv.innerHTML).to.equal(realDiv.innerHTML);
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

          fakeOA.addLine(
              'There was an error. ' + Strings.INTERNAL_ERROR_MESSAGE);
          fakeOA.addError(Strings.EXIT_STATUS_LABEL +
            ': ' + -1);

          runButton.click();
          await ServerWorker.delay(2 * 250);
          await fetchMock.flush(true);

          expect(fakeDiv.innerHTML).to.equal(realDiv.innerHTML);
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

          fakeOA.addLine('Test message');
          fakeOA.addError(Strings.MACHINE_BUSY_LABEL);

          runButton.click();
          await ServerWorker.delay(2 * 250);
          await fetchMock.flush(true);

          expect(fakeDiv.innerHTML).to.equal(realDiv.innerHTML);
        });
      });
    });

    describe('Settings Bar', () => {
      let editor: ace.Editor;

      before(() => {
        const editorDiv = getElemById(root.id + '.editor');
        editor = ace.edit(editorDiv);
      });

      it('should have a checkbox that switches editor theme', () => {
        const box = getElemById(root.id + '.settings-bar.theme-setting') as
            HTMLInputElement;
        const origTheme = editor.getTheme();
        box.checked = !box.checked;
        triggerEvent(box, 'change');
        expect(editor.getTheme()).to.not.equal(origTheme);
      });

      it('should have a checkbox that switches tab setting', () => {
        // const box = getElemById(root.id + '.settings-bar.tab-setting') as
        //     HTMLInputElement;
        // expect.fail('Test not implemented.');
      });

      it('should have a button that resets the editor', () => {
        const btn = getElemById(root.id + '.settings-bar.reset-btn') as
            HTMLButtonElement;

        const session = editor.getSession();
        const origContent = session.getValue();

        session.doc.insert({row: 0, column: 0}, '\n');
        expect(session.getValue()).to.not.equal(origContent);

        // overwrite window.confirm because jsdom doesn't implement this
        window.confirm = (): boolean => true;

        btn.click();

        expect(session.getValue()).to.equal(origContent);
      });

      describe('Download Button', () => {
        let btn: HTMLButtonElement;

        before(() => {
          btn = getElemById(root.id + '.settings-bar.download-btn') as
              HTMLButtonElement;
          // stub this because JSDOM doesn't have it
          // eslint-disable-next-line max-len
          // eslint-disable-next-line @typescript-eslint/no-explicit-any, @typescript-eslint/no-unused-vars
          global.URL.createObjectURL = (object: any): string => {
            return '#';
          };
          // eslint-disable-next-line max-len
          // eslint-disable-next-line @typescript-eslint/no-explicit-any, @typescript-eslint/no-unused-vars, @typescript-eslint/no-empty-function
          global.URL.revokeObjectURL = (url: any): void => {};
        });

        afterEach(() => {
          fetchMock.reset();
        });

        it('should have a button that downloads the project', async () => {
          const filename = 'test.zip';
          const blob = new Blob(['test'], {type: 'text/plain;charset=utf-8'});

          fetchMock.post(baseURL + '/download/', {
            body: blob,
            headers: {
              'Content-Disposition': 'attachment; filename=' + filename,
            },
          },
          {
            sendAsJson: false,
          });

          btn.click();
          await fetchMock.flush(true);

          const downloadRequest = fetchMock.calls(baseURL + '/download/');
          expect(downloadRequest).to.have.length(1);

          const expectSwitches = {
            'Builder': ['-test'],
            'Compiler': ['-test'],
          };

          const request =
              JSON.parse(downloadRequest[0][1]['body'] as string) as
              DownloadRequest;
          expect(request.files).to.have.length(1);
          expect(request.switches).to.deep.equal(expectSwitches);
          expect(request.name).to.equal('Test.Single');
        });

        it('should handle an exception', async () => {
          fetchMock.post(baseURL + '/download/', 500);
          const fakeDiv = document.createElement('div');
          const fakeOA = new OutputArea(fakeDiv);
          fakeOA.addError(Strings.MACHINE_BUSY_LABEL);

          const realOA = getElemById(root.id + '.output-area');

          btn.click();
          await fetchMock.flush(true);
          expect(realOA.innerHTML).to.equal(fakeDiv.innerHTML);
        });
      });
    });
  });

  describe('Lab Widget', () => {
    before(() => {
      fillDOM('lab.html');
      inTest = getElemsByTag(document, 'widget');
      widgetFactory(inTest as Array<HTMLDivElement>);
      root = inTest[0];
    });

    after(() => {
      clearDOM();
    });

    it('should have a single LabWidget on the page', () => {
      expect(inTest).to.have.length(1);
    });

    it('should have lab setting true', () => {
      const lab = root.dataset.lab;
      expect(lab).to.equal('True');
    });

    it('should have a lab area', () => {
      expect(() => {
        getElemById(root.id + '.lab-area');
      }).not.to.throw();
    });

    it('should have a shadow file', () => {
      expect(root).to.have.descendants('.shadow-file').and.have.length(1);
    });

    describe('Action Buttons', () => {
      let buttonGroup: HTMLElement;
      let outputDiv: HTMLElement;
      let submitButton: HTMLButtonElement;

      before(() => {
        buttonGroup = getElemById(root.id + '.button-group');
        outputDiv = getElemById(root.id + '.output-area');
      });

      it('should have a single submit button', () => {
        const buttons = getElemsByTag(buttonGroup, 'button');
        expect(buttons).to.have.length(1);
        submitButton = buttons[0] as HTMLButtonElement;
        const mode = submitButton.dataset.mode;
        expect(mode).to.equal('submit');
      });

      describe('Normal Behavior', () => {
        const identifier = 123;
        const consoleMsg = 'General message';

        before(async () => {
          fetchMock.post(baseURL + '/run_program/', {
            body: {
              'identifier': identifier.toString(),
              'message': 'Pending',
            },
          });

          fetchMock.post(baseURL + '/check_output/', {
            body: {
              'completed': true,
              'message': 'SUCCESS',
              'output': [
                {
                  'msg': {
                    'data': consoleMsg,
                    'type': 'console',
                  },
                }, {
                  'msg': {
                    'data': 'test ref 0',
                    'type': 'console',
                  },
                  'ref': '0',
                }, {
                  'msg': {
                    'data': {
                      'cases': {
                        '0': {
                          'actual': 'actual',
                          'in': [],
                          'out': 'out',
                          'status': 'Success',
                        },
                      },
                      'success': true,
                    },
                    'type': 'lab',
                  },
                },
              ],
              'status': 0,
            },
          });

          submitButton.click();
          await ServerWorker.delay(3 * 250);
          await fetchMock.flush(true);
        });

        after(() => {
          fetchMock.reset();
        });

        it('should trigger a submit post when clicked', () => {
          const submission = fetchMock.calls(baseURL + '/run_program/');
          expect(submission).to.have.length(1);

          const request = JSON.parse(submission[0][1]['body'] as string) as
            RunProgram.TS;

          expect(request.files).to.have.length(2);
          expect(request.mode).to.equal('submit');
          expect(request.name).to.equal('Test.Lab');
          expect(request.lab).to.be.true;
        });

        it('should have an output area with the received data', () => {
          const fakeOutputAreaDiv = document.createElement('div');
          const fakeOutputArea = new OutputArea(fakeOutputAreaDiv);
          fakeOutputArea.add(['output_info', 'console_output'],
              Strings.CONSOLE_OUTPUT_LABEL + ':');
          fakeOutputArea.addConsole(consoleMsg);
          fakeOutputArea.addLabStatus(true);

          expect(outputDiv).to.have.html(fakeOutputAreaDiv.innerHTML);
        });
      });
    });
  });
});
