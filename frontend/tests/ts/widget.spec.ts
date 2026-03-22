// Import testing libs
import { expect, use } from 'chai';
import chaiAsPromised from 'chai-as-promised';
import chaiDom from 'chai-dom';
import {Client, Server, WebSocket} from 'mock-socket';
import FileSaver from 'file-saver';

// const chai = use(chaiDom);
const chai = use(chaiAsPromised);

import {readFileSync} from 'fs';
import {resolve} from 'path';

import * as Ace from 'ace-builds';

import {OutputArea} from '../../src/ts/areas';
import * as Strings from '../../src/ts/strings';

import {widgetFactory} from '../../src/ts/widget';
import {ServerWorker} from '../../src/ts/server';
import {CheckOutput, RunProgram} from '../../src/ts/server-types';
import {getElemsByTag, getElemById, getElemsByClass}
  from '../../src/ts/dom-utils';

global.WebSocket = WebSocket;

import { fileURLToPath } from 'url';
import { dirname } from 'path';

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

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

/**
* Remove all event listeners from the server
*
* @param {Server} server - The server to remove the listeners from
*/
function removeListeners(server: Server): void {
 for (let type in server.listeners) {
   server.listeners[type] = [];
 }
}

describe('Widget', () => {
  let inTest: Array<HTMLElement>;
  let root: HTMLElement;
  const baseURL = 'wss://sandbox.backend.learn.adacore.com';
  const server: Server = new Server(baseURL);

  const originalLogFunction = console.log;
  const originalErrorFunction = console.error;

  before(() => {
    // This is use to prevent logging from appearing in the test output
    console.log = () => {};
    console.error = () => {};
  });

  after(() => {
    server.stop();
    console.log = originalLogFunction;
    console.error = originalErrorFunction;
  });

  describe('Single Widget', () => {
    before(() => {
      fillDOM('single.html');
      inTest = getElemsByClass(document, 'widget');
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
        let editor: Ace.Ace.Editor;
        const consoleMsg = 'This is a console message';
        const clickableInfoMsg = 'test.adb:1:2: info: This is an info message';
        const clickableStdoutMsg = 'test.adb:2:3: Clickable stdout message';
        const raisedMsg = 'raised TEST_ERROR : test.adb:3 explicit raise';
        const serverResponses: Array<CheckOutput.FS> = [
          {
            output: [
              {type: 'console', data: consoleMsg}
            ],
            status: 0,
            completed: false,
            message: 'PENDING',
          },
          {
            output: [
              {type: 'stdout', data: clickableInfoMsg}
            ],
            status: 0,
            completed: false,
            message: 'PENDING',
          },
          {
            output: [
              {type: 'stdout', data: clickableStdoutMsg},
              {type: 'stderr', data: raisedMsg}
            ],
            status: 0,
            completed: true,
            message: 'SUCCESS',
          },
        ];
        let receivedMessages: Array<string> = [];

        before(async () => {
          const editorDiv = getElemById(root.id + '.editors.editor');
          editor = Ace.edit(editorDiv);

          server.on('connection', (socket) => {
            socket.on('message', (event) => {
              receivedMessages.push(event as string);
              serverResponses.forEach((msg) => {
                socket.send(JSON.stringify(msg));
              });
            });
          });

          runButton.click();
          await ServerWorker.delay(3 * 250);
        });

        after(() => {
          removeListeners(server);
        });

        it('should trigger a run program post when clicked', () => {
          expect(receivedMessages).to.have.length(1);

          const expectSwitches = {
            'Builder': ['-g'],
            'Compiler': ['-gnata', '-gnatX'],
          };

          const request: RunProgram.TS = JSON.parse(receivedMessages[0]) as RunProgram.TS;

          expect(request.data.files).to.have.length(1);
          expect(request.data.mode).to.equal('run');
          expect(request.data.switches).to.deep.equal(expectSwitches);
          expect(request.data.name).to.equal('Test.Single');
          expect(request.data.lab).to.be.false;
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
            row: 0, column: 1
          });
        });
      });

      describe('Error Behavior', () => {
        let fakeDiv: HTMLDivElement;
        let fakeOA: OutputArea;
        let realDiv: HTMLElement;
        let receivedMessages: Array<string> = [];

        beforeEach(() => {
          fakeDiv = document.createElement('div') as HTMLDivElement;
          fakeOA = new OutputArea(fakeDiv);
          fakeOA.add(['output_info', 'console_output'],
              Strings.CONSOLE_OUTPUT_LABEL + ':');
          realDiv = getElemById(root.id + '.output-area');
        });

        it('should handle an error if ServerWorker throws', async () => {
          const serverResponse: CheckOutput.FS_Error = {
            "message": "Forbidden",
            "connectionId": "abc_-=2",
            "requestId": "abc_-=2"
          };
          server.on('connection', (socket) => {
            socket.on('message', (event) => {
              socket.send(JSON.stringify(serverResponse));
            });
          });

          fakeOA.addError(Strings.MACHINE_BUSY_LABEL);
          runButton.click();
          await ServerWorker.delay(250);

          expect(fakeDiv.innerHTML).to.equal(realDiv.innerHTML);
          removeListeners(server);
        });

        it('should handle an error if response has lab ref', async () => {
          const serverResponses: Array<CheckOutput.FS> = [
            {
              output: [
                {type: 'console', data: "test data"}
              ],
              status: 0,
              completed: false,
              message: 'PENDING',
              ref: 0
            },
            {
              output: [],
              status: 0,
              completed: false,
              message: 'SUCCESS'
            }
          ];
          server.on('connection', (socket) => {
            socket.on('message', (event) => {
              serverResponses.forEach((msg) => {
                socket.send(JSON.stringify(msg));
              });
            });
          });

          fakeOA.addError(Strings.MACHINE_BUSY_LABEL);

          runButton.click();
          await ServerWorker.delay(250);

          expect(fakeDiv.innerHTML).to.equal(realDiv.innerHTML);
          removeListeners(server);
        });

        it('should report internal errors normally', async () => {
          const serverResponses: Array<CheckOutput.FS> = [
            {
              output: [
                {type: 'internal_error', data: "There was an error."}
              ],
              status: -1,
              completed: false,
              message: 'PENDING',
            },
            {
              output: [],
              status: -1,
              completed: true,
              message: 'SUCCESS'
            }
          ];
          server.on('connection', (socket) => {
            socket.on('message', (event) => {
              serverResponses.forEach((msg) => {
                socket.send(JSON.stringify(msg));
              });
            });
          });

          fakeOA.addLine(
              'There was an error. ' + Strings.INTERNAL_ERROR_MESSAGE);
          fakeOA.addError(Strings.EXIT_STATUS_LABEL +
            ': ' + -1);

          runButton.click();
          await ServerWorker.delay(250);

          expect(fakeDiv.innerHTML).to.equal(realDiv.innerHTML);
          removeListeners(server);
        });

        it('should throw an error when msg has a bad type', async () => {
          const serverResponses: Array<CheckOutput.FS> = [
            {
              output: [
                {type: 'blahblahblah', data: "Test message"}
              ],
              status: 0,
              completed: false,
              message: 'PENDING',
            },
            {
              output: [],
              status: 0,
              completed: true,
              message: 'SUCCESS'
            }
          ];
          server.on('connection', (socket) => {
            socket.on('message', (event) => {
              serverResponses.forEach((msg) => {
                socket.send(JSON.stringify(msg));
              });
            });
          });

          fakeOA.addLine('Test message');
          fakeOA.addError(Strings.MACHINE_BUSY_LABEL);

          runButton.click();
          await ServerWorker.delay(250);

          expect(fakeDiv.innerHTML).to.equal(realDiv.innerHTML);
          removeListeners(server);
        });

        it('should show error if switches JSON is invalid on run', async () => {
          const originalSwitches = root.dataset.switches;
          root.dataset.switches = 'invalid json';
          runButton.click();
          await ServerWorker.delay(250);
          expect(realDiv.textContent).to.include(Strings.INTERNAL_ERROR_MESSAGE);
          root.dataset.switches = originalSwitches as string;
        });

        it('should add output line for stdout from a file not in viewMap', async () => {
          const serverResponse: CheckOutput.FS = {
            output: [
              {type: 'stdout', data: 'unknown.adb:1:2: error: test error'},
            ],
            status: 0,
            completed: true,
            message: 'SUCCESS',
          };
          server.on('connection', (socket) => {
            socket.on('message', (_event) => {
              socket.send(JSON.stringify(serverResponse));
            });
          });

          runButton.click();
          await ServerWorker.delay(250);

          expect(realDiv.innerHTML).to.not.be.empty;
          removeListeners(server);
        });
      });
    });

    describe('Settings Bar', () => {
      let editorDiv: HTMLElement;
      let editor: Ace.Ace.Editor;

      before(() => {
        editorDiv = getElemById(root.id + '.editors.editor');
        editor = Ace.edit(editorDiv);
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

      it('should revert tab setting when user cancels', () => {
        const tabSetting = getElemById(root.id + '.settings-bar.tab-setting') as
          HTMLInputElement;
        window.confirm = (): boolean => false;
        const originalChecked = tabSetting.checked;
        tabSetting.checked = !originalChecked;
        triggerEvent(tabSetting, 'change');
        expect(tabSetting.checked).to.equal(originalChecked);
      });

      it('should apply tab setting and reload when user confirms', () => {
        const tabSetting = getElemById(root.id + '.settings-bar.tab-setting') as
          HTMLInputElement;
        window.confirm = (): boolean => true;
        tabSetting.checked = false;
        try {
          triggerEvent(tabSetting, 'change');
        } catch { /* location.reload may throw in test environment */ }
        const editorContainer = getElemById(root.id + '.editors.editor');
        expect(editorContainer.hidden).to.be.true;
      });

      it('should revert theme setting when user cancels', () => {
        const themeSetting = getElemById(root.id + '.settings-bar.theme-setting') as
          HTMLInputElement;
        window.confirm = (): boolean => false;
        const originalChecked = themeSetting.checked;
        themeSetting.checked = !originalChecked;
        triggerEvent(themeSetting, 'change');
        expect(themeSetting.checked).to.equal(originalChecked);
      });

      it('should apply dark theme and reload when user confirms', () => {
        const themeSetting = getElemById(root.id + '.settings-bar.theme-setting') as
          HTMLInputElement;
        window.confirm = (): boolean => true;
        themeSetting.checked = true;
        try {
          triggerEvent(themeSetting, 'change');
        } catch { /* location.reload may throw in test environment */ }
        expect(themeSetting.checked).to.be.true;
      });
    });

    describe('Compiler Switches', () => {
      let compilerSwitchesSetting: HTMLElement;

      before(() => {
        compilerSwitchesSetting =
          getElemById(root.id + '.settings-bar.compiler-switches');
      });

      it('should deactivate mutually exclusive switches when one is checked', () => {
        const gnato = document.getElementById(
            root.id + '.settings-bar.compiler-switches.-gnato') as HTMLInputElement;
        const gnato0 = document.getElementById(
            root.id + '.settings-bar.compiler-switches.-gnato0') as HTMLInputElement;
        gnato.checked = true;
        gnato0.checked = true;
        triggerEvent(gnato0, 'change');
        expect(gnato.checked).to.be.false;
        expect(gnato0.checked).to.be.true;
      });

      it('should clear help info when clicked if not disabled', () => {
        const d = compilerSwitchesSetting.getElementsByClassName(
            'compiler-switch-help-info')[0] as HTMLElement;
        d.classList.remove('disabled');
        d.textContent = 'some content';
        d.click();
        expect(d.textContent).to.equal('');
        expect(d.classList.contains('disabled')).to.be.true;
      });

      it('should not clear help info when clicked if disabled', () => {
        const d = compilerSwitchesSetting.getElementsByClassName(
            'compiler-switch-help-info')[0] as HTMLElement;
        d.classList.add('disabled');
        d.textContent = 'some content';
        d.click();
        expect(d.textContent).to.equal('some content');
      });

      it('should populate help info when help button is clicked', () => {
        const d = compilerSwitchesSetting.getElementsByClassName(
            'compiler-switch-help-info')[0] as HTMLElement;
        const firstEntry = compilerSwitchesSetting.getElementsByClassName(
            'compiler-switch-entry')[0] as HTMLElement;
        const b = firstEntry.getElementsByTagName('button')[0] as HTMLButtonElement;
        b.click();
        expect(d.classList.contains('disabled')).to.be.false;
        expect(d.querySelector('b')).to.not.be.null;
      });
    });

    describe('Download Button', () => {
      let dlButton: HTMLElement;
      let savedFilename: string | undefined;

      before(() => {
        dlButton = getElemById(root.id + '.settings-bar.download-btn');
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        (FileSaver as any).saveAs = (_blob: Blob, name: string): void => {
          savedFilename = name;
        };
      });

      beforeEach(() => {
        savedFilename = undefined;
      });

      it('should trigger a download when clicked', async () => {
        dlButton.click();
        await ServerWorker.delay(300);
        expect(savedFilename).to.equal('Test.Single.zip');
      });

      it('should show an error if switches JSON is invalid', async () => {
        const originalSwitches = root.dataset.switches;
        root.dataset.switches = 'invalid json';
        const outputDiv = getElemById(root.id + '.output-area');
        dlButton.click();
        await ServerWorker.delay(100);
        expect(outputDiv.textContent).to.include(Strings.INTERNAL_ERROR_MESSAGE);
        root.dataset.switches = originalSwitches as string;
      });
    });
  });

  describe('Lab Widget', () => {
    before(() => {
      fillDOM('lab.html');
      inTest = getElemsByClass(document, 'widget');
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
        let receivedMessages: Array<string> = [];

        before(async () => {
          const serverResponses: Array<CheckOutput.FS> = [
            {
              output: [
                {type: 'console', data: consoleMsg}
              ],
              status: 0,
              completed: false,
              message: 'PENDING',
            },
            {
              output: [
                {type: 'console', data: 'test ref 0'},
                {
                  type: 'lab',
                  data: {
                    success: true,
                    cases: [{'0': {
                      actual: 'actual',
                      in: '',
                      out: 'out',
                      status: 'Success'}}]
                  }
                }
              ],
              status: 0,
              completed: false,
              message: 'PENDING',
              ref: 0
            },
            {
              output: [],
              status: 0,
              completed: true,
              message: 'SUCCESS'
            }
          ];
          server.on('connection', (socket) => {
            socket.on('message', (event) => {
              receivedMessages.push(event as string);
              serverResponses.forEach((msg) => {
                socket.send(JSON.stringify(msg));
              });
            });
          });

          submitButton.click();
          await ServerWorker.delay(250);
        });

        after(() => {
          removeListeners(server);
        });

        it('should trigger a submit post when clicked', () => {
          expect(receivedMessages).to.have.length(1);

          const request = JSON.parse(receivedMessages[0]) as RunProgram.TS;

          expect(request.data.files).to.have.length(2);
          expect(request.data.mode).to.equal('submit');
          expect(request.data.name).to.equal('Test.Lab');
          expect(request.data.lab).to.be.true;
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

    describe('Reset', () => {
      it('should reset output area and lab container', () => {
        const resetBtn = getElemById(root.id + '.settings-bar.reset-btn') as
          HTMLButtonElement;
        const outputDiv = getElemById(root.id + '.output-area');
        const labArea = getElemById(root.id + '.lab-area');
        window.confirm = (): boolean => true;
        resetBtn.click();
        expect(outputDiv.innerHTML).to.equal('');
        expect(labArea.innerHTML).to.equal('');
      });
    });
  });

  describe('Malformed Widget', () => {
    let malformedElem: HTMLDivElement;

    before(() => {
      global.document = window.document;
      document.documentElement.innerHTML = '<head></head><body></body>';
      malformedElem = document.createElement('div') as HTMLDivElement;
      malformedElem.id = 'malformed-widget-test';
      malformedElem.dataset.url = baseURL;
      malformedElem.dataset.name = 'Test.Malformed';
      malformedElem.dataset.lab = 'False';
      malformedElem.dataset.main = '';
      malformedElem.dataset.switches = '{"Builder":["-g"],"Compiler":[]}';
      const editorDiv = document.createElement('div');
      editorDiv.id = 'malformed-widget-test.editors.editor';
      malformedElem.appendChild(editorDiv);
      document.body.appendChild(malformedElem);
    });

    after(() => {
      clearDOM();
    });

    it('should show an error message for a widget with no files', () => {
      widgetFactory([malformedElem] as Array<HTMLDivElement>);
      const errorP = malformedElem.querySelector('p');
      expect(errorP).to.not.be.null;
      expect(errorP!.textContent).to.include('An error has occured');
    });
  });

  describe('Code Block Info Widget', () => {
    before(() => {
      fillDOM('code_block_info.html');
      inTest = getElemsByClass(document, 'widget');
      widgetFactory(inTest as Array<HTMLDivElement>);
      root = inTest[0];
    });

    after(() => {
      clearDOM();
    });

    it('should have a single widget on the page', () => {
      expect(inTest).to.have.length(1);
    });

    it('should populate the code-block-info output element', () => {
      const cbiContents = getElemById(root.id + '.code_block_info.run info.contents');
      expect(cbiContents.innerText).to.include('Hello from run output!');
    });
  });

  describe('Multi Widget', () => {
    let multiWidgets: Array<HTMLElement>;

    before(() => {
      fillDOM('multi.html');
      multiWidgets = getElemsByClass(document, 'widget');
      widgetFactory(multiWidgets as Array<HTMLDivElement>);
    });

    after(() => {
      clearDOM();
    });

    it('should have two widgets on the page', () => {
      expect(multiWidgets).to.have.length(2);
    });

    it('should create both widgets without errors', () => {
      multiWidgets.forEach((w) => {
        expect(w.querySelector('p')).to.be.null;
      });
    });
  });
});
