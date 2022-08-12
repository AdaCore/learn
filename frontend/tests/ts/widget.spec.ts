// Import testing libs
import chai, {expect} from 'chai';
import chaiAsPromised from 'chai-as-promised';
import chaiDom from 'chai-dom';
import {Client, Server, WebSocket} from 'mock-socket';
chai.use(chaiDom);
chai.use(chaiAsPromised);

import {readFileSync} from 'fs';
import {resolve} from 'path';

import ace from 'brace';

import {OutputArea} from '../../src/ts/areas';
import * as Strings from '../../src/ts/strings';

import {widgetFactory} from '../../src/ts/widget';
import {ServerWorker} from '../../src/ts/server';
import {CheckOutput, RunProgram} from '../../src/ts/server-types';
import {getElemsByTag, getElemById, getElemsByClass}
  from '../../src/ts/dom-utils';

global.WebSocket = WebSocket;

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
  const baseURL = 'wss://api-staging.learn.r53.adacore.com';
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
          const editorDiv = getElemById(root.id + '.editor');
          editor = ace.edit(editorDiv);

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
            'Builder': ['-test'],
            'Compiler': ['-test'],
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
  });
});
