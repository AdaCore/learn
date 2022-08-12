// Import testing libs
import chai, {expect} from 'chai';
import chaiAsPromised from 'chai-as-promised';
chai.use(chaiAsPromised);

import {Server, WebSocket} from 'mock-socket';

import {ServerWorker} from '../../src/ts/server';
import {CheckOutput, RunProgram} from '../../src/ts/server-types';

global.WebSocket = WebSocket;

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

describe('ServerWorker', () => {
  let cbCount = 0;
  const baseURL = 'wss://localhost:8080';
  const tsData: RunProgram.TSData = {
    files: [],
    main: 'test',
    mode: 'test',
    switches: {},
    name: 'test',
    lab: false,
  };
  let server: Server = new Server(baseURL);
  let client: ServerWorker = new ServerWorker(baseURL, (data: CheckOutput.FS): boolean => {
    cbCount++;
    return data.completed;
  });

  after(() => {
    server.stop();
  });

  describe('Normal Behaviour', () => {
    const serverResponses: Array<CheckOutput.FS> = [
      {
        output: [
          {type: 'console', data: 'gprbuild -P main.gpr'},
          {type: 'stdout', data: 'Hello World!'},
        ],
        status: 0,
        completed: false,
        message: 'PENDING',
      },
      {
        output: [],
        status: 0,
        completed: true,
        message: 'SUCCESS',
      },
    ];

    before(() => {
      server.on('connection', (socket) => {
        socket.on('message', (event) => {
          serverResponses.forEach((msg) => {
            socket.send(JSON.stringify(msg));
          });
        });
      });
    });

    after(() => {
      removeListeners(server);
    });

    it('should call the callback once for each message received', async () => {
      await client.execute(tsData, 2000);
      expect(cbCount).to.equal(2);
    });
  });

  describe('Error Behaviour', () => {
    const serverResponse: CheckOutput.FS_Error = {
      "message": "Forbidden",
      "connectionId": "abc_-=1",
      "requestId": "abc_-=1"
    };
    const expectedErrorMsg =
      `Executer gave server error: ${JSON.stringify(serverResponse)}`;

    before(() => {
      server.on('connection', (socket) => {
        socket.on('message', (event) => {
          socket.send(JSON.stringify(serverResponse));
        });
      });
    });

    after(() => {
      removeListeners(server);
    });

    it('should throw an exception when AWS rejects the request', async () => {
      expect(client.execute(tsData, 2000)).to.be.rejectedWith(expectedErrorMsg);
    });
  });

  describe('Timeout Test', () => {
    const timeout = 250;
    const expectedErrorMsg =
      `Timeout: No response from server in ${timeout}ms`

    before(() => {
      server.on('connection', (socket) => {
        socket.on('message', (event) => {});
      });
    });

    after(() => {
      removeListeners(server);
    });

    it('should timeout if no response is recieved', async () => {
      expect(client.execute(tsData, timeout)).to.be.rejectedWith(expectedErrorMsg);
    });
  });
});
