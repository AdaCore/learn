// Import testing libs
import chai, {expect} from 'chai';
import chaiAsPromised from 'chai-as-promised';
chai.use(chaiAsPromised);

import fetchMock from 'fetch-mock';

import {ServerWorker} from '../../src/ts/server';
import {CheckOutput, RunProgram} from '../../src/ts/server-types';


describe('ServerWorker', () => {
  let cbCount = 0;
  let inTest: ServerWorker;
  const identifier = 123;
  const baseURL = 'http://test.com';

  before(() => {
    inTest = new ServerWorker(baseURL, (data: CheckOutput.FS): number => {
      cbCount++;
      return data.output.length;
    });
  });

  describe('#request()', () => {
    const serverData: RunProgram.TS = {
      files: [],
      main: 'test',
      mode: 'test',
      switches: {},
      name: 'test',
      lab: false,
    };

    describe('Normal operation test', () => {
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
                  'data': 'test',
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
            'completed': true,
            'message': 'PENDING',
            'output': [],
            'status': 0,
          },
        }, {
          repeat: 1,
          overwriteRoutes: false,
        });

        await inTest.request(serverData, 'run_program');
      });

      after(() => {
        fetchMock.reset();
      });

      it('should send a run_program request with payload', () => {
        const runProgram = fetchMock.calls(baseURL + '/run_program/');
        expect(runProgram).to.have.length(1);
        expect(runProgram[0][1]['body']).to.equal(JSON.stringify(serverData));
      });

      it('should follow with check_output calls', () => {
        const checkOutput = fetchMock.calls(baseURL + '/check_output/');
        expect(checkOutput).to.have.length(3);
      });

      it('should have returned the number of lines read', () => {
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
      });

      it('should have called the callback', () => {
        expect(cbCount).to.equal(3);
      });
    });

    describe('Error operation test', () => {
      describe('Incorrect server data', () => {
        before(() => {
          fetchMock.post(baseURL + '/run_program/', {
            body: {
              'identifier': '',
              'message': 'Pending',
            },
          });
        });

        after(() => {
          fetchMock.reset();
        });

        it('should throw an exception when no identifier is returned',
            async () => {
              await expect(inTest.request(serverData, 'run_program')).to.
                  eventually.be.rejectedWith('No identifier sent from server.');
            });
      });

      describe('Max poll test', () => {
        before(() => {
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
        });

        after(() => {
          fetchMock.reset();
        });

        it('should time out after many server polls', async () => {
          await expect(inTest.request(serverData, 'run_program')).to.
              eventually.be.rejectedWith(
                  'Request timed out. Please report this issue on '
                  + 'https://github.com/AdaCore/learn/issues');
          expect(fetchMock.calls(baseURL + '/run_program/')).to.have.length(1);
          expect(fetchMock.calls(baseURL + '/check_output/')).to.have.
              length(200);
        });
      });
    });
  });
});
