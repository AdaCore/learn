// Import testing libs
import chai, {expect} from 'chai';
import chaiAsPromised from 'chai-as-promised';
chai.use(chaiAsPromised);

import fetchMock from 'fetch-mock';

import {ServerWorker} from '../../src/ts/server';

describe('ServerWorker', () => {
  const identifier = 123;
  const baseURL = 'test.com';
  const consoleMsg = 'test';

  describe('#request()', () => {

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
    });

    after(() => {
      fetchMock.reset();
    });
  });


});
