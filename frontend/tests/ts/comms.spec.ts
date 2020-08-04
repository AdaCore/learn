// Import testing libs
import chai, {expect} from 'chai';
chai.use(require('chai-as-promised'));

import fetchMock from 'fetch-mock';

import {fetchBlob, fetchJSON, DownloadRequest, DownloadResponse} from '../../src/ts/comms';

describe('fetchBlob()', () => {
  const blobURL = 'http://example.com/get_blob';

  const filename = 'example.zip';
  const obj = 'hello world';
  const blob = new Blob([obj], {type: 'text/plain;charset=utf-8'});

  const serverData: DownloadRequest = {
    files: [{
      basename: 'test.adb',
      contents: 'This is a test'
    }],
    name: 'Test',
  };

  afterEach(() => {
    fetchMock.reset()
  });

  it('should return a blob', async () => {
    fetchMock.mock(blobURL, {
      body: blob,
      headers: {
        'Content-Disposition': 'attachment; filename=' + filename,
      }
    },
    {
      sendAsJson: false,
    });

    const response: DownloadResponse = {
      blob: blob,
      filename: filename
    };

    const ret = fetchBlob(serverData, blobURL);

    const lastCall = fetchMock.lastCall();
    expect(lastCall[0]).to.equal(blobURL);
    expect(lastCall[1]['body']).to.equal(JSON.stringify(serverData));

    // TODO: Figure out how to check the actual blob
    // return expect(ret).to.become(response);
    return expect(ret).to.eventually.have.property('filename', filename);
  });

  it('should throw an exception', () => {
    fetchMock.mock(blobURL, 500);
    const ret = fetchBlob(serverData, blobURL);
    return expect(ret).to.be.rejectedWith(Error, 'Status: 500 Msg: Internal Server Error');
  });
});

describe('fetchJSON()', () => {
  const jsonURL = 'http://example.com/get_json';

  interface testData {
    key: string;
    value: string;
  }

  const toBody: testData = {
    key: 'to',
    value: 'server'
  };

  const fromBody: testData = {
    key: 'from',
    value: 'server'
  };

  afterEach(() => {
    fetchMock.reset()
  });

  it('should return some JSON', async () => {
    fetchMock.mock(jsonURL, {
      body: JSON.stringify(fromBody),
    });

    const ret = await fetchJSON<testData, testData>(toBody, jsonURL);
    expect(ret).to.deep.equal(fromBody);
  });

  it('should throw an exception', () => {
    fetchMock.mock(jsonURL, 500);
    const ret = fetchJSON<testData, testData>(toBody, jsonURL);
    return expect(ret).to.be.rejectedWith(Error, 'Status: 500 Msg: Internal Server Error');
  });
});