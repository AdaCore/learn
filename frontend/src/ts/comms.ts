import 'whatwg-fetch';

import {Download} from './types';

/**
* Perform a POST via the fetch method to retrieve json data
* @param {string} data - the json to send
* @param {string} url - the url suffix to send the fetch to
* @typeparam R - This is the json object type to send
* @typeparam T - This is the json object type to return
* @return {T} returns a promise to a json object of type T
*/
export async function fetchJSON<R, T>(data: R, url: string): Promise<T> {
  const response = await fetch(url, {
    method: 'POST',
    mode: 'cors',
    cache: 'no-cache',
    redirect: 'follow',
    body: JSON.stringify(data),
    headers: {
      'Content-Type': 'application/json',
    },
  });
  if (!response.ok) {
    throw new Error('Status: ' + response.status + ' Msg: ' +
      response.statusText);
  }
  return await response.json();
}

/**
* Perform a POST via the fetch method to retrieve a file
* @param {string} data - the json to send
* @param {string} url - the url suffix to send the fetch to
* @return {Promise<Download.FS>} returns a promise to a dl file
*/
export async function fetchBlob<R, T>(data: Download.TS, url: string):
    Promise<Download.FS> {
  const response = await fetch(url, {
    method: 'POST',
    mode: 'cors',
    cache: 'no-cache',
    redirect: 'follow',
    body: JSON.stringify(data),
    headers: {
      'Content-Type': 'application/json',
    },
  });
  if (!response.ok) {
    throw new Error('Status: ' + response.status + ' Msg: ' +
      response.statusText);
  }
  const blob: Blob = await response.blob();
  const disposition = response.headers.get('content-disposition');
  const filename = disposition.match(
      /filename[^;=\n]*=((['"]).*?\2|[^;\n]*)/)[1];

  return {
    blob: blob,
    filename: filename,
  };
}
