import 'whatwg-fetch';

import {ResourceList} from './resource';

/**
* Perform a POST via the fetch method to retrieve data
* @param {string} data - the data to send
* @param {string} url - the url to send the fetch to
* @typeparam R - This is the json object type to send
* @return {Promise<Response>} returns a promise to a response object
*/
async function fetchGeneric<R>(data: R, url: string): Promise<Response> {
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
  return response;
}

/**
* Perform a POST via the fetch method to retrieve json data
* @param {string} data - the data to send
* @param {string} url - the url to send the fetch to
* @typeparam R - This is the json object type to send
* @typeparam T - This is the json object type to return
* @return {Promise<T>} returns a promise to an object of type T
*/
export async function fetchJSON<R, T>(data: R, url: string): Promise<T> {
  const response = await fetchGeneric<R>(data, url);
  return await response.json();
}

export interface DownloadRequest {
  files: ResourceList;
  switches: Array<string>;
  name: string;
}

export interface DownloadResponse {
  blob: Blob;
  filename: string;
}

/**
* Perform a POST via the fetch method to retrieve a file
* @param {DownloadRequest} data - the json to send
* @param {string} url - the url to send the fetch to
* @return {Promise<DownloadResponse>} returns a promise to a dl file
*/
export async function fetchBlob(data: DownloadRequest, url: string):
    Promise<DownloadResponse> {
  const response = await fetchGeneric<DownloadRequest>(data, url);
  const blob: Blob = await response.blob();
  const disposition = response.headers.get('content-disposition');
  const matches = disposition?.match(
      /filename[^;=\n]*=((['"]).*?\2|[^;\n]*)/);
  let filename: string | undefined;

  if (matches?.length === 3) {
    filename = matches[1];
  } else {
    filename = 'Download';
  }

  return {
    blob: blob,
    filename: filename,
  };
}
