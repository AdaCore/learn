import {fetchJSON} from './comms';
import {RunProgram, CheckOutput} from './server-types';

/**
 * Delay function
 *
 * @param {number} ms - Number of milliseconds to delay-+
 * @return {Promise<unknown>} - A promise to await
 */
function delay(ms: number): Promise<unknown> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

/**
 * Worker class for server REST sequence
 *
 * @export
 * @class ServerWorker
 */
export class ServerWorker {
  private readonly server: string;
  private readonly cb: (data: CheckOutput.FS) => number;
  private readonly INTERNAL_ERROR_MESSAGE =
  'Please report this issue on https://github.com/AdaCore/learn/issues';

  /**
   * The data processing callback for server data
   *
   * @callback dataCallback
   * @param {CheckOutput.FS} data - The data to process
   */

  /**
   * Creates an instance of ServerWorker.
   * @param {string} server - The server address
   * @param {dataCallback} dataCB - The process data callback
   */
  constructor(server: string, dataCB: (data: CheckOutput.FS) => number) {
    this.server = server;
    this.cb = dataCB;
  }

  /**
   * Entrypoint for initiating requests to the server
   *
   * @param {RunProgram.TS} serverData - The data to send
   * @param {string} address - The suffix to send the request to
   */
  public async request(serverData: RunProgram.TS, address: string):
      Promise<void> {
    const json =
      await
      fetchJSON<RunProgram.TS, RunProgram.FS>(serverData,
          this.serverAddress(address));
    if (json.identifier == '') {
      throw new Error(json.message);
    }

    await this.getOutputFromIdentifier(json);
  }

  /**
   * Get the run output using the return identifier from the button CB
   * @param {RunProgram.FS} json - the json data returned from button CB
   * @param {number} lRead - the number of lines already read from the stream
   * @param {number} nReq - the number of requests sent
   */
  private async getOutputFromIdentifier(json: RunProgram.FS,
      lRead = 0, nReq = 0): Promise<void> {
    const data: CheckOutput.TS = {
      identifier: json.identifier,
      read: lRead,
    };

    const rdata =
      await fetchJSON<CheckOutput.TS, CheckOutput.FS>(data,
          this.serverAddress('check_output'));
    nReq++;

    if (nReq >= 200) {
      throw new Error('Request timed out. ' + this.INTERNAL_ERROR_MESSAGE);
    }

    lRead += this.cb(rdata);

    if (!rdata.completed) {
      // We have not finished processing the output: call this again
      await delay(250);
      await this.getOutputFromIdentifier(json, lRead, nReq);
    }
  }

  /**
   * Construct the server address string
   * @param {string} url - the url suffix
   * @return {string} - the full constructed url
   */
  private serverAddress(url: string): string {
    return this.server + '/' + url + '/';
  }
}
