/* eslint-disable */

import {ResourceList} from './resource';

// FS - From Server
// TS - To Server

export namespace RunProgram {

  interface SwitchType {
    [key : string]: Array<string>;
  }

  export interface TSData {
    files: ResourceList;
    main: string;
    mode: string;
    switches: SwitchType;
    name: string;
    lab: boolean;
  }

  export interface TS {
      action: string,
      data: TSData
  }
}

export namespace CheckOutput {
  export interface TestResult {
    status: string;
    out: string;
    actual: string;
    in: string;
  }

  export interface TestCase {
    [key: string]: TestResult;
  }

  export interface LabOutput {
    success: boolean;
    cases: Array<TestCase>;
  }

  export type MessageData = string | LabOutput;

  export interface RunMsg {
    type: string;
    data: MessageData;
  }

  export interface FS {
    output: Array<RunMsg>;
    status: number;
    completed: boolean;
    message: string;
    ref?: number;
  }

  export interface FS_Error {
    message : string;
    connectionId: string;
    requestId: string;
  }
}
