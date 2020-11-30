/* eslint-disable */

import {ResourceList} from './resource';

// FS - From Server
// TS - To Server

export namespace RunProgram {
  export interface FS {
    identifier: string;
    message: string;
  }

  interface SwitchType {
    [key : string]: Array<string>;
  }

  export interface TS {
    files: ResourceList;
    main: string;
    mode: string;
    switches: SwitchType;
    name: string;
    lab: boolean;
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

  export interface RunMsg {
    type: string;
    data: string;
  }

  export interface OutputLine {
    msg: RunMsg;
    ref: number;
  }

  export interface FS {
    output: Array<OutputLine>;
    status: number;
    completed: boolean;
    message: string;
  }

  export interface TS {
    identifier: string;
    read: number;
  }
}
