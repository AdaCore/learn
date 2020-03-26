/* eslint-disable */

// FS - From Server
// TS - To Server

export interface Resource {
  basename: string;
  contents: string;
}

export namespace Download {
  export interface TS {
    files: Array<Resource>;
    name: string;
  }

  export interface FS {
    blob: Blob;
    filename: string;
  }
}

export namespace RunProgram {
  export interface FS {
    identifier: string;
    message: string;
  }

  export interface TS {
    files: Array<Resource>;
    mode: string;
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
