interface Resource {
  basename : string;
  contents : string;
}

// FS - From Server
// TS - To Server

namespace RunProgram {
  export interface FS {
    identifier : string;
    message : string;
  }

  export interface TS {
    files : Array<Resource>;
    mode : string;
    lab : string;
  }
}

namespace CheckOutput {
  export interface TestResult {
    status : string;
    out : string;
    actual : string;
    in : string;
  }

  export interface TestCase {
    [key : string] : TestResult;
  }

  export interface LabOutput {
    success : boolean;
    test_cases : Array<TestCase>;
  }

  export interface TestCases {
    lab_output : LabOutput;
  }

  export interface RunMsg {
    type : string;
    data : string;
  }

  export interface OutputLine {
    msg : RunMsg;
    lab_ref : number;
  }

  export interface FS {
    output_lines : Array<OutputLine>;
    status : number;
    completed : boolean;
    message : string;
  }

  export interface TS {
    identifier : string;
    already_read : number;
  }
}
