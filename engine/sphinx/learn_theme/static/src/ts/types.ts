interface Resource {
  basename : string;
  contents : string;
}

// FS - From Server
// TS - To Server

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
        test_cases : TestCase[];
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
        output_lines : OutputLine[];
        status : number;
        completed : boolean;
        message : string;
    }
}
