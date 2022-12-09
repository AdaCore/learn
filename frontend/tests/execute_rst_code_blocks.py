"""
Find all code blocks in a given rst file, and execute all of their defined modes using the defined AWS Lambda function.
"""


import argparse
import json
import os
import sys
import traceback

from pathlib import Path
from typing import Dict, List, Set, Optional
from websocket import WebSocket

SPHINX_PATH = (Path(__file__).parent.parent / 'sphinx').resolve()
sys.path.append(str(SPHINX_PATH))

from widget.chop import manual_chop, real_gnatchop

from code_block import CodeBlock, get_blocks


FUNCTION_URLS = {'prod': 'wss://api.learn.r53.adacore.com',
                 'sandbox': 'wss://api-sandbox.learn.r53.adacore.com'}

EXPECT_FAILURE_CLASSES = {
    'ada': ['ada-expect-compile-error', 'ada-run-expect-failure', 'ada-expect-prove-error'],
    'c': ['c-expect-compile-error', 'c-run-expect-failure']}

EXPECT_FAILURE_CLASS_BTNS = {
    'ada-expect-compile-error': ['compile', 'run'],
    'ada-run-expect-failure': ['run'],
    'ada-expect-prove-error': ['prove', 'prove_flow', 'prove_flow_report_all', 'prove_report_all'],
    'c-expect-compile-error': ['compile', 'run'],
    'c-run-expect-failure': ['run']
}

class CodeBlockException(Exception):
    pass

def parse_args() -> argparse.Namespace:
    """Parse program arguments

    Returns:
        argparse.Namespace: parsed arguments object
    """
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('rst_files', type=str, nargs="+", help="The rst file from which to extract doc")
    parser.add_argument('--verbose', '-v', action='store_true', help='Show more information')
    parser.add_argument('--env', type=str, default='prod', choices=['sandbox', 'prod'],
                        help='Whether to use the prod or sandbox environment to run the code blocks')
    parser.add_argument('--projects', type=str, default='',
                        help='Comma seperated string of project names to run e.g. Proj_1,Proj_2,Other_proj')
    parser.add_argument('--halt-on-failure', action='store_true', help='Exit with non-zero exit status on test failure')
    args = parser.parse_args()
    args.rst_files = [os.path.abspath(f) for f in args.rst_files]
    args.projects = set() if len(args.projects) == 0 else set(args.projects.split(','))
    return args


def get_code_blocks(rst_file: str) -> List[CodeBlock]:
    """Reads the provided rst_file and extracts all CodeBlocks from it. Any
    CodeBlocks that have a language other than Ada or c are filtered out.
    The ordering of CodeBlocks is maintained relative to how they appear in the file.

    Args:
        rst_file (str): path to rst file to parsed

    Raises:
        CodeBlockException: if the project name of a CodeBlock is not set

    Returns:
        List[CodeBlock]: a List of CodeBlocks found in the rst file
    """
    with open(rst_file) as f:
        content = f.read()
    all_blocks = get_blocks(content, rst_file)
    filtered_blocks = []
    for b in all_blocks:
        if not isinstance(b, CodeBlock): continue
        if not b.language in ['ada', 'c']: continue
        if b.project is None:
            raise CodeBlockException(f"Error: project not set: {b.loc}")
        filtered_blocks.append(b)
    return filtered_blocks


def group_by_project(blocks: List[CodeBlock]) -> Dict[str, List[CodeBlock]]:
    """Group a list of CodeBlocks by their project name. The ordering of CodeBlocks is maintained

    Args:
        blocks (List[CodeBlock]): CodeBlocks ot be grouped

    Returns:
        Dict[str, List[CodeBlock]]: map of projects to a list of CodeBlocks that share that project name
    """
    projects: Dict[str, List[CodeBlock]] = {}
    for b in code_blocks:
        if b.project not in projects:
            projects[b.project] = []
        projects[b.project].append(b)
    return projects


def filter_projects(projects: Dict[str, List[CodeBlock]],
                    keep: Set[str]) -> Dict[str, List[CodeBlock]]:
    """Filter a dictionary of projects, only keeping projects whose name is in `keep`.
    If `keep` is emtpy, it will return all projects.

    Args:
        projects (Dict[str, List[CodeBlock]]): dict of projects to filter
        keep (Set[str]): Optional set of project names to keep.

    Returns:
        Dict[str, List[CodeBlock]]: filtered dict of projects
    """
    if len(keep) == 0:
        return projects
    return {k: v for k, v in projects.items() if k in keep}


def call_websocket_endpoint(name: str,
                            mode: str,
                            main_file: str,
                            switches: Dict[str, List[str]],
                            files: List[Dict[str, str]],
                            websocket_url: str) -> Optional[List[dict]]:
    """Call the websocket endpoint with the provided information

    Args:
        name (str): used to identify a run
        mode (str): mode to be executed. Relates the button being pressed
        main_file (str): file name of the main file. Empty if one is not defined
        switches (Dict[str, List[str]]): map of switch type, to a list of switches to be set
        files (List[Dict[str, str]]): List of dicts that contain filenames, and file contents
        websocket_url (str): websocket url to be used

    Returns:
        Optional[List[dict]]: list of responses if successful, None otherwise
    """
    msg = msg = {
        "action": "execute",
        "data": {
            "files": files,
            "main": main_file,
            "mode": mode,
            "switches": switches,
            "name": name,
            "lab": False
            }
        }
    retry_count = 5
    all_exceptions = []
    while retry_count > 0:
        retry_count -= 1
        try:
            ws = WebSocket()
            ws.connect(websocket_url)
            ws.send(json.dumps(msg))
            received = None
            result = []
            result_with_msg = [msg]
            while True:
                received = json.loads(ws.recv())
                result.append(received)
                if 'connectionId' in received:
                    break
                if received['completed']:
                    break
            ws.close()
            result_with_msg.extend(result)
            return result_with_msg
        except Exception as e:
            print(f"Retry: {name}")
            all_exceptions.append[e]
    print(f"Failed to run: {name}: {all_exceptions}")
    return None


def run_code_block(block: CodeBlock,
                   deps: Dict[str, str],
                   websocket_url: str,
                   verbose: bool,
                   halt_on_failure: bool):
    """Run a single code block using the provided url. `deps` should contain a mapping of file names to file
    contents from the same project, but earlier in the file. This is because projects files are accumulated if not
    overwritten. `deps` will also be updated with all files in the current CodeBlock.

    Args:
        block (CodeBlock): to be run
        deps (Dict[str, str]): dict of file names to file contents files from earlier CodeBlocks in the same project
        websocket_url (str): url of the websocket endpoint that should be used to execute a task
        verbose (bool): if True, extra log statements will be emitted
        halt_on_failure (bool): if True and a test fails, exit with a non-zero exit status
    """#
    # Extract source files from the current editor block
    if block.manual_chop:
        source_files = manual_chop(block.text.splitlines())
    else:
        source_files = real_gnatchop(block.text.splitlines())

    # Replace any existing project files with the same name
    for f in source_files:
        deps[f.basename] = f.content

    # Update the list of source files with the previously found files
    source_files = [{'basename': f, 'contents': deps[f]} for f in deps]

    # Skip the CodeBlock if ...
    no_check = any(sphinx_class in ["ada-nocheck", "c-nocheck"] for sphinx_class in block.classes)
    if no_check or not block.run or 'ada-syntax-only' in block.classes or 'ada-norun' in block.classes:
        if verbose:
            print(f"Skipping code block: {block.loc}")
        return

    # If there are no source files something went wrong
    if len(source_files) == 0:
        print(f"Skipping code block: Failed to chop example: {block.loc}")
        return

    main_file = "" if block.main_file is None else block.main_file

    switches = {}
    if len(block.compiler_switches) > 0:
        switches['Compiler'] = block.compiler_switches

    # For each button, run the CodeBlock using the lambda function
    for b in block.buttons:
        if b == 'no':
            continue
        name = f"{block.project}.{block.loc}.{b}"
        res = call_websocket_endpoint(name=name,
                                      mode=b,
                                      main_file=main_file,
                                      switches=switches,
                                      files=source_files,
                                      websocket_url=websocket_url)
        if res is None:
            continue
        if 'connectionId' in res[-1]:
            print(f"AWS error: {name}: {res}")
            continue
        # If it ran, check that the exit status matched the expectation
        exit_status = res[-1]['status']
        expect_failure = False
        for c in EXPECT_FAILURE_CLASSES[block.language]:
            if c in block.classes and b in EXPECT_FAILURE_CLASS_BTNS[c]:
                expect_failure = True
                break
        if expect_failure and exit_status == 0:
            print(f"Fail: {name}: expected failure, received zero exit status: {res}")
            if halt_on_failure:
                exit(1)
        elif not expect_failure and exit_status != 0:
            print(f"Fail: {name}: expected success, received non-zero exit status: {res}")
            if halt_on_failure:
                exit(2)
        elif verbose:
            print(f"Pass: {name}")


def run_all_projects(projects: Dict[str, List[CodeBlock]],
                     websocket_url: str,
                     verbose: bool,
                     halt_on_failure: bool):
    """Run all of the CodeBlocks in `projects` using the websocket url

    Args:
        projects (Dict[str, List[CodeBlock]]): dict of project names that each map to a list of CodeBlocks to be run
        websocket_url (str): url of the websocket endpoint that should be used to execute a task
        verbose (bool): if True, extra log statements will be emitted
        halt_on_failure (bool): if True and a test fails, exit with a non-zero exit status
    """
    all_deps = {}
    for p in projects:
        for b in projects[p]:
            if p not in all_deps:
                all_deps[p] = {}
            run_code_block(b, all_deps[p], websocket_url, verbose, halt_on_failure)


if __name__ == "__main__":
    args = parse_args()
    lambda_url = FUNCTION_URLS[args.env]
    for f in args.rst_files:
        code_blocks = get_code_blocks(f)
        projects = group_by_project(code_blocks)
        projects = filter_projects(projects, args.projects)
        run_all_projects(projects, lambda_url, args.verbose, args.halt_on_failure)
