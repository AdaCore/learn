import runpy


def main() -> None:
    runpy.run_module('rst_code_example_pipeline.check_projects',
                     run_name='__main__', alter_sys=True)
