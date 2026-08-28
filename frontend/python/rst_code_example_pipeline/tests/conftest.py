"""
Fixtures shared by the whole rst_code_example_pipeline test suite.

Several entry points in the package change the process working directory and
never change it back: check_block() chdirs into the block directory it is
checking, and get_projects() chdirs into the build directory it is scanning.
A test that exercises either one therefore leaves the whole pytest session
pointing somewhere else -- usually at a temporary directory that is deleted
soon afterwards -- which makes every later test that uses a relative path
fail for reasons that have nothing to do with what it is testing.

The autouse fixture below restores the directory the session started in after
every test, so no test can leak a working-directory change into the next one.
"""
import os

import pytest


@pytest.fixture(autouse=True)
def restore_cwd():
    """Restore the working directory after each test."""
    original = os.getcwd()
    yield
    os.chdir(original)
