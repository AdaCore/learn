# Browser-based widget tests

End-to-end tests that open generated pages in a real (headless) browser, click
each **Run** button, and assert that no widget exits with a non-zero status.

These tests require a running dev server and a separate Python virtual
environment; they are kept separate from `frontend/tests/` for that reason.

## Prerequisites

- **Remote site** (`--base-url https://learn-latest.adacore.com`): no local
  server needed; Playwright opens the live page directly.

- **Local build** (default `http://127.0.0.1:8080`): the dev server must be
  running before the test:

  ```bash
  cd frontend
  pnpm run dev        # or: make local && python3 -m http.server 8080 -d dist/html
  ```

- One-time setup — create a dedicated virtual environment:

  ```bash
  cd frontend/web_tests
  python3 -m venv .venv
  source .venv/bin/activate
  pip install -r requirements.txt
  playwright install chromium
  ```

## Running the tests

Activate the virtual environment first:

```bash
source frontend/web_tests/.venv/bin/activate
cd frontend/web_tests
```

Test a single page:

```bash
pytest --page courses/advanced-ada/parts/data_types/numerics.html
```

Test several pages:

```bash
pytest \
  --page courses/advanced-ada/parts/data_types/numerics.html \
  --page courses/advanced-ada/parts/data_types/strings.html
```

Test against a remote site:

```bash
pytest --base-url https://learn-latest.adacore.com \
       --page courses/advanced-ada/parts/data_types/numerics.html
```

## Useful options

| Option | Effect |
|---|---|
| `--headed` | Run with a visible browser window |
| `-s` | Print per-widget progress to the terminal |
| `-v` | Verbose pytest output (one line per page) |
| `--base-url URL` | Override the server URL (default: `http://127.0.0.1:8080`) |
| `--browser firefox` | Use Firefox instead of Chromium |

## How it works

For each `<pre class="widget">` element that has a **Run** button:

1. The button is clicked.
2. The test waits for the button to be re-enabled — `widget.ts` re-enables all
   buttons in the group only after the server round-trip completes.
3. Any `div.output_error` elements in the output area are collected.
4. If any errors were found the test fails and lists the offending widget IDs.
