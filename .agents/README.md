# Orb development environment

Setup prepares Python from `.python-version`, installs `uv.lock` with all extras
and development dependencies, and installs SBCL with Debian's Alexandria and
FiveAM packages. Repeated setup runs reuse the environment and compilation caches.
Resume does not install anything. No secrets or backing services are required.

Run Python commands from the repository root:

```sh
uv run --no-sync pytest
uv run --no-sync ruff check src test
uv run --no-sync pyright
```

The Lisp Makefile assumes a personal Quicklisp installation. In orbs, use the
installed ASDF systems directly instead (from the repository root):

```sh
sbcl --noinform --non-interactive --eval '(require :asdf)' \
  --eval '(asdf:load-asd (truename "lisp/tagflow.asd"))' \
  --eval '(asdf:test-system "tagflow")'
```

The development dependencies include Trio and Rich for `demo.py`, and HTTPX
for integration tests. To use the prepared environment and local Tagflow code
rather than the demo's inline script environment, run:

```sh
uv run --no-sync python demo.py
```

Setup also installs the dashboard's locked Tailwind and Playwright tools, compiles
its CSS, and prepares Chromium. Run browser regressions with
`npm --prefix examples/dashboard test`. No demo server starts automatically.
Run the HTML/htmx dashboard with:

```sh
amp orb services ensure
```

Use the printed portal URL. The finite simulation starts with the process; use
`amp orb service restart dashboard` to replay it. See
[the dashboard guide](../examples/dashboard/README.md) for styling and protocol details.
