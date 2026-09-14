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

No demo server starts automatically.
