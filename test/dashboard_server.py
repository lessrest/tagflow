"""Deterministic browser fixture; never used by the demo service."""

from examples.dashboard.app import create_app

app = create_app(simulate=False)
