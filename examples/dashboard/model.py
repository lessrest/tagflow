"""A shared, finite simulation. No Nix, database, or viewer sessions."""

from dataclasses import dataclass
from uuid import uuid4

import anyio


PACKAGES = (
    "zlib",
    "libffi",
    "openssl",
    "sqlite",
    "curl",
    "ripgrep",
    "python",
    "ninja",
    "cmake",
    "meson",
    "git",
    "wayland",
    "llvm",
    "rustc",
    "neovim",
    "helix",
    "firefox",
    "inkscape",
)
PHASES = ("Fetching sources", "Configuring", "Building", "Running checks")
STATES = ("queued", "running", "passed", "failed")


@dataclass(frozen=True)
class Build:
    id: int
    name: str
    state: str
    phase: str
    lines: tuple[str, ...]


@dataclass(frozen=True)
class Snapshot:
    epoch: str
    tick: int
    builds: tuple[Build, ...]

    @property
    def revision(self) -> str:
        return f"{self.epoch}.{self.tick}"

    @property
    def complete(self) -> bool:
        return all(b.state in ("passed", "failed") for b in self.builds)


class Campaign:
    def __init__(self):
        self.epoch = uuid4().hex
        self.snapshot = self.at(0)

    def at(self, tick: int) -> Snapshot:
        builds = []
        for i, name in enumerate(PACKAGES):
            age = tick + 10 - i * 2
            state = (
                "queued"
                if age < 0
                else "running"
                if age < 8
                else "failed"
                if i % 7 == 3
                else "passed"
            )
            phase = (
                "Waiting for a builder"
                if age < 0
                else PHASES[age // 2]
                if age < 8
                else "Check failed"
                if state == "failed"
                else "Output verified"
            )
            records = (
                f"→ {name}: source acquired",
                "checking source digest… ok",
                "configuring for x86_64-linux",
                "configuration complete",
                f"building {name}",
                "linking outputs",
                "running install checks",
                "checking output references",
                "error: expected fixture was not produced"
                if state == "failed"
                else "all checks passed; output verified",
            )
            lines = records[: max(0, min(age + 1, len(records)))]
            builds.append(Build(i + 1, name, state, phase, lines))
        return Snapshot(self.epoch, tick, tuple(builds))

    def advance(self) -> None:
        if not self.snapshot.complete:
            self.snapshot = self.at(self.snapshot.tick + 1)

    async def run(self) -> None:
        while not self.snapshot.complete:
            await anyio.sleep(4)
            self.advance()
