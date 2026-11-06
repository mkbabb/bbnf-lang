"""Shared fail-closed mechanics for the CSS L4 epoch-2 oracle."""

from __future__ import annotations

import hashlib
import json
import os
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Any, NoReturn


ROOT = Path(__file__).resolve().parents[1]
EPOCH1 = ROOT.parent
DEFAULT_CSSWG = Path("/private/tmp/bbnf-csswg.X76O73/csswg-drafts")
DEFAULT_WPT = ROOT / "vendor" / "wpt"
CSSWG_REVISION = "c7573530343759ace8e46438a1fa2c44515b5554"
WPT_REVISION = "37cd7ff74eb974fd41600bf7bc01d37576abf8b5"


@dataclass(frozen=True)
class EpochError(Exception):
    code: str
    message: str
    context: dict[str, Any]

    def __str__(self) -> str:
        return json.dumps(
            {"error": self.code, "message": self.message, "context": self.context},
            sort_keys=True,
        )


def fail(code: str, message: str, **context: Any) -> NoReturn:
    raise EpochError(code, message, context)


def sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def read_json(path: Path) -> dict[str, Any]:
    try:
        value = json.loads(path.read_text())
    except (OSError, json.JSONDecodeError) as error:
        fail("INPUT_JSON_INVALID", "cannot read canonical JSON input", path=str(path), detail=str(error))
    if not isinstance(value, dict):
        fail("INPUT_JSON_SHAPE", "top-level JSON value must be an object", path=str(path))
    return value


def encoded_json(value: Any) -> bytes:
    return (json.dumps(value, indent=2, sort_keys=True, ensure_ascii=False) + "\n").encode()


def write_json(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(encoded_json(value))


def git(root: Path, *arguments: str) -> str:
    try:
        result = subprocess.run(
            ["git", "-C", str(root), *arguments],
            check=True,
            capture_output=True,
            text=True,
            env={**os.environ, "GIT_NO_LAZY_FETCH": "1", "GIT_TERMINAL_PROMPT": "0"},
        )
    except subprocess.CalledProcessError as error:
        fail(
            "GIT_INPUT_INVALID",
            "pinned repository command failed",
            root=str(root),
            arguments=list(arguments),
            stderr=error.stderr.strip(),
        )
    return result.stdout.strip()


def require_revision(root: Path, revision: str, label: str) -> None:
    actual = git(root, "rev-parse", "HEAD")
    if actual != revision:
        fail("REVISION_MISMATCH", "pinned checkout has the wrong revision", label=label, expected=revision, actual=actual)


def line_of(data: bytes, offset: int) -> int:
    return data.count(b"\n", 0, offset) + 1


def collapse(text: str) -> str:
    return " ".join(text.split())


def terminal(status: str) -> bool:
    return status not in {"UNMAPPED", "UNRUN", "UNPROVEN", "MISSING", "CONFLICT", "BLOCKED_INPUT"}


def reject_candidate_schema_use(document: dict[str, Any]) -> None:
    policy = document.get("candidate_input_policy")
    if policy != "OBSERVATION_ONLY_FORBID_SCHEMA_OR_TYPE_INPUT":
        fail(
            "CANDIDATE_INPUT_POLICY",
            "oracle artifact does not mechanically forbid schema/type consumption",
            actual=policy,
        )
