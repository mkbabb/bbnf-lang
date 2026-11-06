"""Small deterministic primitives shared by the epoch-3 observation assay."""

from __future__ import annotations

import hashlib
import json
import os
import socket
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any, NoReturn


ROOT = Path(__file__).resolve().parents[1]
TRANCHE_ROOT = ROOT.parents[2]
INPUTS = ROOT / "inputs"
OBJECTS = INPUTS / "objects" / "sha256"
DEFAULT_CSSWG = Path("/private/tmp/bbnf-csswg.X76O73/csswg-drafts")
DEFAULT_WPT = ROOT.parent / "epoch-2" / "vendor" / "wpt"
CSSWG_REVISION = "c7573530343759ace8e46438a1fa2c44515b5554"
WPT_REVISION = "37cd7ff74eb974fd41600bf7bc01d37576abf8b5"
WPT_TREE = "12fa64c9afe86e199ee0e737755b6507f363ceff"
WPT_PATH_OID_SHA256 = "cf3aedd08cbb161aeebd3c9e19c10f855f2be0be23fa5ecbbd2035d2e31db699"
PYTHON_VERSION = (3, 14, 5)
PYTHON_BINARY_SHA256 = "2477b47fa3ae65b9574eb18a15edb364e96948eaa1875ad3f1c80d780efc9c12"


@dataclass(frozen=True)
class GateError(Exception):
    code: str
    message: str
    context: dict[str, Any]

    def __str__(self) -> str:
        return json.dumps(
            {"code": self.code, "message": self.message, "context": self.context},
            sort_keys=True,
        )


def fail(code: str, message: str, **context: Any) -> NoReturn:
    raise GateError(code, message, context)


def sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def require_toolchain() -> dict[str, Any]:
    executable = Path(sys.executable).resolve()
    actual_version = tuple(sys.version_info[:3])
    try:
        digest = sha256(executable.read_bytes())
    except OSError as error:
        fail("PYTHON_TOOLCHAIN_UNREADABLE", "cannot hash the executing Python binary", executable=str(executable), detail=str(error))
    if sys.implementation.name != "cpython" or actual_version != PYTHON_VERSION or digest != PYTHON_BINARY_SHA256:
        fail(
            "PYTHON_TOOLCHAIN_MISMATCH",
            "epoch-3 replay requires its exact frozen CPython interpreter",
            implementation=sys.implementation.name,
            expected_version=PYTHON_VERSION,
            actual_version=actual_version,
            expected_sha256=PYTHON_BINARY_SHA256,
            actual_sha256=digest,
        )
    return {"implementation": "CPython", "version": ".".join(map(str, actual_version)), "binary_sha256": digest}


def enforce_offline() -> None:
    def denied(*_args: Any, **_kwargs: Any) -> NoReturn:
        fail("NETWORK_FORBIDDEN", "epoch-3 verification attempted to create a network connection")

    socket.create_connection = denied
    socket.socket.connect = denied
    socket.socket.connect_ex = denied


def encode(value: Any) -> bytes:
    return (json.dumps(value, indent=2, sort_keys=True, ensure_ascii=False) + "\n").encode()


def load(path: Path) -> Any:
    def unique(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
        result = {}
        for key, value in pairs:
            if key in result:
                fail("INPUT_JSON_DUPLICATE_KEY", "input JSON contains a duplicate object key", path=str(path), key=key)
            result[key] = value
        return result
    try:
        return json.loads(path.read_text(), object_pairs_hook=unique)
    except (OSError, json.JSONDecodeError) as error:
        fail("INPUT_JSON_INVALID", "input is not canonical readable JSON", path=str(path), detail=str(error))


def store(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(encode(value))


def object_path(digest: str) -> Path:
    if len(digest) != 64 or any(char not in "0123456789abcdef" for char in digest):
        fail("DIGEST_INVALID", "content digest is not lowercase SHA-256", digest=digest)
    return OBJECTS / digest[:2] / digest[2:]


def object_bytes(record: dict[str, Any]) -> bytes:
    digest = record.get("sha256")
    path = object_path(digest)
    try:
        data = path.read_bytes()
    except OSError as error:
        fail("OBJECT_MISSING", "content-addressed authority byte object is absent", sha256=digest, detail=str(error))
    if len(data) != record.get("bytes") or sha256(data) != digest:
        fail("OBJECT_DRIFT", "content-addressed authority byte object does not match its receipt", sha256=digest)
    return data


def git(root: Path, *arguments: str) -> str:
    try:
        result = subprocess.run(
            ["git", "-C", str(root), *arguments],
            check=True,
            capture_output=True,
            text=True,
            env={**os.environ, "GIT_NO_LAZY_FETCH": "1", "GIT_TERMINAL_PROMPT": "0"},
        )
    except (OSError, subprocess.CalledProcessError) as error:
        detail = getattr(error, "stderr", str(error))
        fail("GIT_INPUT_INVALID", "pinned repository verification failed", root=str(root), detail=str(detail).strip())
    return result.stdout


def require_checkout(root: Path, revision: str, label: str) -> None:
    actual = git(root, "rev-parse", "HEAD").strip()
    if actual != revision:
        fail("REVISION_MISMATCH", "pinned checkout revision differs", label=label, expected=revision, actual=actual)


def line_of(data: bytes, offset: int) -> int:
    return data.count(b"\n", 0, offset) + 1


def blockers(stage: dict[str, Any]) -> list[dict[str, Any]]:
    value = stage.get("root_blockers", [])
    if not isinstance(value, list):
        fail("BLOCKER_SHAPE", "root blockers must be an array", stage=stage.get("stage"))
    return value


def require_clean(stage: dict[str, Any], consumer: str) -> None:
    open_items = blockers(stage)
    if open_items:
        fail(
            "PREREQUISITE_BLOCKED",
            "downstream stage refused a semantically unclean prerequisite",
            prerequisite=stage.get("stage"),
            consumer=consumer,
            blocker_count=len(open_items),
        )


def governing_receipts() -> list[dict[str, str]]:
    document = load(INPUTS / "governing.json")
    rows = document.get("documents")
    if document.get("schema_version") != 1 or not isinstance(rows, list) or len(rows) != 3:
        fail("GOVERNING_MANIFEST_INVALID", "epoch-3 must bind the lock, wave, and predecessor adjudication")
    roles = {"lock-constitution", "governing-wave", "binding-successor-contract"}
    if {row.get("role") for row in rows} != roles:
        fail("GOVERNING_ROLE_SET_INVALID", "governing document roles differ from the epoch-3 contract")
    for row in rows:
        path = TRANCHE_ROOT / row["path"]
        try:
            data = path.read_bytes()
        except OSError as error:
            fail("GOVERNING_DOCUMENT_MISSING", "governing document is unreadable", path=str(path), detail=str(error))
        if sha256(data) != row.get("sha256"):
            fail("GOVERNING_DOCUMENT_DRIFT", "governing document differs from its sealed epoch-3 hash", path=row["path"], expected=row.get("sha256"), actual=sha256(data))
    return rows
