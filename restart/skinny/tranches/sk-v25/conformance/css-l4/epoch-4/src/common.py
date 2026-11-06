"""Deterministic primitives for the compact epoch-4 CSS observation assay."""

from __future__ import annotations

import hashlib
import json
import socket
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any, NoReturn


ROOT = Path(__file__).resolve().parents[1]
TRANCHE_ROOT = ROOT.parents[2]
INPUTS = ROOT / "inputs"
OBJECTS = INPUTS / "objects" / "sha256"
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


def git_blob_oid(data: bytes) -> str:
    return hashlib.sha1(b"blob " + str(len(data)).encode() + b"\0" + data).hexdigest()


def encode(value: Any) -> bytes:
    return (json.dumps(value, indent=2, sort_keys=True, ensure_ascii=False) + "\n").encode()


def loads(data: bytes, *, source: str) -> Any:
    def unique(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
        result: dict[str, Any] = {}
        for key, value in pairs:
            if key in result:
                fail("JSON_DUPLICATE_KEY", "JSON object key is duplicated", source=source, key=key)
            result[key] = value
        return result

    try:
        return json.loads(data, object_pairs_hook=unique)
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        fail("JSON_INVALID", "JSON input is not valid UTF-8 JSON", source=source, detail=str(error))


def load(path: Path) -> Any:
    try:
        return loads(path.read_bytes(), source=path.relative_to(ROOT).as_posix())
    except OSError as error:
        fail("INPUT_UNREADABLE", "required input is unreadable", path=str(path), detail=str(error))


def store(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(encode(value))


def object_path(digest: str) -> Path:
    if not isinstance(digest, str) or len(digest) != 64 or any(c not in "0123456789abcdef" for c in digest):
        fail("DIGEST_INVALID", "object digest is not lowercase SHA-256", digest=digest)
    return OBJECTS / digest[:2] / digest[2:]


def object_bytes(record: dict[str, Any]) -> bytes:
    digest = record.get("sha256")
    path = object_path(digest)
    try:
        data = path.read_bytes()
    except OSError as error:
        fail("OBJECT_MISSING", "content-addressed input object is absent", sha256=digest, detail=str(error))
    if len(data) != record.get("bytes") or sha256(data) != digest:
        fail("OBJECT_DRIFT", "content-addressed input object differs from its receipt", sha256=digest)
    return data


def require_toolchain() -> dict[str, Any]:
    executable = Path(sys.executable).resolve()
    actual_version = tuple(sys.version_info[:3])
    try:
        digest = sha256(executable.read_bytes())
    except OSError as error:
        fail("PYTHON_UNREADABLE", "cannot hash the executing Python binary", detail=str(error))
    if sys.implementation.name != "cpython" or actual_version != PYTHON_VERSION or digest != PYTHON_BINARY_SHA256:
        fail(
            "PYTHON_MISMATCH",
            "replay requires the exact frozen CPython binary",
            implementation=sys.implementation.name,
            expected_version=PYTHON_VERSION,
            actual_version=actual_version,
            expected_sha256=PYTHON_BINARY_SHA256,
            actual_sha256=digest,
        )
    return {
        "implementation": "CPython",
        "version": ".".join(map(str, actual_version)),
        "binary_sha256": digest,
    }


def enforce_offline() -> None:
    def denied(*_args: Any, **_kwargs: Any) -> NoReturn:
        fail("NETWORK_FORBIDDEN", "offline proof attempted to create a network connection")

    socket.create_connection = denied
    socket.socket.connect = denied
    socket.socket.connect_ex = denied


def evidence(kind: str, identity: str, digest: str, **extra: Any) -> dict[str, Any]:
    if not identity or len(digest) != 64:
        fail("EVIDENCE_INVALID", "positive state evidence lacks an exact identity", kind=kind)
    return {"kind": kind, "identity": identity, "sha256": digest, **extra}


def positive(state: str, receipts: list[dict[str, Any]]) -> dict[str, Any]:
    if not receipts:
        fail("POSITIVE_WITHOUT_EVIDENCE", "a positive local state requires checked evidence", state=state)
    return {"status": state, "evidence": receipts}
