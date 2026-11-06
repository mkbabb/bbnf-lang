"""Exact-byte primitives for the epoch-5 CSS W0 assay."""

from __future__ import annotations

import gzip
import hashlib
import json
import os
import socket
import subprocess
import sys
import sysconfig
from dataclasses import dataclass
from pathlib import Path
from typing import Any, NoReturn


ROOT = Path(__file__).resolve().parents[1]
CSS_ROOT = ROOT.parent
EPOCH4 = CSS_ROOT / "epoch-4"
INPUTS = ROOT / "inputs"
OBJECTS = INPUTS / "objects" / "sha256"
EPOCH4_OBJECTS = EPOCH4 / "inputs" / "objects" / "sha256"


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


def git_tree_oid(entries: list[tuple[str, str, str]]) -> str:
    body = bytearray()
    # Git compares a tree name as if it had a trailing slash. Plain byte sorting
    # is wrong for cases such as directory `foo` beside file `foo.ext`.
    def key(row: tuple[str, str, str]) -> bytes:
        mode, name, _oid = row
        return name.encode("utf-8", errors="strict") + (b"/" if mode == "040000" else b"")

    for mode, name, oid in sorted(entries, key=key):
        encoded_mode = "40000" if mode == "040000" else mode
        body.extend(encoded_mode.encode() + b" " + name.encode() + b"\0" + bytes.fromhex(oid))
    return hashlib.sha1(b"tree " + str(len(body)).encode() + b"\0" + body).hexdigest()


def encode(value: Any) -> bytes:
    return (json.dumps(value, indent=2, sort_keys=True, ensure_ascii=False) + "\n").encode()


def loads(data: bytes, *, source: str) -> Any:
    def unique(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
        result: dict[str, Any] = {}
        for key, value in pairs:
            if key in result:
                fail("JSON_DUPLICATE_KEY", "JSON key is duplicated", source=source, key=key)
            result[key] = value
        return result

    try:
        return json.loads(data, object_pairs_hook=unique)
    except (UnicodeDecodeError, json.JSONDecodeError) as error:
        fail("JSON_INVALID", "input is not unique-key UTF-8 JSON", source=source, detail=str(error))


def load(path: Path) -> Any:
    try:
        return loads(path.read_bytes(), source=str(path))
    except OSError as error:
        fail("INPUT_UNREADABLE", "required input cannot be read", path=str(path), detail=str(error))


def store(path: Path, value: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(encode(value))


def exact_file(path: Path, expected: str) -> bytes:
    try:
        data = path.read_bytes()
    except OSError as error:
        fail("BOUND_INPUT_MISSING", "hash-bound input is absent", path=str(path), detail=str(error))
    if sha256(data) != expected:
        fail("BOUND_INPUT_DRIFT", "hash-bound input differs", path=str(path), expected=expected, actual=sha256(data))
    return data


def object_bytes(record: dict[str, Any], *, epoch4: bool = True) -> bytes:
    digest = record.get("sha256")
    if not isinstance(digest, str) or len(digest) != 64 or any(char not in "0123456789abcdef" for char in digest):
        fail("DIGEST_INVALID", "object digest is not lowercase SHA-256", digest=digest)
    root = EPOCH4_OBJECTS if epoch4 else OBJECTS
    path = root / digest[:2] / digest[2:]
    data = exact_file(path, digest)
    if len(data) != record.get("bytes"):
        fail("OBJECT_LENGTH_DRIFT", "content-addressed object length differs", sha256=digest)
    return data


def gzip_object(record: dict[str, Any], *, epoch4: bool = True) -> bytes:
    compressed = object_bytes(record, epoch4=epoch4)
    try:
        return gzip.decompress(compressed)
    except (OSError, EOFError) as error:
        fail("GZIP_INVALID", "content-addressed input is not valid gzip", detail=str(error))


def load_contract() -> dict[str, Any]:
    contract = load(INPUTS / "contract.json")
    if contract.get("schema_version") != 1:
        fail("CONTRACT_SCHEMA", "input contract schema is unsupported")
    for row in contract.get("epoch4_inputs", []):
        exact_file((ROOT / row["path"]).resolve(), row["sha256"])
    adjudication = contract["adjudication"]
    exact_file((ROOT / adjudication["path"]).resolve(), adjudication["sha256"])
    return contract


def load_epoch4_inputs() -> dict[str, Any]:
    contract = load_contract()
    by_name = {Path(row["path"]).name: row for row in contract["epoch4_inputs"]}
    return {
        "contract": contract,
        "manifest": loads(exact_file(EPOCH4 / "inputs" / "manifest.json", by_name["manifest.json"]["sha256"]), source="epoch4-manifest"),
        "policy": loads(exact_file(EPOCH4 / "inputs" / "policy.json", by_name["policy.json"]["sha256"]), source="epoch4-policy"),
        "governing": loads(exact_file(EPOCH4 / "inputs" / "governing.json", by_name["governing.json"]["sha256"]), source="epoch4-governing"),
        "controls": loads(exact_file(EPOCH4 / "inputs" / "downstream-controls.json", by_name["downstream-controls.json"]["sha256"]), source="epoch4-controls"),
    }


def require_toolchain() -> dict[str, str]:
    contract = load_contract()["python"]
    executable = Path(sys.executable).resolve()
    digest = sha256(executable.read_bytes())
    version = ".".join(map(str, sys.version_info[:3]))
    if sys.implementation.name != "cpython" or version != contract["version"] or digest != contract["binary_sha256"]:
        fail("PYTHON_MISMATCH", "replay requires the exact sealed CPython", version=version, digest=digest)
    return {"implementation": "CPython", "version": version, "binary_sha256": digest}


def verify_runtime() -> dict[str, Any]:
    contract = load_contract()["python"]
    toolchain = require_toolchain()
    expected_environment = contract["environment"]
    actual_environment = dict(os.environ)
    if actual_environment != expected_environment:
        fail(
            "ENVIRONMENT_NOT_ALLOWLISTED",
            "replay environment differs from the closed allowlist",
            unexpected=sorted(set(actual_environment) - set(expected_environment)),
            missing=sorted(set(expected_environment) - set(actual_environment)),
            changed=sorted(key for key in set(actual_environment) & set(expected_environment) if actual_environment[key] != expected_environment[key]),
        )
    actual_flags = {
        "dont_write_bytecode": sys.flags.dont_write_bytecode,
        "isolated": sys.flags.isolated,
        "no_user_site": sys.flags.no_user_site,
        "optimize": sys.flags.optimize,
        "safe_path": sys.flags.safe_path,
        "utf8_mode": sys.flags.utf8_mode,
        "warn_default_encoding": sys.flags.warn_default_encoding,
    }
    if actual_flags != contract["startup_flags"]:
        fail("STARTUP_FLAGS_DRIFT", "behavior-affecting Python startup flags differ", expected=contract["startup_flags"], actual=actual_flags)

    stdlib = contract["stdlib"]
    root = Path(sysconfig.get_paths()["stdlib"]).resolve()
    if str(root) != stdlib["root"]:
        fail("STDLIB_ROOT_DRIFT", "executing stdlib root differs", actual=str(root))
    rows: list[tuple[str, int, str]] = []
    extensions: list[Path] = []
    for path in sorted(root.rglob("*"), key=lambda item: item.relative_to(root).as_posix().encode("utf-8", errors="strict")):
        relative = path.relative_to(root)
        if not path.is_file() or path.is_symlink() or "__pycache__" in relative.parts or "site-packages" in relative.parts or path.suffix in {".pyc", ".pyo"}:
            continue
        data = path.read_bytes()
        rows.append((relative.as_posix(), len(data), sha256(data)))
        if path.suffix == ".so":
            extensions.append(path)
    digest = hashlib.sha256()
    for relative, size, value in rows:
        digest.update(relative.encode() + b"\0" + str(size).encode() + b"\0" + bytes.fromhex(value) + b"\0")
    actual_stdlib = {"file_count": len(rows), "bytes": sum(row[1] for row in rows), "subject_sha256": digest.hexdigest()}
    if any(actual_stdlib[key] != stdlib[key] for key in actual_stdlib):
        fail("STDLIB_TREE_DRIFT", "sealed stdlib/runtime tree differs", expected={key: stdlib[key] for key in actual_stdlib}, actual=actual_stdlib)

    otool = contract["otool"]
    exact_file(Path(otool["path"]), otool["sha256"])
    if Path(otool["path"]).stat().st_size != otool["bytes"]:
        fail("OTOOL_LENGTH_DRIFT", "sealed dynamic-loader inspector differs")
    dynamic_paths = {Path(row["path"]).resolve(): row for row in contract["dynamic_closure"]}
    for path, row in dynamic_paths.items():
        data = exact_file(path, row["sha256"])
        if len(data) != row["bytes"]:
            fail("DYNAMIC_INPUT_LENGTH_DRIFT", "non-system dynamic input length differs", path=str(path))
    targets = [Path(sys.executable).resolve(), *extensions, *dynamic_paths]
    result = subprocess.run(
        [otool["path"], "-L", *(str(path) for path in targets)],
        check=True,
        capture_output=True,
        env=expected_environment,
    )
    observed: set[Path] = set()
    current_target: Path | None = None
    for raw in result.stdout.decode("utf-8", errors="strict").splitlines():
        if not raw[:1].isspace():
            if raw.endswith(":"):
                current_target = Path(raw[:-1]).resolve()
            continue
        line = raw.strip()
        if not line.startswith("/"):
            continue
        dependency = line.split(" (compatibility version", 1)[0]
        if dependency.startswith(("/System/", "/usr/lib/")):
            continue
        resolved = Path(dependency).resolve()
        if resolved != current_target:
            observed.add(resolved)
    # Ignore only a target's own install-name edge above. Every other non-system
    # edge must resolve into the closed list, and every listed node must be used.
    expected_dynamic = set(dynamic_paths)
    if observed != expected_dynamic:
        fail("DYNAMIC_CLOSURE_DRIFT", "non-system loader edges differ from sealed closure", observed=sorted(map(str, observed)), expected=sorted(map(str, expected_dynamic)))
    return {
        **toolchain,
        "startup_flags": actual_flags,
        "environment_sha256": sha256(encode(expected_environment)),
        "stdlib": actual_stdlib,
        "dynamic_file_count": len(dynamic_paths),
        "dynamic_bytes": sum(row["bytes"] for row in dynamic_paths.values()),
    }


def enforce_offline() -> None:
    def denied(*_args: Any, **_kwargs: Any) -> NoReturn:
        fail("NETWORK_FORBIDDEN", "offline replay attempted a network connection")

    socket.create_connection = denied
    socket.socket.connect = denied
    socket.socket.connect_ex = denied


def receipt(kind: str, identity: str, data: bytes, **extra: Any) -> dict[str, Any]:
    return {"kind": kind, "identity": identity, "bytes": len(data), "sha256": sha256(data), **extra}


def evidence_index(rows: list[dict[str, Any]]) -> dict[str, tuple[int, str]]:
    result: dict[str, tuple[int, str]] = {}
    for row in rows:
        identity = row.get("identity")
        digest = row.get("sha256")
        size = row.get("bytes")
        if not isinstance(identity, str) or not identity:
            fail("EVIDENCE_IDENTITY_INVALID", "evidence identity is empty")
        if not isinstance(digest, str) or len(digest) != 64 or any(c not in "0123456789abcdef" for c in digest):
            fail("EVIDENCE_DIGEST_INVALID", "evidence digest is not lowercase SHA-256", identity=identity)
        if not isinstance(size, int) or size < 0:
            fail("EVIDENCE_LENGTH_INVALID", "evidence length is invalid", identity=identity)
        if identity in result and result[identity] != (size, digest):
            fail("EVIDENCE_IDENTITY_COLLISION", "evidence identity resolves two ways", identity=identity)
        result[identity] = (size, digest)
    return result
