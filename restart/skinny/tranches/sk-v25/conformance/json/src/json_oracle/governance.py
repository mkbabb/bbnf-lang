"""One exact startup, argv, executable, and child-environment boundary."""

from __future__ import annotations

import os
from pathlib import Path
import sys
from typing import Mapping


PYTHON = (
    "/opt/homebrew/Cellar/python@3.14/3.14.5/Frameworks/Python.framework/"
    "Versions/3.14/Resources/Python.app/Contents/MacOS/Python"
)
NODE = "/opt/homebrew/Cellar/node/26.0.0/bin/node"
ENV = "/usr/bin/env"
GIT = "/usr/bin/git"
OTOOL = "/usr/bin/otool"
SH = "/bin/sh"
SW_VERS = "/usr/bin/sw_vers"

PYTHON_FLAGS = ("-I", "-S", "-B", "-X", "utf8")
BASE_ENVIRONMENT = {
    "LC_ALL": "C",
    "TZ": "UTC",
    "__CF_USER_TEXT_ENCODING": "0x1F8:0x0:0x0",
}
ALLOWED_DYNAMIC_ENVIRONMENT = frozenset({"JSON_TEST_SUITE"})

_FLAG_NAMES = (
    "debug",
    "inspect",
    "interactive",
    "optimize",
    "dont_write_bytecode",
    "no_user_site",
    "no_site",
    "ignore_environment",
    "verbose",
    "bytes_warning",
    "quiet",
    "hash_randomization",
    "isolated",
    "dev_mode",
    "utf8_mode",
    "warn_default_encoding",
    "safe_path",
    "int_max_str_digits",
)
EXPECTED_FLAGS = {
    "bytes_warning": 0,
    "debug": 0,
    "dev_mode": False,
    "dont_write_bytecode": 1,
    "hash_randomization": 1,
    "ignore_environment": 1,
    "inspect": 0,
    "int_max_str_digits": 4300,
    "interactive": 0,
    "isolated": 1,
    "no_site": 1,
    "no_user_site": 1,
    "optimize": 0,
    "quiet": 0,
    "safe_path": True,
    "utf8_mode": 1,
    "verbose": 0,
    "warn_default_encoding": 0,
}
EXPECTED_XOPTIONS = {"utf8": True}

_MODULE_BOOTSTRAP = (
    "import runpy,sys;"
    "root=sys.argv.pop(1);sys.path[:0]=[root+'/src',root];"
    "module=sys.argv.pop(1);sys.argv[0]=module;"
    "runpy.run_module(module,run_name='__main__')"
)
_CODE_BOOTSTRAP = (
    "import sys;"
    "root=sys.argv.pop(1);sys.path[:0]=[root+'/src',root];"
    "code=sys.argv.pop(1);sys.argv[0]='-c';"
    "exec(compile(code,'<governed>','exec'),{'__name__':'__main__'})"
)


def child_environment(extras: Mapping[str, str] | None = None) -> dict[str, str]:
    environment = dict(BASE_ENVIRONMENT)
    if extras:
        unexpected = set(extras) - ALLOWED_DYNAMIC_ENVIRONMENT
        if unexpected:
            raise ValueError(f"unbound child environment: {sorted(unexpected)[0]}")
        if any(type(value) is not str for value in extras.values()):
            raise TypeError("child environment values must be strings")
        environment.update(extras)
    return environment


def python_module_argv(root: Path, module: str, *arguments: str) -> list[str]:
    return [
        PYTHON,
        *PYTHON_FLAGS,
        "-c",
        _MODULE_BOOTSTRAP,
        str(root.resolve()),
        module,
        *arguments,
    ]


def python_code_argv(root: Path, code: str, *arguments: str) -> list[str]:
    return [
        PYTHON,
        *PYTHON_FLAGS,
        "-c",
        _CODE_BOOTSTRAP,
        str(root.resolve()),
        code,
        *arguments,
    ]


def node_argv(*arguments: str) -> list[str]:
    return [NODE, *arguments]


def startup_description() -> dict[str, object]:
    return {
        "argv_prefix": [PYTHON, *PYTHON_FLAGS],
        "environment": dict(sorted(BASE_ENVIRONMENT.items())),
        "flags": {name: getattr(sys.flags, name) for name in _FLAG_NAMES},
        "orig_argv": list(sys.orig_argv),
        "xoptions": dict(sorted(sys._xoptions.items())),
    }


def enforce_verifier_startup(script: Path) -> None:
    expected_prefix = [PYTHON, *PYTHON_FLAGS, str(script.resolve())]
    actual_flags = {name: getattr(sys.flags, name) for name in _FLAG_NAMES}
    if (
        actual_flags != EXPECTED_FLAGS
        or sys._xoptions != EXPECTED_XOPTIONS
        or sys.orig_argv[: len(expected_prefix)] != expected_prefix
        or dict(os.environ) != BASE_ENVIRONMENT
        or not __debug__
    ):
        raise SystemExit("governed Python startup mismatch")


def command_schema() -> dict[str, object]:
    return {
        "child_environment": dict(sorted(BASE_ENVIRONMENT.items())),
        "dynamic_environment": sorted(ALLOWED_DYNAMIC_ENVIRONMENT),
        "git": [GIT, "-C", "{repository}", "{arguments}"],
        "launcher": [
            ENV,
            "-i",
            *[f"{name}={value}" for name, value in BASE_ENVIRONMENT.items()],
            PYTHON,
            *PYTHON_FLAGS,
            "{root}/verify.py",
            "{arguments}",
        ],
        "node": [NODE, "{arguments}"],
        "otool": [OTOOL, "-L", "{executable}"],
        "python_code": [PYTHON, *PYTHON_FLAGS, "-c", _CODE_BOOTSTRAP, "{arguments}"],
        "python_module": [
            PYTHON,
            *PYTHON_FLAGS,
            "-c",
            _MODULE_BOOTSTRAP,
            "{arguments}",
        ],
        "sw_vers": [SW_VERS, "{arguments}"],
    }
