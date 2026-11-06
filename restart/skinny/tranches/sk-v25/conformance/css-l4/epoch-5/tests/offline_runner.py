#!/usr/bin/env python3
"""Cache-disabled stdlib-only test runner; runtime was checked by launcher."""

from __future__ import annotations

import json
import sys
import unittest
from pathlib import Path


def main() -> int:
    root = Path(__file__).resolve().parent
    suite = unittest.defaultTestLoader.discover(str(root), pattern="test_*.py")
    result = unittest.TextTestRunner(verbosity=2).run(suite)
    print(json.dumps({"status": "PASS" if result.wasSuccessful() else "FAIL", "tests_run": result.testsRun, "failures": len(result.failures), "errors": len(result.errors)}, sort_keys=True))
    return 0 if result.wasSuccessful() else 1


if __name__ == "__main__":
    raise SystemExit(main())
