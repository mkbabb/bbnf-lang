from __future__ import annotations

import json
import sys
from functools import lru_cache
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SRC = ROOT / "src"
REPOSITORY = ROOT.parents[6]
if str(SRC) not in sys.path:
    sys.path.insert(0, str(SRC))

import boundary
import dialects
import extract
import matrix
import membership
import modules
from common import DEFAULT_CSSWG, DEFAULT_WPT, require_toolchain


@lru_cache(maxsize=1)
def mutations():
    return json.loads((ROOT / "fixtures" / "mutations.json").read_text())


@lru_cache(maxsize=1)
def stages():
    require_toolchain()
    scope = membership.capture(DEFAULT_CSSWG, DEFAULT_WPT)
    carriers = extract.capture(scope, DEFAULT_CSSWG)
    parsed = dialects.capture(carriers)
    closed = modules.capture(scope, carriers, parsed)
    observed = matrix.capture(scope, carriers, closed, REPOSITORY, DEFAULT_CSSWG)
    bounded = boundary.capture()
    return scope, carriers, parsed, closed, observed, bounded
