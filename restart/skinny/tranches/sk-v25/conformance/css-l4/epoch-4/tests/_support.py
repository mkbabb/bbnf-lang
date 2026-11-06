from __future__ import annotations

import json
import os
import sys
from functools import lru_cache
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
DATA_ROOT = Path(os.environ.get("CSS_EPOCH4_DATA_ROOT", ROOT / "data"))
SRC = ROOT / "src"
if str(SRC) not in sys.path:
    sys.path.insert(0, str(SRC))


@lru_cache(maxsize=1)
def observations():
    return json.loads((DATA_ROOT / "observations.json").read_text())


@lru_cache(maxsize=1)
def mutations():
    return json.loads((ROOT / "fixtures" / "mutations.json").read_text())
