from __future__ import annotations

import json
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SRC = ROOT / "src"
if str(SRC) not in sys.path:
    sys.path.insert(0, str(SRC))


def load(name: str) -> dict:
    return json.loads((ROOT / "data" / f"{name}.json").read_text())


def fixtures() -> dict:
    return json.loads((ROOT / "fixtures" / "counterexamples.json").read_text())
