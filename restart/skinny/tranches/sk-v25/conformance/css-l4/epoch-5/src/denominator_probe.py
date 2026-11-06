#!/usr/bin/env python3
"""Fresh isolated source rederivation using the same sealed extractor."""

from __future__ import annotations

import argparse
from pathlib import Path

from assay import load_inputs
from common import store
from source import denominator


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", required=True)
    options = parser.parse_args()
    inputs = load_inputs()
    store(Path(options.output), denominator(inputs["sources"], inputs["policy"], inputs["obligations"]))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
