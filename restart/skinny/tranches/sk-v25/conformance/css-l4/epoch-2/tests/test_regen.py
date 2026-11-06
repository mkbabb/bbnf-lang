from __future__ import annotations

import hashlib
import subprocess
import unittest
from unittest.mock import patch

from _support import ROOT
import common
from common import DEFAULT_CSSWG, DEFAULT_WPT, encoded_json
import regen


class DeterministicOfflineRegenTests(unittest.TestCase):
    def test_two_complete_regenerations_are_identical_and_offline(self) -> None:
        publications = ROOT / "inputs" / "publications.json"
        with patch("socket.create_connection", side_effect=AssertionError("network forbidden during verification")):
            first = regen.build(DEFAULT_CSSWG, DEFAULT_WPT, publications)
            second = regen.build(DEFAULT_CSSWG, DEFAULT_WPT, publications)
        first_hashes = {name: hashlib.sha256(encoded_json(value)).hexdigest() for name, value in first.items()}
        second_hashes = {name: hashlib.sha256(encoded_json(value)).hexdigest() for name, value in second.items()}
        self.assertEqual(first_hashes, second_hashes)
        for name, value in first.items():
            self.assertEqual((ROOT / "data" / name).read_bytes(), encoded_json(value), name)

    def test_pipeline_inputs_do_not_include_the_candidate_matrix(self) -> None:
        self.assertEqual(regen.build.__code__.co_varnames[:3], ("csswg", "wpt", "publications"))
        self.assertEqual(regen.FILES, ("scope.json", "carriers.json", "grammar.json", "matrix.json"))

    def test_git_verification_disables_lazy_network_fetch(self) -> None:
        completed = subprocess.CompletedProcess([], 0, stdout="pinned\n", stderr="")
        with patch("common.subprocess.run", return_value=completed) as run:
            self.assertEqual(common.git(ROOT, "rev-parse", "HEAD"), "pinned")
        environment = run.call_args.kwargs["env"]
        self.assertEqual(environment["GIT_NO_LAZY_FETCH"], "1")
        self.assertEqual(environment["GIT_TERMINAL_PROMPT"], "0")


if __name__ == "__main__":
    unittest.main()
