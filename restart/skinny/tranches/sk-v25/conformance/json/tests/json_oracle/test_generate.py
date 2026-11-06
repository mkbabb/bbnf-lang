import json
import hashlib
import os
from pathlib import Path
import shutil
import tempfile
import unittest
from unittest import mock

import json_oracle.generate as generator
from json_oracle.generate import generate_manifests, generate_numbers, generate_suite


ROOT = Path(__file__).resolve().parents[2]


class GenerateTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        suite = os.environ.get("JSON_TEST_SUITE")
        if not suite:
            raise RuntimeError("JSON_TEST_SUITE must name the frozen local checkout")
        cls.suite = Path(suite)

    def test_all_318_rows_match_frozen_observations(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            generate_suite(self.suite, output)
            for name in ("json-test-suite-manifest.json", "expected-results.jsonl"):
                self.assertEqual((output / name).read_bytes(), (ROOT / "data" / name).read_bytes())
        rows = [json.loads(line) for line in (ROOT / "data/expected-results.jsonl").read_text().splitlines()]
        self.assertEqual(len(rows), 318)
        self.assertTrue(all(row["suite_or_declared_contract_met"] for row in rows))
        accepted = [row for row in rows if row["base"]["status"] == "accepted"]
        self.assertTrue(accepted)
        for row in accepted:
            self.assertIn("nodes", row["base"])
            self.assertIn("root", row["base"])
            self.assertIn("counters", row["base"])
            self.assertTrue(all("span" in node for node in row["base"]["nodes"]))
            self.assertTrue(
                all(
                    ("raw" in node) == (node["kind"] not in ("array", "object"))
                    for node in row["base"]["nodes"]
                )
            )

    def test_every_i_case_has_one_explicit_policy(self):
        policy = json.loads((ROOT / "data/i-case-policy.json").read_text())["cases"]
        manifest = json.loads((ROOT / "data/json-test-suite-manifest.json").read_text())
        names = {Path(row["path"]).name for row in manifest["files"] if row["label"] == "i"}
        self.assertEqual(len(policy), 35)
        self.assertEqual({row["file"] for row in policy}, names)

    def test_ecmascript_number_corpus_is_producer_reproducible(self):
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory)
            generate_numbers(output)
            self.assertEqual(
                (output / "ecmascript-number-corpus.json").read_bytes(),
                (ROOT / "data/ecmascript-number-corpus.json").read_bytes(),
            )

    def number_subject(self, root: Path, include_target: bool) -> Path:
        (root / "data/producers").mkdir(parents=True)
        shutil.copy2(
            ROOT / "data/producers/ecmascript-numbers.mjs",
            root / "data/producers/ecmascript-numbers.mjs",
        )
        shutil.copy2(ROOT / "data/runtime-closure.json", root / "data/runtime-closure.json")
        if include_target:
            shutil.copy2(
                ROOT / "data/ecmascript-number-corpus.json",
                root / "data/ecmascript-number-corpus.json",
            )
        return root

    def test_number_generation_ignores_poisoned_target_metadata(self):
        with tempfile.TemporaryDirectory() as directory:
            subject = self.number_subject(Path(directory) / "subject", True)
            target = subject / "data/ecmascript-number-corpus.json"
            poison = json.loads(target.read_text())
            poison.update(
                authority_scope="poison-authority",
                focus="poison-focus",
                revision=777,
            )
            target.write_text(json.dumps(poison))
            output = Path(directory) / "output"
            with mock.patch.object(generator, "ROOT", subject):
                generate_numbers(output)
            self.assertEqual(
                (output / "ecmascript-number-corpus.json").read_bytes(),
                (ROOT / "data/ecmascript-number-corpus.json").read_bytes(),
            )

    def test_number_generation_from_absent_target_is_repeatable(self):
        with tempfile.TemporaryDirectory() as directory:
            subject = self.number_subject(Path(directory) / "subject", False)
            first, second = Path(directory) / "first", Path(directory) / "second"
            with mock.patch.object(generator, "ROOT", subject):
                generate_numbers(first)
                generate_numbers(second)
            checked = (ROOT / "data/ecmascript-number-corpus.json").read_bytes()
            self.assertEqual((first / "ecmascript-number-corpus.json").read_bytes(), checked)
            self.assertEqual((second / "ecmascript-number-corpus.json").read_bytes(), checked)

    def test_every_number_producer_identity_field_is_independently_checked(self):
        corpus = json.loads((ROOT / "data/ecmascript-number-corpus.json").read_text())
        program = ROOT / "data/producers/ecmascript-numbers.mjs"
        expected = {
            "architecture": "arm64",
            "binary_sha256": "08dad0581f00a0cabf4d49ec92ca1f25fdfd01c2c18fa8e92b35f04d4c24c164",
            "command": "node --no-warnings data/producers/ecmascript-numbers.mjs corpus",
            "corpus_output_sha256": "5a98ae4059f67693b2d0994c3f2d570787ed2be523718e4ffd4defb591fa5c3c",
            "differential": {
                "command": "node --no-warnings data/producers/ecmascript-numbers.mjs differential 0x878520260719 100000",
                "count": 100_000,
                "output_sha256": "330db74dcc212bff1f8eb7e2124d99b827db3a57edc743929e0221c3fae619d8",
                "seed": "0x878520260719",
                "uniqueness": "distinct finite binary64 bit patterns",
            },
            "executable": "/opt/homebrew/Cellar/node/26.0.0/bin/node",
            "node": "v26.0.0",
            "platform": "darwin",
            "program": "data/producers/ecmascript-numbers.mjs",
            "program_sha256": hashlib.sha256(program.read_bytes()).hexdigest(),
            "v8": "14.6.202.33-node.19",
        }
        self.assertEqual(corpus["producer"], expected)

    def test_root_review_namespace_is_append_only_and_cache_is_rejected(self):
        with tempfile.TemporaryDirectory() as directory:
            copy = Path(directory) / "json"
            shutil.copytree(
                ROOT,
                copy,
                ignore=shutil.ignore_patterns("__pycache__", "*.pyc", "GESTALT-*.md"),
            )
            generate_manifests(copy)
            before = (copy / "data/file-manifest.json").read_bytes()
            (copy / "GESTALT-FUTURE.md").write_text("append-only review\n")
            generate_manifests(copy)
            self.assertEqual((copy / "data/file-manifest.json").read_bytes(), before)
            cache = copy / "src/json_oracle/__pycache__"
            cache.mkdir()
            (cache / "mutation.pyc").write_bytes(b"not bytecode")
            with self.assertRaises(SystemExit):
                generate_manifests(copy)


if __name__ == "__main__":
    unittest.main()
