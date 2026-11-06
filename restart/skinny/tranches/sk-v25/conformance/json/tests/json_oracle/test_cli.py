import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

from json_oracle.governance import child_environment, python_module_argv
from tests.json_oracle.resource_probe import measure


ROOT = Path(__file__).resolve().parents[2]
FIXTURES = ROOT / "fixtures"


class CliTests(unittest.TestCase):
    MAX_OUTPUT_BYTES = 16 * 1024 * 1024
    MAX_RSS_BYTES = 128 * 1024 * 1024

    def run_arguments(self, *arguments: str):
        return subprocess.run(
            python_module_argv(ROOT, "json_oracle.cli", *arguments),
            capture_output=True,
            text=True,
            env=child_environment(),
        )

    def run_cli(self, relative: str, *arguments: str):
        return subprocess.run(
            python_module_argv(
                ROOT, "json_oracle.cli", str(FIXTURES / relative), *arguments
            ),
            capture_output=True,
            text=True,
            env=child_environment(),
        )

    def run_source(self, source: bytes, *arguments: str):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "input.json"
            path.write_bytes(source)
            return subprocess.run(
                python_module_argv(ROOT, "json_oracle.cli", str(path), *arguments),
                capture_output=True,
                text=True,
                env=child_environment(),
            )

    def test_success_observation_contains_source_and_flat_nodes(self):
        result = self.run_cli("positive/jcs-sort.json", "--jcs")
        self.assertEqual(result.returncode, 0, result.stderr)
        observation = json.loads(result.stdout)
        self.assertEqual(observation["status"], "accepted")
        self.assertTrue(observation["source_preserving_roundtrip"])
        self.assertEqual(observation["jcs"]["status"], "accepted")

    def test_failure_is_typed_and_nonzero(self):
        result = self.run_cli("negative/invalid-utf8.json")
        self.assertEqual(result.returncode, 1)
        self.assertEqual(json.loads(result.stdout)["category"], "encoding")

    def test_scalar_policy_is_distinct_from_jcs_projection(self):
        scalar = self.run_cli("mutation/lone-surrogate.json", "--require-scalars")
        jcs = self.run_cli("mutation/lone-surrogate.json", "--jcs")
        self.assertEqual(json.loads(scalar.stdout)["category"], "scalar_validity")
        self.assertEqual(json.loads(jcs.stdout)["category"], "typed_projection")

    def test_cli_budget_boundaries_for_every_field(self):
        cases = {
            "--max-input-bytes": (b" 0", 2),
            "--max-depth": (b"[[0]]", 2),
            "--max-nodes": (b"[[0]]", 3),
            "--max-aggregate-entries": (b"[[0]]", 2),
            "--max-decoded-utf16-units": (b'"ab"', 2),
        }
        for option, (source, required) in cases.items():
            for limit in (0, required - 1, required, required + 1):
                with self.subTest(option=option, limit=limit):
                    result = self.run_source(source, option, str(limit))
                    self.assertEqual(result.returncode, 1 if limit < required else 0, result.stderr)
                    self.assertEqual(
                        json.loads(result.stdout)["status"],
                        "rejected" if limit < required else "accepted",
                    )

    def test_cli_invalid_budget_domain_is_typed_before_file_io(self):
        options = (
            "--max-input-bytes",
            "--max-depth",
            "--max-nodes",
            "--max-aggregate-entries",
            "--max-decoded-utf16-units",
        )
        for option in options:
            with self.subTest(option=option):
                result = self.run_cli("does-not-exist.json", option, "-1")
                observed = json.loads(result.stdout)
                self.assertEqual(observed["category"], "configuration")
                self.assertEqual(observed["code"], "exact_non_negative_integer_required")
        malformed = self.run_cli("does-not-exist.json", "--max-nodes", "NaN")
        self.assertEqual(json.loads(malformed.stdout)["category"], "configuration")

    def test_every_argument_rejection_is_total_before_file_io(self):
        missing_path = self.run_arguments()
        unknown = self.run_arguments("does-not-exist.json", "--unknown")
        missing_value = self.run_arguments("does-not-exist.json", "--max-nodes")
        for result in (missing_path, unknown, missing_value):
            with self.subTest(stderr=result.stderr):
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stderr, "")
                self.assertEqual(json.loads(result.stdout)["category"], "configuration")
        for option in (
            "--max-input-bytes",
            "--max-depth",
            "--max-nodes",
            "--max-aggregate-entries",
            "--max-decoded-utf16-units",
        ):
            result = self.run_arguments("does-not-exist.json", option, "9" * 5000)
            with self.subTest(option=option):
                self.assertEqual(result.returncode, 1)
                self.assertEqual(result.stderr, "")
                self.assertEqual(json.loads(result.stdout)["category"], "configuration")

    def test_input_acquisition_stops_after_limit_plus_one(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "large.json"
            with path.open("wb") as stream:
                stream.truncate(64 * 1024 * 1024)
            result = self.run_arguments(str(path))
        observed = json.loads(result.stdout)
        self.assertEqual(result.returncode, 1)
        self.assertEqual(observed["category"], "resource")
        self.assertEqual(observed["code"], "input_byte_budget")

    def test_simultaneous_default_ceilings_fit_public_cli_envelope(self):
        alphabet = b"0123456789abcdefghijklmnopqrstuvwxyz"

        def name(index: int) -> bytes:
            return bytes(
                (
                    alphabet[index // (36 * 36)],
                    alphabet[(index // 36) % 36],
                    alphabet[index % 36],
                )
            )

        entry_count = 29_999
        decoded_units = 131_072
        extra_units = decoded_units - entry_count * 3
        members = [b'"' + name(0) + b"x" * extra_units + b'":0']
        members.extend(b'"' + name(index) + b'":0' for index in range(1, entry_count))
        source = b"{" + b",".join(members) + b"}"
        source += b" " * (1_048_576 - len(source))
        self.assertEqual(len(source), 1_048_576)

        expected_counters = {
            "aggregate_entries": 29_999,
            "decoded_utf16_units": 131_072,
            "input_bytes": 1_048_576,
            "max_depth": 1,
            "nodes": 59_999,
        }
        with tempfile.TemporaryDirectory() as directory:
            temporary = Path(directory)
            path = temporary / "simultaneous-ceilings.json"
            path.write_bytes(source)
            environment = child_environment()
            for label, arguments in (
                ("base", ()),
                ("require-scalars", ("--require-scalars",)),
                ("jcs", ("--jcs",)),
            ):
                stdout = temporary / f"{label}.stdout"
                stderr = temporary / f"{label}.stderr"
                command = python_module_argv(
                    ROOT, "json_oracle.cli", str(path), *arguments
                )
                with stdout.open("wb") as output, stderr.open("wb") as errors:
                    process = subprocess.Popen(
                        command,
                        stdout=output,
                        stderr=errors,
                        env=environment,
                    )
                    _, status, usage = os.wait4(process.pid, 0)
                    process.returncode = os.waitstatus_to_exitcode(status)
                observed = json.loads(stdout.read_bytes())
                with self.subTest(route=label):
                    self.assertEqual(process.returncode, 0, stderr.read_text())
                    self.assertEqual(observed["status"], "accepted")
                    self.assertEqual(observed["counters"], expected_counters)
                    self.assertLessEqual(stdout.stat().st_size, self.MAX_OUTPUT_BYTES + 1)
                    self.assertLessEqual(usage.ru_maxrss, self.MAX_RSS_BYTES)
                    if label == "jcs":
                        self.assertEqual(observed["jcs"]["status"], "accepted")

    def test_unique_lone_surrogate_key_ceiling_fits_three_fresh_processes(self):
        expected_counters = {
            "aggregate_entries": 29_999,
            "decoded_utf16_units": 131_072,
            "input_bytes": 1_048_576,
            "max_depth": 1,
            "nodes": 59_999,
        }
        for result in measure():
            with self.subTest(sample=result["sample"]):
                self.assertEqual(result["exit"], 0, result["stderr"])
                self.assertEqual(result["counters"], expected_counters)
                self.assertLessEqual(result["stdout_bytes"], self.MAX_OUTPUT_BYTES + 1)
                self.assertLessEqual(result["rss_bytes"], self.MAX_RSS_BYTES)


if __name__ == "__main__":
    unittest.main()
