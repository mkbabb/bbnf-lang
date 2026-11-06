import json
from pathlib import Path
import subprocess
import unittest

from json_oracle import Budgets, ConfigurationError, OracleError, observe, parse
from json_oracle.governance import child_environment, python_code_argv


FIXTURES = Path(__file__).resolve().parents[2] / "fixtures"


class ParserTests(unittest.TestCase):
    def observation(self, source: bytes):
        return json.loads(observe(parse(source)))

    def test_full_observation_preserves_order_associations_literals_and_spans(self):
        source = (FIXTURES / "positive/source-facts.json").read_bytes()
        observed = self.observation(source)
        root = observed["nodes"][observed["root"]]
        self.assertEqual(root["kind"], "object")
        self.assertEqual(len(root["members"]), 3)
        keys = [observed["nodes"][member["key"]]["raw"] for member in root["members"]]
        self.assertEqual(keys, ['"a"', '"a"', '"z"'])
        self.assertEqual(root["members"][1]["duplicate_of"], root["members"][0]["key"])
        first_value = observed["nodes"][root["members"][0]["value"]]
        self.assertEqual(first_value["literal_kind"], "number")
        self.assertEqual(first_value["number_lexeme"], "1e+02")
        self.assertEqual(source[slice(*first_value["span"])], b"1e+02")
        self.assertTrue(observed["source_preserving_roundtrip"])

    def test_observation_is_canonical_bytes_and_document_is_frozen(self):
        document = parse(b'{"a":[true,null]}')
        before = observe(document)
        self.assertIsInstance(before, bytes)
        self.assertEqual(before, observe(document))
        with self.assertRaises((AttributeError, TypeError)):
            document._root = 1

    def test_observation_construction_is_bounded_and_typed(self):
        document = parse(b'{"a":[true,null]}')
        required = len(observe(document))
        self.assertEqual(len(observe(document, required)), required)
        with self.assertRaises(OracleError) as raised:
            observe(document, required - 1)
        self.assertEqual(raised.exception.category, "resource")
        self.assertEqual(raised.exception.code, "output_byte_budget")
        with self.assertRaises(ConfigurationError):
            observe(document, True)

    def test_source_is_exact_owned_bytes(self):
        mutable = bytearray(b'"a"')
        with self.assertRaises(ConfigurationError) as raised:
            parse(mutable)
        self.assertEqual(raised.exception.field, "source")
        self.assertEqual(raised.exception.observation()["category"], "configuration")

    def test_combined_and_maximum_shapes_bound_observation_and_fresh_rss(self):
        script = """
import json, resource, sys
from json_oracle import observe, parse
shape = sys.argv[1]
source = (
    b'[' * 500 + b' ' * 250000 + b'0' + b']' * 500
    if shape == 'nested'
    else b'[' + b'0,' * 39999 + b'0]'
    if shape == 'array'
    else b'{' + b'\"a\":0,' * 29998 + b'\"a\":0}'
)
observation = observe(parse(source))
print(json.dumps([len(source), len(observation), resource.getrusage(resource.RUSAGE_SELF).ru_maxrss]))
"""
        environment = child_environment()
        limits = {
            "nested": (400_000, 128 * 1024 * 1024),
            "array": (5_000_000, 128 * 1024 * 1024),
            "object": (10_000_000, 128 * 1024 * 1024),
        }
        for shape, (output_limit, rss_limit) in limits.items():
            result = subprocess.run(
                python_code_argv(FIXTURES.parent, script, shape),
                check=True,
                capture_output=True,
                text=True,
                env=environment,
            )
            _, output_bytes, rss = json.loads(result.stdout)
            with self.subTest(shape=shape):
                self.assertLessEqual(output_bytes, output_limit)
                self.assertLessEqual(rss, rss_limit)

    def test_full_observation_kills_relation_and_fact_mutations(self):
        source = (FIXTURES / "positive/source-facts.json").read_bytes()
        baseline = json.loads(observe(parse(source)))
        encoded = lambda value: json.dumps(
            value, ensure_ascii=True, sort_keys=True, separators=(",", ":")
        ).encode("ascii")
        baseline_bytes = encoded(baseline)
        root = baseline["nodes"][baseline["root"]]
        array_id = root["members"][2]["value"]
        first_key = root["members"][0]["key"]
        first_value = root["members"][0]["value"]
        string_id = root["members"][1]["value"]

        mutations = []
        child_order = json.loads(baseline_bytes)
        child_order["nodes"][array_id]["children"].reverse()
        mutations.append(child_order)
        association = json.loads(baseline_bytes)
        association["nodes"][baseline["root"]]["members"][0]["value"] = string_id
        mutations.append(association)
        span = json.loads(baseline_bytes)
        span["nodes"][first_value]["span"][1] -= 1
        mutations.append(span)
        decoded = json.loads(baseline_bytes)
        decoded["nodes"][first_key]["utf16_units"][0] += 1
        mutations.append(decoded)
        validity = json.loads(baseline_bytes)
        validity["nodes"][string_id]["scalar_valid"] = False
        mutations.append(validity)
        literal = json.loads(baseline_bytes)
        literal["nodes"][first_value]["literal_kind"] = "string"
        mutations.append(literal)
        for mutation in mutations:
            self.assertNotEqual(encoded(mutation), baseline_bytes)

    def test_escaped_lone_surrogate_is_syntax_valid_but_not_scalar_valid(self):
        observed = self.observation((FIXTURES / "mutation/lone-surrogate.json").read_bytes())
        string = observed["nodes"][observed["root"]]
        self.assertEqual(string["utf16_units"], [0xDEAD])
        self.assertEqual(string["scalars"], [{"lone_surrogate": 0xDEAD}])
        self.assertFalse(string["scalar_valid"])

    def test_utf8_byte_spans_are_not_code_point_indices(self):
        source = (FIXTURES / "mutation/utf8-span.json").read_bytes()
        observed = self.observation(source)
        array = observed["nodes"][observed["root"]]
        first = observed["nodes"][array["children"][0]]
        second = observed["nodes"][array["children"][1]]
        self.assertEqual(source[slice(*first["span"])], '"é"'.encode())
        self.assertEqual(source[slice(*second["span"])], '{"𝄞":0}'.encode())

    def test_complete_input_error_planes_and_bom_precedence(self):
        cases = {
            "negative/trailing.json": ("syntax", "trailing_content"),
            "negative/invalid-utf8.json": ("encoding", "invalid_utf8"),
            "negative/bom.json": ("encoding", "bom_forbidden"),
            "resource/depth-501.json": ("depth", "depth_budget"),
        }
        for relative, expected in cases.items():
            with self.subTest(relative=relative), self.assertRaises(OracleError) as raised:
                parse((FIXTURES / relative).read_bytes())
            self.assertEqual((raised.exception.category, raised.exception.code), expected)
        with self.assertRaises(OracleError) as malformed_suffix:
            parse(b"\xef\xbb\xbf\x80")
        self.assertEqual(str(malformed_suffix.exception), "encoding:bom_forbidden@0")

    def test_depth_500_is_inclusive(self):
        observed = self.observation((FIXTURES / "resource/depth-500.json").read_bytes())
        self.assertEqual(observed["counters"]["max_depth"], 500)

    def test_budget_domain_is_exact_non_negative_integer_for_every_field(self):
        for field in Budgets.__dataclass_fields__:
            for invalid in (-1, True, 1.0, float("nan"), "1", None):
                with self.subTest(field=field, invalid=repr(invalid)):
                    with self.assertRaises(ConfigurationError) as raised:
                        Budgets(**{field: invalid})
                    self.assertEqual(raised.exception.field, field)
                    self.assertEqual(raised.exception.category, "configuration")
        with self.assertRaises(ConfigurationError):
            parse(b"0", object())

    def test_zero_limit_minus_one_limit_and_plus_one_for_all_budgets(self):
        cases = {
            "max_input_bytes": (b" 0", 2),
            "max_depth": (b"[[0]]", 2),
            "max_nodes": (b"[[0]]", 3),
            "max_aggregate_entries": (b"[[0]]", 2),
            "max_decoded_utf16_units": (b'"ab"', 2),
        }
        for field, (source, required) in cases.items():
            for limit in (0, required - 1, required, required + 1):
                with self.subTest(field=field, limit=limit):
                    budgets = Budgets(**{field: limit})
                    if limit < required:
                        with self.assertRaises(OracleError):
                            parse(source, budgets)
                    else:
                        parse(source, budgets)

    def test_payload_work_is_preceded_by_reservations_and_counters_are_stable(self):
        probes = [
            (
                b'"' + b"a" * 300_000 + b'"',
                Budgets(max_input_bytes=400_000, max_decoded_utf16_units=1),
                "decoded_utf16_budget",
                2,
                {"decoded_utf16_units": 1, "nodes": 1},
            ),
            (
                b'["' + b"a" * 300_000 + b'"]',
                Budgets(max_input_bytes=400_000, max_nodes=1),
                "node_budget",
                1,
                {"nodes": 1, "aggregate_entries": 1},
            ),
            (
                b'["' + b"a" * 300_000 + b'"]',
                Budgets(max_input_bytes=400_000, max_aggregate_entries=0),
                "aggregate_entry_budget",
                1,
                {"nodes": 1, "aggregate_entries": 0},
            ),
        ]
        for source, budgets, code, offset, counters in probes:
            observations = []
            for _ in range(2):
                with self.assertRaises(OracleError) as raised:
                    parse(source, budgets)
                observations.append(raised.exception.observation())
            self.assertEqual(observations[0], observations[1])
            self.assertEqual(observations[0]["code"], code)
            self.assertEqual(observations[0]["offset"], offset)
            for name, value in counters.items():
                self.assertEqual(observations[0]["counters"][name], value)

    def test_default_counters_are_independent_below_the_input_ceiling(self):
        defaults = Budgets()
        self.assertLess(2 * defaults.max_aggregate_entries + 1, defaults.max_input_bytes)
        self.assertLess(defaults.max_nodes, 2 * defaults.max_aggregate_entries)
        self.assertLess(defaults.max_decoded_utf16_units + 2, defaults.max_input_bytes)

    def test_each_default_ceiling_is_live_on_its_own_shape(self):
        cases = [
            (b" " * 1_048_576 + b"0", "input_byte_budget"),
            (b"[" * 501 + b"]" * 501, "depth_budget"),
            (b"[" + b"0," * 40_000 + b"0]", "aggregate_entry_budget"),
            (b"{" + b'"a":0,' * 30_000 + b'"a":0}', "node_budget"),
            (b'"' + b"a" * 131_073 + b'"', "decoded_utf16_budget"),
        ]
        for source, code in cases:
            with self.subTest(code=code), self.assertRaises(OracleError) as raised:
                parse(source)
            self.assertEqual(raised.exception.code, code)


if __name__ == "__main__":
    unittest.main()
