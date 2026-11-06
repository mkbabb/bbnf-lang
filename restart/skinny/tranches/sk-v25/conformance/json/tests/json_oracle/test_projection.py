from dataclasses import replace
import json
from pathlib import Path
import struct
import unittest

from json_oracle import (
    ConfigurationError,
    ProjectionError,
    ProjectionIntegrityError,
    ScalarValidityError,
    canonicalize,
    observe,
    parse,
    require_unicode_scalars,
)
from json_oracle.projection import _number


FIXTURES = Path(__file__).resolve().parents[2] / "fixtures"


class ProjectionTests(unittest.TestCase):
    def test_rfc_8785_primitive_example(self):
        source = (
            b'{"numbers":[333333333.33333329,1E30,4.50,2e-3,'
            b'0.000000000000000000000000001],'
            b'"string":"\\u20ac$\\u000F\\u000aA\'\\u0042\\u0022\\u005c\\\\\\\"\\/",'
            b'"literals":[null,true,false]}'
        )
        expected = (
            '{"literals":[null,true,false],"numbers":[333333333.3333333,'
            '1e+30,4.5,0.002,1e-27],"string":"€$\\u000f\\nA\'B\\"\\\\\\\\\\"/"}'
        ).encode()
        self.assertEqual(canonicalize(parse(source)), expected)

    def test_utf16_property_sort(self):
        source = (FIXTURES / "positive/jcs-sort.json").read_bytes()
        self.assertEqual(canonicalize(parse(source)), '{"\\r":2,"1":4,"€":1,"😀":3}'.encode())

    def test_duplicate_names_rejected_after_unescaping(self):
        document = parse((FIXTURES / "mutation/duplicate-escape.json").read_bytes())
        with self.assertRaises(ProjectionError) as raised:
            canonicalize(document)
        self.assertEqual(raised.exception.code, "duplicate_object_name")

    def test_lone_surrogate_and_nonfinite_binary64_are_typed_failures(self):
        cases = {
            "mutation/lone-surrogate.json": "non_unicode_scalar_string",
            "mutation/exact-number.json": "number_not_finite_binary64",
        }
        for relative, code in cases.items():
            with self.subTest(relative=relative), self.assertRaises(ProjectionError) as raised:
                canonicalize(parse((FIXTURES / relative).read_bytes()))
            self.assertEqual(raised.exception.code, code)

    def test_scalar_validity_is_a_separate_optional_failure_plane(self):
        document = parse((FIXTURES / "mutation/lone-surrogate.json").read_bytes())
        with self.assertRaises(ScalarValidityError) as raised:
            require_unicode_scalars(document)
        self.assertEqual(raised.exception.category, "scalar_validity")

    def test_canonical_projection_construction_is_bounded_and_typed(self):
        document = parse(b'{"a":[true,null]}')
        required = len(canonicalize(document))
        self.assertEqual(len(canonicalize(document, required)), required)
        with self.assertRaises(ProjectionError) as raised:
            canonicalize(document, required - 1)
        self.assertEqual(raised.exception.code, "projection_byte_budget")

    def test_corrupt_private_document_is_one_typed_integrity_failure(self):
        document = parse(b'[0,{"a":1}]')
        corrupt = replace(document, _root=999)
        with self.assertRaises(ProjectionIntegrityError) as raised:
            canonicalize(corrupt)
        self.assertEqual(raised.exception.code, "projection_integrity")
        with self.assertRaises(ProjectionIntegrityError):
            canonicalize(object())

    def test_every_private_field_family_uses_one_source_derived_boundary(self):
        document = parse(b'[0,{"a":"x"}]')

        def node_change(node_id, **changes):
            nodes = list(document._nodes)
            nodes[node_id] = replace(nodes[node_id], **changes)
            return replace(document, _nodes=tuple(nodes))

        mutations = [
            replace(document, _root=999),
            replace(document, _source=bytearray(document._source)),
            replace(document, _source=b'[1,{"a":"x"}]'),
            replace(document, _counters=(("nodes", 4),)),
            replace(document, _source_sha256=b"x" * 32),
            node_change(0, id=9),
            node_change(0, kind="object"),
            node_change(0, span=(0,)),
            node_change(0, children=(2, 1)),
            node_change(2, members=((3, 1),)),
            node_change(3, utf16=b"\x00b"),
            node_change(3, scalar_valid=False),
        ]
        literal = parse(b"true")
        mutations.append(
            replace(literal, _nodes=(replace(literal._nodes[0], kind="false"),))
        )
        for mutation in mutations:
            for operation in (observe, require_unicode_scalars, canonicalize):
                with self.subTest(mutation=repr(mutation), operation=operation.__name__):
                    with self.assertRaises(ProjectionIntegrityError):
                        operation(mutation)

    def test_invalid_public_cap_precedes_every_corrupt_document_family(self):
        document = parse(b'[0,{"a":"x"}]')

        def node_change(node_id, **changes):
            nodes = list(document._nodes)
            nodes[node_id] = replace(nodes[node_id], **changes)
            return replace(document, _nodes=tuple(nodes))

        corruptions = [
            replace(document, _root=999),
            replace(document, _source=bytearray(document._source)),
            replace(document, _source=b'[1,{"a":"x"}]'),
            replace(document, _counters=(("nodes", 4),)),
            replace(document, _source_sha256=b"x" * 32),
            node_change(0, id=9),
            node_change(0, kind="object"),
            node_change(0, span=(0,)),
            node_change(0, children=(2, 1)),
            node_change(2, members=((3, 1),)),
            node_change(3, utf16=b"\x00b"),
            node_change(3, scalar_valid=False),
        ]
        literal = parse(b"true")
        corruptions.append(
            replace(literal, _nodes=(replace(literal._nodes[0], kind="false"),))
        )
        invalid_caps = (-1, True, 1.0, float("nan"), float("inf"), "1", None, object())
        for corruption in corruptions:
            for cap in invalid_caps:
                for field, operation in (
                    ("max_output_bytes", observe),
                    ("max_projection_bytes", canonicalize),
                ):
                    with self.subTest(corruption=repr(corruption), cap=repr(cap), field=field):
                        with self.assertRaises(ConfigurationError) as raised:
                            operation(corruption, cap)
                        self.assertEqual(raised.exception.field, field)
        for operation in (observe, canonicalize):
            with self.subTest(operation=operation.__name__):
                with self.assertRaises(ProjectionIntegrityError):
                    operation(corruptions[0], 0)

    def test_ecmascript_number_boundaries(self):
        cases = {
            "-0": "0",
            "5e-324": "5e-324",
            "1e-7": "1e-7",
            "1e-6": "0.000001",
            "1e20": "100000000000000000000",
            "1e21": "1e+21",
            "1.7976931348623157e308": "1.7976931348623157e+308",
            "333333333.33333329": "333333333.3333333",
        }
        for raw, expected in cases.items():
            with self.subTest(raw=raw):
                self.assertEqual(_number(raw, 0), expected)

    def test_rfc_8785_appendix_b_binary64_vectors(self):
        cases = {
            "0000000000000000": "0",
            "8000000000000000": "0",
            "0000000000000001": "5e-324",
            "8000000000000001": "-5e-324",
            "7fefffffffffffff": "1.7976931348623157e+308",
            "ffefffffffffffff": "-1.7976931348623157e+308",
            "4340000000000000": "9007199254740992",
            "c340000000000000": "-9007199254740992",
            "4430000000000000": "295147905179352830000",
            "44b52d02c7e14af5": "9.999999999999997e+22",
            "44b52d02c7e14af6": "1e+23",
            "44b52d02c7e14af7": "1.0000000000000001e+23",
            "444b1ae4d6e2ef4e": "999999999999999700000",
            "444b1ae4d6e2ef4f": "999999999999999900000",
            "444b1ae4d6e2ef50": "1e+21",
            "3eb0c6f7a0b5ed8c": "9.999999999999997e-7",
            "3eb0c6f7a0b5ed8d": "0.000001",
            "41b3de4355555553": "333333333.3333332",
            "41b3de4355555554": "333333333.33333325",
            "41b3de4355555555": "333333333.3333333",
            "41b3de4355555556": "333333333.3333334",
            "41b3de4355555557": "333333333.33333343",
            "becbf647612f3696": "-0.0000033333333333333333",
            "43143ff3c1cb0959": "1424953923781206.2",
        }
        for bits, expected in cases.items():
            value = struct.unpack(">d", bytes.fromhex(bits))[0]
            with self.subTest(bits=bits):
                self.assertEqual(_number(repr(value), 0), expected)

    def test_checked_in_independent_ecmascript_number_corpus(self):
        corpus = json.loads((FIXTURES.parent / "data/ecmascript-number-corpus.json").read_text())
        self.assertEqual(corpus["producer"]["node"], "v26.0.0")
        self.assertGreaterEqual(len(corpus["cases"]), 60)
        for row in corpus["cases"]:
            value = struct.unpack(">d", bytes.fromhex(row["binary64_hex"]))[0]
            with self.subTest(bits=row["binary64_hex"]):
                self.assertEqual(_number(repr(value), 0), row["json_stringify"])


if __name__ == "__main__":
    unittest.main()
