import importlib.util
import unittest
from pathlib import Path


SOURCE = Path(__file__).parents[1] / "src" / "inventory.py"
SPEC = importlib.util.spec_from_file_location("css_l4_inventory", SOURCE)
assert SPEC is not None and SPEC.loader is not None
inventory = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(inventory)


class CurrentWorkParserTest(unittest.TestCase):
    def test_selects_only_level_four_rows_and_normalizes_nbsp(self) -> None:
        parser = inventory.CurrentWorkParser()
        parser.feed(
            """
            <table><tbody id="stable">
              <tr id="color"><td><a href="/TR/css-color-4/">CSS Color Level&nbsp;4</a></td><td>CRD</td></tr>
              <tr id="grid"><td><a href="/TR/css-grid-2/">CSS Grid Level 2</a></td><td>CR</td></tr>
            </tbody></table>
            """
        )
        self.assertEqual(
            parser.rows,
            [
                {
                    "id": "color",
                    "group": "stable",
                    "title": "CSS Color Level 4",
                    "latest_url": "https://www.w3.org/TR/css-color-4/",
                    "status": "CRD",
                }
            ],
        )

    def test_requires_dated_this_version_link(self) -> None:
        document = b'This version:<dd><a href="/TR/2026/WD-example-20260101/">dated</a>'
        self.assertEqual(
            inventory.dated_version("https://www.w3.org/TR/example/", document),
            "https://www.w3.org/TR/2026/WD-example-20260101/",
        )

    def test_accepts_current_work_optional_end_tags(self) -> None:
        parser = inventory.CurrentWorkParser()
        parser.feed(
            '<tbody id="refine"><tr id="values"><td><a href="/TR/css-values-4/">'
            "CSS Values Level 4</a><td>WD<tr id=next><td>Other<td>WD</tbody>"
        )
        self.assertEqual(len(parser.rows), 1)
        self.assertEqual(parser.rows[0]["id"], "values")

    def test_snapshot_union_resolves_bibliography_and_direct_exceptions(self) -> None:
        document = b"""
        <h3 data-level="2.1">official</h3>
        <a href="#biblio-css-a">[A]</a>
        <h3 data-level="2.2">reliable</h3>
        <h3 data-level="2.3">stable</h3>
        <h3 data-level="2.4">rough</h3>
        <h3 data-level="2.5">levels</h3>
        <h2 data-level="4">exceptions</h2>
        <a href="https://www.w3.org/TR/css-b/#feature">feature</a>
        <h2 data-level="5">media</h2>
        <dl><dt id="biblio-css-a">[A]
        <dd>Authors. <a href="https://www.w3.org/TR/css-a/"><cite>CSS A</cite></a>.
        </dl>
        """
        self.assertEqual(
            inventory.snapshot_members(document),
            [
                {"scope": "snapshot:official", "title": "CSS A", "latest_url": "https://www.w3.org/TR/css-a/"},
                {"scope": "snapshot:pre-cr-exception", "title": "css-b", "latest_url": "https://www.w3.org/TR/css-b/"},
            ],
        )

    def test_bikeshed_union_uses_section_scopes_and_canonical_spec_roots(self) -> None:
        document = b"""
        <h3 id=css-official>official</h3>
        <a href="https://www.w3.org/TR/CSS2/intro.html">CSS2</a>
        <h3 id=reliable-cr>reliable</h3>
        <h3 id=fairly-stable>stable</h3>
        <h3 id=rough-interop>rough</h3>
        <h3 id=css-levels>levels</h3>
        <h2 id=CR-exceptions>exceptions</h2>
        <a href="https://www.w3.org/TR/css-color-5/#funcdef-color-mix">color-mix</a>
        [[!CSS-VALUES-5]]
        <h2 id=media-type>media</h2>
        """
        self.assertEqual(
            inventory.bikeshed_members(document),
            [
                {"scope": "snapshot:official", "latest_url": "https://www.w3.org/TR/CSS2/"},
                {
                    "scope": "snapshot:pre-cr-exception",
                    "latest_url": "https://www.w3.org/TR/css-color-5/",
                },
                {
                    "scope": "snapshot:pre-cr-exception",
                    "latest_url": "https://www.w3.org/TR/css-values-5/",
                },
            ],
        )


if __name__ == "__main__":
    unittest.main()
