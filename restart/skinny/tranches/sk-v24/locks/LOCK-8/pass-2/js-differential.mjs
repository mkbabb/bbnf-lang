// LOCK-8 pass-2 probe t2 (8C Brief 2): JS twin of the regex differential.
// Replicates parse-that TS `regex()` leaf EXACTLY: sticky RegExp (leftmost-FIRST, backtracking),
// anchored at offset 0, matched length = lastIndex - offset (parser.ts leaf.ts:321-347).
// Emits `JS <id> <matched_len|NONE>`; ids + pattern source identical to the Rust twin.

// [id, source, input] — `source` strings use the SAME escaping as the Rust string literals,
// so `new RegExp(source,"y")` and Rust `Dfa::compile(pat)` receive the identical pattern.
const CASES = [
  ["alt_a_ab__ab",        "a|ab",        "ab"],
  ["alt_ab_a__ab",        "ab|a",        "ab"],
  ["alt_a_ab__abc",       "a|ab",        "abc"],
  ["alt_foo_foobar__fb",  "foo|foobar",  "foobar"],
  ["alt_foobar_foo__fb",  "foobar|foo",  "foobar"],
  ["alt_cat_ca__cats",    "cat|ca",      "cats"],
  ["alt_ca_cat__cats",    "ca|cat",      "cats"],
  ["lazy_digits__123",    "\\d+?",       "123"],
  ["lazy_astar_b__axbxb", "a.*?b",       "axbxb"],
  ["json_num__neg",       "-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?", "-12.5e3"],
  ["json_num__zero",      "-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?", "0"],
  ["json_num__int",       "-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?", "123"],
  ["json_num__zeroish",   "-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?", "0123"],
  ["json_str__esc",       "\"(?:[^\"\\\\]|\\\\(?:[\"\\\\/bfnrt]|u[0-9a-fA-F]{4}))*\"", "\"a\\nb\""],
  ["csv_notquote__ab",    "[^\"]+",      "ab,cd"],
  ["csv_notcomma__ab",    "[^,]+",       "ab,cd"],
  ["ws_star__spaces",     "\\s*",        "   x"],
  ["ws_star__none",       "\\s*",        "x"],
  ["hex4__cafe",          "[0-9a-fA-F]{4}", "cafe99"],
];

let out = "";
for (const [id, source, input] of CASES) {
  const sticky = new RegExp(source, "y");
  sticky.lastIndex = 0;
  const res = sticky.test(input) ? String(sticky.lastIndex - 0) : "NONE";
  out += `JS ${id} ${res}\n`;
}
process.stdout.write(out);
