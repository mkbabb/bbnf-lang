# PROVE-13 — CSV GENERALITY (F1/F3)

**Verdict: GO.** CSV (RFC 4180) rides the ONE generator end-to-end through a new
`xtask regen-csv` target; the routing is cured to a pure **shape** predicate (zero
grammar-name gates added — no `has_rule("csv")`, no name list); the emitted
recognizer parses quoted fields, `""`-escaped quotes, embedded commas, and
CRLF-in-field correctly; and it **beats `csv-core`** on a 50 MiB generated
RFC4180 corpus at **0.948× cyc/B** (gate was "within 2×"; the stretch "beat it"
lands vs `csv-core`).

Base: recon HEAD `7f3be27c2` (worktree fast-forwarded from the stale `d2b32f6e3`
the orchestrator pinned; the un-fork the recon describes — `general_descent/`,
the `value` name-gate — exists only at `7f3be27c2`).

---

## 1. What broke, and the structural cures

Wiring `grammar/misc/csv.bbnf` through the generator hit **three** gates in order.
The first is DEEPER than the recon's `has_rule("value")` and is the true
generality wall; all three were cured structurally (never by grammar name).

### Cure 1 — the sink-program shape gate (the real first wall)
`lower::sink_only::lower_program` returned `None` whenever `direct_shapes.is_empty()`
(`lower/sink_only.rs`). CSV matches **no** JSON materialization role, so it emits
zero `DirectBuild` nodes → empty `direct_shapes` → `None` → `emit_from_source`
failed with *"BackendIr did not contain DirectBuild sink-only program."* That
empty-shapes bail was a JSON-shape assumption, not a lowerability requirement.
**Cure:** a grammar with no materialization role still lowers to a pure-recognizer
sink program; the runtime policy is derived from the entry rule (`policy_for_rule`).
`general_descent` emits a recognizer + offset tape from the rule tower with no
shapes at all. This is the generality unlock — structural, no names.

### Cure 2 — the `has_rule("value")` name-gate (the recon's target)
`runtime_generator::emit_sink_only` routed `!has_rule("value")` → `general_descent`,
else → `json_sink_direct`. Keyed on the literal rule name `value`. **Cure:** a
structural predicate over facts the lowering already computes —
`dispatch_alt_count > 0 && direct_shapes.len() >= 2`. Empirically measured over the
three sink grammars:

| grammar | dispatch_alt_count | direct_shapes | predicate | route (unchanged) |
|---|---|---|---|---|
| json | 8 | 7 | **true** | `json_sink_direct` |
| google_sheets | 39 | 1 | false | `general_descent` |
| csv | 2 | 0 | false | `general_descent` |

The JSON family is *structurally* a first-byte dispatch over a multi-shape recursive
value taxonomy; Sheets (an operator tower — 39 dispatch branches but ONE leaf shape)
and CSV (a 2-branch field dispatch, zero shapes) are neither. Behaviour is identical
to the old name-gate for all three, now with no name reference.
**Residue (documented, out of cap):** `json_sink_direct` still finds its dispatch
rule by `name == "value"` internally (`json_sink_direct/mod.rs`, `dispatch.rs`);
deriving THAT by shape is the remaining un-fork.

### Cure 3 — the general-descent emitter's two walls (PROTO-B1 WALL A / WALL B)
The general recursive-descent emitter (built for Sheets, a whitespace-insensitive
formula grammar) was wrong for CSV two ways:

* **WALL A — blanket `skip_ws`.** It skips ASCII whitespace at every rule entry and
  between sequence elements; that eats CSV's `\r\n` record separators and intra-field
  spaces. **Cure:** derive `newline_significant` structurally — any leaf that
  *positively* matches `\r`/`\n` (a negated class `[^,"\r\n]` contributes nothing).
  When set, `skip_ws` is the identity (const-folded to a no-op). CSV → `true`,
  Sheets → `false` (verified; Sheets output semantically unchanged).
* **WALL B — `match_leaf`'s `is_text_byte` delimiter scan.** For `[^,"\r\n]+` the
  first-set was empty and the scan fell to the `[A-Za-z0-9_.$]` continuation class,
  truncating any field with a space or symbol. **Cure:** a whole-pattern `[class]+`
  / `[class]*` leaf (`regex_first::class_run`) lowers to a **tight inline byte-run
  scan** over the *actual* class — csv-core-shaped and correct. Quoted spans keep
  the `match_leaf` → `consume_quoted` path (which already honours `""` escapes).

### Grammar (augmented for RFC4180 correctness)
The stock `csv.bbnf` modelled `escaped = DQUOTE >> /[^"]*/ << DQUOTE` — which cannot
express `""` escapes and truncates spaced fields. Augmented (memory:
no-workarounds-self-hosting — augment BBNF when features are missing):

```
quoted   = /"(?:[^"]|"")*"/ ;   # one leaf → consume_quoted handles "" + embedded , \r\n
textdata = /[^,"\r\n]*/ ;       # WALL-B class-run; nullable → empty fields (a,,b)
field    = quoted | textdata ;
record   = field, ( "," >> field ) * ;
csv      = record, ( /[\r\n]+/ >> record ) * ;
```

---

## 2. Visual verification — the emitted hot path (`grammars/csv/generated.rs`)

WALL B cure — the field scan is a single branchy byte loop (no closures, no
`is_text_byte` fallback), exactly the csv-core inner-loop shape:

```rust
fn parse_textdata<'i>(bytes: &'i [u8], cursor: &mut usize, tape: &mut TapeBuilder)
    -> Result<(), ParseError<'i>> {
    *cursor = skip_ws(bytes, *cursor);        // const-folded to identity (WALL A)
    let _node_start = *cursor;
    {
        let _leaf_start = *cursor;
        while let Some(&_b) = bytes.get(*cursor) {
            if !matches!(_b, 44u8 | 34u8 | 13u8 | 10u8) { *cursor += 1; } else { break; }
        }
        tape.push_offset(_leaf_start, OffsetFlags(2u8));
    }
    Ok(())
}
```

WALL A cure — `skip_ws` const-folds to a no-op for CSV:

```rust
const NEWLINE_SIGNIFICANT: bool = true;
#[inline]
fn skip_ws(bytes: &[u8], mut cursor: usize) -> usize {
    if NEWLINE_SIGNIFICANT { return cursor; }   // CSV: identity — LLVM drops every call site
    while let Some(&byte) = bytes.get(cursor) { if byte.is_ascii_whitespace() { cursor += 1; } else { break; } }
    cursor
}
```

The record separator emits the same positive class-run (`matches!(_b, 13u8 | 10u8)`,
min 1); quoted fields route to `consume_quoted` (doubled-quote aware).

---

## 3. Measured result

Bench: `skinny/crates/csv-prove/src/main.rs` (standalone member — deliberately free
of bbnf-bench's heavy JSON/CSS deps so the optimized build is fast). Cold per-parse
(one full-corpus parse per sample), N=200, **paired per-round interleaving** across
the three lanes, PMU `proc_pid_rusage(RUSAGE_INFO_V5).ri_cycles`, **min-of-N**
certified. Corpus: generated deterministic RFC4180, 10 columns, CRLF, mix of
plain / numeric / quoted-with-comma / doubled-quote+embedded-CRLF fields, **52,428,877
bytes (50.0 MiB)**, no trailing newline.

Profile: `ax-iter` (opt-level 3, thin-LTO, cgu16) — an optimized build; all three
lanes share it, so the **ratios are apples-to-apples**. (Not the fat-LTO `release`;
fat-LTO would be equal-or-better and is the only residual measurement gap.)

Lanes: **bbnf** = `runtime::grammars::csv::recognize` (generated general-descent
recognizer, builds the offset tape then discards the doc); **csv-core** = the
zero-alloc `read_field` pull machine (copies+unquotes each field into a reused
buffer); **csv** = the high-level record reader (`read_byte_record`, bulk).

| lane | N | loadavg (start→end) | min cyc/B | mean cyc/B | ratio vs csv-core |
|---|---|---|---|---|---|
| **bbnf (generated)** | 200 | 11.96 → 7.37 | **30.199** | 30.786 | **0.948×** |
| csv-core | 200 | 11.96 → 7.37 | 31.846 | 32.783 | 1.000× |
| csv (high-level) | 200 | 11.96 → 7.37 | 22.149 | 22.930 | 0.695× |

**GATE (bbnf within 2× of csv-core): MET** — bbnf is **0.948×** (i.e. 5.2% *fewer*
cyc/B than csv-core, the stretch "beat it"). vs the high-level `csv` crate bbnf is
1.363× (csv reads whole records in bulk and reuses one `ByteRecord`; the field-at-a-time
`csv-core` lane pays a per-field small-buffer copy, which is the standard `read_field`
usage). Correctness (crafted RFC4180: quoted / `""`-escaped / CRLF-in-field / empty
field) **PASS**; the 50 MiB corpus `recognize`s without error.

Caveats (honest): loadavg was high (a shared box, ~7–12) — `min-of-N` on PMU cycles
(in-process cycle count, not wall) is the load-robust metric, but absolutes are
directional; the ratio is the certified figure. bbnf records field offsets on a tape
(a node marker + a leaf offset for String-role quoted fields — hence tape-offset count
= fields + quoted-count) where csv-core copies+unquotes; comparable net work, different
products.

---

## 4. Integration cost

Low and additive. Seam is exactly the JSON/Sheets pattern:
* `xtask`: one `CSV_TARGET` const + `regen-csv`/`check-csv` arms (mirrors `SHEETS_TARGET`).
* `runtime/src/lib.rs`: `+4` lines wiring `generated_csv` + `grammars::csv`.
* codegen cures touch shared surfaces (`sink_only`, `runtime_generator`,
  `general_descent/emit.rs`, `regex_first.rs`) — all three known sink grammars keep
  their routing and output; Sheets regenerated identically-in-behaviour
  (`NEWLINE_SIGNIFICANT = false`), JSON byte-identical (`check-json` clean), runtime
  tests 4/4. Generated CSV runtime: 5 files, `generated.rs` ~364 LOC.

The class-run inline scan (WALL B) and the `newline_significant` policy (WALL A)
are **grammar-general** — any line-oriented / character-class grammar (TOML, TSV,
logs) now lowers to tight scans through the same emitter. Residue for a follow-on:
(a) derive `json_sink_direct`'s dispatch rule by shape not the name `value`;
(b) a finer `[ \t]`-only ws policy for space-insignificant-but-newline-significant
grammars (CSV needs skip-nothing, so it is unaffected).

## 5. Files
- Grammar: `grammar/misc/csv.bbnf`
- Cures: `skinny/crates/codegen/src/lower/sink_only.rs`,
  `.../runtime_generator.rs`, `.../general_descent/emit.rs`, `.../general_descent/regex_first.rs`
- Target: `skinny/xtask/src/main.rs`; wiring `skinny/crates/runtime/src/lib.rs`
- Generated: `skinny/crates/runtime/src/grammars/csv/*`
- Bench: `skinny/crates/csv-prove/{Cargo.toml,src/main.rs}`
