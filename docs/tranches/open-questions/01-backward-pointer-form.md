# Q1 — backward-pointer form

**Status**: resolved (sidecar), final micro-bench deferred to BA.W0
**Owner tranche**: AZ (sidecar decision); BA (parent-pointer geometry)
**Decision date**: 2026-04-23
**Affects**: AZ, BA

## Context

Several downstream passes — pointer queries, rule-inference search,
incremental re-derivation — need to walk from a child node to its parent.
On the tape substrate this was trivially realized as a back-reference
column inside the tape metadata. Under AZ's direct-to-struct regime the
parent link must be reified somewhere in the struct tree.

Two shapes were considered. The first embeds a parent pointer (or node
id) inside every struct node, so every navigation step is an O(1) field
read. The second keeps structs lean and parks the back-references in a
sidecar tree or parallel index, populated only when a pass opts in.

The first shape pays a memory cost on every parse whether or not parent
navigation is used. Every typed node grows by a word. For tightly-packed
CSS and JSON payloads, where a selector list or value array holds
thousands of small records, this is a real overhead against the
lightningcss target.

## Decision

**Sidecar column / parallel index.** Parent pointers live in a structure
parallel to the struct tree, built on demand by a pass that declares it
needs parent navigation. Struct nodes themselves gain no parent field.
Final parent-pointer geometry (flat Vec<NodeId> vs. per-arena sidecar
vs. hash index) is deferred to BA.W0 micro-bench.

## Reasoning

The sidecar answers the load-bearing constraint: no bloat when the
feature is unused. Parse-and-throw consumers (fused prettify, raw value
extraction) never pay for parent navigation they don't request. Passes
that need it (rule inference, incremental redux) build the sidecar
once at entry and walk it freely.

The tradeoff is that the first access in a pass that needs parents costs
a sidecar build. Preliminary cost modeling suggests the build is linear
in tree size and amortizes across any non-trivial pass, but the numbers
are not yet in. BA.W0 will measure the sidecar under realistic pass
workloads before committing to final geometry.

An embedded parent pointer was rejected on the grounds that direct-to-
struct's whole point is to compact the working set. Undoing the compaction
to make a future feature cheaper is the wrong trade.

## Resolution mechanism

1. AZ defines the sidecar interface (`ParentIndex::build(&tree) ->
   ParentIndex`) with a placeholder implementation (flat `Vec<NodeId>`
   keyed by depth-first index).
2. AZ wires the interface into at least one consuming pass stub so the
   API is exercised.
3. BA.W0 micro-bench compares flat-Vec, dense arena sidecar, and hash
   index on synthetic trees at three sizes. Winner ships in BA.W1.

## Follow-up gate

BA.W0 bench report must commit numbers for build cost, per-lookup cost,
and memory overhead under each geometry. BA opening blocks on the
micro-bench landing. If no geometry meets the "no cost when unused,
sub-microsecond lookup when used" bar, BA re-plans rather than papering
over the gap.

## References

- `docs/tranches/AZ/AZ.md` (sidecar interface definition)
- `docs/tranches/BA/BA.md` (parent-pointer micro-bench — to be authored)
- Q0 Shape C re-sequence: `00-tape-abrogation-shape-c.md`
