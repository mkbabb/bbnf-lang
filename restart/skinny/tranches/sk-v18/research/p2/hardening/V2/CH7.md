# SK-V18 S-P2 CHALLENGE — CH7 SECTION-6-HONESTY (cycle V2)

Lens: are the §6 findings recorded HONESTLY as named-primitive findings with the (a)-(b)-(c) gate —
never silently smuggled, never an escape hatch to relabel arbitrary hand-written code; is each §6
primitive genuinely NARROW; could a reviewer tell a legitimate primitive from a relabeled blob via
the stated falsifiers? Reviewer: orchestrator (infra dropped the sub-agent dispatch).
Read: SYNTHESIS-RESEARCH.md §4 (post-fold) + rB-css-lowering.md + rC-json-projection.md +
audit-overfit/SYNTHESIS-AUDIT-OVERFIT.md §6.

## Claims

### C1 [ACCEPT] — the three §6 findings are surfaced explicitly, not smuggled
SYNTHESIS §4 names the PRIMARY (R-B CSS balanced scan), SECONDARY (R-C JSON leaf kernels), and
CANDIDATE (R-E precedence tower) §6 findings openly, each as a place a grammar-derived parser cannot
preserve >SOTA without a hand-shaped core. This is the inflection-point honesty the campaign demands
— it does NOT pretend everything is freely grammar-derivable. ACCEPT.

### C2 [ACCEPT] — each primitive carries the machine-checked (a)-(b)-(c) gate
§4 closing binds every primitive to (a) grammar-INVOKED-by-name + (b) emitted-output-VARIES-under-
invoking-rule-mutation + (c) verbatim_blob_present == false; "failing any of the three is a relabeled
hand-written blob = REJECT." Post-CH2-V6 fold, the R-C leaf (b) gate is now concrete (byte-set/class
mutation). A reviewer CAN distinguish a derived leaf from a relabeled one via (b). ACCEPT.

### C3 [ACCEPT] — the R-B primitive's (b) falsifier is concrete and rule-coupled
"mutate the invoking `.bbnf` rule → emitted ARG byte sets change" is an executable per-primitive
falsifier wired into the G2 exit-gate. The primitive is NOT the verbatim CSS_GENERATED_RS courier it
retires (c gate: grep == 0). ACCEPT.

### C4 [REVISE] — the §6 escape lacks an explicit SCOPE/NARROWNESS criterion; (a)-(b)-(c) alone do not bound primitive SIZE
The escape is "the single largest paper-close surface in the contract" (R-A0-3). The (a)-(b)-(c)
gate proves a primitive is grammar-INVOKED and rule-COUPLED, but does NOT bound how MUCH code a
single primitive may absorb: a large hand-written body that happens to vary under some rule mutation
passes (b) yet is exactly the relabeled blob the campaign forbids. The synthesis IMPLIES narrowness
(R-C "only the proven-hot leaf kernels", R-B "the 94.1% scan", skeleton walk-derived) but does not
state it as a GENERAL gate criterion. EDIT (SYNTHESIS-RESEARCH §4 closing paragraph): add an explicit
(d) criterion — each §6 primitive must be a PROFILE-PROVEN-hot NARROW leaf (a single scan/classify/
emit kernel attributable to a named hot leaf in the S-P1 profile), and the surrounding structural
SKELETON MUST be walk-derived; a "primitive" spanning a rule's whole body or an unprofiled region is
a REJECT regardless of (a)-(b)-(c). This converts the implied narrowness into a machine-checkable
bound (primitive LOC vs the profiled hot-leaf extent), closing the paper-close surface. REVISE.

### C5 [ACCEPT] — the R-E §6 candidate is correctly conditional, not pre-claimed
The precedence tower is a §6 CANDIDATE that fires ONLY if G3 cannot render recursive
CallRule/RepeatLoop chains from grammar structure (PROVE make-or-break) — it is not pre-emptively
declared a primitive. The honest fallback (if Sheets needs a shim, generalization is NOT real) is
binding. ACCEPT.

## Net
The §6 findings are honest, explicit, and (a)-(b)-(c)-gated; the R-E candidate is correctly
conditional. One honesty-hardening REVISE: add the (d) PROFILE-PROVEN-NARROW-LEAF + walk-derived-
skeleton criterion so the §6 escape cannot admit an arbitrarily large relabeled blob that merely
varies under mutation (C4) — the implied narrowness must be a machine-checkable bound.

TALLY accept=4 revise=1 reject=0
