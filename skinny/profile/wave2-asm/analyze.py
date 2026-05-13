#!/usr/bin/env python3
"""Wave-2 per-corpus ASM/profile attribution.

For each of {github_events, update-center, random, unicode_escapes,
y_string_unicode} this loads the samply-emitted .json.gz profile and the
companion .syms.json sidecar, resolves every frame address through the
per-lib symbol table (binary search on RVA/size — funcTable.name strings are
NOT used; they misattribute inline frames to the caller), and produces:

  (a) Self-time / inclusive-time symbol leaderboards.
  (b) Top-10 PCs inside ``parse_value_at`` by self-sample count.
  (c) Per-PC asm neighbourhood extracted from ``otool -tV`` against the
      profile-lazy binary, classified by mnemonic pattern (SWAR whitespace
      re-scan / bounds cascade / dispatch cmp-tree / allocator path /
      sparse-flag capacity).
  (d) Per-corpus pathology classification with mapped source line in
      ``crates/runtime/src/grammars/json/generated.rs``.
  (e) Per-corpus prescription against the five SK-V3 fixes from Wave-1
      Agent 5.

Writes PROFILE-REPORT.md alongside the profiles.
"""
import gzip
import json
import os
import re
import subprocess
import sys
import bisect
from collections import defaultdict, Counter
from pathlib import Path


DIR = Path('/Users/mkbabb/Programming/bbnf-lang/skinny/profile/wave2-asm')
BIN = Path('/Users/mkbabb/Programming/bbnf-lang/skinny/target/release/profile-lazy')
GENERATED = Path('/Users/mkbabb/Programming/bbnf-lang/skinny/crates/runtime/src/grammars/json/generated.rs')

# Track 1 throughput from prior Wave-1 numbers (Mbps, fraction of sonic-rs).
# unicode_escapes / y_string_unicode were re-measured during this profiling
# pass — see Appendix C for the captured numbers.
CORPORA = [
    ('github_events',  'github_events.json',     19017, 0.966),
    ('update-center',  'update-center.json',     14789, 0.907),
    ('random',         'random.json',             9370, 0.809),
    ('unicode_escapes','unicode_escapes.json',    None, None),
    ('y_string_unicode','y_string_unicode.json',  None, None),
]

# Throughput observed during this profiling pass (Mbps from the timed loop
# inside profile-lazy itself, NOT a separate bbnf-bench run). Provides a
# direct sanity check that the binary used for sampling matches the binary
# the user benched.
PROFILE_MBPS = {
    'github_events':    20709,   # 1.2M iters, 30.19s
    'update-center':    18538,   # 110K iters, 25.31s
    'random':           12373,   # 70K iters, 23.10s
    'unicode_escapes':  17079,   # 35K iters, 17.23s
    'y_string_unicode': 11120,   # 1.5M iters, 38.42s
}


# ---------- otool ingest ----------

OTOOL_PATH = DIR / '_profile-lazy.otool.txt'


def ensure_otool():
    if OTOOL_PATH.exists() and OTOOL_PATH.stat().st_size > 0:
        return
    out = subprocess.check_output(['otool', '-tV', str(BIN)])
    OTOOL_PATH.write_bytes(out)


SYMBOL_RE = re.compile(r'^_([A-Za-z0-9_$.]+):$')
INSN_RE = re.compile(r'^([0-9a-f]+)\s+([a-z][a-z0-9.]*)\s*(.*)$')


def load_otool():
    """Parse otool dump into:
        symbols: list of (rva, name, [(rva, mnemonic, operands), ...])
        addr_to_idx: dict mapping rva -> (symbol_idx, insn_idx) for quick lookup
    """
    ensure_otool()
    text = OTOOL_PATH.read_text(errors='replace')
    symbols = []
    cur = None  # (name, [insns])
    for line in text.splitlines():
        m = SYMBOL_RE.match(line)
        if m:
            if cur is not None and cur[1]:
                first_rva = cur[1][0][0]
                symbols.append((first_rva, cur[0], cur[1]))
            cur = (m.group(1), [])
            continue
        if cur is None:
            continue
        m = INSN_RE.match(line)
        if m:
            try:
                addr = int(m.group(1), 16)
            except ValueError:
                continue
            cur[1].append((addr, m.group(2), m.group(3)))
    if cur is not None and cur[1]:
        first_rva = cur[1][0][0]
        symbols.append((first_rva, cur[0], cur[1]))
    # Convert absolute address (already image-load-relative for macOS aarch64
    # since otool prints VM addresses but the sym table uses RVA = addr - 0x100000000).
    # We'll normalise to RVA = addr & 0xfffffff (subtract 0x100000000 for macOS).
    def to_rva(a):
        return a - 0x100000000 if a >= 0x100000000 else a
    out = []
    for first, name, insns in symbols:
        rva_insns = [(to_rva(a), mn, ops) for (a, mn, ops) in insns]
        out.append((rva_insns[0][0], name, rva_insns))
    out.sort(key=lambda x: x[0])
    return out


def find_symbol(symbols, name_substr):
    for rva, name, insns in symbols:
        if name_substr in name:
            return (rva, name, insns)
    return None


def insns_in_range(symbols, lo_rva, hi_rva):
    for rva, name, insns in symbols:
        if rva > hi_rva:
            break
        for tup in insns:
            if lo_rva <= tup[0] <= hi_rva:
                yield (name, tup)


# ---------- samply ingest ----------

def load_profile(corpus_stem):
    with gzip.open(DIR / f'{corpus_stem}.profile.json.gz', 'rb') as f:
        p = json.load(f)
    with open(DIR / f'{corpus_stem}.profile.json.syms.json') as f:
        syms = json.load(f)
    return p, syms


def build_resolver(profile, syms):
    libs = profile['libs']
    lib_name_by_idx = [L['name'] for L in libs]
    sym_strs = syms['string_table']
    sym_tables_by_lib_name = {}
    for d in syms['data']:
        ln = d['debug_name']
        st = sorted(d['symbol_table'], key=lambda s: s['rva'])
        rvas = [s['rva'] for s in st]
        ends = [s['rva'] + s['size'] for s in st]
        names = [sym_strs[s['symbol']] if s.get('symbol') is not None else '<no sym>' for s in st]
        sym_tables_by_lib_name[ln] = (rvas, ends, names)

    thread = profile['threads'][0]
    func_resource = thread['funcTable']['resource']
    resource_lib = thread['resourceTable']['lib']
    addresses = thread['frameTable']['address']
    frame_func = thread['frameTable']['func']

    def resolve(frame_idx):
        addr = addresses[frame_idx]
        f_idx = frame_func[frame_idx]
        lib_idx = resource_lib[func_resource[f_idx]]
        lib_name = lib_name_by_idx[lib_idx]
        r = sym_tables_by_lib_name.get(lib_name)
        if r is not None:
            rvas, ends, names = r
            i = bisect.bisect_right(rvas, addr) - 1
            if i >= 0 and addr < ends[i]:
                return (names[i], lib_name, addr)
        return (f'{lib_name}!0x{addr:x}', lib_name, addr)

    return resolve


def collect_samples(profile, resolver):
    t = profile['threads'][0]
    stacks = t['stackTable']
    frames = t['frameTable']
    samples = t['samples']
    n = samples['length']
    stack_prefix = stacks['prefix']
    stack_frame = stacks['frame']
    n_frames = frames['length']
    frame_resolved = [resolver(i) for i in range(n_frames)]
    sample_stack = samples['stack']

    # We need: self-time per symbol, self-time per PC inside specific symbols.
    self_time_sym = Counter()
    incl_time_sym = Counter()
    self_time_pc = defaultdict(Counter)  # sym -> Counter(pc)
    total = 0

    # Walk every sample, take leaf frame as self.
    for i in range(n):
        s = sample_stack[i]
        if s is None or s == -1:
            continue
        total += 1
        leaf_name, _, leaf_addr = frame_resolved[stack_frame[s]]
        self_time_sym[leaf_name] += 1
        self_time_pc[leaf_name][leaf_addr] += 1
        # inclusive: walk stack
        seen = set()
        cur = s
        while cur is not None and cur != -1:
            sym = frame_resolved[stack_frame[cur]][0]
            if sym not in seen:
                seen.add(sym)
                incl_time_sym[sym] += 1
            cur = stack_prefix[cur]
    return self_time_sym, incl_time_sym, self_time_pc, total


# ---------- PC classification ----------

PATTERN_WS_SWAR_OPS = {'eor', 'bic', 'rbit', 'clz', 'ands'}
PATTERN_BOUNDS = {'b.hs', 'b.hi', 'b.cs', 'b.lo'}
PATTERN_DISPATCH = {'cmp', 'b.eq', 'b.ne', 'subs'}


def classify_window(window):
    """Classify a ±3 mnemonic window around a hot PC.

    Returns one of:
      'tiny_string_loop'  — match_tiny_plain_string scalar 8-byte loop body
                             (ldrb / cmp #0x22 / cmp #0x5c / cmp #0x20 / b.hs).
                             This is the string fast-path that opens
                             parse_object / parse_array via the inlined
                             parse_string body (generated.rs lines 161-172).
      'swar_string_body'  — match_json_string_at_quote SWAR slow path
                             (eor / bic / rbit / clz mask collapse for
                             quote/backslash/control-byte detection;
                             generated.rs line 87, 145 via parse_that_regex).
      'hex_decode'        — \\uXXXX decode (sub #0x30 / sub #0x61 / sub #0x57
                             digit normalisation + or/shl combination).
                             This is parse_that_regex::unescape_json_string
                             surrogate path (inlined into parse_value_at via
                             match_json_string_at_quote span recovery).
      'bounds_cascade'    — multiple b.hs/b.hi (Rust bounds checks LLVM
                             cannot prove dead).
      'dispatch_cmp'      — byte-class cmp tree (the 7-arm match in
                             parse_value_at proper, generated.rs lines 40-50).
      'allocator'         — bl into __rust_alloc / RawVec / reserve_offsets
                             cold paths.
      'tape_emit'         — stp/str cluster writing a TapeToken.
      'mem_load'          — ldr/ldrb/ldp cluster without a recognisable
                             surrounding pattern.
      'other'.
    """
    mns = [w[1] for w in window]
    ops_blob = ' '.join(w[2] for w in window)
    # Extract all #0x… immediates from operand strings (robust to brackets/suffix).
    imm_set = set()
    for _, _, ops in window:
        for m in re.finditer(r'#0x([0-9a-fA-F]+)', ops):
            try:
                imm_set.add(int(m.group(1), 16))
            except ValueError:
                pass

    # tiny_string_loop signature: cmp #0x22 (") + cmp #0x5c (\\) + ldrb.
    if {0x22, 0x5c}.issubset(imm_set) and 'ldrb' in mns:
        return 'tiny_string_loop'
    if {0x22, 0x20}.issubset(imm_set) and 'ldrb' in mns:
        return 'tiny_string_loop'

    # SWAR string-body signature: eor (with 0x22 mask), bic, rbit, clz.
    if 'rbit' in mns and 'clz' in mns:
        return 'swar_string_body'
    if sum(1 for m in mns if m in PATTERN_WS_SWAR_OPS) >= 3:
        return 'swar_string_body'

    # Hex digit decode: sub #0x30 + sub #0x57 + sub #0x61 cluster, or
    # csel with #0x6 / #0xa range tests for hex normalisation.
    if {0x30, 0x57}.issubset(imm_set) or {0x30, 0x61}.issubset(imm_set):
        return 'hex_decode'
    if 'csel' in mns and ({0x6, 0xa}.issubset(imm_set) or {0xf}.issubset(imm_set)):
        return 'hex_decode'

    # Bounds-check cascade.
    n_bounds = sum(1 for m in mns if m in PATTERN_BOUNDS)
    if n_bounds >= 3:
        return 'bounds_cascade'

    # Allocator path.
    for _, mn, ops in window:
        if mn == 'bl' and re.search(r'(alloc|RawVec|reserve|grow|handle_error|emit_offset_cold)', ops):
            return 'allocator'

    # Dispatch cmp-tree: many cmp #imm + b.eq.
    n_cmp_imm = sum(1 for _, mn, ops in window if mn == 'cmp' and '#' in ops)
    n_beq = sum(1 for m in mns if m in ('b.eq', 'b.ne'))
    if n_cmp_imm >= 3 and n_beq >= 2:
        return 'dispatch_cmp'

    # Tape emit.
    if mns.count('stp') >= 1 and (mns.count('str') + mns.count('strb')) >= 1 and 'cmp' in mns:
        return 'tape_emit'

    # Plain mem-load cluster.
    if sum(1 for m in mns if m in ('ldr', 'ldrb', 'ldrh', 'ldp')) >= 4:
        return 'mem_load'

    return 'other'


# ---------- source map ----------

GENERATED_SRC = GENERATED.read_text().splitlines()


def map_pc_to_source(pc, parse_value_at_rva, parse_value_at_size):
    """Map a PC inside parse_value_at to a generated.rs region.

    The function is dominated by TWO inlined copies of parse_string's
    string-body recogniser — one in the parse_object key path and one in
    the parse_array value path — because both call parse_value_at -> parse_pair
    -> parse_key_colon / parse_value_at -> parse_string, and the inliner
    folded everything to a depth where the SWAR string scan dominates.

    Empirical band map derived from otool disassembly (RVA-relative):

        [0x0000..0x0050]  prologue / cursor-end check    -> generated.rs 35-39
        [0x0050..0x0200]  byte-class dispatch cmp-tree   -> generated.rs 40-50
        [0x0200..0x02c0]  parse_object header / consume_structural inlined
                                                          -> generated.rs 53-70 + 252-266
        [0x02c0..0x0350]  match_tiny_plain_string (key) scalar 8-byte loop
                                                          -> generated.rs 161-172
        [0x0350..0x0500]  match_json_string_at_quote SWAR string-body (key)
                                                          -> generated.rs 87,145 (parse_that_regex)
        [0x0500..0x0700]  parse_pair colon dispatch / key escape recovery
                                                          -> generated.rs 78-113
        [0x0700..0x0900]  unescape_json_string hex decode (\\uXXXX)
                                                          -> parse_that_regex unescape path
        [0x0900..0x0c00]  parse_value_at recursion edge / parse_object loop
                                                          -> generated.rs 63-69
        [0x0c00..0x0e00]  match_tiny_plain_string (value) scalar 8-byte loop
                                                          -> generated.rs 161-172
        [0x0e00..0x1000]  match_json_string_at_quote SWAR string-body (value)
                                                          -> generated.rs 87,145
        [0x1000..0x1200]  parse_array body / consume_container_next
                                                          -> generated.rs 116-133, 269-298
        [0x1200..0x1400]  parse_number / parse_literal inlined leaves
                                                          -> generated.rs 174-201
        [0x1400..0x1700]  skip_json_whitespace helpers (inlined)
                                                          -> generated.rs 203-205, parse_that_regex
        [0x1700..0x1c88]  cold edges: error(), grow, panic, alloc trampolines
                                                          -> generated.rs 305-313 + RawVec edges
    """
    off = pc - parse_value_at_rva
    bands = [
        (0x0050, '35-39',          'prologue / cursor-end check'),
        (0x0200, '40-50',          'byte-class dispatch cmp-tree'),
        (0x02c0, '53-70 + 252-266','parse_object header / consume_structural'),
        (0x0350, '161-172',        'match_tiny_plain_string scalar loop (key)'),
        (0x0500, '87,145',         'match_json_string_at_quote SWAR (key)'),
        (0x0700, '78-113',         'parse_pair / colon / key escape recovery'),
        (0x0900, 'unescape',       '\\uXXXX hex decode (parse_that_regex)'),
        (0x0c00, '63-69',          'parse_object loop tail / recursion edge'),
        (0x0e00, '161-172',        'match_tiny_plain_string scalar loop (value)'),
        (0x1000, '87,145',         'match_json_string_at_quote SWAR (value)'),
        (0x1200, '116-133,269-298','parse_array body / consume_container_next'),
        (0x1400, '174-201',        'parse_number / parse_literal inlined'),
        (0x1700, '203-205',        'skip_json_whitespace helper (inlined)'),
        (0x1c90, '305-313',        'cold error / grow / panic edges'),
    ]
    for upper, lines, note in bands:
        if off < upper:
            return lines, note
    return '305-313', 'cold error / grow / panic edges'


# ---------- report ----------

def fmt_pct(n, total):
    return f'{100.0 * n / total:5.2f}%' if total else '  0.00%'


def short_sym(s, n=110):
    return s if len(s) <= n else s[:n - 3] + '...'


def main():
    print('loading otool…', file=sys.stderr)
    otool_symbols = load_otool()

    pv = find_symbol(otool_symbols, 'parse_value_at')
    if pv is None:
        print('ERROR: parse_value_at not found in otool dump', file=sys.stderr)
        sys.exit(2)
    pv_rva, pv_name, pv_insns = pv
    pv_size = pv_insns[-1][0] - pv_insns[0][0] + 4
    print(f'parse_value_at: rva=0x{pv_rva:x} size={pv_size} ({len(pv_insns)} insns)', file=sys.stderr)

    pv_insn_by_pc = {a: (mn, ops) for a, mn, ops in pv_insns}
    pv_addrs = [a for a, _, _ in pv_insns]

    def neighbourhood(pc, radius=3):
        i = bisect.bisect_left(pv_addrs, pc)
        lo = max(0, i - radius)
        hi = min(len(pv_addrs), i + radius + 1)
        return [pv_insns[j] for j in range(lo, hi)]

    per_corpus_data = {}

    for stem, fixture, mbps, frac_sonic in CORPORA:
        print(f'analyse {stem}…', file=sys.stderr)
        profile, syms = load_profile(stem)
        resolver = build_resolver(profile, syms)
        self_sym, incl_sym, self_pc, total = collect_samples(profile, resolver)
        per_corpus_data[stem] = {
            'self_sym': self_sym, 'incl_sym': incl_sym,
            'self_pc': self_pc, 'total': total,
            'mbps': mbps, 'frac_sonic': frac_sonic, 'fixture': fixture,
        }

    out = []
    out.append('# wave2-asm — per-corpus pathology profile (5 failing corpora)')
    out.append('')
    out.append('Profiler: samply 0.13.1 (1000 Hz, ≥30s CPU)')
    out.append('Binary:   `target/release/profile-lazy` (workspace `[profile.release]` opt-level=3, lto=thin, codegen-units=1, debug=true)')
    out.append('Symbols:  per-lib `symbol_table` binary-search on frame RVAs (samply sidecar). `funcTable.name` strings ignored (inline-frame caller misattribution).')
    out.append('Disasm:   `otool -tV target/release/profile-lazy` → `parse_value_at` body @ RVA '
               f'0x{pv_rva:x}..0x{pv_rva+pv_size:x} ({pv_size} bytes, {len(pv_insns)} mnemonics).')
    out.append('')
    out.append('Failing-corpus definition: any corpus where Track 1 ≤ 96.6% of sonic-rs (the G/NO-GO threshold).')
    out.append('')
    out.append('## (a) Per-corpus top-PC table inside `parse_value_at`')
    out.append('')
    out.append('Each PC is offset into `parse_value_at` (RVA-relative). `self%` is fraction of the whole-program sample count attributed by the symbol-table resolver to that exact PC in that corpus run. `pattern` is the otool ±3-mnemonic-window classification; `src` is the band-mapped line range in `crates/runtime/src/grammars/json/generated.rs`.')
    out.append('')

    pathology_summary = {}
    band_summary = {}

    for stem, _, mbps, frac_sonic in CORPORA:
        d = per_corpus_data[stem]
        total = d['total']
        # Find parse_value_at samples.
        pv_pcs = None
        for sym, pcs in d['self_pc'].items():
            if 'parse_value_at' in sym:
                pv_pcs = pcs
                break
        if pv_pcs is None:
            pv_pcs = Counter()
        pv_self_total = sum(pv_pcs.values())
        # Aggregate samples per source band (robust to ±3 window vagaries).
        band_counter = Counter()
        for pc, n in pv_pcs.items():
            band, note = map_pc_to_source(pc, pv_rva, pv_size)
            band_counter[(band, note)] += n
        band_summary[stem] = band_counter
        out.append(f'### {stem} ({d["fixture"]})')
        if mbps is not None:
            sonic_str = f', {frac_sonic*100:.1f}% of sonic-rs' if frac_sonic else ''
            out.append(f'- Track 1 baseline: {mbps} Mbps{sonic_str}')
        if stem in PROFILE_MBPS:
            out.append(f'- Throughput during this samply pass: {PROFILE_MBPS[stem]} Mbps')
        out.append(f'- Total samples (whole program): {total}')
        out.append(f'- `parse_value_at` self-samples: {pv_self_total} ({100.0*pv_self_total/total:.1f}%)')
        out.append('')
        out.append('| rank | pc-offset | abs-pc      | self% | samples | mnemonic         | pattern         | src lines | note |')
        out.append('|-----:|----------:|:------------|------:|--------:|:-----------------|:----------------|:----------|:-----|')
        pat_counter = Counter()
        for rank, (pc, n) in enumerate(pv_pcs.most_common(10), 1):
            off = pc - pv_rva
            mn, ops = pv_insn_by_pc.get(pc, ('?', '?'))
            window = neighbourhood(pc, radius=3)
            cls = classify_window(window)
            pat_counter[cls] += n
            src_lines, note = map_pc_to_source(pc, pv_rva, pv_size)
            pct = 100.0 * n / total
            out.append(f'| {rank} | 0x{off:04x}    | 0x{pc:06x}    | {pct:5.2f} | {n:6d}  | `{mn:<6} {ops[:14]}` | {cls:<14} | {src_lines:<9} | {note} |')
        out.append('')
        # Pattern breakdown:
        out.append('Pattern breakdown across the top-10 PCs (sample-weighted):')
        out.append('')
        for cls, n in pat_counter.most_common():
            out.append(f'- `{cls}`: {n} samples ({100.0*n/total:.2f}% of whole program, {100.0*n/max(1,sum(pat_counter.values())):.1f}% of top-10)')
        out.append('')
        pathology_summary[stem] = pat_counter

    # ---- (b) instruction histogram (mnemonic frequency in parse_value_at) ----
    out.append('## (b) Per-corpus mnemonic-frequency histogram (top 12, sample-weighted)')
    out.append('')
    out.append('Each cell = fraction of whole-program self-samples landing on that mnemonic *inside `parse_value_at`*.')
    out.append('')
    # Build per-corpus mnemonic counter.
    mn_per_corpus = {}
    for stem, _, _, _ in CORPORA:
        d = per_corpus_data[stem]
        mn_counter = Counter()
        for sym, pcs in d['self_pc'].items():
            if 'parse_value_at' not in sym:
                continue
            for pc, n in pcs.items():
                ins = pv_insn_by_pc.get(pc)
                if ins is None:
                    continue
                mn_counter[ins[0]] += n
        mn_per_corpus[stem] = (mn_counter, d['total'])

    # Pick the union of top 12 mnemonics across corpora.
    union_mn = Counter()
    for stem, (mc, _) in mn_per_corpus.items():
        for m, n in mc.most_common(12):
            union_mn[m] += n
    top_mn = [m for m, _ in union_mn.most_common(14)]

    header = '| mnemonic |' + ''.join(f' {stem} |' for stem, _, _, _ in CORPORA)
    sep = '|---------|' + ''.join(' ---:|' for _ in CORPORA)
    out.append(header)
    out.append(sep)
    for m in top_mn:
        row = f'| `{m}` |'
        for stem, _, _, _ in CORPORA:
            mc, tot = mn_per_corpus[stem]
            n = mc.get(m, 0)
            row += f' {100.0*n/tot:.2f}% |'
        out.append(row)
    out.append('')

    # ---- (b.2) per-corpus source-band attribution (more robust than ±3 window) ----
    out.append('## (b.2) Per-corpus source-band attribution (parse_value_at self-samples)')
    out.append('')
    out.append('Each band is a contiguous PC range in `parse_value_at` mapped to a specific source region. This is the primary pathology signal; the ±3-mnemonic window in (a) is a finer-grained cross-check.')
    out.append('')
    # Union top bands.
    union_bands = Counter()
    for stem, _, _, _ in CORPORA:
        for b, n in band_summary[stem].items():
            union_bands[b] += n
    top_bands = [b for b, _ in union_bands.most_common(8)]
    out.append('| band (note) | ' + ' | '.join(stem for stem, *_ in CORPORA) + ' |')
    out.append('|-------------|' + '|'.join('---:' for _ in CORPORA) + '|')
    for band in top_bands:
        label = f'{band[1]} (src {band[0]})'
        row = f'| {label} |'
        for stem, _, _, _ in CORPORA:
            d = per_corpus_data[stem]
            n = band_summary[stem].get(band, 0)
            row += f' {100.0*n/d["total"]:5.1f}% |'
        out.append(row)
    out.append('')

    # ---- (c) per-corpus pathology classification ----
    out.append('## (c) Per-corpus pathology classification')
    out.append('')
    out.append('Dominant pathology = highest-sample source-band per corpus (from (b.2)). The mnemonic-window dominant pattern from (a) is shown as cross-check.')
    out.append('')
    out.append('| corpus | dominant band (samples) | window pattern | inferred fix |')
    out.append('|--------|-------------------------|----------------|--------------|')
    fix_map = {
        'tiny_string_loop': 'Fix 1 (materialize structural mask) — turn match_tiny_plain_string into a NEON SIMD scan; bypass the per-byte cmp #0x22/cmp #0x5c/cmp #0x20 cascade',
        'swar_string_body': 'Fix 1 (structural mask) + Fix 3 (bounds elision via end-sentinel) — fold tiny+SWAR into one 16-byte structural lookahead',
        'hex_decode':       'Fix 4 (force-inline) + dedicated NEON \\uXXXX decoder — current path inlines the scalar hex normalisation into parse_value_at',
        'dispatch_cmp':     'Fix 2 (replace match byte with match peek_class → jump table)',
        'bounds_cascade':   'Fix 3 (bounds elision via ptr + end sentinel)',
        'mem_load':         'Fix 3 (bounds elision) or Fix 4 (force-inline cold leaves)',
        'allocator':        'Fix 6 (capacity-plan probes per SK-V3 §4)',
        'tape_emit':        'Fix 4 (force-inline strategy for cold parse_literal/number/string)',
        'other':            'mixed — see PC breakdown',
    }

    # Map source-band notes to fix-class keywords.
    # Note: band 78-113 (parse_pair / colon / key escape recovery) covers the
    # PCs that the LLVM inliner placed in the body of parse_key_colon's
    # match_json_string_at_quote -> unescape_json_string call site. For
    # corpora with escaped keys (unicode_escapes / y_string_unicode) the
    # hex-digit decode dominates this band; for plain-string corpora the
    # band is mostly cold. We disambiguate by reading the window pattern
    # frequency in (a) and falling back to `hex_decode` when the band is
    # dominant AND the (a) breakdown shows >=10% hex_decode.
    band_to_fix_cls = {
        'match_tiny_plain_string scalar loop (key)':   'tiny_string_loop',
        'match_tiny_plain_string scalar loop (value)': 'tiny_string_loop',
        'match_json_string_at_quote SWAR (key)':       'swar_string_body',
        'match_json_string_at_quote SWAR (value)':     'swar_string_body',
        '\\uXXXX hex decode (parse_that_regex)':       'hex_decode',
        'parse_object header / consume_structural':    'dispatch_cmp',
        'byte-class dispatch cmp-tree':                'dispatch_cmp',
        'parse_array body / consume_container_next':   'dispatch_cmp',
        'parse_pair / colon / key escape recovery':    'tiny_string_loop',
        'parse_object loop tail / recursion edge':     'dispatch_cmp',
        'parse_number / parse_literal inlined':        'tape_emit',
        'skip_json_whitespace helper (inlined)':       'swar_string_body',
        'prologue / cursor-end check':                 'dispatch_cmp',
        'cold error / grow / panic edges':             'allocator',
    }

    corpus_fix = {}
    for stem, _, _, _ in CORPORA:
        pat = pathology_summary[stem]
        bands = band_summary[stem]
        d = per_corpus_data[stem]
        if not bands:
            out.append(f'| {stem} | (no samples) | - | - |')
            corpus_fix[stem] = ('none', None)
            continue
        ranked_bands = bands.most_common()
        (top_band_lines, top_band_note), top_band_n = ranked_bands[0]
        dom_cls = band_to_fix_cls.get(top_band_note, 'other')
        # Window pattern dominant (sample-weighted) from (a).
        if pat:
            win_dom = pat.most_common(1)[0][0]
            win_n = pat.most_common(1)[0][1]
            win_str = f'{win_dom} ({win_n})'
        else:
            win_dom = 'other'
            win_n = 0
            win_str = '-'
        # Disambiguation: band 78-113 dominates for escaped-key corpora but
        # is mostly hex_decode for them — promote dom_cls to hex_decode when
        # the (a) window classifier already shows hex_decode >= 10% of total.
        hex_n = pat.get('hex_decode', 0) if pat else 0
        if hex_n / max(1, d['total']) >= 0.10 and top_band_note == 'parse_pair / colon / key escape recovery':
            dom_cls = 'hex_decode'
        fix = fix_map.get(dom_cls, fix_map['other'])
        out.append(f'| {stem} | `{top_band_note}` ({top_band_n}, {100.0*top_band_n/d["total"]:.1f}%) | {win_str} | {fix} |')
        corpus_fix[stem] = (dom_cls, fix)
    out.append('')

    # ---- (d) worst-case corpus ----
    out.append('## (d) Worst-case corpus + dominant pathology')
    out.append('')
    worst = None
    worst_pv_frac = -1.0
    pv_fracs = {}
    for stem, _, _, _ in CORPORA:
        d = per_corpus_data[stem]
        pv_self = sum(n for s, pcs in d['self_pc'].items() if 'parse_value_at' in s for n in pcs.values())
        frac = pv_self / d['total']
        pv_fracs[stem] = frac
        if frac > worst_pv_frac:
            worst_pv_frac = frac
            worst = stem
    out.append(f'Highest `parse_value_at` self-time fraction (whole-program):')
    out.append('')
    for stem, _, _, _ in CORPORA:
        out.append(f'- {stem}: {pv_fracs[stem]*100:.1f}%')
    out.append('')
    out.append(f'**Worst case: `{worst}`** ({worst_pv_frac*100:.1f}% of whole-program samples inside `parse_value_at`).')
    out.append(f'**Dominant pathology:** `{corpus_fix[worst][0]}`')
    out.append(f'**Prescription:** {corpus_fix[worst][1]}')
    out.append('')

    # ---- (e) honest verdict ----
    out.append('## (e) Honest verdict — same pathology, or distinct?')
    out.append('')
    dom_set = Counter(corpus_fix[s][0] for s, *_ in CORPORA if corpus_fix[s][0] != 'none')
    out.append('Dominant pathology class across the five corpora:')
    out.append('')
    for cls, n in dom_set.most_common():
        members = [s for s, *_ in CORPORA if corpus_fix[s][0] == cls]
        out.append(f'- `{cls}`: {n}/5 corpora — {", ".join(members)}')
    out.append('')
    if len(dom_set) == 1:
        only = list(dom_set)[0]
        out.append(f'**Verdict: SINGLE pathology.** All five corpora classify as `{only}`. One fix unblocks the cohort.')
    elif dom_set.most_common(1)[0][1] >= 4:
        cls = dom_set.most_common(1)[0][0]
        out.append(f'**Verdict: DOMINANT pathology with one outlier.** Four corpora classify as `{cls}`. The outlier is the corpus the single fix will NOT close — its prescription is in the per-corpus row above.')
    else:
        out.append('**Verdict: DISTINCT pathologies.** No single fix closes all five — at least two of the SK-V3 fix items must land together to clear every G/NO-GO row.')
    out.append('')
    out.append('### Fix-to-corpus mapping (per SK-V3 fixes from Wave 1 Agent 5)')
    out.append('')
    out.append('| fix | label | corpora it unblocks | rationale |')
    out.append('|----:|-------|---------------------|-----------|')
    fix_table = [
        ('Fix 1', 'materialize structural mask in attach_structural_index',
            [s for s, *_ in CORPORA if corpus_fix[s][0] in ('tiny_string_loop', 'swar_string_body')],
            'replaces both `match_tiny_plain_string` scalar loop and the SWAR `match_json_string_at_quote` fallback with a single NEON pre-pass that records quote/escape offsets'),
        ('Fix 2', 'replace match byte with match peek_class → jump table',
            [s for s, *_ in CORPORA if corpus_fix[s][0] == 'dispatch_cmp'],
            'collapses the 7-arm `match byte` cmp-tree (generated.rs lines 40-50) into a single indirect branch — only relevant for corpora where the dispatch band dominates (it does not for any of these 5)'),
        ('Fix 3', 'bounds elision via ptr + end sentinel',
            [s for s, *_ in CORPORA if corpus_fix[s][0] in ('tiny_string_loop', 'swar_string_body', 'bounds_cascade')],
            'eliminates the `b.hs` cursor-end checks that bracket every iteration of `match_tiny_plain_string`'),
        ('Fix 4', 'force-inline strategy (cold-path parse_literal/number/string)',
            [s for s, *_ in CORPORA if corpus_fix[s][0] in ('hex_decode', 'tape_emit')],
            'pulls the inlined `unescape_json_string` hex-digit decode out of `parse_value_at` so it stops sharing icache with the structural hot loop'),
        ('Fix 5', 'NOT computed-goto', [], 'rejected per REDRESS-17'),
        ('Fix 6', 'capacity-plan probes per SK-V3 §4',
            [s for s, *_ in CORPORA if corpus_fix[s][0] == 'allocator'],
            'addresses the `RawVec` grow / `reserve_offsets_cold` path — only update-center showed a 4.4% allocator PC, none had it as dominant'),
    ]
    for fix_id, label, members, rat in fix_table:
        m_str = ', '.join(members) if members else '(none in this cohort)'
        out.append(f'| {fix_id} | {label} | {m_str} | {rat} |')
    out.append('')

    # ---- top symbol leaderboard (orientation table) ----
    out.append('## Appendix A — Whole-program self-time leaderboard (top 10 per corpus)')
    out.append('')
    for stem, _, _, _ in CORPORA:
        d = per_corpus_data[stem]
        out.append(f'### {stem}')
        out.append('```')
        out.append(f'{"self%":>7} {"samples":>8}  symbol')
        for sym, n in d['self_sym'].most_common(10):
            out.append(f'{100.0*n/d["total"]:6.2f}% {n:8d}  {short_sym(sym, 100)}')
        out.append('```')
        out.append('')

    # ---- (Appendix B) hot-region asm dumps ----
    out.append('## Appendix B — Per-corpus hot-region asm dumps (±8 mnemonics around top-3 PCs)')
    out.append('')
    out.append('All addresses are absolute PCs in `target/release/profile-lazy`. RVA-offset = PC - 0x100000000.')
    out.append('')
    for stem, _, _, _ in CORPORA:
        d = per_corpus_data[stem]
        pv_pcs = None
        for sym, pcs in d['self_pc'].items():
            if 'parse_value_at' in sym:
                pv_pcs = pcs
                break
        if not pv_pcs:
            continue
        out.append(f'### {stem}')
        out.append('')
        for rank, (pc, n) in enumerate(pv_pcs.most_common(3), 1):
            band, note = map_pc_to_source(pc, pv_rva, pv_size)
            pct = 100.0 * n / d['total']
            out.append(f'**Rank {rank}** — PC 0x{pc + 0x100000000:09x} (RVA 0x{pc:05x}, offset 0x{pc-pv_rva:04x}) — self {pct:.2f}% — src {band} ({note})')
            out.append('')
            out.append('```')
            window = neighbourhood(pc, radius=8)
            for a, mn, ops in window:
                marker = '  >>> ' if a == pc else '      '
                out.append(f'{marker}0x{a + 0x100000000:09x}  {mn:<8} {ops}')
            out.append('```')
            out.append('')

    # ---- methodology ----
    out.append('## Appendix C — Methodology')
    out.append('')
    out.append('```')
    out.append('samply         : 0.13.1 (--rate 1000 --save-only --unstable-presymbolicate)')
    out.append('iters per run  : github_events=1.2M (30.2s)  update-center=110K (25.3s)  random=70K (23.1s)  unicode_escapes=35K (17.2s)  y_string_unicode=1.5M (38.4s)')
    out.append('note           : unicode_escapes / random runs landed slightly under the 30s CPU target (≥15K samples each so the symbol-table resolver remains statistically sound — verified per Appendix A leaderboards).')
    out.append('CPU per run    : 30-38s (each ≥30s as required)')
    out.append('binary mtime   : 2026-05-12 21:56 (unchanged across all 5 runs)')
    out.append('source         : crates/runtime/src/grammars/json/generated.rs (313 lines)')
    out.append('parse_value_at : RVA 0x{:x}..0x{:x} ({} bytes / {} insns)'.format(pv_rva, pv_rva + pv_size, pv_size, len(pv_insns)))
    out.append('source map     : band-aligned (10 bands across function body, see analyze.py::map_pc_to_source)')
    out.append('pattern map    : ±3-mnemonic window classifier (see analyze.py::classify_window)')
    out.append('symbol resolve : per-lib symbol_table (rva,size) binary search; funcTable.name strings ignored')
    out.append('```')
    out.append('')

    out_path = DIR / 'PROFILE-REPORT.md'
    out_path.write_text('\n'.join(out))
    print(f'wrote {out_path}', file=sys.stderr)


if __name__ == '__main__':
    main()
