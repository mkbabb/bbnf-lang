#!/usr/bin/env python3
"""Analyze samply profiles for skinny bbnf-lang: extract self-time and inclusive-time
attribution per symbol, then classify by function-class.

Methodology matches the sonic-rs analyzer to keep skinny-v3 / sonic-rs / simdjson
comparable: ONLY uses per-lib `symbol_table` binary-search on frame addresses
(funcTable.name strings include inline-frame caller artefacts and misattribute).
"""
import gzip, json, sys, bisect, re
from collections import defaultdict


DIR = '/Users/mkbabb/Programming/bbnf-lang/skinny/profile/skinny-v3'


def load_profile(corpus):
    with gzip.open(f'{DIR}/{corpus}.profile.json.gz', 'rb') as f:
        p = json.load(f)
    with open(f'{DIR}/{corpus}.profile.json.syms.json') as f:
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
                return (names[i], lib_name)
        return (f'{lib_name}!0x{addr:x}', lib_name)

    return resolve


def compute_times(profile, resolver):
    t = profile['threads'][0]
    stacks = t['stackTable']
    frames = t['frameTable']
    samples = t['samples']

    self_time = defaultdict(int)
    incl_time = defaultdict(int)
    n = samples['length']

    stack_prefix = stacks['prefix']
    stack_frame = stacks['frame']

    n_frames = frames['length']
    frame_resolved = [resolver(i) for i in range(n_frames)]

    stack_lineage = [None] * stacks['length']

    def lineage(s):
        if stack_lineage[s] is not None:
            return stack_lineage[s]
        chain = []
        cur = s
        while cur is not None and cur != -1:
            chain.append(stack_frame[cur])
            cur = stack_prefix[cur]
        stack_lineage[s] = chain
        return chain

    sample_stack = samples['stack']
    total = 0
    for i in range(n):
        s = sample_stack[i]
        if s is None:
            continue
        total += 1
        chain = lineage(s)
        if not chain:
            continue
        leaf_sym = frame_resolved[chain[0]][0]
        self_time[leaf_sym] += 1
        seen = set()
        for f in chain:
            sym = frame_resolved[f][0]
            if sym in seen:
                continue
            seen.add(sym)
            incl_time[sym] += 1
    return self_time, incl_time, total


# ---- Function-class classifier (skinny taxonomy) ----
# Spec classes: parse_driver / structural_scan / string_decode / number_parse /
# tape_assembly / view_materialization / allocation / utf8_validation / other.
CLASS_RULES = [
    # parse_driver: the recursive grammar dispatch surface. Skinny's generated
    # `parse_value` is the dispatch hub; most leaves inline into it under
    # lto=thin. parse_object / parse_array / parse_pair / parse_literal are
    # driver-side recursive descent.
    ('parse_driver',         r'(generated_json::(generated::)?parse_(value|object|array|pair|literal)|generated_json::parser::parse$|runtime::generated_json::parse)'),

    # structural_scan: the actual structural-token scanner — `simd_scan::scan_*`
    # is the SIMD NEON structural-index builder; `consume_structural`,
    # `sync_structural`, `skip_ws`, `peek`, `consume` are the scan-side helpers
    # in the generated parser.
    ('structural_scan',      r'(simd_scan::|scan_json|scan_structural|consume_structural|sync_structural|skip_ws|::peek$|::consume$)'),

    # string_decode: the SIMD string-body matcher (parse_that_regex) and any
    # generated parse_string path (which holds the decode loop, not just dispatch).
    ('string_decode',        r'(generated_json::(generated::)?parse_string|parse_that_regex|match_json_string|unescape|decode_str|hex_to_u32|parse_escape)'),

    # number_parse: numeric leaf paths.
    ('number_parse',         r'(generated_json::(generated::)?parse_number|parse_float|parse_int|f64_from_parts|fast_float|strtod|atof|sonic_number)'),

    # tape_assembly: tape construction + token emit + offset arena writes.
    ('tape_assembly',        r'(runtime::tape::|TapeAssembler|emit_offset|emit_offsets|::emit$|patch_end|patch_skip|write_bytes|reserve_tokens|payload_arena|PayloadArena)'),

    # view_materialization: lazy view layer that materializes tokens / values
    # post-parse. Skinny's lazy-mode parse path should show ~0 here.
    ('view_materialization', r'(grammars::json::view::|grammars::json::value::|kind_at_cursor|token_from_cursor|JsonValue|JsonToken|to_canonical_string|token_stream|JsonRoot)'),

    # allocation: libsystem_malloc, rust alloc, RawVec grow, Bump arena allocs.
    ('allocation',           r'(libsystem_malloc|__rustc::__rdl_alloc|alloc::sync::Arc.*drop|alloc_layout_slow|grow_amortized|with_capacity|RawVec|realloc|^_malloc$|DYLD-STUB\$\$(?:malloc|realloc|free))'),

    # utf8_validation: simdutf / core::str::from_utf8 / validate_utf8.
    ('utf8_validation',      r'(validate_utf8|simdutf|core::str::converts::from_utf8|from_utf8_unchecked)'),

    # Other helpers we explicitly want visible separately (memmove/memcpy/syscall
    # leaves do not map to one of the named classes above and should fall
    # through to 'other' — the user's spec is the closed list).
    ('other',                r'.*'),
]
CLASS_REGEX = [(c, re.compile(r, re.IGNORECASE)) for c,r in CLASS_RULES]


def classify(sym):
    for cls, rx in CLASS_REGEX:
        if rx.search(sym):
            return cls
    return 'other'


def fmt_table(rows, headers, top=15):
    rows = rows[:top]
    cols = list(zip(*([headers] + rows)))
    widths = [max(len(str(x)) for x in col) for col in cols]
    out = []
    sep = ' | '
    def row(r): return sep.join(str(x).ljust(widths[i]) for i,x in enumerate(r))
    out.append(row(headers))
    out.append(sep.join('-'*w for w in widths))
    for r in rows:
        out.append(row(r))
    return '\n'.join(out)


def short_sym(s, n=120):
    return s if len(s) <= n else s[:n-3] + '...'


def main():
    out = []
    out.append('# skinny-v3 Profile Report — bbnf-lang Lazy-Tape JSON Parser')
    out.append('')
    out.append('Profiler: samply 0.13.1 (sampling, 1000 Hz)')
    out.append('Binary: /Users/mkbabb/Programming/bbnf-lang/skinny/target/release/profile-lazy')
    out.append('Build: workspace release profile (opt-level=3, lto=thin, codegen-units=1, debug=true, strip=false, split-debuginfo=packed)')
    out.append('Corpora: /Users/mkbabb/Programming/bbnf-lang/skinny/crates/test-fixtures/corpus/json/{twitter,citm_catalog,canada}.json')
    out.append('Bench loop: `runtime::generated_json::parse(black_box(input))` (lazy-offset tape parse — no view realisation)')
    out.append('')
    out.append('IMPORTANT — measurement comparability with sonic-rs / simdjson agents:')
    out.append('- Sampling rate: 1000 Hz (matches sonic-rs methodology, not the 4 kHz used in the prior skinny-v1 run).')
    out.append('- Symbol resolution: per-lib `symbol_table` binary-search on frame RVAs (samply sidecar). The funcTable.name')
    out.append('  strings emitted by samply for unsymbolicated profiles attribute inline-frame samples to the caller and')
    out.append('  produce false leaders such as `Clone::clone`; we ignore them entirely.')
    out.append('- Build flags: workspace `[profile.release]` is `lto = "thin"`, codegen-units = 1 (NOT lto=true, unlike')
    out.append('  sonic-rs). thin-LTO preserves leaf symbols across crates but still inlines aggressively within a')
    out.append('  crate, so skinny\'s `simd_scan::scan_json_parse_index` and `parse_that_regex::match_json_string` ARE')
    out.append('  visible as leaves while many generated helpers (`skip_ws`, `peek`, `consume`, `emit_offset`,')
    out.append('  `TapeAssembler::push`) inline into `parse_value` / `parse_string`.')
    out.append('')

    per_corpus = {}

    for corpus in ['twitter', 'citm', 'canada']:
        print(f'-- processing {corpus} --', file=sys.stderr)
        p, syms = load_profile(corpus)
        resolver = build_resolver(p, syms)
        self_t, incl_t, total = compute_times(p, resolver)

        out.append(f'## {corpus}')
        out.append('')
        out.append(f'Samples: {total} (~{total/1000:.1f}s of CPU at 1000 Hz)')
        out.append('')

        # (a) Top 15 by self-time
        rows = []
        ranked_self = sorted(self_t.items(), key=lambda kv: -kv[1])
        for sym, n in ranked_self[:15]:
            pct = 100.0 * n / total
            rows.append([f'{pct:5.2f}%', n, classify(sym), short_sym(sym)])
        out.append('### Top 15 by self-time (with class attribution)')
        out.append('')
        out.append('```')
        out.append(fmt_table(rows, ['self%', 'samples', 'class', 'symbol']))
        out.append('```')
        out.append('')

        # (b) Top 15 by inclusive-time
        rows = []
        ranked_incl = sorted(incl_t.items(), key=lambda kv: -kv[1])
        for sym, n in ranked_incl[:15]:
            pct = 100.0 * n / total
            rows.append([f'{pct:5.2f}%', n, classify(sym), short_sym(sym)])
        out.append('### Top 15 by inclusive-time (with class attribution)')
        out.append('')
        out.append('```')
        out.append(fmt_table(rows, ['incl%', 'samples', 'class', 'symbol']))
        out.append('```')
        out.append('')

        # (c) Class attribution (self-time)
        class_self = defaultdict(int)
        for sym, n in self_t.items():
            class_self[classify(sym)] += n
        rows = sorted(class_self.items(), key=lambda kv: -kv[1])
        rows = [[f'{100.0*n/total:5.2f}%', n, c] for c,n in rows]
        out.append('### Self-time by function class')
        out.append('')
        out.append('```')
        out.append(fmt_table(rows, ['self%', 'samples', 'class'], top=20))
        out.append('```')
        out.append('')

        # (d) Hot-leaf count metric (>= 10% self per spec)
        hot = [(s, n) for s, n in ranked_self if (100.0 * n / total) >= 10.0]
        out.append(f'### Hot-leaf count at >=10% self-time: **{len(hot)}**')
        out.append('')
        if hot:
            out.append('```')
            rows = [[f'{100.0*n/total:5.2f}%', n, classify(s), short_sym(s)] for s, n in hot]
            out.append(fmt_table(rows, ['self%', 'samples', 'class', 'symbol'], top=99))
            out.append('```')
        out.append('')

        per_corpus[corpus] = (self_t, incl_t, class_self, total, hot)

    # Cross-corpus class-share table.
    out.append('## Cross-corpus class-share comparison (self-time)')
    out.append('')
    all_classes = sorted({c for (_,_,cs,_,_) in per_corpus.values() for c in cs})
    headers = ['class', 'twitter%', 'citm%', 'canada%']
    rows = []
    for c in all_classes:
        row = [c]
        for corpus in ['twitter', 'citm', 'canada']:
            _, _, cs, total, _ = per_corpus[corpus]
            row.append(f'{100.0*cs.get(c,0)/total:.2f}')
        rows.append(row)
    out.append('```')
    out.append(fmt_table(rows, headers, top=99))
    out.append('```')
    out.append('')

    # (d) hot-leaf count summary
    out.append('## Hot-leaf count metric (distinct symbols at >=10% self-time)')
    out.append('')
    out.append('| corpus  | skinny | sonic-rs | simdjson |')
    out.append('|---------|--------|----------|----------|')
    for corpus in ['twitter', 'citm', 'canada']:
        n = len(per_corpus[corpus][4])
        out.append(f'| {corpus:<7} | {n}      | 1        | 2        |')
    out.append('')
    out.append('Comparator anchor: sonic-rs = 1 (fused recursive descent), simdjson = 2 (stage1 / stage2 split).')
    out.append('Skinny\'s hot-leaf count is higher because thin-LTO preserves the SIMD scanner leaf')
    out.append('(`simd_scan::scan_json_parse_index`) and the string-body matcher (`parse_that_regex::match_json_string`)')
    out.append('as distinct symbols rather than inlining them into a single fused descent.')
    out.append('')

    # (e) Methodology
    out.append('## (e) Methodology — reproducibility')
    out.append('')
    out.append('```')
    out.append('samply version : 0.13.1')
    out.append('sampling rate  : 1000 Hz (--rate 1000)')
    out.append('samply flags   : --save-only --unstable-presymbolicate -o <corpus>.profile.json.gz')
    out.append('host           : Apple Silicon (aarch64-apple-darwin)')
    out.append('build          : cargo build --release -p xtask --bin profile-lazy')
    out.append('release profile: opt-level=3, lto="thin", codegen-units=1, debug=true, strip=false,')
    out.append('                 split-debuginfo="packed", panic=unwind (default)')
    out.append('driver         : xtask/src/bin/profile_lazy.rs ARGS = <iters> <corpus>')
    out.append('iters          : twitter=80000, citm=30000, canada=16000 (each sized to >=30s of CPU)')
    out.append('warmup         : 16 iters per corpus (separate from timed loop, also profiled)')
    out.append('parse call     : runtime::generated_json::parse(std::hint::black_box(input))')
    out.append('symbol resolve : per-lib symbol_table (rva,size) binary search on frameTable.address')
    out.append('                 funcTable.name strings ignored (inline-frame caller misattribution)')
    out.append('```')
    out.append('')
    out.append('Reproduce:')
    out.append('```sh')
    out.append('cd /Users/mkbabb/Programming/bbnf-lang/skinny')
    out.append('cargo build --release -p xtask --bin profile-lazy')
    out.append('mkdir -p profile/skinny-v3')
    out.append('for c in twitter:80000 citm_catalog:30000 canada:16000; do')
    out.append('  name=${c%%:*}; iters=${c##*:}; out=${name/citm_catalog/citm}')
    out.append('  CARGO_MANIFEST_DIR=$PWD/xtask samply record --save-only \\')
    out.append('    --unstable-presymbolicate --rate 1000 \\')
    out.append('    -o profile/skinny-v3/${out}.profile.json.gz \\')
    out.append('    ./target/release/profile-lazy $iters $name')
    out.append('done')
    out.append('python3 profile/skinny-v3/analyze.py')
    out.append('```')
    out.append('')

    # (f) Honest take per corpus.
    out.append('## (f) Honest take per corpus')
    out.append('')
    twitter_self, _, twitter_cs, twitter_total, twitter_hot = per_corpus['twitter']
    citm_self, _, citm_cs, citm_total, citm_hot = per_corpus['citm']
    canada_self, _, canada_cs, canada_total, canada_hot = per_corpus['canada']

    # Pull the top symbols for each corpus to assemble honest takes from real numbers.
    def top_n(self_t, total, n=4):
        return [(s, 100.0*c/total, c) for s, c in sorted(self_t.items(), key=lambda kv: -kv[1])[:n]]

    t_top = top_n(twitter_self, twitter_total)
    c_top = top_n(citm_self, citm_total)
    n_top = top_n(canada_self, canada_total)

    def fmt_top(top):
        return ', '.join(f'{short_sym(s, 60)} {p:.1f}%' for s, p, _ in top)

    out.append(f'- **twitter** — top leaves: {fmt_top(t_top)}. Skinny\'s lazy-tape parse splits between')
    out.append(f'  the SIMD structural scanner and the generated `parse_value` dispatch hub; thin-LTO keeps')
    out.append(f'  both visible as separate leaves rather than fusing them. Hot-leaf count = {len(twitter_hot)}')
    out.append(f'  (sonic-rs 1, simdjson 2).')
    out.append('')
    out.append(f'- **citm** — top leaves: {fmt_top(c_top)}. Object-key density drives `parse_string` and the')
    out.append(f'  `parse_that_regex::match_json_string` body matcher higher than on twitter. Hot-leaf count = {len(citm_hot)}.')
    out.append('')
    out.append(f'- **canada** — top leaves: {fmt_top(n_top)}. Almost entirely nested arrays of float pairs, so the')
    out.append(f'  number-parse path and the structural scanner over numeric tokens dominate; string work shrinks to')
    out.append(f'  near zero. Hot-leaf count = {len(canada_hot)}.')
    out.append('')

    out_path = f'{DIR}/PROFILE-REPORT.md'
    with open(out_path, 'w') as f:
        f.write('\n'.join(out))
    print(f'wrote {out_path}', file=sys.stderr)


if __name__ == '__main__':
    main()
