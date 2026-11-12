#!/usr/bin/env python3
"""S9-PERF mutants: each re-introduces a defect the unit's gate guards; read x2; restore."""
import subprocess, os, sys, hashlib, pathlib
WT = pathlib.Path(os.path.expanduser('~/.dev-logs/bbnf-lockstep/lw3/s9-perf/wt-red'))
L = pathlib.Path(os.path.expanduser('~/.dev-logs/bbnf-lockstep/lw3/s9-perf/mutants')); L.mkdir(exist_ok=True)
G = os.path.expanduser('~/.claude/guard/guarded')
env = dict(os.environ, CARGO_TARGET_DIR=os.path.expanduser('~/.dev-logs/bbnf-lockstep/lw3/target-parse-that'),
           PARSE_THAT_CORPUS_DIR=os.path.expanduser('~/Programming/parse-that/data'))
S = 'rust/simd/src/'
MUTANTS = [
  ('1a-comment-mask-dropped', 'PERF-1a', S+'stage1.rs',
   'let outside = valid & !qbody & !cbody;', 'let outside = valid & !qbody;', 'stage1', 'css_brace_inside_a_comment_is_not_structural'),
  ('1a-atom-starts-dropped', 'PERF-1a', S+'stage1.rs',
   'let mut slots = structural | starts;', 'let starts = 0u64;\n        let mut slots = structural | starts;', 'stage1', 'atom_starts_are_in_the_index'),
  ('1a-neon-utf8-accepts-all', 'PERF-1a', S+'neon.rs',
   'unsafe { vmaxvq_u8(vorrq_u8(self.error, self.incomplete)) == 0 }', 'unsafe { vmaxvq_u8(vorrq_u8(self.error, self.incomplete)) < 0x80 }', 'stage1', 'every_tier_utf8_sweep'),
  ('1a-utf8-table-surrogate', 'PERF-1a', S+'stage1.rs',
   'CARRY | TOO_LARGE | TOO_LARGE_1000 | SURROGATE,', 'CARRY | TOO_LARGE | TOO_LARGE_1000,', 'stage1', 'every_tier_utf8_sweep'),
  ('1a-empty-shape-digraphs', 'PERF-1a', S+'neon.rs',
   'KernelShape::Empty => scan_multi(input, alphabet),', 'KernelShape::Empty => StructuralIndex::new(),', 'dispatch_tiers', 'every_supported_tier_matches_the_scalar_oracle'),
  ('1a-padding-read', 'PERF-1a', S+'stage1.rs',
   'let pair_valid = low(rest - 1);', 'let pair_valid = low(rest);', 'stage1', ''),
  ('2-neon-first-high', 'PERF-2', S+'neon.rs',
   'return i + movemask_u8x16(high).trailing_zeros() as usize;', 'return i + (15 - movemask_u8x16(high).leading_zeros() as usize);', 'ascii_prefix', 'every_tier_every_length_alignment_and_position'),
  ('2-neon-tail-overread', 'PERF-2', S+'neon.rs',
   '        while i + 16 <= len {\n            let v = vld1q_u8(p.add(i));\n            if vmaxvq_u8(v) >= 0x80 {',
   '        while i < len {\n            let v = vld1q_u8(p.add(i));\n            if vmaxvq_u8(v) >= 0x80 {', 'ascii_prefix', 'no_tier_reads_past_the_end'),
]
only = sys.argv[1:]
for name, unit, f, old, new, test, filt in MUTANTS:
    if only and name not in only: continue
    p = WT/f; orig = p.read_bytes(); h0 = hashlib.sha256(orig).hexdigest()
    s = orig.decode(); assert s.count(old) == 1, (name, s.count(old)); p.write_text(s.replace(old, new))
    for r in (1, 2):
        cmd = [G, '--timeout', '30', '--', 'cargo', 'test', '--locked', '-p', 'parse-that-simd', '--test', test, '--', '--test-threads', '2'] + ([filt] if filt else [])
        out = subprocess.run(cmd, cwd=WT/'rust', env=env, capture_output=True, text=True)
        log = out.stdout + out.stderr; (L/f'{name}-r{r}.log').write_text(log)
        fails = [l for l in log.splitlines() if l.endswith('FAILED') or 'panicked' in l or 'signal' in l][:3]
        print(f'{name} ({unit}) r{r} rc={out.returncode} :: ' + ' | '.join(fails)[:300], flush=True)
    p.write_bytes(orig); assert hashlib.sha256(p.read_bytes()).hexdigest() == h0
    print(f'{name} restored byte-equal sha256 {h0[:12]}', flush=True)
