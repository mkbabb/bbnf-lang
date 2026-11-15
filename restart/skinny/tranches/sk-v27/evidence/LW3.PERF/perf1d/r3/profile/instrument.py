#!/usr/bin/env python3
"""PERF1D-B2 (b) hold profile: count the cure's routed calls in a diagnostic copy of json.rs (never committed)."""
import sys
p = sys.argv[1]
s = open(p).read()
counters = '''
pub mod __perf1d_prof {
    use core::sync::atomic::{AtomicU64, Ordering::Relaxed};
    pub static QB_CALLS: AtomicU64 = AtomicU64::new(0);
    pub static QB_BYTES: AtomicU64 = AtomicU64::new(0);
    pub static WS_CALLS: AtomicU64 = AtomicU64::new(0);
    pub static SLOW_CALLS: AtomicU64 = AtomicU64::new(0);
    pub static SLOW_BYTES: AtomicU64 = AtomicU64::new(0);
    pub static ST_CALLS: AtomicU64 = AtomicU64::new(0);
    pub static ST_BYTES: AtomicU64 = AtomicU64::new(0);
    pub fn add(c: &AtomicU64, n: u64) { c.fetch_add(n, Relaxed); }
    pub fn reset() { for c in [&QB_CALLS,&QB_BYTES,&WS_CALLS,&SLOW_CALLS,&SLOW_BYTES,&ST_CALLS,&ST_BYTES] { c.store(0, Relaxed); } }
    pub fn read() -> [u64; 7] { [QB_CALLS.load(Relaxed),QB_BYTES.load(Relaxed),WS_CALLS.load(Relaxed),SLOW_CALLS.load(Relaxed),SLOW_BYTES.load(Relaxed),ST_CALLS.load(Relaxed),ST_BYTES.load(Relaxed)] }
}
'''
P = "crate::grammar::generated::json::__perf1d_prof"
def rep(old, new):
    global s
    assert s.count(old) == 1, old[:80]
    s = s.replace(old, new)
rep('''            !::parse_that::scan::byte_class_from_eq_set_64(stripe, b" \\t\\n\\r")''',
    '''            %s::add(&%s::WS_CALLS, 1);
            !::parse_that::scan::byte_class_from_eq_set_64(stripe, b" \\t\\n\\r")''' % (P, P))
rep('''            ::parse_that::scan::find_quote_or_backslash(bytes)
        }''',
    '''            let __r = ::parse_that::scan::find_quote_or_backslash(bytes);
            %s::add(&%s::QB_CALLS, 1);
            %s::add(&%s::QB_BYTES, __r.map_or(bytes.len(), |(o, _)| o + 1) as u64);
            __r
        }''' % (P, P, P, P))
rep('''                    ::parse_that::scan::scan_structural(input, &alphabet)''',
    '''                    %s::add(&%s::ST_CALLS, 1);
                    %s::add(&%s::ST_BYTES, input.len() as u64);
                    ::parse_that::scan::scan_structural(input, &alphabet)''' % (P, P, P, P))
# skip_space_slow entry: count calls and bytes skipped
old = '''        pub(crate) fn skip_space_slow(
            input: &[u8],
            p: &mut usize,
            state: &mut ScanState,
        ) {'''
assert s.count(old) == 1
s = s.replace(old, old + '''
            %s::add(&%s::SLOW_CALLS, 1);
            let __p0 = *p;
            skip_space_slow_inner(input, p, state);
            %s::add(&%s::SLOW_BYTES, (*p - __p0) as u64);
        }
        #[inline(always)]
        pub(crate) fn skip_space_slow_inner(
            input: &[u8],
            p: &mut usize,
            state: &mut ScanState,
        ) {''' % (P, P, P, P))
s = s + counters
open(p, "w").write(s)
print("instrumented")
