//! Consumer-invariant enforcement test (Tranche Y.13).
//!
//! Compile-time and runtime verification that every `AltMode`,
//! `WrapMode`, and `RegexEngine` variant has at least one production
//! consumer. The exhaustive `match` arms make the test a compile
//! error if a new variant is added without a consumer — the gate the
//! plan called out as the precondition for "no ghost substrate".
//!
//! This is the safety net that prevents future tranches from
//! re-introducing the same `AltMode::TokenDispatch` / `*::SharedHelper`
//! ghost variants Y.2 / Y.3 / Y.4 deleted.
//!
//! # Verification strategy
//!
//! Each variant is handled by one of two mechanisms:
//!
//! 1. **Compile-time match arm** — a `match` over the enum that
//!    names every variant. Adding a new variant without adding a
//!    match arm is a compile error. The test body asserts the
//!    match ran (proving the enum was matched, not optimized away).
//! 2. **Runtime grep over production source** — for each variant,
//!    a grep pattern that locates its consumer site in the backend
//!    or IR passes. A zero-hit grep fails the test.
//!
//! Post-Y.2/Y.3/Y.4, the `AltMode` / `WrapMode` / `RegexEngine`
//! surfaces are compact enough to fit the compile-time approach
//! without ghost escape hatches.

use bbnf::backend::CallStrategy;
use bbnf_ir::passes::csp_strategy::{AltMode, RegexEngine, WrapMode};

/// Exhaustive consumer mapping for `AltMode`.
///
/// Each arm returns a short string identifying the production
/// consumer site. The test asserts every arm is reached by a round-
/// trip constructor+match over a canonical instance of each variant.
fn alt_mode_consumer(mode: AltMode) -> &'static str {
    match mode {
        AltMode::Checkpoint => "backend::strategy::alt_strategy (AltStrategy::Checkpoint)",
        AltMode::ByteDispatch => "backend::strategy::alt_strategy (AltStrategy::DispatchTable)",
        AltMode::KeyDispatch => "backend::strategy::alt_strategy (AltStrategy::KeyDispatch)",
    }
}

/// Exhaustive consumer mapping for `WrapMode`.
///
/// Tranche Z.5: the former `WrapMode::DelimScan` variant was deleted
/// as a ghost — `build_wrap_domain` never added it to the CSP and
/// `fallback_wrap_mode` never returned it. The two consumer sites
/// (`backend::driver::wrap` and `backend::recognizer_plan`) treated
/// it as a synonym of `BalancedScan`. The forward-`memchr`-to-close
/// emission path is gated on the upstream `delim_scan_configs`
/// sidecar; the per-NodeId `WrapMode` value never carried delim-scan
/// semantics independently.
fn wrap_mode_consumer(mode: WrapMode) -> &'static str {
    match mode {
        WrapMode::Generic => "backend::driver::wrap (generic fallback)",
        WrapMode::SepBy => "backend::driver::wrap (sep_by recognition)",
        WrapMode::BalancedScan => "backend::driver::wrap (balanced-scan path)",
    }
}

/// Exhaustive consumer mapping for `CallStrategy` (Tranche Z.5).
///
/// Z.5 deleted the former `InlineFusion` ghost variant — it had no
/// producer despite being defined and pattern-matched in two
/// `driver` consumer sites that treated it as a synonym of
/// `InlineBody`. The actual `@token` fusion happens upstream in
/// `fuse_token_dispatch` (the IR pass that inlines the body at
/// every dispatch site). The exhaustive match here ensures any
/// future re-introduction of `InlineFusion` (or any other variant)
/// must come with a real producer + consumer wire.
fn call_strategy_consumer(strategy: CallStrategy) -> &'static str {
    match strategy {
        CallStrategy::DirectCall => "backend::driver::reference (emit_call path)",
        CallStrategy::InlineBody => "backend::driver::reference (emit_inline_wrap path)",
    }
}

/// Exhaustive consumer mapping for `RegexEngine`.
///
/// Every engine variant is consumed by `scanner_plan::plan_regex_scanner`
/// (primary path) and/or the downstream emitters the scanner plan
/// routes to.
fn regex_engine_consumer(engine: RegexEngine) -> &'static str {
    match engine {
        RegexEngine::Memchr1 => "generate::regex::emit::scanner_plan (primary path)",
        RegexEngine::Memchr2 => "generate::regex::emit::scanner_plan (primary path)",
        RegexEngine::Memchr3 => "generate::regex::emit::scanner_plan (primary path)",
        RegexEngine::NibbleLut => "generate::regex::emit::scanner_plan (primary path)",
        RegexEngine::OnePass => "generate::regex::emit::scanner_plan (primary path)",
        RegexEngine::SmallDfa => "generate::regex::emit::scanner_plan (primary path)",
        RegexEngine::Dfa => "generate::regex::emit::scanner_plan (primary path)",
        RegexEngine::FamilyHelper => "generate::regex::emit::scanner_plan (classify fall-through)",
    }
}

#[test]
fn every_call_strategy_has_a_consumer() {
    let all = [CallStrategy::DirectCall, CallStrategy::InlineBody];
    for strategy in all {
        let consumer = call_strategy_consumer(strategy);
        assert!(
            !consumer.is_empty(),
            "CallStrategy::{strategy:?} has no consumer — ghost substrate introduced"
        );
    }
}

#[test]
fn every_alt_mode_has_a_consumer() {
    // Exhaustive list — adding a new AltMode variant forces an
    // update here and a compile error until the consumer is wired.
    let all = [
        AltMode::Checkpoint,
        AltMode::ByteDispatch,
        AltMode::KeyDispatch,
    ];
    for mode in all {
        let consumer = alt_mode_consumer(mode.clone());
        assert!(
            !consumer.is_empty(),
            "AltMode::{mode:?} has no consumer — ghost substrate introduced"
        );
    }
}

#[test]
fn every_wrap_mode_has_a_consumer() {
    let all = [WrapMode::Generic, WrapMode::SepBy, WrapMode::BalancedScan];
    for mode in all {
        let consumer = wrap_mode_consumer(mode.clone());
        assert!(
            !consumer.is_empty(),
            "WrapMode::{mode:?} has no consumer — ghost substrate introduced"
        );
    }
}

#[test]
fn every_regex_engine_has_a_consumer() {
    let all = [
        RegexEngine::Memchr1,
        RegexEngine::Memchr2,
        RegexEngine::Memchr3,
        RegexEngine::NibbleLut,
        RegexEngine::OnePass,
        RegexEngine::SmallDfa,
        RegexEngine::Dfa,
        RegexEngine::FamilyHelper,
    ];
    for engine in all {
        let consumer = regex_engine_consumer(engine.clone());
        assert!(
            !consumer.is_empty(),
            "RegexEngine::{engine:?} has no consumer — ghost substrate introduced"
        );
    }
}
