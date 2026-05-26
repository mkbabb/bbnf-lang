# Pass Omega V6 CH5 Hidden Coupling

Disposition: ACCEPT.

The hidden coupling is the provider mesh inside the W5A request path:
`grammar_provider.rs` delegates to `render_runtime_profile`, which delegates to
`RuntimeProvider` and provider modules. V6 exposes that coupling and forbids
deletion until the coupling is removed by W5B-GEN.
