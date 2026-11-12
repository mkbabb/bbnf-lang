# W3 BBNF born-RED hostile reviews

Terminal disposition: `ACCEPT`.

The accepted subjects are:

- instance/frontend contract `d20ef0a176758305e2e3adc1f63f34229df3087c7248d5570a4dbe7e99f60c8f`,
  9,182 lines;
- replay/public contract `ce60dbcf8b26e2934644136e46046c4ac0e34e916c02a7d98906faa6f03f0239`,
  9,897 lines;
- semantic/Pratt contract `8c6f7018a8b19f72fb7a8de599efe0ba8b4616e42c37a7ec49d7bdc93947a6ae`,
  12,481 lines.

Each final subject received read-only review by an agent that did not author
that subject. No P0, P1, or P2 finding remains on the accepted hashes.

## Rejected subjects and repairs

Rejections remain evidence. They are not authority for implementation.

### Instance/frontend

1. `507f8dc62893431a5c00ead42daf4a13d9c398da1386581a3754bbb74b1644b1`
   was rejected because qualified/UFCS nominal producers and compound or
   unlisted post-attachment mutations escaped.
2. `4b18052d2e25c2526fe708410a8fbee09c59f3c99ac13dc3128ab4b6c392cf06`
   was rejected because a pre-loop raw-pointer alias could mutate the returned
   Program and a trait associated output could erase nominal provenance.
3. `3937bd5b827ad59d837cbf79a62fc88a347c8124d084586257f8b9414d79820c`
   was rejected because `addr_of_mut!` hid carrier provenance in macro tokens
   and method-name-only associated-output promotion contaminated sibling
   implementations.
4. The accepted subject tokenizes macro arguments during fixed-point carrier
   propagation and resolves associated producers by exact receiver, trait, and
   method identity. Same-catalog nominal and `String` controls distinguish the
   two cases.

Independent terminal reviewer:
`/root/w3_semantic_accept_final`, `ACCEPT`, P0/P1/P2 = 0/0/0.

### Replay/public

1. `db406c1913421b8c40732548b929d7fbed1f028787e84e9ecf8b97e042168ce4`
   was rejected because root-self imports, cross-module `stringify` shadowing,
   and imports inside macro transcribers evaded provenance.
2. `5301ae409fe3bc9fc526d55e849f90bfadfc9d21053697ca6dcdb7580820710e`
   closed those paths but was rejected because its TOML identity mutation was
   a no-op and frozen-basis Git reads honored replacement objects.
3. The accepted subject derives macro names from `ItemMacro.ident`, aggregates
   macro provenance over the active module graph, rejects transcriber-local
   imports, proves a material one-occurrence TOML mutation, and routes every
   frozen-basis Git object read through `GIT_NO_REPLACE_OBJECTS=1`. A real
   replacement-tree probe proves that the unsealed reader exposes injected
   candidate bytes while the shared immutable reader does not.

Independent terminal reviewer:
`/root/w3_replay_cross_accept`, `ACCEPT`, P0/P1/P2 = 0/0/0.

### Semantic/Pratt

1. `01d0715d4ed516e718b011d596c4e08bd424d5894de96497b52cd26354f7601e`
   was rejected because recursive Pratt forwarding was not exact and Limits
   and scalar-event ownership were not global.
2. `cb32162c1fafcbf92913656ecc266caddedce628defe5bc7209f27458a897fc6`
   was rejected because type aliases disguised `Limits` and `ScalarEvent`
   struct construction.
3. `85f03f57b28b6a671deda31b6bf6897920e208824536b4b09581e0287b729bc0`
   was rejected because renamed imports retained the same disguise and the
   surface falsely rejected the required `Vec::new`.
4. `2b787738bc5b22e58c4cbff329e6e9ff44e4ff42c2de7e5d2658c42622ab3393`
   was rejected because an extern-crate rename disguised `ScalarEvent` and
   generic path arguments evaded the exact `Vec::new()` law.
5. The accepted subject requires exact recursive three-arm Step forwarding,
   global Limits/event/identity/ScalarEvent ownership, the exact
   `EventSink::Event = ScalarEvent` associated type, no generated type or
   import aliases, no extern-crate rename, and one argument-free `Vec::new()`
   owned by `EventBuffer::new`.

Independent terminal reviewer:
`/root/w3_replay_cross_accept`, `ACCEPT`, P0/P1/P2 = 0/0/0.

## Root union replay

The root independently reproduced:

- instance RED: 39 = 2 E0432 + 23 E0599 + 14 E0609;
- replay RED: 4 = 1 E0432 + 3 E0599;
- semantic RED: 57 = 2 E0432 + 55 E0599;
- incumbent BBNF floor: 57 = 13 library + 44 integration tests;
- external controls: 8 generated + 5 recursive + 4 record tests;
- formatting, schema JSON, locked/offline metadata, and diff checks;
- exact accepted hashes and line counts;
- absence of every postselection artifact and selected instance.

The parse-that worktree remained clean at
`ed1ffa24e862b797b01dc4f9215d6be8f7fa0955`; its ABI-3 receipt remained
`eb8678899905d725926ebf892a8d60874aa9343c60cd7bfd24dac33b9ae92e55`,
and its exact packages remained
`51933a1d4796f7628995fc433b0041a98973ebb5a8cd5eb4e596335a4904bc24`
for `parse_that`, and
`8399e72b99d10ad96ad7f7499577dd0e84f7e360fbdb2d800b32115ab03b45b0`
for `bbnf-regex`.

## Bound interpretation

These contracts are pre-production executable laws, not a general proof that
arbitrary Rust source can be classified perfectly. Hostile review was bounded
after every named product-semantic P0/P1/P2 finding was repaired and the
accepted hashes passed the full ordinary gate set. Further source-scanner
maximalism is not authorized to defer the clean-break implementation.

No production W3 source, held-out selection, or candidate materialization
occurred before this terminal acceptance.
