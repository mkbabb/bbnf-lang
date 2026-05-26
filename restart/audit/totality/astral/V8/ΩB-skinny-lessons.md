# Omega-B Skinny Lessons - Pass Omega V8 W5B-FRONTENDR

Date: 2026-05-26.
Scope: REDRESS-209, REDRESS-210, REDRESS-211, REDRESS-212, V5/V6/V7
Omega-B signoffs, W5B-FRONTEND redress/corrective packet, and
W5B-FRONTEND challenge V2.
Disposition: ACCEPT.

## Lesson

V8's skinny lesson is a cap-authority lesson, not a new implementation claim.
V7 correctly routed generic frontend/import/IR closure before the provider-free
runtime generator and before provider/template deletion. REDRESS-212 shows that
the routed target still cannot execute as one capped W5B-FRONTEND wave: the
accepted closure requires serial Lock 14 routing, import closure,
layout/discard lowering, pretty/span/projection lowering, and request-consumer
proof carry. Informal internal slices either overrun the dispatch hard cap or
turn the remaining frontend work into an unauthorised deferral while W5C-GEN is
still blocked.

## Longitudinal Lesson Table

| Omega pass | Evidence | Correction | Load-bearing lesson |
|---|---|---|---|
| V5 W5R | REDRESS-209 rejected monolithic W5 because provider/template deletion was demanded before a real source-consuming generator existed; static centralization only renamed the old provider mesh. | Split W5 into W5A request/source boundary before later deletion. | Provider deletion waits for rebuild capability. A moved or centralized static body is not a replacement generator. |
| V6 W5BR | REDRESS-210 rejected W5B because W5A admitted source at the request boundary, but runtime bytes still flowed through `render_runtime_profile`, `RuntimeProvider`, and provider/template modules. | Split W5B-GEN provider-free runtime generator body before W5C-DELETE. | Request source consumption is not runtime-byte generation. The generator body must exist before destructive deletion. |
| V7 W5B-GENR | REDRESS-211 rejected W5B-GEN because the source-fact scanner did not compile CSS L4 source into generic frontend/import/IR facts. | Route W5B-FRONTEND before W5C-GEN, with CSS L4 as the strict positive witness. | Frontend closure precedes generator body. A generator cannot consume grammar constructs the frontend cannot parse, import-resolve, lower, and carry into IR. |
| V8 W5B-FRONTENDR | REDRESS-212 rejected W5B-FRONTEND because the valid frontend target exceeded the one-wave cap and V2 challenge found SPEC authority and maintain-gate conflicts. | Formalize capped sub-waves W5B.0 through W5B.4; keep W5C-GEN blocked until aggregate W5B-FRONTEND close. | Sub-wave cap authority is load-bearing. Internal slices do not satisfy the hard cap unless SPEC/Omega grants formal wave boundaries and aggregate close semantics. |

## Corrected Chain

The corrected PRUNE-3B chain is:

1. W5A: admitted request boundary.
2. W5B.0 LOCK14-GATE: owner-path roster, parent-diff routing,
   modified-provider/template rejection tests, all-template guard, and generic
   owner-path leak census; no frontend source edits.
3. W5B.1 IMPORT-CLOSURE: request-local import graph resolution, stable source
   hashing, missing-import rejection, and import-cycle rejection.
4. W5B.2 LAYOUT-DISCARD: request-local lowering for `@ws`, `?w`, `>>`, and
   `<<`, with public syntax still retired.
5. W5B.3 PRETTY-SPAN-PROJECTION: request-local lowering for `@pretty`,
   `@{...}` span capture, projection metadata, and typed projections.
6. W5B.4 REQUEST-CONSUMER: `emit_runtime_from_request` consumes the frontend
   closure, carries JSON/Sheets/BBNF proof, runs `regen-css` plus the seven CSS
   companions, and resolves the W5B maintain gate by SPEC-authorised evidence.
7. W5C-GEN: provider-free runtime generator body consuming request/frontend IR.
8. W5D-DELETE: provider/template deletion and Lock 14 baseline close.
9. W6: root-runtime collapse only after W5D-DELETE.

W5B-FRONTEND closes only after W5B.0 through W5B.4 admit. W5C-GEN is blocked
until aggregate W5B-FRONTEND close. W5D-DELETE is blocked until W5C-GEN admits.
W6, W7, and W8/W9/W10 remain blocked until the amended PRUNE chain closes.

## Next-Dispatch Lesson

W5B.0 LOCK14-GATE is the next dispatch. Its lesson is that frontend closure
begins by constraining authority, not by touching frontend/codegen/xtask source
paths. W5B.0 may prove owner routing, all-template guards, modified
provider/template rejection, W5C/W5D subject rejection, and generic owner-path
leak census. It must not claim W5B-FRONTEND closure, must not unblock W5C-GEN,
and must only unlock W5B.1 IMPORT-CLOSURE after its own capped gate admits.
