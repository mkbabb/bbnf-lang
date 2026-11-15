# Mutant M2 (the ORC2-WS-C row's mutant): the skip before every Ref call restored in emit_ref_call_shape.
import sys
p=sys.argv[1]; s=open(p).read()
a="    let expr = if super::super::ws_read::root_reads_ow(&target.body) {"
assert s.count(a)==1
s=s.replace(a,"    let expr = if true {")
b="    let (pre, post) = if super::super::ws_read::root_reads_ow(&target.body) {"
assert s.count(b)==1
s=s.replace(b,"    let __ow = super::super::ws_read::root_reads_ow(&target.body);\n    let (pre, post) = if true {")
# keep the post-skip only for a root ?w target
s=s.replace("                if __r.is_ok() {\n                    let _ = #support_mod::skip_space(input, p, state);\n                }","                if __r.is_ok() && #__ow {\n                    let _ = #support_mod::skip_space(input, p, state);\n                }",1)
open(p,'w').write(s)
