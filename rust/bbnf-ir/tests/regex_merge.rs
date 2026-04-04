use bbnf_ir::passes::regex::merge::pattern_has_top_level_pipe;

#[test]
fn top_level_pipe_detection() {
    assert!(pattern_has_top_level_pipe("a|b"));
    assert!(!pattern_has_top_level_pipe("(?:a|b)"));
    assert!(!pattern_has_top_level_pipe("[a|b]"));
    assert!(pattern_has_top_level_pipe("(?:a|b)|c"));
    assert!(!pattern_has_top_level_pipe(r"a\|b"));
}
