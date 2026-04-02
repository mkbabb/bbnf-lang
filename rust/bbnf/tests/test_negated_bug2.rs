// Test using the regex/classify.rs directly (the one used by emit)
use bbnf::generate::regex::classify::{classify_regex, RegexClass};

#[test]
fn test_negated_class_from_regex_classify() {
    use regex_syntax::hir::{Class, HirKind};
    
    let pattern = "[^{};]";
    let hir = regex_syntax::ParserBuilder::new()
        .utf8(false)
        .unicode(false)
        .build()
        .parse(pattern)
        .unwrap();

    println!("\nDetailed analysis of {}", pattern);
    if let HirKind::Class(Class::Bytes(bc)) = hir.kind() {
        let ranges = bc.ranges();
        println!("Total ranges: {}", ranges.len());
        
        for (i, r) in ranges.iter().enumerate() {
            if i < 5 || i >= ranges.len().saturating_sub(5) {
                println!("  [{}] 0x{:02X}-0x{:02X}", i, r.start(), r.end());
            } else if i == 5 {
                println!("  ... ({} more ranges) ...", ranges.len() - 10);
            }
        }
        
        let total_bytes: usize = ranges.iter().map(|r| (r.end() - r.start() + 1) as usize).sum();
        println!("Total bytes covered: {}", total_bytes);
        
        let has_lower = ranges.iter().any(|r| r.start() <= b'a' && r.end() >= b'z');
        let has_upper = ranges.iter().any(|r| r.start() <= b'A' && r.end() >= b'Z');
        println!("Has a-z: {}, Has A-Z: {}", has_lower, has_upper);
    }

    let result = classify_regex(pattern);
    println!("Classification result: {:?}", result);
}
