use super::document::{CssDocument, CssPathQuery};
use crate::grammar::generated::css_l4::{
    __path_plan, __shape_support_CssL4Parser, parse_CssL4Parser_stylesheet,
};
use crate::path::cursor::Decision;
use crate::path::executor::PathExecutor;
use crate::path::ir::{PathSegment as TypedSegment, TypedPath};
use crate::path::markers::CssL4;
use crate::runtime::css_l4::CssStructBuilder;
use crate::runtime::path::{Path as LegacyPath, PathSegment as LegacySegment};
fn lower<'a>(seg: &TypedSegment<'a>) -> Option<LegacySegment<'a>> {
    match seg {
        TypedSegment::Field(s) => Some(LegacySegment::Field(s)),
        TypedSegment::Index(i) => Some(LegacySegment::Index(*i)),
        TypedSegment::VariantName(s) => Some(LegacySegment::Field(s)),
        TypedSegment::Wildcard => None,
    }
}
pub fn parse_with<T>(input: &str, path: &TypedPath<CssL4, T>) -> Option<T>
where
    T: CssPathQuery,
{
    PathExecutor::execute(
        input,
        path,
        |rule_id, kind, _idx| {
            __path_plan::lookup(rule_id, kind)
                .map(|e| e.decision)
                .unwrap_or(Decision::ParseFully)
        },
        |src, cursor| {
            let mut state = __shape_support_CssL4Parser::ScanState::new();
            let mut builder = CssStructBuilder::new();
            let mut pos: usize = 0;
            parse_CssL4Parser_stylesheet(
                    src.as_bytes(),
                    &mut pos,
                    &mut state,
                    &mut builder,
                    cursor,
                )
                .ok()?;
            let doc: CssDocument<'_> = builder.finalise(src);
            let mut legacy: Vec<LegacySegment<'_>> = Vec::with_capacity(path.len());
            for owned in path.owned_segments() {
                legacy.push(lower(&owned.as_borrowed())?);
            }
            doc.get::<T>(LegacyPath::new(&legacy))
        },
    )
}
