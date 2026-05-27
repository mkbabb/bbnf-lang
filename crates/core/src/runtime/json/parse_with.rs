use super::document::{JsonDocument, JsonPathQuery};
use crate::grammar::generated::json::{
    __path_plan, __shape_support_JsonParser, parse_JsonParser_value,
};
use crate::path::cursor::Decision;
use crate::path::executor::PathExecutor;
use crate::path::ir::{PathSegment as TypedSegment, TypedPath};
use crate::path::markers::Json;
use crate::runtime::path::{Path, PathSegment};
use crate::runtime::json::JsonStructBuilder;
fn lower<'a>(seg: &TypedSegment<'a>) -> Option<PathSegment<'a>> {
    match seg {
        TypedSegment::Field(s) => Some(PathSegment::Field(s)),
        TypedSegment::Index(i) => Some(PathSegment::Index(*i)),
        TypedSegment::VariantName(s) => Some(PathSegment::Field(s)),
        TypedSegment::Wildcard => None,
    }
}
pub fn parse_with<T>(input: &str, path: &TypedPath<Json, T>) -> Option<T>
where
    T: JsonPathQuery,
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
            let mut state = __shape_support_JsonParser::ScanState::new();
            let mut builder = JsonStructBuilder::new();
            let mut pos: usize = 0;
            parse_JsonParser_value(
                    src.as_bytes(),
                    &mut pos,
                    &mut state,
                    &mut builder,
                    cursor,
                )
                .ok()?;
            let doc: JsonDocument<'_> = builder.finalise(src);
            let mut segments: Vec<PathSegment<'_>> = Vec::with_capacity(path.len());
            for owned in path.owned_segments() {
                segments.push(lower(&owned.as_borrowed())?);
            }
            doc.get::<T>(Path::new(&segments))
        },
    )
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::path::ir::OwnedPathSegment;
    #[test]
    fn parse_with_resolves_string_leaf() {
        let path: TypedPath<Json, &str> = TypedPath::from_owned(
            vec![OwnedPathSegment::Field("title".to_owned())],
        );
        let out = parse_with::<&str>(r#"{"title":"hi"}"#, &path);
        assert_eq!(out, Some("hi"));
    }
    #[test]
    fn parse_with_resolves_number_leaf() {
        let path: TypedPath<Json, f64> = TypedPath::from_owned(
            vec![OwnedPathSegment::Field("count".to_owned())],
        );
        let out = parse_with::<f64>(r#"{"count":42}"#, &path);
        assert_eq!(out, Some(42.0));
    }
    #[test]
    fn parse_with_returns_none_on_missing_field() {
        let path: TypedPath<Json, &str> = TypedPath::from_owned(
            vec![OwnedPathSegment::Field("absent".to_owned())],
        );
        let out = parse_with::<&str>(r#"{"title":"hi"}"#, &path);
        assert!(out.is_none());
    }
}
