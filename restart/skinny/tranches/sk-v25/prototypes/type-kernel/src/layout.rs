use crate::TypeId;

/// Physical representation is deliberately downstream of semantic typing.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Layout {
    Scalar {
        bytes: u8,
        align: u8,
    },
    Pointer,
    Aggregate(Vec<Layout>),
    Tagged {
        tag: Box<Layout>,
        payloads: Vec<Layout>,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LayoutPlan {
    pub semantic_type: TypeId,
    pub layout: Layout,
}
