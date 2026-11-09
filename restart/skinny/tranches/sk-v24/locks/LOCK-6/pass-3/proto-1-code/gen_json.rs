#[derive(Copy, Clone, Eq, PartialEq, Debug)]
#[repr(u16)]
pub enum JsonNodeKind {
    Root = 0,
    ObjectOpen = 1,
    ObjectClose = 2,
    ArrayOpen = 3,
    ArrayClose = 4,
    Pair = 5,
    String = 6,
    Number = 7,
    True = 8,
    False = 9,
    Null = 10,
    ElementSeparator = 11,
    MemberSeparator = 12,
}

impl JsonNodeKind {
    pub fn at_cursor(tape: &crate::tape::Tape<'_>, cursor: u32) -> Self {
        let offset = tape
            .offset_at(cursor)
            .expect("JSON cursor must point into sealed offset tape");
        match tape.source()[offset] {
            b'{' => Self::ObjectOpen,
            b'}' => Self::ObjectClose,
            b'[' => Self::ArrayOpen,
            b']' => Self::ArrayClose,
            b',' => Self::ElementSeparator,
            b':' => Self::MemberSeparator,
            b'"' => Self::String,
            b'-' | b'0'..=b'9' => Self::Number,
            b't' => Self::True,
            b'f' => Self::False,
            b'n' => Self::Null,
            _ => unreachable!("JSON verifier emits only JSON tape offsets"),
        }
    }
}
