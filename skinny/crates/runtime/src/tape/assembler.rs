use super::{checked_u32, PayloadArena, Tape};

pub struct TapeAssembler<'input> {
    source: &'input [u8],
    offsets: Vec<u32>,
    payloads: PayloadArena,
}

impl<'input> TapeAssembler<'input> {
    pub fn new(source: &'input [u8], structural_capacity: usize) -> Self {
        Self {
            source,
            offsets: Vec::with_capacity(structural_capacity),
            payloads: PayloadArena::empty(),
        }
    }

    #[inline]
    pub fn push_offset(&mut self, offset: usize) -> u32 {
        let cursor = checked_u32(self.offsets.len());
        self.offsets.push(checked_u32(offset));
        cursor
    }

    pub fn finish(
        self,
        string_escape_offsets: Vec<u32>,
        string_control_offsets: Vec<u32>,
    ) -> Tape<'input> {
        Tape::from_offsets(
            self.source,
            self.offsets,
            string_escape_offsets,
            string_control_offsets,
            self.payloads,
        )
    }
}
