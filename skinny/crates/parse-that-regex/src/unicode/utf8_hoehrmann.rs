use super::utf8_block::ValidateStatus;

#[inline]
pub fn validate_block(input: &[u8; 16]) -> ValidateStatus {
    let mut cursor = 0usize;
    while cursor < input.len() {
        let first = input[cursor];
        if first < 0x80 {
            cursor += 1;
            continue;
        }
        let Some(width) = width(first) else {
            return invalid(cursor);
        };
        if cursor + width > input.len() {
            return ValidateStatus {
                is_valid: true,
                complete_bytes: cursor as u8,
                bad_byte_offset: None,
                continues: true,
            };
        }
        if valid(input, cursor, width) {
            cursor += width;
        } else {
            return invalid(cursor);
        }
    }
    ValidateStatus {
        is_valid: true,
        complete_bytes: 16,
        bad_byte_offset: None,
        continues: false,
    }
}

#[inline]
fn invalid(cursor: usize) -> ValidateStatus {
    ValidateStatus {
        is_valid: false,
        complete_bytes: cursor as u8,
        bad_byte_offset: Some(cursor as u8),
        continues: false,
    }
}

#[inline]
fn width(first: u8) -> Option<usize> {
    match first {
        0xc2..=0xdf => Some(2),
        0xe0..=0xef => Some(3),
        0xf0..=0xf4 => Some(4),
        _ => None,
    }
}

#[inline]
fn valid(bytes: &[u8; 16], cursor: usize, width: usize) -> bool {
    let first = bytes[cursor];
    match width {
        2 => cont(bytes[cursor + 1]),
        3 => match first {
            0xe0 => (0xa0..=0xbf).contains(&bytes[cursor + 1]) && cont(bytes[cursor + 2]),
            0xed => (0x80..=0x9f).contains(&bytes[cursor + 1]) && cont(bytes[cursor + 2]),
            _ => cont(bytes[cursor + 1]) && cont(bytes[cursor + 2]),
        },
        4 => match first {
            0xf0 => {
                (0x90..=0xbf).contains(&bytes[cursor + 1])
                    && cont(bytes[cursor + 2])
                    && cont(bytes[cursor + 3])
            }
            0xf4 => {
                (0x80..=0x8f).contains(&bytes[cursor + 1])
                    && cont(bytes[cursor + 2])
                    && cont(bytes[cursor + 3])
            }
            _ => cont(bytes[cursor + 1]) && cont(bytes[cursor + 2]) && cont(bytes[cursor + 3]),
        },
        _ => false,
    }
}

#[inline]
fn cont(byte: u8) -> bool {
    (0x80..=0xbf).contains(&byte)
}
