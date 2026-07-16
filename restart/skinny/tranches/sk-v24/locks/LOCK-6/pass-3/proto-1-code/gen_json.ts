// --- node kinds (first source byte) ---
const K_OBJ_OPEN = 1;
const K_OBJ_CLOSE = 2;
const K_ARR_OPEN = 3;
const K_ARR_CLOSE = 4;
const K_STRING = 6;
const K_NUMBER = 7;
const K_TRUE = 8;
const K_FALSE = 9;
const K_NULL = 10;

const STRING_NEEDS_DECODE = 0x01; // OffsetFlags::GRAMMAR_BIT0

  // kind := classify(source[offsets[cursor]])  (JsonNodeKind::at_cursor)
  classify(cursor) {
    const b = this.source[this.offsets[cursor]];
    switch (b) {
      case 0x7b: return K_OBJ_OPEN;
      case 0x7d: return K_OBJ_CLOSE;
      case 0x5b: return K_ARR_OPEN;
      case 0x5d: return K_ARR_CLOSE;
      case 0x22: return K_STRING;
      case 0x74: return K_TRUE;
      case 0x66: return K_FALSE;
      case 0x6e: return K_NULL;
      default: return K_NUMBER; // '-' | '0'..'9'
    }
  }
