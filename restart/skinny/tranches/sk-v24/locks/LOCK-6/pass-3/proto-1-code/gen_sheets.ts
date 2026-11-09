// --- rule kinds (flag tag table) ---
const K_NUMBER = 1;
const K_STRING = 2;
const K_BOOLEAN = 3;
const K_ERROR_LITERAL = 4;
const K_SHEET_PREFIX = 5;
const K_CELL_REF = 6;
const K_RANGE_END = 7;
const K_CELL = 8;
const K_RANGE_REF = 9;
const K_CELL_OR_RANGE = 10;
const K_IDENTIFIER = 11;
const K_COMPARE_OP = 12;
const K_COMPARISON_EXPR = 13;
const K_CONCAT_EXPR = 14;
const K_ADD_OP = 15;
const K_ADD_EXPR = 16;
const K_MUL_OP = 17;
const K_MUL_EXPR = 18;
const K_EXP_EXPR = 19;
const K_UNARY_PREFIX = 20;
const K_UNARY_EXPR = 21;
const K_POSTFIX_EXPR = 22;
const K_PRIMARY = 23;
const K_PAREN_EXPR = 24;
const K_FUNC_OPEN = 25;
const K_ARG = 26;
const K_COMMA = 27;
const K_FUNC_ARGS = 28;
const K_FUNC_CALL = 29;
const K_LET_BINDING = 30;
const K_LET_ARGS = 31;
const K_LET_CALL = 32;
const K_LAMBDA_PARAMS = 33;
const K_LAMBDA_CALL = 34;
const K_ARRAY_ROW = 35;
const K_ARRAY_ROWS = 36;
const K_ARRAY_LITERAL = 37;
const K_EXPRESSION = 38;
const K_FORMULA = 39;
const K_NAMED_RANGE = 40;

// kind := KIND_TABLE[flags_at(cursor) - 1]
classify(cursor) {
  const tag = this.flagAt(cursor);
  if (tag === 0) return 0;
  return KIND_TABLE[tag - 1];
}
