"use strict";
Object.defineProperty(exports, Symbol.toStringTag, { value: "Module" });
const packrat = require("./packrat-entry-46NYx4_U.cjs");
const diagnostics = require("./diagnostics-DpUY87_d.cjs");
const core = require("./core.cjs");
const utils = require("./utils.cjs");
exports.Parser = packrat.Parser;
exports.ParserState = packrat.ParserState;
exports.all = packrat.all;
exports.any = packrat.any;
exports.createLazyCached = packrat.createLazyCached;
exports.createParserContext = packrat.createParserContext;
exports.dispatch = packrat.dispatch;
exports.eof = packrat.eof;
exports.getLazyParser = packrat.getLazyParser;
exports.lazy = packrat.lazy;
exports.memoize = packrat.memoize;
exports.mergeMemos = packrat.mergeMemos;
exports.mergeSpans = packrat.mergeSpans;
exports.regex = packrat.regex;
exports.resetPackrat = packrat.resetPackrat;
exports.spanToString = packrat.spanToString;
exports.string = packrat.string;
exports.trimStateWhitespace = packrat.trimStateWhitespace;
Object.defineProperty(exports, "whitespace", {
  enumerable: true,
  get: () => packrat.whitespace
});
exports.clearCollectedDiagnostics = diagnostics.clearCollectedDiagnostics;
exports.collectDiagnostic = diagnostics.collectDiagnostic;
exports.disableDiagnostics = diagnostics.disableDiagnostics;
exports.enableDiagnostics = diagnostics.enableDiagnostics;
exports.getCollectedDiagnostics = diagnostics.getCollectedDiagnostics;
exports.mergeErrorState = diagnostics.mergeErrorState;
exports.skipBlockComments = diagnostics.skipBlockComments;
exports.skipWhitespace = diagnostics.skipWhitespace;
exports.containsDelimiter = core.containsDelimiter;
exports.splitBalanced = core.splitBalanced;
exports.csvParser = utils.csvParser;
exports.escapedString = utils.escapedString;
exports.jsonParser = utils.jsonParser;
exports.numberParser = utils.numberParser;
exports.quotedString = utils.quotedString;
//# sourceMappingURL=parse.cjs.map
