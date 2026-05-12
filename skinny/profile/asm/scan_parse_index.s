	.globl	runtime::tape::scan_parse_index
	.p2align	2
runtime::tape::scan_parse_index:
Lfunc_begin45:
	.cfi_startproc
	b simd_scan::scan_json_parse_index
