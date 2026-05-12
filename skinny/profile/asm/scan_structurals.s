	.globl	runtime::tape::scan_structurals
	.p2align	2
runtime::tape::scan_structurals:
Lfunc_begin46:
	.cfi_startproc
	b simd_scan::scan_json_structurals
