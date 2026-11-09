427483: __RNvNtNtNtCshXrMbbbkDC1_10parse_that7parsers4scan10ws_comment19scan_ws_bitmap_cold:
427484: 	.cfi_startproc
427485: 	ldr	x8, [x0, #16]
427486: 	cmp	x1, x8
427487: 	b.hs	LBB286_68
427488: 	ldr	x9, [x0]
427489: 	add	x10, x9, x8
427490: 	sub	x11, x10, #64
427491: 	sub	x12, x8, #64
427492: 	sub	x13, x10, #16
427493: 	add	x14, x9, #32
427494: 	movi.16b	v0, #15
427495: Lloh6496:
427496: 	adrp	x15, lCPI286_0@PAGE
427497: Lloh6497:
427498: 	ldr	q1, [x15, lCPI286_0@PAGEOFF]
427499: Lloh6498:
427500: 	adrp	x15, lCPI286_1@PAGE
427501: Lloh6499:
427502: 	ldr	q2, [x15, lCPI286_1@PAGEOFF]
427503: Lloh6500:
427504: 	adrp	x15, lCPI286_2@PAGE
427505: Lloh6501:
427506: 	ldr	q3, [x15, lCPI286_2@PAGEOFF]
427507: 	movi.16b	v4, #47
427508: 	movi.16b	v5, #42
427509: 	mov	w15, #16
427510: 	mov	x16, #-1
427511: LBB286_2:
427512: 	add	x17, x9, x1
427513: 	ldp	q6, q7, [x17]
427514: 	and.16b	v16, v6, v0
427515: 	ushr.16b	v17, v6, #4
427516: 	tbl.16b	v16, { v1 }, v16
427517: 	tbl.16b	v17, { v2 }, v17
427518: 	and.16b	v16, v17, v16
427519: 	cmeq.16b	v16, v16, #0
427520: 	bic.16b	v16, v3, v16
427521: 	ext.16b	v17, v16, v16, #8
427522: 	zip1.16b	v16, v16, v17
427523: 	addv.8h	h16, v16
427524: 	fmov	w0, s16
427525: 	and.16b	v16, v7, v0
427526: 	ushr.16b	v17, v7, #4
427527: 	tbl.16b	v16, { v1 }, v16
427528: 	tbl.16b	v17, { v2 }, v17
427529: 	and.16b	v16, v17, v16
427530: 	cmeq.16b	v16, v16, #0
427531: 	bic.16b	v16, v3, v16
427532: 	ext.16b	v17, v16, v16, #8
427533: 	zip1.16b	v16, v16, v17
427534: 	addv.8h	h16, v16
427535: 	fmov	w2, s16
427536: 	lsl	w2, w2, #16
427537: 	bfxil	x2, x0, #0, #16
427538: 	ldp	q16, q17, [x17, #32]
427539: 	and.16b	v18, v16, v0
427540: 	ushr.16b	v19, v16, #4
427541: 	tbl.16b	v18, { v1 }, v18
427542: 	tbl.16b	v19, { v2 }, v19
427543: 	and.16b	v18, v19, v18
427544: 	cmeq.16b	v18, v18, #0
427545: 	bic.16b	v18, v3, v18
427546: 	ext.16b	v19, v18, v18, #8
427547: 	zip1.16b	v18, v18, v19
427548: 	addv.8h	h18, v18
427549: 	fmov	w17, s18
427550: 	orr	x17, x2, x17, lsl #32
427551: 	and.16b	v18, v17, v0
427552: 	ushr.16b	v19, v17, #4
427553: 	tbl.16b	v18, { v1 }, v18
427554: 	tbl.16b	v19, { v2 }, v19
427555: 	and.16b	v18, v19, v18
427556: 	cmeq.16b	v18, v18, #0
427557: 	bic.16b	v18, v3, v18
427558: 	ext.16b	v19, v18, v18, #8
427559: 	zip1.16b	v18, v18, v19
427560: 	addv.8h	h18, v18
427561: 	fmov	w0, s18
427562: 	orr	x17, x17, x0, lsl #48
427563: 	cmeq.16b	v6, v6, v4
427564: 	and.16b	v6, v6, v3
427565: 	ext.16b	v18, v6, v6, #8
427566: 	zip1.16b	v6, v6, v18
427567: 	addv.8h	h6, v6
427568: 	fmov	w0, s6
427569: 	cmeq.16b	v6, v7, v4
427570: 	and.16b	v6, v6, v3
427571: 	ext.16b	v7, v6, v6, #8
427572: 	zip1.16b	v6, v6, v7
427573: 	addv.8h	h6, v6
427574: 	fmov	w2, s6
427575: 	lsl	w2, w2, #16
427576: 	bfxil	x2, x0, #0, #16
427577: 	cmeq.16b	v6, v16, v4
427578: 	and.16b	v6, v6, v3
427579: 	ext.16b	v7, v6, v6, #8
427580: 	zip1.16b	v6, v6, v7
427581: 	addv.8h	h6, v6
427582: 	fmov	w0, s6
427583: 	orr	x0, x2, x0, lsl #32
427584: 	cmeq.16b	v6, v17, v4
427585: 	and.16b	v6, v6, v3
427586: 	ext.16b	v7, v6, v6, #8
427587: 	zip1.16b	v6, v6, v7
427588: 	addv.8h	h6, v6
427589: 	fmov	w2, s6
427590: 	orr	x0, x0, x2, lsl #48
427591: 	cmn	x17, #1
427592: 	b.eq	LBB286_34
427593: 	mvn	x17, x17
427594: 	rbit	x17, x17
427595: 	clz	x17, x17
427596: 	lsl	x2, x16, x17
427597: 	bics	x0, x0, x2
427598: 	b.eq	LBB286_69
427599: 	rbit	x17, x0
427600: 	clz	x17, x17
427601: 	add	x0, x17, x1
427602: 	cmp	x0, x8
427603: 	b.hs	LBB286_68
427604: 	add	x17, x0, #1
427605: 	cmp	x17, x8
427606: 	b.hs	LBB286_70
427607: 	ldrb	w17, [x9, x17]
427608: 	cmp	w17, #42
427609: 	b.ne	LBB286_70
427610: 	add	x17, x0, #2
427611: 	cmp	x17, x8
427612: 	b.hs	LBB286_68
427613: LBB286_8:
427614: 	add	x0, x9, x17
427615: 	sub	x2, x8, x17
427616: 	cmp	x2, #15
427617: 	b.hi	LBB286_12
427618: 	add	x1, x9, x17
427619: LBB286_10:
427620: 	ldrb	w3, [x1]
427621: 	cmp	w3, #42
427622: 	b.eq	LBB286_26
427623: 	add	x1, x1, #1
427624: 	subs	x2, x2, #1
427625: 	b.ne	LBB286_10
427626: 	b	LBB286_68
427627: LBB286_12:
427628: 	ldr	q6, [x0]
427629: 	cmeq.16b	v6, v6, v5
427630: 	shrn.8b	v6, v6, #4
427631: 	fmov	x1, d6
427632: 	ands	x1, x1, #0x8888888888888888
427633: 	b.eq	LBB286_14
427634: 	rbit	x1, x1
427635: 	clz	x1, x1
427636: 	add	x1, x0, x1, lsr #2
427637: 	b	LBB286_26
427638: LBB286_14:
427639: 	and	x3, x0, #0xf
427640: 	sub	x4, x15, x3
427641: 	add	x1, x0, x4
427642: 	cmp	x2, #64
427643: 	b.lo	LBB286_20
427644: 	add	x2, x4, x17
427645: 	cmp	x2, x12
427646: 	b.gt	LBB286_20
427647: 	sub	x1, x17, x3
427648: 	add	x1, x14, x1
427649: LBB286_17:
427650: 	mov	x2, x1
427651: 	ldr	q6, [x2, #-16]!
427652: 	ldp	q7, q18, [x1]
427653: 	ldr	q19, [x1, #32]
427654: 	cmeq.16b	v17, v6, v5
427655: 	cmeq.16b	v16, v7, v5
427656: 	cmeq.16b	v7, v18, v5
427657: 	cmeq.16b	v6, v19, v5
427658: 	orr.16b	v18, v16, v17
427659: 	orr.16b	v19, v7, v6
427660: 	orr.16b	v18, v18, v19
427661: 	umaxp.16b	v18, v18, v18
427662: 	fmov	x3, d18
427663: 	cbnz	x3, LBB286_28
427664: 	add	x1, x1, #64
427665: 	add	x2, x2, #64
427666: 	cmp	x2, x11
427667: 	b.ls	LBB286_17
427668: 	sub	x1, x1, #16
427669: LBB286_20:
427670: 	cmp	x1, x13
427671: 	b.hi	LBB286_23
427672: LBB286_21:
427673: 	ldr	q6, [x1]
427674: 	cmeq.16b	v6, v6, v5
427675: 	shrn.8b	v6, v6, #4
427676: 	fmov	x2, d6
427677: 	ands	x2, x2, #0x8888888888888888
427678: 	b.ne	LBB286_25
427679: 	add	x1, x1, #16
427680: 	cmp	x1, x13
427681: 	b.ls	LBB286_21
427682: LBB286_23:
427683: 	cmp	x1, x10
427684: 	b.hs	LBB286_68
427685: 	add	x1, x9, x8
427686: 	ldr	q6, [x1, #-16]!
427687: 	cmeq.16b	v6, v6, v5
427688: 	shrn.8b	v6, v6, #4
427689: 	fmov	x2, d6
427690: 	ands	x2, x2, #0x8888888888888888
427691: 	b.eq	LBB286_68
427692: LBB286_25:
427693: 	rbit	x2, x2
427694: 	clz	x2, x2
427695: 	add	x1, x1, x2, lsr #2
427696: LBB286_26:
427697: 	sub	x0, x1, x0
427698: 	add	x17, x17, x0
427699: 	add	x17, x17, #1
427700: 	cmp	x17, x8
427701: 	b.hs	LBB286_68
427702: 	ldrb	w0, [x9, x17]
427703: 	cmp	w0, #47
427704: 	b.ne	LBB286_8
427705: 	b	LBB286_66
427706: LBB286_28:
427707: 	shrn.8b	v17, v17, #4
427708: 	fmov	x2, d17
427709: 	ands	x2, x2, #0x8888888888888888
427710: 	b.eq	LBB286_30
427711: 	rbit	x2, x2
427712: 	clz	x2, x2
427713: 	add	x1, x1, x2, lsr #2
427714: 	sub	x1, x1, #16
427715: 	b	LBB286_26
427716: LBB286_30:
427717: 	shrn.8b	v16, v16, #4
427718: 	fmov	x2, d16
427719: 	ands	x2, x2, #0x8888888888888888
427720: 	b.ne	LBB286_25
427721: 	shrn.8b	v7, v7, #4
427722: 	fmov	x2, d7
427723: 	ands	x2, x2, #0x8888888888888888
427724: 	b.eq	LBB286_33
427725: 	rbit	x2, x2
427726: 	clz	x2, x2
427727: 	add	x1, x1, x2, lsr #2
427728: 	add	x1, x1, #16
427729: 	b	LBB286_26
427730: LBB286_33:
427731: 	shrn.8b	v6, v6, #4
427732: 	fmov	x2, d6
427733: 	and	x2, x2, #0x8888888888888888
427734: 	rbit	x2, x2
427735: 	clz	x2, x2
427736: 	add	x1, x1, x2, lsr #2
427737: 	add	x1, x1, #32
427738: 	b	LBB286_26
427739: LBB286_34:
427740: 	cbz	x0, LBB286_65
427741: 	rbit	x17, x0
427742: 	clz	x17, x17
427743: 	add	x0, x17, x1
427744: 	cmp	x0, x8
427745: 	b.hs	LBB286_68
427746: 	add	x17, x0, #1
427747: 	cmp	x17, x8
427748: 	b.hs	LBB286_70
427749: 	ldrb	w17, [x9, x17]
427750: 	cmp	w17, #42
427751: 	b.ne	LBB286_70
427752: 	add	x17, x0, #2
427753: 	cmp	x17, x8
427754: 	b.hs	LBB286_68
427755: LBB286_39:
427756: 	add	x0, x9, x17
427757: 	sub	x2, x8, x17
427758: 	cmp	x2, #15
427759: 	b.hi	LBB286_43
427760: 	add	x1, x9, x17
427761: LBB286_41:
427762: 	ldrb	w3, [x1]
427763: 	cmp	w3, #42
427764: 	b.eq	LBB286_57
427765: 	add	x1, x1, #1
427766: 	subs	x2, x2, #1
427767: 	b.ne	LBB286_41
427768: 	b	LBB286_68
427769: LBB286_43:
427770: 	ldr	q6, [x0]
427771: 	cmeq.16b	v6, v6, v5
427772: 	shrn.8b	v6, v6, #4
427773: 	fmov	x1, d6
427774: 	ands	x1, x1, #0x8888888888888888
427775: 	b.eq	LBB286_45
427776: 	rbit	x1, x1
427777: 	clz	x1, x1
427778: 	add	x1, x0, x1, lsr #2
427779: 	b	LBB286_57
427780: LBB286_45:
427781: 	and	x3, x0, #0xf
427782: 	sub	x4, x15, x3
427783: 	add	x1, x0, x4
427784: 	cmp	x2, #64
427785: 	b.lo	LBB286_51
427786: 	add	x2, x4, x17
427787: 	cmp	x2, x12
427788: 	b.gt	LBB286_51
427789: 	sub	x1, x17, x3
427790: 	add	x1, x14, x1
427791: LBB286_48:
427792: 	mov	x2, x1
427793: 	ldr	q6, [x2, #-16]!
427794: 	ldp	q7, q18, [x1]
427795: 	ldr	q19, [x1, #32]
427796: 	cmeq.16b	v17, v6, v5
427797: 	cmeq.16b	v16, v7, v5
427798: 	cmeq.16b	v7, v18, v5
427799: 	cmeq.16b	v6, v19, v5
427800: 	orr.16b	v18, v16, v17
427801: 	orr.16b	v19, v7, v6
427802: 	orr.16b	v18, v18, v19
427803: 	umaxp.16b	v18, v18, v18
427804: 	fmov	x3, d18
427805: 	cbnz	x3, LBB286_59
427806: 	add	x1, x1, #64
427807: 	add	x2, x2, #64
427808: 	cmp	x2, x11
427809: 	b.ls	LBB286_48
427810: 	sub	x1, x1, #16
427811: LBB286_51:
427812: 	cmp	x1, x13
427813: 	b.hi	LBB286_54
427814: LBB286_52:
427815: 	ldr	q6, [x1]
427816: 	cmeq.16b	v6, v6, v5
427817: 	shrn.8b	v6, v6, #4
427818: 	fmov	x2, d6
427819: 	ands	x2, x2, #0x8888888888888888
427820: 	b.ne	LBB286_56
427821: 	add	x1, x1, #16
427822: 	cmp	x1, x13
427823: 	b.ls	LBB286_52
427824: LBB286_54:
427825: 	cmp	x1, x10
427826: 	b.hs	LBB286_68
427827: 	add	x1, x9, x8
427828: 	ldr	q6, [x1, #-16]!
427829: 	cmeq.16b	v6, v6, v5
427830: 	shrn.8b	v6, v6, #4
427831: 	fmov	x2, d6
427832: 	ands	x2, x2, #0x8888888888888888
427833: 	b.eq	LBB286_68
427834: LBB286_56:
427835: 	rbit	x2, x2
427836: 	clz	x2, x2
427837: 	add	x1, x1, x2, lsr #2
427838: LBB286_57:
427839: 	sub	x0, x1, x0
427840: 	add	x17, x17, x0
427841: 	add	x17, x17, #1
427842: 	cmp	x17, x8
427843: 	b.hs	LBB286_68
427844: 	ldrb	w0, [x9, x17]
427845: 	cmp	w0, #47
427846: 	b.ne	LBB286_39
427847: 	b	LBB286_67
427848: LBB286_59:
427849: 	shrn.8b	v17, v17, #4
427850: 	fmov	x2, d17
427851: 	ands	x2, x2, #0x8888888888888888
427852: 	b.eq	LBB286_61
427853: 	rbit	x2, x2
427854: 	clz	x2, x2
427855: 	add	x1, x1, x2, lsr #2
427856: 	sub	x1, x1, #16
427857: 	b	LBB286_57
427858: LBB286_61:
427859: 	shrn.8b	v16, v16, #4
427860: 	fmov	x2, d16
427861: 	ands	x2, x2, #0x8888888888888888
427862: 	b.ne	LBB286_56
427863: 	shrn.8b	v7, v7, #4
427864: 	fmov	x2, d7
427865: 	ands	x2, x2, #0x8888888888888888
427866: 	b.eq	LBB286_64
427867: 	rbit	x2, x2
427868: 	clz	x2, x2
427869: 	add	x1, x1, x2, lsr #2
427870: 	add	x1, x1, #16
427871: 	b	LBB286_57
427872: LBB286_64:
427873: 	shrn.8b	v6, v6, #4
427874: 	fmov	x2, d6
427875: 	and	x2, x2, #0x8888888888888888
427876: 	rbit	x2, x2
427877: 	clz	x2, x2
427878: 	add	x1, x1, x2, lsr #2
427879: 	add	x1, x1, #32
427880: 	b	LBB286_57
427881: LBB286_65:
427882: 	add	x1, x1, #64
427883: 	cmp	x1, x8
427884: 	b.lo	LBB286_2
427885: 	b	LBB286_68
427886: LBB286_66:
427887: 	add	x1, x17, #1
427888: 	cmp	x1, x8
427889: 	b.lo	LBB286_2
427890: 	b	LBB286_68
427891: LBB286_67:
427892: 	add	x1, x17, #1
427893: 	cmp	x1, x8
427894: 	b.lo	LBB286_2
427895: LBB286_68:
427896: 	mov	x0, x8
427897: 	ret
427898: LBB286_69:
427899: 	add	x9, x17, x1
427900: 	cmp	x8, x9
427901: 	csel	x0, x8, x9, lo
427902: LBB286_70:
427903: 	ret
427904: 	.loh AdrpLdr	Lloh6500, Lloh6501
427905: 	.loh AdrpAdrp	Lloh6498, Lloh6500
427906: 	.loh AdrpLdr	Lloh6498, Lloh6499
427907: 	.loh AdrpAdrp	Lloh6496, Lloh6498
427908: 	.loh AdrpLdr	Lloh6496, Lloh6497
427909: 	.cfi_endproc
427910: 
427911: 	.section	__TEXT,__literal16,16byte_literals
427912: 	.p2align	4, 0x0
427913: lCPI287_0:
427914: 	.byte	1
427915: 	.byte	2
427916: 	.byte	4
427917: 	.byte	8
427918: 	.byte	16
427919: 	.byte	32
427920: 	.byte	64
427921: 	.byte	128
427922: 	.byte	1
427923: 	.byte	2
427924: 	.byte	4
427925: 	.byte	8
427926: 	.byte	16
427927: 	.byte	32
427928: 	.byte	64
427929: 	.byte	128
427930: 	.section	__TEXT,__text,regular,pure_instructions
427931: 	.globl	__RNvNtNtNtCshXrMbbbkDC1_10parse_that7parsers4scan12quote_parity19filter_quote_parity
427932: 	.p2align	2
