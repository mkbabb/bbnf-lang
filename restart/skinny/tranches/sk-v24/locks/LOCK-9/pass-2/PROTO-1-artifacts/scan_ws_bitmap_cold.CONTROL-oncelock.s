427482: __RNvNtNtNtCslGOFyeFj4i1_10parse_that7parsers4scan10ws_comment19scan_ws_bitmap_cold:
427483: 	.cfi_startproc
427484: 	ldr	x8, [x0, #16]
427485: 	cmp	x1, x8
427486: 	b.hs	LBB286_68
427487: 	ldr	x9, [x0]
427488: 	add	x10, x9, x8
427489: 	sub	x11, x10, #64
427490: 	sub	x12, x8, #64
427491: 	sub	x13, x10, #16
427492: 	add	x14, x9, #32
427493: 	movi.16b	v0, #15
427494: Lloh6496:
427495: 	adrp	x15, lCPI286_0@PAGE
427496: Lloh6497:
427497: 	ldr	q1, [x15, lCPI286_0@PAGEOFF]
427498: Lloh6498:
427499: 	adrp	x15, lCPI286_1@PAGE
427500: Lloh6499:
427501: 	ldr	q2, [x15, lCPI286_1@PAGEOFF]
427502: Lloh6500:
427503: 	adrp	x15, lCPI286_2@PAGE
427504: Lloh6501:
427505: 	ldr	q3, [x15, lCPI286_2@PAGEOFF]
427506: 	movi.16b	v4, #47
427507: 	movi.16b	v5, #42
427508: 	mov	w15, #16
427509: 	mov	x16, #-1
427510: LBB286_2:
427511: 	add	x17, x9, x1
427512: 	ldp	q6, q7, [x17]
427513: 	and.16b	v16, v6, v0
427514: 	ushr.16b	v17, v6, #4
427515: 	tbl.16b	v16, { v1 }, v16
427516: 	tbl.16b	v17, { v2 }, v17
427517: 	and.16b	v16, v17, v16
427518: 	cmeq.16b	v16, v16, #0
427519: 	bic.16b	v16, v3, v16
427520: 	ext.16b	v17, v16, v16, #8
427521: 	zip1.16b	v16, v16, v17
427522: 	addv.8h	h16, v16
427523: 	fmov	w0, s16
427524: 	and.16b	v16, v7, v0
427525: 	ushr.16b	v17, v7, #4
427526: 	tbl.16b	v16, { v1 }, v16
427527: 	tbl.16b	v17, { v2 }, v17
427528: 	and.16b	v16, v17, v16
427529: 	cmeq.16b	v16, v16, #0
427530: 	bic.16b	v16, v3, v16
427531: 	ext.16b	v17, v16, v16, #8
427532: 	zip1.16b	v16, v16, v17
427533: 	addv.8h	h16, v16
427534: 	fmov	w2, s16
427535: 	lsl	w2, w2, #16
427536: 	bfxil	x2, x0, #0, #16
427537: 	ldp	q16, q17, [x17, #32]
427538: 	and.16b	v18, v16, v0
427539: 	ushr.16b	v19, v16, #4
427540: 	tbl.16b	v18, { v1 }, v18
427541: 	tbl.16b	v19, { v2 }, v19
427542: 	and.16b	v18, v19, v18
427543: 	cmeq.16b	v18, v18, #0
427544: 	bic.16b	v18, v3, v18
427545: 	ext.16b	v19, v18, v18, #8
427546: 	zip1.16b	v18, v18, v19
427547: 	addv.8h	h18, v18
427548: 	fmov	w17, s18
427549: 	orr	x17, x2, x17, lsl #32
427550: 	and.16b	v18, v17, v0
427551: 	ushr.16b	v19, v17, #4
427552: 	tbl.16b	v18, { v1 }, v18
427553: 	tbl.16b	v19, { v2 }, v19
427554: 	and.16b	v18, v19, v18
427555: 	cmeq.16b	v18, v18, #0
427556: 	bic.16b	v18, v3, v18
427557: 	ext.16b	v19, v18, v18, #8
427558: 	zip1.16b	v18, v18, v19
427559: 	addv.8h	h18, v18
427560: 	fmov	w0, s18
427561: 	orr	x17, x17, x0, lsl #48
427562: 	cmeq.16b	v6, v6, v4
427563: 	and.16b	v6, v6, v3
427564: 	ext.16b	v18, v6, v6, #8
427565: 	zip1.16b	v6, v6, v18
427566: 	addv.8h	h6, v6
427567: 	fmov	w0, s6
427568: 	cmeq.16b	v6, v7, v4
427569: 	and.16b	v6, v6, v3
427570: 	ext.16b	v7, v6, v6, #8
427571: 	zip1.16b	v6, v6, v7
427572: 	addv.8h	h6, v6
427573: 	fmov	w2, s6
427574: 	lsl	w2, w2, #16
427575: 	bfxil	x2, x0, #0, #16
427576: 	cmeq.16b	v6, v16, v4
427577: 	and.16b	v6, v6, v3
427578: 	ext.16b	v7, v6, v6, #8
427579: 	zip1.16b	v6, v6, v7
427580: 	addv.8h	h6, v6
427581: 	fmov	w0, s6
427582: 	orr	x0, x2, x0, lsl #32
427583: 	cmeq.16b	v6, v17, v4
427584: 	and.16b	v6, v6, v3
427585: 	ext.16b	v7, v6, v6, #8
427586: 	zip1.16b	v6, v6, v7
427587: 	addv.8h	h6, v6
427588: 	fmov	w2, s6
427589: 	orr	x0, x0, x2, lsl #48
427590: 	cmn	x17, #1
427591: 	b.eq	LBB286_34
427592: 	mvn	x17, x17
427593: 	rbit	x17, x17
427594: 	clz	x17, x17
427595: 	lsl	x2, x16, x17
427596: 	bics	x0, x0, x2
427597: 	b.eq	LBB286_69
427598: 	rbit	x17, x0
427599: 	clz	x17, x17
427600: 	add	x0, x17, x1
427601: 	cmp	x0, x8
427602: 	b.hs	LBB286_68
427603: 	add	x17, x0, #1
427604: 	cmp	x17, x8
427605: 	b.hs	LBB286_70
427606: 	ldrb	w17, [x9, x17]
427607: 	cmp	w17, #42
427608: 	b.ne	LBB286_70
427609: 	add	x17, x0, #2
427610: 	cmp	x17, x8
427611: 	b.hs	LBB286_68
427612: LBB286_8:
427613: 	add	x0, x9, x17
427614: 	sub	x2, x8, x17
427615: 	cmp	x2, #15
427616: 	b.hi	LBB286_12
427617: 	add	x1, x9, x17
427618: LBB286_10:
427619: 	ldrb	w3, [x1]
427620: 	cmp	w3, #42
427621: 	b.eq	LBB286_26
427622: 	add	x1, x1, #1
427623: 	subs	x2, x2, #1
427624: 	b.ne	LBB286_10
427625: 	b	LBB286_68
427626: LBB286_12:
427627: 	ldr	q6, [x0]
427628: 	cmeq.16b	v6, v6, v5
427629: 	shrn.8b	v6, v6, #4
427630: 	fmov	x1, d6
427631: 	ands	x1, x1, #0x8888888888888888
427632: 	b.eq	LBB286_14
427633: 	rbit	x1, x1
427634: 	clz	x1, x1
427635: 	add	x1, x0, x1, lsr #2
427636: 	b	LBB286_26
427637: LBB286_14:
427638: 	and	x3, x0, #0xf
427639: 	sub	x4, x15, x3
427640: 	add	x1, x0, x4
427641: 	cmp	x2, #64
427642: 	b.lo	LBB286_20
427643: 	add	x2, x4, x17
427644: 	cmp	x2, x12
427645: 	b.gt	LBB286_20
427646: 	sub	x1, x17, x3
427647: 	add	x1, x14, x1
427648: LBB286_17:
427649: 	mov	x2, x1
427650: 	ldr	q6, [x2, #-16]!
427651: 	ldp	q7, q18, [x1]
427652: 	ldr	q19, [x1, #32]
427653: 	cmeq.16b	v17, v6, v5
427654: 	cmeq.16b	v16, v7, v5
427655: 	cmeq.16b	v7, v18, v5
427656: 	cmeq.16b	v6, v19, v5
427657: 	orr.16b	v18, v16, v17
427658: 	orr.16b	v19, v7, v6
427659: 	orr.16b	v18, v18, v19
427660: 	umaxp.16b	v18, v18, v18
427661: 	fmov	x3, d18
427662: 	cbnz	x3, LBB286_28
427663: 	add	x1, x1, #64
427664: 	add	x2, x2, #64
427665: 	cmp	x2, x11
427666: 	b.ls	LBB286_17
427667: 	sub	x1, x1, #16
427668: LBB286_20:
427669: 	cmp	x1, x13
427670: 	b.hi	LBB286_23
427671: LBB286_21:
427672: 	ldr	q6, [x1]
427673: 	cmeq.16b	v6, v6, v5
427674: 	shrn.8b	v6, v6, #4
427675: 	fmov	x2, d6
427676: 	ands	x2, x2, #0x8888888888888888
427677: 	b.ne	LBB286_25
427678: 	add	x1, x1, #16
427679: 	cmp	x1, x13
427680: 	b.ls	LBB286_21
427681: LBB286_23:
427682: 	cmp	x1, x10
427683: 	b.hs	LBB286_68
427684: 	add	x1, x9, x8
427685: 	ldr	q6, [x1, #-16]!
427686: 	cmeq.16b	v6, v6, v5
427687: 	shrn.8b	v6, v6, #4
427688: 	fmov	x2, d6
427689: 	ands	x2, x2, #0x8888888888888888
427690: 	b.eq	LBB286_68
427691: LBB286_25:
427692: 	rbit	x2, x2
427693: 	clz	x2, x2
427694: 	add	x1, x1, x2, lsr #2
427695: LBB286_26:
427696: 	sub	x0, x1, x0
427697: 	add	x17, x17, x0
427698: 	add	x17, x17, #1
427699: 	cmp	x17, x8
427700: 	b.hs	LBB286_68
427701: 	ldrb	w0, [x9, x17]
427702: 	cmp	w0, #47
427703: 	b.ne	LBB286_8
427704: 	b	LBB286_66
427705: LBB286_28:
427706: 	shrn.8b	v17, v17, #4
427707: 	fmov	x2, d17
427708: 	ands	x2, x2, #0x8888888888888888
427709: 	b.eq	LBB286_30
427710: 	rbit	x2, x2
427711: 	clz	x2, x2
427712: 	add	x1, x1, x2, lsr #2
427713: 	sub	x1, x1, #16
427714: 	b	LBB286_26
427715: LBB286_30:
427716: 	shrn.8b	v16, v16, #4
427717: 	fmov	x2, d16
427718: 	ands	x2, x2, #0x8888888888888888
427719: 	b.ne	LBB286_25
427720: 	shrn.8b	v7, v7, #4
427721: 	fmov	x2, d7
427722: 	ands	x2, x2, #0x8888888888888888
427723: 	b.eq	LBB286_33
427724: 	rbit	x2, x2
427725: 	clz	x2, x2
427726: 	add	x1, x1, x2, lsr #2
427727: 	add	x1, x1, #16
427728: 	b	LBB286_26
427729: LBB286_33:
427730: 	shrn.8b	v6, v6, #4
427731: 	fmov	x2, d6
427732: 	and	x2, x2, #0x8888888888888888
427733: 	rbit	x2, x2
427734: 	clz	x2, x2
427735: 	add	x1, x1, x2, lsr #2
427736: 	add	x1, x1, #32
427737: 	b	LBB286_26
427738: LBB286_34:
427739: 	cbz	x0, LBB286_65
427740: 	rbit	x17, x0
427741: 	clz	x17, x17
427742: 	add	x0, x17, x1
427743: 	cmp	x0, x8
427744: 	b.hs	LBB286_68
427745: 	add	x17, x0, #1
427746: 	cmp	x17, x8
427747: 	b.hs	LBB286_70
427748: 	ldrb	w17, [x9, x17]
427749: 	cmp	w17, #42
427750: 	b.ne	LBB286_70
427751: 	add	x17, x0, #2
427752: 	cmp	x17, x8
427753: 	b.hs	LBB286_68
427754: LBB286_39:
427755: 	add	x0, x9, x17
427756: 	sub	x2, x8, x17
427757: 	cmp	x2, #15
427758: 	b.hi	LBB286_43
427759: 	add	x1, x9, x17
427760: LBB286_41:
427761: 	ldrb	w3, [x1]
427762: 	cmp	w3, #42
427763: 	b.eq	LBB286_57
427764: 	add	x1, x1, #1
427765: 	subs	x2, x2, #1
427766: 	b.ne	LBB286_41
427767: 	b	LBB286_68
427768: LBB286_43:
427769: 	ldr	q6, [x0]
427770: 	cmeq.16b	v6, v6, v5
427771: 	shrn.8b	v6, v6, #4
427772: 	fmov	x1, d6
427773: 	ands	x1, x1, #0x8888888888888888
427774: 	b.eq	LBB286_45
427775: 	rbit	x1, x1
427776: 	clz	x1, x1
427777: 	add	x1, x0, x1, lsr #2
427778: 	b	LBB286_57
427779: LBB286_45:
427780: 	and	x3, x0, #0xf
427781: 	sub	x4, x15, x3
427782: 	add	x1, x0, x4
427783: 	cmp	x2, #64
427784: 	b.lo	LBB286_51
427785: 	add	x2, x4, x17
427786: 	cmp	x2, x12
427787: 	b.gt	LBB286_51
427788: 	sub	x1, x17, x3
427789: 	add	x1, x14, x1
427790: LBB286_48:
427791: 	mov	x2, x1
427792: 	ldr	q6, [x2, #-16]!
427793: 	ldp	q7, q18, [x1]
427794: 	ldr	q19, [x1, #32]
427795: 	cmeq.16b	v17, v6, v5
427796: 	cmeq.16b	v16, v7, v5
427797: 	cmeq.16b	v7, v18, v5
427798: 	cmeq.16b	v6, v19, v5
427799: 	orr.16b	v18, v16, v17
427800: 	orr.16b	v19, v7, v6
427801: 	orr.16b	v18, v18, v19
427802: 	umaxp.16b	v18, v18, v18
427803: 	fmov	x3, d18
427804: 	cbnz	x3, LBB286_59
427805: 	add	x1, x1, #64
427806: 	add	x2, x2, #64
427807: 	cmp	x2, x11
427808: 	b.ls	LBB286_48
427809: 	sub	x1, x1, #16
427810: LBB286_51:
427811: 	cmp	x1, x13
427812: 	b.hi	LBB286_54
427813: LBB286_52:
427814: 	ldr	q6, [x1]
427815: 	cmeq.16b	v6, v6, v5
427816: 	shrn.8b	v6, v6, #4
427817: 	fmov	x2, d6
427818: 	ands	x2, x2, #0x8888888888888888
427819: 	b.ne	LBB286_56
427820: 	add	x1, x1, #16
427821: 	cmp	x1, x13
427822: 	b.ls	LBB286_52
427823: LBB286_54:
427824: 	cmp	x1, x10
427825: 	b.hs	LBB286_68
427826: 	add	x1, x9, x8
427827: 	ldr	q6, [x1, #-16]!
427828: 	cmeq.16b	v6, v6, v5
427829: 	shrn.8b	v6, v6, #4
427830: 	fmov	x2, d6
427831: 	ands	x2, x2, #0x8888888888888888
427832: 	b.eq	LBB286_68
427833: LBB286_56:
427834: 	rbit	x2, x2
427835: 	clz	x2, x2
427836: 	add	x1, x1, x2, lsr #2
427837: LBB286_57:
427838: 	sub	x0, x1, x0
427839: 	add	x17, x17, x0
427840: 	add	x17, x17, #1
427841: 	cmp	x17, x8
427842: 	b.hs	LBB286_68
427843: 	ldrb	w0, [x9, x17]
427844: 	cmp	w0, #47
427845: 	b.ne	LBB286_39
427846: 	b	LBB286_67
427847: LBB286_59:
427848: 	shrn.8b	v17, v17, #4
427849: 	fmov	x2, d17
427850: 	ands	x2, x2, #0x8888888888888888
427851: 	b.eq	LBB286_61
427852: 	rbit	x2, x2
427853: 	clz	x2, x2
427854: 	add	x1, x1, x2, lsr #2
427855: 	sub	x1, x1, #16
427856: 	b	LBB286_57
427857: LBB286_61:
427858: 	shrn.8b	v16, v16, #4
427859: 	fmov	x2, d16
427860: 	ands	x2, x2, #0x8888888888888888
427861: 	b.ne	LBB286_56
427862: 	shrn.8b	v7, v7, #4
427863: 	fmov	x2, d7
427864: 	ands	x2, x2, #0x8888888888888888
427865: 	b.eq	LBB286_64
427866: 	rbit	x2, x2
427867: 	clz	x2, x2
427868: 	add	x1, x1, x2, lsr #2
427869: 	add	x1, x1, #16
427870: 	b	LBB286_57
427871: LBB286_64:
427872: 	shrn.8b	v6, v6, #4
427873: 	fmov	x2, d6
427874: 	and	x2, x2, #0x8888888888888888
427875: 	rbit	x2, x2
427876: 	clz	x2, x2
427877: 	add	x1, x1, x2, lsr #2
427878: 	add	x1, x1, #32
427879: 	b	LBB286_57
427880: LBB286_65:
427881: 	add	x1, x1, #64
427882: 	cmp	x1, x8
427883: 	b.lo	LBB286_2
427884: 	b	LBB286_68
427885: LBB286_66:
427886: 	add	x1, x17, #1
427887: 	cmp	x1, x8
427888: 	b.lo	LBB286_2
427889: 	b	LBB286_68
427890: LBB286_67:
427891: 	add	x1, x17, #1
427892: 	cmp	x1, x8
427893: 	b.lo	LBB286_2
427894: LBB286_68:
427895: 	mov	x0, x8
427896: 	ret
427897: LBB286_69:
427898: 	add	x9, x17, x1
427899: 	cmp	x8, x9
427900: 	csel	x0, x8, x9, lo
427901: LBB286_70:
427902: 	ret
427903: 	.loh AdrpLdr	Lloh6500, Lloh6501
427904: 	.loh AdrpAdrp	Lloh6498, Lloh6500
427905: 	.loh AdrpLdr	Lloh6498, Lloh6499
427906: 	.loh AdrpAdrp	Lloh6496, Lloh6498
427907: 	.loh AdrpLdr	Lloh6496, Lloh6497
427908: 	.cfi_endproc
427909: 
427910: 	.section	__TEXT,__literal16,16byte_literals
427911: 	.p2align	4, 0x0
427912: lCPI287_0:
427913: 	.byte	1
427914: 	.byte	2
427915: 	.byte	4
427916: 	.byte	8
427917: 	.byte	16
427918: 	.byte	32
427919: 	.byte	64
427920: 	.byte	128
427921: 	.byte	1
427922: 	.byte	2
427923: 	.byte	4
427924: 	.byte	8
427925: 	.byte	16
427926: 	.byte	32
427927: 	.byte	64
427928: 	.byte	128
427929: 	.section	__TEXT,__text,regular,pure_instructions
427930: 	.globl	__RNvNtNtNtCslGOFyeFj4i1_10parse_that7parsers4scan12quote_parity19filter_quote_parity
427931: 	.p2align	2
