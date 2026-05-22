use bbnf_bench::nonjson_css_l4;
use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};
use std::time::Duration;

fn bench_nonjson_css_l4(c: &mut Criterion) {
    let input = nonjson_css_l4::read_fixture().expect("CSS L4 fixture is readable");
    nonjson_css_l4::assert_strict_equality(&input).expect("CSS Track 1 equals cssparser oracle");
    nonjson_css_l4::assert_lightningcss_strict_equality(&input)
        .expect("CSS Track 1 equals lightningcss fact stream");
    let report =
        nonjson_css_l4::write_report_with_quick_measurement().expect("CSS L4 report is emitted");
    report.validate_gate().expect("CSS L4 report gate passes");

    let mut group = c.benchmark_group("nonjson_css_l4");
    group.throughput(Throughput::Bytes(input.len() as u64));
    group.bench_function("track1_generated_css_l4_decl_values", |b| {
        b.iter(|| {
            black_box(
                nonjson_css_l4::track1_facts(black_box(&input)).expect("Track 1 CSS fact stream"),
            )
        })
    });
    group.bench_function("track2_cssparser_oracle", |b| {
        b.iter(|| {
            black_box(
                nonjson_css_l4::oracle_facts(black_box(&input)).expect("cssparser CSS fact stream"),
            )
        })
    });
    group.bench_function("lightningcss_same_plane_fact_stream", |b| {
        b.iter(|| {
            black_box(
                nonjson_css_l4::lightningcss_facts(black_box(&input))
                    .expect("lightningcss CSS fact stream"),
            )
        })
    });
    group.finish();

    let selector_input = nonjson_css_l4::read_stylesheet_selectors_fixture()
        .expect("CSS L4 stylesheet/selectors fixture is readable");
    nonjson_css_l4::assert_stylesheet_selectors_strict_equality(&selector_input)
        .expect("CSS stylesheet/selectors Track 1 equals golden oracle");
    nonjson_css_l4::assert_stylesheet_selectors_lightningcss_strict_equality(&selector_input)
        .expect("CSS stylesheet/selectors Track 1 equals lightningcss fact stream");
    let selector_report =
        nonjson_css_l4::write_stylesheet_selectors_report_with_quick_measurement()
            .expect("CSS L4 stylesheet/selectors report is emitted");
    selector_report
        .validate_gate()
        .expect("CSS L4 stylesheet/selectors report gate passes");

    let mut selector_group = c.benchmark_group("nonjson_css_l4_w2");
    selector_group.throughput(Throughput::Bytes(selector_input.len() as u64));
    selector_group.bench_function("track1_generated_css_l4_stylesheet_selectors", |b| {
        b.iter(|| {
            black_box(
                nonjson_css_l4::stylesheet_selectors_track1_facts(black_box(&selector_input))
                    .expect("Track 1 CSS stylesheet/selectors fact stream"),
            )
        })
    });
    selector_group.bench_function("track2_golden_stylesheet_selectors_oracle", |b| {
        b.iter(|| {
            black_box(
                nonjson_css_l4::stylesheet_selectors_oracle_facts(black_box(&selector_input))
                    .expect("golden CSS stylesheet/selectors fact stream"),
            )
        })
    });
    selector_group.bench_function(
        "lightningcss_stylesheet_selectors_same_plane_fact_stream",
        |b| {
            b.iter(|| {
                black_box(
                    nonjson_css_l4::stylesheet_selectors_lightningcss_facts(black_box(
                        &selector_input,
                    ))
                    .expect("lightningcss CSS stylesheet/selectors fact stream"),
                )
            })
        },
    );
    selector_group.finish();

    let extended_input = nonjson_css_l4::read_declaration_values_extended_fixture()
        .expect("CSS L4 declaration-values-extended fixture is readable");
    nonjson_css_l4::assert_declaration_values_extended_strict_equality(&extended_input)
        .expect("CSS declaration-values-extended Track 1 equals cssparser oracle");
    nonjson_css_l4::assert_declaration_values_extended_lightningcss_strict_equality(
        &extended_input,
    )
    .expect("CSS declaration-values-extended Track 1 equals lightningcss fact stream");
    let extended_report =
        nonjson_css_l4::write_declaration_values_extended_report_with_quick_measurement()
            .expect("CSS L4 declaration-values-extended report is emitted");
    extended_report
        .validate_gate()
        .expect("CSS L4 declaration-values-extended report gate passes");

    let mut extended_group = c.benchmark_group("nonjson_css_l4_w3");
    extended_group.throughput(Throughput::Bytes(extended_input.len() as u64));
    extended_group.bench_function("track1_generated_css_l4_decl_values_extended", |b| {
        b.iter(|| {
            black_box(
                nonjson_css_l4::declaration_values_extended_track1_facts(black_box(
                    &extended_input,
                ))
                .expect("Track 1 CSS declaration-values-extended fact stream"),
            )
        })
    });
    extended_group.bench_function("track2_cssparser_decl_values_extended_oracle", |b| {
        b.iter(|| {
            black_box(
                nonjson_css_l4::declaration_values_extended_oracle_facts(black_box(
                    &extended_input,
                ))
                .expect("cssparser CSS declaration-values-extended fact stream"),
            )
        })
    });
    extended_group.bench_function(
        "lightningcss_decl_values_extended_same_plane_fact_stream",
        |b| {
            b.iter(|| {
                black_box(
                    nonjson_css_l4::declaration_values_extended_lightningcss_facts(black_box(
                        &extended_input,
                    ))
                    .expect("lightningcss CSS declaration-values-extended fact stream"),
                )
            })
        },
    );
    extended_group.finish();

    let visual_input = nonjson_css_l4::read_visual_functions_fixture()
        .expect("CSS L4 visual-functions fixture is readable");
    nonjson_css_l4::assert_visual_functions_strict_equality(&visual_input)
        .expect("CSS visual-functions Track 1 equals golden oracle");
    nonjson_css_l4::assert_visual_functions_lightningcss_strict_equality(&visual_input)
        .expect("CSS visual-functions Track 1 equals lightningcss fact stream");
    let visual_report = nonjson_css_l4::write_visual_functions_report_with_quick_measurement()
        .expect("CSS L4 visual-functions report is emitted");
    visual_report
        .validate_gate()
        .expect("CSS L4 visual-functions report gate passes");

    let mut visual_group = c.benchmark_group("nonjson_css_l4_w4");
    visual_group.throughput(Throughput::Bytes(visual_input.len() as u64));
    visual_group.bench_function("track1_generated_css_l4_visual_functions", |b| {
        b.iter(|| {
            black_box(
                nonjson_css_l4::visual_functions_track1_facts(black_box(&visual_input))
                    .expect("Track 1 CSS visual-functions fact stream"),
            )
        })
    });
    visual_group.bench_function("track2_golden_visual_functions_oracle", |b| {
        b.iter(|| {
            black_box(
                nonjson_css_l4::visual_functions_oracle_facts(black_box(&visual_input))
                    .expect("golden CSS visual-functions fact stream"),
            )
        })
    });
    visual_group.bench_function(
        "lightningcss_visual_functions_same_plane_fact_stream",
        |b| {
            b.iter(|| {
                black_box(
                    nonjson_css_l4::visual_functions_lightningcss_facts(black_box(&visual_input))
                        .expect("lightningcss CSS visual-functions fact stream"),
                )
            })
        },
    );
    visual_group.finish();

    let at_rules_input = nonjson_css_l4::read_at_rules_and_media_fixture()
        .expect("CSS L4 at-rules/media fixture is readable");
    nonjson_css_l4::assert_at_rules_and_media_strict_equality(&at_rules_input)
        .expect("CSS at-rules/media Track 1 equals golden oracle");
    nonjson_css_l4::assert_at_rules_and_media_lightningcss_strict_equality(&at_rules_input)
        .expect("CSS at-rules/media Track 1 equals lightningcss fact stream");
    let at_rules_report = nonjson_css_l4::write_at_rules_and_media_report_with_quick_measurement()
        .expect("CSS L4 at-rules/media report is emitted");
    at_rules_report
        .validate_gate()
        .expect("CSS L4 at-rules/media report gate passes");

    let mut at_rules_group = c.benchmark_group("nonjson_css_l4_w10_1");
    at_rules_group.throughput(Throughput::Bytes(at_rules_input.len() as u64));
    at_rules_group.bench_function("track1_generated_css_l4_at_rules_and_media", |b| {
        b.iter(|| {
            black_box(
                nonjson_css_l4::at_rules_and_media_track1_facts(black_box(&at_rules_input))
                    .expect("Track 1 CSS at-rules/media fact stream"),
            )
        })
    });
    at_rules_group.bench_function("track2_golden_at_rules_and_media_oracle", |b| {
        b.iter(|| {
            black_box(
                nonjson_css_l4::at_rules_and_media_oracle_facts(black_box(&at_rules_input))
                    .expect("golden CSS at-rules/media fact stream"),
            )
        })
    });
    at_rules_group.bench_function(
        "lightningcss_at_rules_and_media_same_plane_fact_stream",
        |b| {
            b.iter(|| {
                black_box(
                    nonjson_css_l4::at_rules_and_media_lightningcss_facts(black_box(
                        &at_rules_input,
                    ))
                    .expect("lightningcss CSS at-rules/media fact stream"),
                )
            })
        },
    );
    at_rules_group.finish();
}

criterion_group! {
    name = benches;
    config = Criterion::default()
        .warm_up_time(Duration::from_secs(3))
        .measurement_time(Duration::from_secs(5))
        .sample_size(30)
        .confidence_level(0.95)
        .significance_level(0.05)
        .noise_threshold(0.02);
    targets = bench_nonjson_css_l4
}
criterion_main!(benches);
