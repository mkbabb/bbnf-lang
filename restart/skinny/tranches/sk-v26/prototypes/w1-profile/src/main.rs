use std::hint::black_box;
use std::time::Instant;

use parse_that::{EventSink, Input, RegexPlan, RegexTerminal, Run, Step, literal, regex};

const SAMPLE_COUNT: usize = 20;
const WARMUP_COUNT: usize = 5;
const BYTES_PER_SAMPLE: usize = 8 * 1024 * 1024;

const BYTE_CLASSES: [u8; 256] = {
    let mut classes = [0; 256];
    classes[b'a' as usize] = 1;
    classes
};
const TRANSITIONS: [u32; 4] = [RegexPlan::DEAD, 1, RegexPlan::DEAD, 1];
const ACCEPTING: [bool; 2] = [false, true];
const PLAN: RegexPlan<'static> =
    match RegexPlan::new(&BYTE_CLASSES, 2, 2, 0, &TRANSITIONS, &ACCEPTING) {
        Ok(plan) => plan,
        Err(_) => panic!("static W1 profile plan must be valid"),
    };
const TERMINAL: RegexTerminal<'static> = RegexTerminal::new(PLAN);

#[derive(Default)]
struct RetainedSink {
    events: Vec<usize>,
}

impl EventSink for RetainedSink {
    type Event = usize;

    fn len(&self) -> usize {
        self.events.len()
    }

    fn truncate(&mut self, len: usize) {
        self.events.truncate(len);
    }

    fn push(&mut self, event: Self::Event) {
        self.events.push(event);
    }
}

#[derive(Clone, Copy)]
enum Probe {
    Literal,
    Regex,
    RetainedRegexEvent,
}

impl Probe {
    const ALL: [Self; 3] = [Self::Literal, Self::Regex, Self::RetainedRegexEvent];

    const fn name(self) -> &'static str {
        match self {
            Self::Literal => "literal_no_sink",
            Self::Regex => "regex_no_sink",
            Self::RetainedRegexEvent => "regex_retained_event_fresh",
        }
    }
}

fn run_once(probe: Probe, bytes: &[u8], literal_expected: &[u8]) -> usize {
    match probe {
        Probe::Literal => {
            let mut run = Run::new(Input::new(bytes));
            match run.parse_full(|run| literal(run, literal_expected)) {
                Step::Matched(span) => span.end() - span.start(),
                outcome => panic!("literal profile changed semantics: {outcome:?}"),
            }
        }
        Probe::Regex => {
            let mut run = Run::new(Input::new(bytes));
            match run.parse_full(|run| regex(run, &TERMINAL)) {
                Step::Matched(span) => span.end() - span.start(),
                outcome => panic!("regex profile changed semantics: {outcome:?}"),
            }
        }
        Probe::RetainedRegexEvent => {
            let mut run = Run::with_sink(Input::new(bytes), RetainedSink::default());
            let outcome = run.parse_full(|run| match regex(run, &TERMINAL) {
                Step::Matched(span) => {
                    run.emit_with(|| span.end() - span.start());
                    Step::Matched(span)
                }
                Step::Mismatch(failure) => Step::Mismatch(failure),
                Step::Fault(fault) => Step::Fault(fault),
            });
            let span_len = match outcome {
                Step::Matched(span) => span.end() - span.start(),
                outcome => panic!("retained profile changed semantics: {outcome:?}"),
            };
            let sink = run.into_sink();
            assert_eq!(sink.events, [bytes.len()]);
            span_len.wrapping_mul(31).wrapping_add(sink.events[0])
        }
    }
}

fn main() {
    let sizes = [64, 1024, 65_536];
    let mut first = true;
    println!(
        "{{\"schema\":\"sk-v26.w1.raw-profile.v1\",\"sample_count\":{SAMPLE_COUNT},\
         \"warmup_count\":{WARMUP_COUNT},\"bytes_per_sample\":{BYTES_PER_SAMPLE},\"rows\":["
    );

    for probe in Probe::ALL {
        for size in sizes {
            let bytes = vec![b'a'; size];
            let literal_expected = black_box(vec![b'a'; size]);
            let iterations = (BYTES_PER_SAMPLE / size).max(1);
            for _ in 0..WARMUP_COUNT {
                black_box(run_once(
                    probe,
                    black_box(&bytes),
                    black_box(&literal_expected),
                ));
            }

            let mut samples = Vec::with_capacity(SAMPLE_COUNT);
            let mut checksum = 0usize;
            for _ in 0..SAMPLE_COUNT {
                let started = Instant::now();
                for _ in 0..iterations {
                    checksum = checksum.wrapping_add(black_box(run_once(
                        probe,
                        black_box(&bytes),
                        black_box(&literal_expected),
                    )));
                }
                samples.push(started.elapsed().as_nanos());
            }

            if !first {
                println!(",");
            }
            first = false;
            print!(
                "{{\"probe\":\"{}\",\"input_bytes\":{size},\"iterations_per_sample\":{iterations},\
                 \"checksum\":{checksum},\"elapsed_ns\":[",
                probe.name()
            );
            for (index, sample) in samples.iter().enumerate() {
                if index != 0 {
                    print!(",");
                }
                print!("{sample}");
            }
            print!("]}}");
        }
    }

    println!("\n]}}");
}
