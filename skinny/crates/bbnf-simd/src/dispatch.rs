use crate::classifier::{ClassifyResult, SimdClassifier};

#[derive(Clone, Copy)]
pub struct SelectedClassifier {
    alphabet: &'static [u8; 64],
    backend: SelectedBackend,
}

#[derive(Clone, Copy)]
enum SelectedBackend {
    Scalar,
    #[cfg(target_arch = "aarch64")]
    NeonTbl4,
}

impl SimdClassifier for SelectedClassifier {
    fn classify_chunk(&self, bytes: &[u8; 64]) -> ClassifyResult {
        match self.backend {
            SelectedBackend::Scalar => crate::scalar::classify_chunk(bytes, self.alphabet),
            #[cfg(target_arch = "aarch64")]
            SelectedBackend::NeonTbl4 => unsafe {
                let table = crate::aarch64::classify_tbl4::build_lo6_table(self.alphabet);
                let table = crate::aarch64::classify_tbl4::load_lo6_table(&table);
                crate::aarch64::classify_tbl4::classify_block_from_table(
                    bytes.as_ptr(),
                    table,
                    b'"',
                    b'\\',
                    0x20,
                )
            },
        }
    }

    fn alphabet(&self) -> &'static [u8; 64] {
        self.alphabet
    }
}

pub fn select_classifier(alphabet: &'static [u8; 64]) -> SelectedClassifier {
    SelectedClassifier {
        alphabet,
        backend: select_backend(alphabet),
    }
}

fn select_backend(alphabet: &[u8; 64]) -> SelectedBackend {
    #[cfg(target_arch = "aarch64")]
    {
        if lo6_table_admissible(alphabet) {
            return SelectedBackend::NeonTbl4;
        }
    }

    SelectedBackend::Scalar
}

#[cfg(target_arch = "aarch64")]
fn lo6_table_admissible(alphabet: &[u8; 64]) -> bool {
    let mut seen = [false; 64];
    let mut len = 0usize;
    for byte in alphabet.iter().copied().take_while(|byte| *byte != 0) {
        len += 1;
        let slot = (byte & 0x3f) as usize;
        if seen[slot] {
            return false;
        }
        seen[slot] = true;
    }
    len != 0
}
