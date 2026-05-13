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
    NeonJson,
}

impl SimdClassifier for SelectedClassifier {
    fn classify_chunk(&self, bytes: &[u8; 64]) -> ClassifyResult {
        match self.backend {
            SelectedBackend::Scalar => crate::scalar::classify_chunk(bytes, self.alphabet),
            #[cfg(target_arch = "aarch64")]
            SelectedBackend::NeonJson => unsafe {
                crate::aarch64::classify_tbl4::classify_json_block(bytes.as_ptr())
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
        if crate::is_json_structural_alphabet(alphabet) {
            return SelectedBackend::NeonJson;
        }
    }

    SelectedBackend::Scalar
}
