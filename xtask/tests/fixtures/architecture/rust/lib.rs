mod model;

use crate::model::Record;
pub use crate::model::State;

pub fn record(state: State) -> Record {
    Record { state }
}
