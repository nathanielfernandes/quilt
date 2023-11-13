use common::error::Error;
use interpreter::{builtins::*, generate_builtins, value::Value, vm::*};

generate_builtins! {
    [export=time]

    fn @now() {
        match std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH) {
                Ok(n) => n.as_secs_f64().into(),
                Err(_) => Err(error("failed to get time"))?,
            }
    }

    fn @date() {
        let now = match std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH) {
                Ok(n) => n,
                Err(_) => return Err(error("failed to get time"))?,
            };

        let secs = now.as_secs();
        let mins = secs / 60;
        let hours = mins / 60;
        let days = hours / 24;

        let year = 1970 + (days / 365);
        let month = (days % 365) / 30;
        let day = (days % 365) % 30;

        let hour = hours % 24;
        let min = mins % 60;
        let sec = secs % 60;

        let date = format!("{}-{}-{} {}:{}:{}", year, month, day, hour, min, sec);

        date.into()
    }
}
