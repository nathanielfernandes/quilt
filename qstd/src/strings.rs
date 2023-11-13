use common::error::Error;
use interpreter::{
    builtins::*,
    generate_builtins,
    value::{make_value_array, Value},
    vm::*,
};

generate_builtins! {
    [export=strings]


    fn @truncate(s: str, len: int, ellipsis: str) {
        // factor in the length of the ellipsis
        let len = len as usize;

        // get the byte index of the nth character
        match s.char_indices().nth(len) {
            Some((i, _)) => {
                let i = i.saturating_sub(ellipsis.chars().count());
                // move the ellipsis so that it touches the last non-whitespace character
                let chars = s.chars().collect::<Vec<char>>();

                let mut i = i.min(chars.len());
                let mut cs = chars[i..].iter();
                while let Some(c) = cs.next() {
                    if !c.is_whitespace() {
                        break;
                    }
                    i = i.saturating_sub(c.len_utf8());
                }

                let mut s = chars[..i].iter().collect::<String>();
                s.push_str(&ellipsis);
                s
            }
            None => s.to_string(),
        }.into()
    }

    fn @capitalize(s: str) {
        let mut c = s.chars();
        match c.next() {
            None => "".to_string(),
            Some(f) => f.to_uppercase().chain(c).collect(),
        }.into()
    }

    fn @title(s: str) {
        fn capitalize(s: &str) -> String {
            let mut c = s.chars();
            match c.next() {
                None => "".to_string(),
                Some(f) => f.to_uppercase().chain(c).collect(),
            }
        }

        s.split_whitespace().map(capitalize).collect::<Vec<String>>().join(" ").into()
    }

    fn @uppercase(s: str) {
        s.to_uppercase().into()
    }

    fn @lowercase(s: str) {
        s.to_lowercase().into()
    }

    [options]
    fn @split(s: str, sep: str) {
        make_value_array(s.split(&sep).map(|s| s.into()).collect::<Vec<Value>>(), options.array_max_size)?
    }

    fn @join(s: list, sep: str) {
        s.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(&sep).into()
    }

    fn @replace(s: str, old: str, new: str) {
        s.replace(&old, &new).into()
    }

    fn @trim(s: str) {
        s.trim().into()
    }

    fn @trim_start(s: str) {
        s.trim_start().into()
    }

    fn @trim_end(s: str) {
        s.trim_end().into()
    }

    fn @starts_with(s: str, start: str) {
        s.starts_with(&start).into()
    }

    fn @ends_with(s: str, end: str) {
        s.ends_with(&end).into()
    }

    fn @contains(s: str, contains: str) {
        s.contains(&contains).into()
    }

    fn @chars(s: str) {
        s.chars().map(|c| c.to_string()).collect::<Vec<String>>().into()
    }
}
