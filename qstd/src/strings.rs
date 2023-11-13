use common::error::Error;
use interpreter::{
    builtins::*,
    generate_builtins,
    value::{make_value_array, Value},
    vm::*,
};

generate_builtins! {
    ///String manipulation functions.
    [export=strings]

    ///Truncate a string to a given length and add a custom ellipsis.
    fn @truncate(s: str, len: int, ellipsis: str) -> str {
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

    ///Capitalize the first letter of a string.
    fn @capitalize(s: str) -> str {
        let mut c = s.chars();
        match c.next() {
            None => "".to_string(),
            Some(f) => f.to_uppercase().chain(c).collect(),
        }.into()
    }

    ///Capitalize the first letter of every word in a string.
    fn @title(s: str) -> str {
        fn capitalize(s: &str) -> String {
            let mut c = s.chars();
            match c.next() {
                None => "".to_string(),
                Some(f) => f.to_uppercase().chain(c).collect(),
            }
        }

        s.split_whitespace().map(capitalize).collect::<Vec<String>>().join(" ").into()
    }

    ///Convert a string to uppercase.
    fn @uppercase(s: str) -> str {
        s.to_uppercase().into()
    }

    ///Convert a string to lowercase.
    fn @lowercase(s: str) -> str {
        s.to_lowercase().into()
    }

    [options]
    ///Split a string into a list of strings at a given separator.
    fn @split(s: str, sep: str) -> list {
        make_value_array(s.split(&sep).map(|s| s.into()).collect::<Vec<Value>>(), options.array_max_size)?
    }

    ///Join a list of values into a single string with a given separator.
    fn @join(s: list, sep: str) -> str {
        s.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(&sep).into()
    }

    ///Replace all instances of a substring with another substring.
    fn @replace(s: str, old: str, new: str) -> str {
        s.replace(&old, &new).into()
    }

    ///Trim whitespace from the beginning and end of a string.
    fn @trim(s: str) -> str {
        s.trim().into()
    }

    ///Trim whitespace from the beginning of a string.
    fn @trim_start(s: str) -> str {
        s.trim_start().into()
    }

    ///Trim whitespace from the end of a string.
    fn @trim_end(s: str) -> str {
        s.trim_end().into()
    }

    ///Check if a string starts with a given substring.
    fn @starts_with(s: str, start: str) -> bool {
        s.starts_with(&start).into()
    }

    ///Check if a string ends with a given substring.
    fn @ends_with(s: str, end: str) -> bool {
        s.ends_with(&end).into()
    }

    ///Check if a string contains a given substring.
    fn @contains(s: str, contains: str) -> bool {
        s.contains(&contains).into()
    }

    ///Get all characters in a string as a list of strings.
    fn @chars(s: str) -> list {
        s.chars().map(|c| c.to_string()).collect::<Vec<String>>().into()
    }
}
