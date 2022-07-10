use std::str::from_utf8;
use std::vec::Vec;

#[derive(Debug, Copy, Clone)]
pub struct ParseState<'a>(&'a [u8], usize);

impl<'a> ParseState<'a> {
    pub fn new(contents: &'a String) -> ParseState<'a> {
        ParseState(contents.as_bytes(), 0)
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Progress<'a, V> {
    Parsed(ParseState<'a>, V),
    Failed,
}

pub type Parser<'a, T> = dyn 'a + Fn(ParseState) -> Progress<T>;

pub fn digits(ps: ParseState) -> Progress<String> {
    let ParseState(content, start_index) = ps;
    let content_len = content.len();
    let mut index = start_index;
    while index < content_len && content[index].is_ascii_digit() {
        index += 1
    }
    if start_index != index {
        let slice: &str = match from_utf8(&content[start_index..index]) {
            Ok(s) => s,
            Err(_) => return Progress::Failed,
        };
        Progress::Parsed(ParseState(content, index), slice.to_string())
    } else {
        Progress::Failed
    }
}

pub fn character(ch: char) -> Box<dyn Send + Sync + Fn(ParseState) -> Progress<String>> {
    let ch = ch as u32;
    Box::new(move |ps: ParseState| -> Progress<String> {
        let ParseState(content, start_index) = ps;
        println!("scanning for {} in {:?}", ch, content);
        let content_len = content.len();
        let mut index = start_index;
        // TODO: handle utf-8
        if index < content_len && content[index] as u32 == ch {
            index += 1;
            match char::from_u32(ch) {
                Some(ch) => Progress::Parsed(ParseState(content, index), ch.to_string()),
                None => Progress::Failed,
            }
        } else {
            Progress::Failed
        }
    })
}

#[macro_export]
macro_rules! sequence_core {
    ($parsers:expr, $parser:expr) => {{
        $parsers.push(Box::new($parser));
        sequence($parsers)
    }};

    ($parsers:expr, $parser:expr, $($tail:expr),+) => {{
        $parsers.push(Box::new($parser));
        sequence_core!($parsers, $($tail),+)
    }}
}

#[macro_export]
macro_rules! sequence {
    ($parser:expr, $($tail:expr),+) => {{
        let mut parsers: Vec<Box<Parser<String>>> = Vec::new();
        sequence_core!(parsers, $parser, $($tail),+)
    }}
}

pub fn sequence<'a, T>(parsers: Vec<Box<Parser<'a, T>>>) -> Box<Parser<'a, Vec<T>>>
where
    T: 'a,
{
    Box::new(move |ps: ParseState| -> Progress<Vec<T>> {
        let mut nodes: Vec<T> = Vec::new();
        let mut parse_state = ps;
        for parser in &parsers[..] {
            match parser(parse_state) {
                Progress::Parsed(next_parse_state, node) => {
                    parse_state = next_parse_state;
                    nodes.push(node);
                }
                Progress::Failed => return Progress::Failed,
            }
        }
        Progress::Parsed(parse_state, nodes)
    })
}

pub fn lift<'a, T, U, L>(f: &'a L, parser: &'a Parser<'a, T>) -> Box<Parser<'a, U>>
where
    T: 'a,
    U: 'a,
    L: 'a + Fn(T) -> U,
{
    Box::new(move |ps: ParseState| -> Progress<U> {
        match parser(ps) {
            Progress::Parsed(next_parse_state, node) => Progress::Parsed(next_parse_state, f(node)),
            Progress::Failed => return Progress::Failed,
        }
    })
}

#[macro_export]
macro_rules! lift {
    ($f: expr, $parser: expr) => {
        lift(&$f, &$parser)
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_sequence() {
        let buffer: String = "1B".to_string();
        let ps: ParseState = ParseState::new(&buffer);

        let language_parser = sequence!(digits, character('B'));
        match language_parser(ps) {
            Progress::Parsed(_, v) => assert_eq!(v, ["1", "B"]),
            Progress::Failed => assert!(false),
        }
    }

    #[test]
    fn test_lift_z_suffiz() {
        let buffer: String = "123".to_string();
        let ps: ParseState = ParseState::new(&buffer);

        let z_suffix = |s: String| -> String {
            let mut s = s.clone();
            s.push_str("z");
            s
        };
        let language_parser = lift!(z_suffix, digits);
        match language_parser(ps) {
            Progress::Parsed(_, v) => assert_eq!(v, "123z"),
            Progress::Failed => assert!(false),
        }
    }

    #[test]
    fn test_lift_int() {
        let buffer: String = "123".to_string();
        let ps: ParseState = ParseState::new(&buffer);

        let int = |s: String| -> i64 {
            let i = s.parse::<i64>().unwrap();
            i
        };
        let language_parser = lift!(int, digits);
        match language_parser(ps) {
            Progress::Parsed(_, v) => assert_eq!(v, 123),
            Progress::Failed => assert!(false),
        }
    }
}
