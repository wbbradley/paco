use std::rc::Rc;
use std::vec::Vec;

#[derive(Debug)]
pub struct ParseState<T>(Rc<Vec<T>>, usize);

impl<T> Clone for ParseState<T> {
    fn clone(self: &Self) -> Self {
        Self(self.0.clone(), self.1)
    }
}

impl ParseState<char> {
    pub fn new(contents: &String) -> Self {
        Self(Rc::new(contents.chars().collect()), 0)
    }
}

#[derive(Debug)]
pub enum Progress<T, V> {
    Parsed(ParseState<T>, V),
    Failed,
}

pub type Parser<'a, T, U> = Box<dyn 'a + Fn(ParseState<T>) -> Progress<T, U>>;

pub fn digits(ps: ParseState<char>) -> Progress<char, String> {
    let ParseState(content, start_index) = ps;
    let content_len: usize = content.len();
    let mut index: usize = start_index;
    while index < content_len && content[index].is_ascii_digit() {
        index += 1
    }
    if start_index != index {
        let s = content[start_index..index].iter().collect::<String>();
        Progress::Parsed(ParseState(content, index), s)
    } else {
        Progress::Failed
    }
}

pub fn character<'a>(ch: char) -> Parser<'a, char, String> {
    Box::new(move |ps: ParseState<char>| -> Progress<char, String> {
        let ParseState(content, start_index) = ps;
        let content_len = content.len();
        let mut index = start_index;
        // TODO: handle utf-8
        if index < content_len && content[index] == ch {
            index += 1;
            Progress::Parsed(ParseState(content, index), ch.to_string())
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
        let mut parsers: Vec<Parser<_, _>> = Vec::new();
        sequence_core!(parsers, $parser, $($tail),+)
    }}
}

pub fn sequence<'a, T, V>(parsers: Vec<Parser<'a, T, V>>) -> Parser<'a, T, Vec<V>>
where
    T: 'a,
    V: 'a,
{
    Box::new(move |ps: ParseState<T>| -> Progress<T, Vec<V>> {
        let mut nodes: Vec<V> = Vec::new();
        let mut ps: ParseState<T> = ps;
        for parser in &parsers[..] {
            match parser(ps) {
                Progress::Parsed(next_parse_state, node) => {
                    ps = next_parse_state;
                    nodes.push(node);
                }
                Progress::Failed => return Progress::Failed,
            }
        }
        Progress::Parsed(ps, nodes)
    })
}

pub fn lift<'a, T, U, V>(f: Box<dyn Fn(U) -> V>, parser: Parser<'a, T, U>) -> Parser<'a, T, V>
where
    T: 'a,
    U: 'a,
    V: 'a,
{
    Box::new(move |ps: ParseState<T>| -> Progress<T, V> {
        match parser(ps) {
            Progress::Parsed(next_parse_state, node) => Progress::Parsed(next_parse_state, f(node)),
            Progress::Failed => Progress::Failed,
        }
    })
}

#[macro_export]
macro_rules! lift {
    ($f: expr, $parser: expr) => {
        lift(Box::new($f), Box::new($parser))
    };
}

pub fn many<'a, T, U>(parser: Parser<'a, T, U>) -> Parser<'a, T, Vec<U>>
where
    T: 'a,
    U: 'a,
{
    Box::new(move |ps_: ParseState<T>| -> Progress<T, Vec<U>> {
        let mut ps = ps_;
        let mut nodes: Vec<U> = Vec::new();
        while let Progress::Parsed(next_parse_state, node) = parser(ps.clone()) {
            ps = next_parse_state;
            nodes.push(node)
        }
        Progress::Parsed(ps, nodes)
    })
}

#[macro_export]
macro_rules! many {
    ($parser: expr) => {
        many(Box::new($parser))
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_parse_eq {
        ($input: expr, $parser: expr, $expect: expr) => {
            let buffer: String = $input.to_string();
            let ps: ParseState<_> = ParseState::new(&buffer);

            let language_parser = $parser;
            match language_parser(ps) {
                Progress::Parsed(_, v) => assert_eq!(v, $expect),
                Progress::Failed => assert!(false),
            }
        };
    }

    #[test]
    fn test_parse_sequence() {
        test_parse_eq!("1B", sequence!(digits, character('B')), ["1", "B"]);
    }

    #[test]
    fn test_lift_z_suffiz() {
        let z_suffix = |s: String| -> String {
            let mut s = s;
            s.push_str("z");
            s
        };
        let language_parser = lift!(z_suffix, digits);
        test_parse_eq!("123", language_parser, "123z");
    }

    #[test]
    fn test_lift_int() {
        let int = |s: String| -> i64 {
            let i = s.parse::<i64>().unwrap();
            i
        };
        test_parse_eq!("123", lift!(int, digits), 123);
    }

    #[test]
    fn test_many() {
        test_parse_eq!("+++--", many!(character('+')), ["+", "+", "+"]);
    }
}
