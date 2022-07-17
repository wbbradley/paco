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

pub trait Parser<T, U>: Fn(ParseState<T>) -> Progress<T, U> {}
impl<C, T, U> Parser<T, U> for C where C: Fn(ParseState<T>) -> Progress<T, U> {}

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

pub fn character(ch: char) -> impl Parser<char, String> {
    move |ps: ParseState<char>| -> Progress<char, String> {
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
    }
}

pub fn exact_string(run: String) -> impl Parser<char, String> {
    let run_chars: Vec<char> = run.chars().collect();
    let run_chars_len = run_chars.len();

    move |ps: ParseState<char>| -> Progress<char, String> {
        if run_chars_len == 0 {
            return Progress::Failed;
        }
        let ParseState(content, start_index) = ps;
        let content_len = content.len();
        if start_index + run_chars_len > content_len {
            return Progress::Failed;
        }
        let mut content_index = start_index;
        let mut run_chars_index = run_chars_len - run_chars_len;
        loop {
            if content[content_index] != run_chars[run_chars_index] {
                return Progress::Failed;
            }
            run_chars_index += 1;
            content_index += 1;
            if run_chars_index == run_chars_len {
                return Progress::Parsed(ParseState(content, content_index), run.clone());
            }
        }
    }
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
        let mut parsers: Vec<Box<dyn Parser<_, _>>> = Vec::new();
        sequence_core!(parsers, $parser, $($tail),+)
    }}
}

pub fn sequence<'a, T, V>(parsers: Vec<Box<dyn 'a + Parser<T, V>>>) -> impl 'a + Parser<T, Vec<V>>
where
    T: 'a,
    V: 'a,
{
    move |ps: ParseState<T>| -> Progress<T, Vec<V>> {
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
    }
}

pub fn lift<'a, F, P, T, U, V>(f: F, parser: P) -> impl 'a + Parser<T, V>
where
    P: 'a + Fn(ParseState<T>) -> Progress<T, U>,
    F: 'a + Fn(U) -> V,
    T: 'a,
    U: 'a,
    V: 'a,
{
    move |ps: ParseState<T>| -> Progress<T, V> {
        match parser(ps) {
            Progress::Parsed(next_parse_state, node) => Progress::Parsed(next_parse_state, f(node)),
            Progress::Failed => Progress::Failed,
        }
    }
}

pub fn many<'a, P, T, U>(parser: P) -> impl 'a + Fn(ParseState<T>) -> Progress<T, Vec<U>>
where
    T: 'a,
    U: 'a,
    P: 'a + Fn(ParseState<T>) -> Progress<T, U>,
{
    move |ps_: ParseState<T>| -> Progress<T, Vec<U>> {
        let mut ps = ps_;
        let mut nodes: Vec<U> = Vec::new();
        while let Progress::Parsed(next_parse_state, node) = parser(ps.clone()) {
            ps = next_parse_state;
            nodes.push(node)
        }
        Progress::Parsed(ps, nodes)
    }
}

pub fn take_while<'a, T, Predicate>(
    pred: Predicate,
) -> impl 'a + Fn(ParseState<T>) -> Progress<T, Vec<T>>
where
    T: 'a + Clone,
    Predicate: 'a + Fn(T) -> bool,
{
    move |ps: ParseState<T>| -> Progress<T, Vec<T>> {
        let ParseState(content, index) = ps;
        let mut result: Vec<T> = Vec::new();
        for i in index..content.len() {
            if pred(content[i].clone()) {
                result.push(content[i].clone());
            } else {
                break;
            }
        }
        let content_len = content.len();
        Progress::Parsed(ParseState(content, content_len), result)
    }
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
        let language_parser = lift(z_suffix, digits);
        test_parse_eq!("123", language_parser, "123z");
    }

    #[test]
    fn test_lift_int() {
        let int = |s: String| -> i64 {
            let i = s.parse::<i64>().unwrap();
            i
        };
        test_parse_eq!("123", lift(int, digits), 123);
    }

    #[test]
    fn test_many_on_fn_pointer() {
        let _parser = many(|_: ParseState<char>| -> Progress<char, String> { Progress::Failed });
    }

    #[test]
    fn test_many() {
        test_parse_eq!("+++--", many(character('+')), ["+", "+", "+"]);
    }

    #[test]
    fn test_many_lifted() {
        test_parse_eq!(
            "+++--",
            lift(
                |xs: Vec<String>| -> String { xs.join("") },
                many(character('+'))
            ),
            "+++"
        );
    }

    #[test]
    fn test_exact_string() {
        test_parse_eq!("a", exact_string("a".to_string()), "a");
        test_parse_eq!("ab", exact_string("a".to_string()), "a");
        test_parse_eq!(
            "abracadabra",
            exact_string("abracadabra".to_string()),
            "abracadabra"
        );
        test_parse_eq!("😄hey", exact_string("😄".to_string()), "😄");
    }

    #[test]
    fn test_take_while() {
        test_parse_eq!(
            "abcabcd",
            lift(
                |xs: Vec<char>| -> String { xs.iter().collect() },
                take_while(|ch: char| { "abc".contains(ch) })
            ),
            "abcabc"
        );
    }
}
