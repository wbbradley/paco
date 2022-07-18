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

pub fn exact_string<T>(run: T) -> impl Parser<char, String>
where
    T: Into<String>,
{
    let run: String = run.into();
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

#[macro_export]
macro_rules! choice_core {
    ($parsers:expr, $parser:expr) => {{
        $parsers.push(Box::new($parser));
        choice($parsers)
    }};

    ($parsers:expr, $parser:expr, $($tail:expr),+) => {{
        $parsers.push(Box::new($parser));
        choice_core!($parsers, $($tail),+)
    }}
}

#[macro_export]
macro_rules! choice {
    ($parser:expr, $($tail:expr),+) => {{
        let mut parsers: Vec<Box<dyn Parser<_, _>>> = Vec::new();
        choice_core!(parsers, $parser, $($tail),+)
    }}
}

pub fn choice<'a, T, V>(parsers: Vec<Box<dyn 'a + Parser<T, V>>>) -> impl 'a + Parser<T, V>
where
    T: 'a,
    V: 'a,
{
    move |ps: ParseState<T>| -> Progress<T, V> {
        for parser in &parsers[..] {
            match parser(ps.clone()) {
                Progress::Failed => continue,
                progress => return progress,
            }
        }
        Progress::Failed
    }
}

pub fn lift<'a, F, P, T, U, V>(f: F, parser: P) -> impl 'a + Parser<T, V>
where
    P: 'a + Parser<T, U>,
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
    P: 'a + Parser<T, U>,
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

pub fn many1<'a, P, T, U>(parser: P) -> impl 'a + Fn(ParseState<T>) -> Progress<T, Vec<U>>
where
    T: 'a,
    U: 'a,
    P: 'a + Parser<T, U>,
{
    move |ps_: ParseState<T>| -> Progress<T, Vec<U>> {
        let mut ps = ps_;
        let mut nodes: Vec<U> = Vec::new();
        while let Progress::Parsed(next_parse_state, node) = parser(ps.clone()) {
            ps = next_parse_state;
            nodes.push(node)
        }
        if nodes.len() >= 1 {
            Progress::Parsed(ps, nodes)
        } else {
            Progress::Failed
        }
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
        let ParseState(content, mut index) = ps;
        let mut result: Vec<T> = Vec::new();
        while index < content.len() {
            if pred(content[index].clone()) {
                result.push(content[index].clone());
            } else {
                break;
            }
            index += 1;
        }
        Progress::Parsed(ParseState(content, index), result)
    }
}

pub fn take_until<'a, T, Predicate>(
    pred: Predicate,
) -> impl 'a + Fn(ParseState<T>) -> Progress<T, Vec<T>>
where
    T: 'a + Clone,
    Predicate: 'a + Fn(T) -> bool,
{
    take_while(move |x| !pred(x))
}

pub fn all_of_input<'a, T, V, P>(parser: P) -> impl Parser<T, V>
where
    T: 'a,
    V: 'a,
    P: 'a + Parser<T, V>,
{
    move |ps: ParseState<T>| -> Progress<T, V> {
        let content_len = ps.0.len();
        let progress = parser(ps);
        match progress {
            Progress::Failed => Progress::Failed,
            Progress::Parsed(ParseState(_, index), _) => {
                if index == content_len {
                    progress
                } else {
                    Progress::Failed
                }
            }
        }
    }
}

pub fn constant<'a, T, U>(u: U) -> impl Fn(T) -> U
where
    T: 'a,
    U: 'a + Clone,
{
    move |_: T| -> U { u.clone() }
}

pub fn concat(xs: Vec<String>) -> String {
    xs.join("")
    // More general string iterator solution:
    // xs.iter().map(|s| &**s).collect::<Vec<&str>>().join("")
}

pub fn collect_string<'a, T>(xs: T) -> String
where
    T: 'a + IntoIterator<Item = char>,
{
    xs.into_iter().collect()
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

    macro_rules! test_parse_fails {
        ($input: expr, $parser: expr) => {
            let buffer: String = $input.to_string();
            let ps: ParseState<_> = ParseState::new(&buffer);

            let language_parser = $parser;
            match language_parser(ps) {
                Progress::Parsed(_, _) => assert!(false),
                Progress::Failed => (),
            }
        };
    }

    #[test]
    fn test_parse_sequence() {
        test_parse_eq!("1B", sequence!(digits, character('B')), ["1", "B"]);
    }

    #[test]
    fn test_lift_z_suffiz() {
        let z_suffix = |mut s: String| -> String {
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
    fn test_many1() {
        test_parse_eq!("+++--", many1(character('+')), ["+", "+", "+"]);
        test_parse_fails!("--+++--", many1(character('+')));
    }

    #[test]
    fn test_many_lifted() {
        test_parse_eq!("+++--", lift(concat, many(character('+'))), "+++");
    }

    #[test]
    fn test_exact_string() {
        test_parse_eq!("a", exact_string("a"), "a");
        test_parse_eq!("ab", exact_string("a"), "a");
        test_parse_eq!("abracadabra", exact_string("abracadabra"), "abracadabra");
        test_parse_eq!("😄hey", exact_string("😄".to_string()), "😄");
    }

    #[test]
    fn test_take_while() {
        test_parse_eq!(
            "abcabcd",
            lift(
                collect_string,
                take_while(|ch: char| { "abc".contains(ch) })
            ),
            "abcabc"
        );
        test_parse_eq!(
            "--++--",
            sequence!(
                take_while(|ch| { '-' == ch }),
                take_while(|ch| { '+' == ch })
            ),
            [['-', '-'], ['+', '+']]
        );
    }

    #[test]
    fn test_choice() {
        test_parse_eq!(
            "12344321",
            many(choice!(
                exact_string("1234"),
                exact_string("21"),
                exact_string("1"),
                exact_string("2"),
                exact_string("3"),
                exact_string("4"),
                exact_string("43")
            )),
            ["1234", "4", "3", "21"]
        );
    }

    #[test]
    fn test_take_until() {
        test_parse_eq!(
            "123 the title is مدخل إلى c++ in arabic. 41😼 😽 🙀 😿 😾 k2j34b12   k3j5b123G ",
            lift(collect_string, take_until(|ch: char| { ch.is_uppercase() })),
            "123 the title is مدخل إلى c++ in arabic. 41😼 😽 🙀 😿 😾 k2j34b12   k3j5b123"
        );
        let input = [1, 2, 3, 4, 5];
        match take_until(|x: i32| x > 3)(ParseState(Rc::new(input.to_vec()), 0)) {
            Progress::Parsed(_, v) => assert_eq!(v, [1, 2, 3]),
            Progress::Failed => assert!(false),
        }
    }

    #[test]
    fn test_all_of_input() {
        test_parse_eq!(
            "{\"hello\": 145.0}",
            lift(
                collect_string,
                all_of_input(take_until(|ch: char| { ch.is_uppercase() }))
            ),
            "{\"hello\": 145.0}"
        );
    }

    #[test]
    fn test_constant() {
        test_parse_eq!(
            "abc",
            lift(constant(123), all_of_input(take_while(|_| { true }))),
            123
        );
    }
}
