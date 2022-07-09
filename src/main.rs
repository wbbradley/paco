use std::env::args;
use std::fs;
use std::str::from_utf8;
use std::vec::Vec;

#[derive(Debug, Copy, Clone)]
pub struct ParseState<'a>(&'a [u8], usize);

#[derive(Debug, Copy, Clone)]
pub enum Progress<'a, V> {
    Parsed(ParseState<'a>, V),
    Failed,
}

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

type Parser<'a, T>
where
    T: 'a + Clone + Copy + Send + Sync,
= Box<dyn 'a + Send + Sync + Fn(ParseState) -> Progress<T>>;

static digs: Parser<String> = Box::new(digits);

pub fn character(ch: char) -> Box<dyn Send + Sync + Fn(ParseState) -> Progress<String>> {
    let ch = ch as u32;
    Box::new(move |ps: ParseState| -> Progress<String> {
        let ParseState(content, start_index) = ps;
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

pub fn sequence<'a>(
    parsers: &'a [Parser<String>],
) -> impl 'a + Clone + Fn(ParseState) -> Progress<Vec<String>> {
    let parsers = parsers.clone();
    move |ps: ParseState| -> Progress<Vec<String>> {
        let mut nodes: Vec<String> = Vec::new();
        let mut parse_state = ps;
        for parser in parsers {
            match parser(parse_state) {
                Progress::Parsed(next_parse_state, node) => {
                    parse_state = next_parse_state;
                    nodes.push(node);
                }
                Progress::Failed => return Progress::Failed,
            }
        }
        Progress::Parsed(parse_state, nodes)
    }
}

#[test]
fn test_parse_sequence() {
    let buffer: String = "1B";
    let ps: ParseState = ParseState(buffer.as_bytes(), 0);
    let seq_parser = sequence([digits, char('B' as u32)]);
    match language_parser(ps) {
        Progress::Parsed(_, v) => asserteq!(v == ["1", "B"]),
        Progress::Failed => assert!(false),
    }
}

fn main() -> Result<(), Box<dyn std::error::Error + 'static>> {
    let args: Vec<String> = args().collect();
    if args.len() != 2 {
        return Ok(());
    }
    let buffer: String = fs::read_to_string(args[1].clone())?;

    let ps: ParseState = ParseState(buffer.as_bytes(), 0);
    let xs: Vec<Parser<String>> = vec![digs, character('Z')];
    let language_parser = sequence(&xs);
    match language_parser(ps) {
        Progress::Parsed(_, v) => println!("found digits {:?}", v),
        Progress::Failed => println!("parse error!"),
    }
    Ok(())
}
