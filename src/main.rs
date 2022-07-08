use std::env::args;
use std::fs;
use std::str::from_utf8;
use std::vec::Vec;

pub struct ParseState<'a>(&'a [u8], usize);

pub enum Progress<'a, V> {
    Parsed(ParseState<'a>, V),
    Failed,
}

pub fn digits(ps: ParseState) -> Progress<&str> {
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
        Progress::Parsed(ParseState(content, index), slice)
    } else {
        Progress::Failed
    }
}

pub fn char(ch: u32) -> impl Fn(ParseState) -> Progress<u32> {
    move |ps: ParseState| -> Progress<u32> {
        let ParseState(content, start_index) = ps;
        let content_len = content.len();
        let mut index = start_index;
        // TODO: handle utf-8
        if index < content_len && content[index] as u32 == ch {
            index += 1;
            Progress::Parsed(ParseState(content, index), ch)
        } else {
            Progress::Failed
        }
    }
}

pub fn sequence<'a, T, P, U>(parsers : &Vec<P>) -> impl Fn(ParseState) -> Progress<Vec<T>>
    where P: FnMut(ParseState) -> Progress<u32>,
          U: IntoIterator {
    move |ps: ParseState| -> Progress<Vec<T>> {
        let mut nodes: Vec<T> = Vec::new();
        let mut parse_state = ps;
        for parser in parsers {
            match parser(parse_state) {
                Parsed(next_parse_state, node) {
                    parse_state = next_parse_state;
                    nodes.append(node);
                }
                Failed {
                    return Progress::Failed
                }
            }
        }
        Progress::Failed
    }
}

fn main() -> Result<(), Box<dyn std::error::Error + 'static>> {
    let args: Vec<String> = args().collect();
    if args.len() != 2 {
        return Ok(());
    }
    let buffer: String = fs::read_to_string(args[1].clone())?;

    let ps: ParseState = ParseState(buffer.as_bytes(), 0);
    match digits(ps) {
        Progress::Parsed(_, v) => println!("found digits {v}"),
        Progress::Failed => println!("parse error!"),
    }

    let ps: ParseState = ParseState(buffer.as_bytes(), 0);
    match char()(ps) {
        Progress::Parsed(_, v) => println!("found char {v}"),
        Progress::Failed => println!("parse error finding character!"),
    }
    Ok(())
}
