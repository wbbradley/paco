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

pub fn char(ch: u32) -> impl Fn(ParseState) -> Progress<&str> {
    move |ps: ParseState| -> Progress<&str> {
        let ParseState(content, start_index) = ps;
        let content_len = content.len();
        let mut index = start_index;
        // TODO: handle utf-8
        if index < content_len && content[index] as u32 == ch {
            index += 1;
            let str_ch = [ch];
            Progress::Parsed(ParseState(content, index), from_utf8(&str_ch))
        } else {
            Progress::Failed
        }
    }
}

pub fn sequence<P>(parsers: &Vec<P>) -> impl Fn(ParseState) -> Progress<Vec<&str>> + Clone
where
    P: Fn(ParseState) -> Progress<&str> + Clone,
{
    let parsers = parsers.clone();
    move |ps: ParseState| -> Progress<Vec<&str>> {
        let mut nodes: Vec<&str> = Vec::new();
        let mut parse_state = ps;
        for parser in &parsers {
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

fn main() -> Result<(), Box<dyn std::error::Error + 'static>> {
    let args: Vec<String> = args().collect();
    if args.len() != 2 {
        return Ok(());
    }
    let buffer: String = fs::read_to_string(args[1].clone())?;

    let ps: ParseState = ParseState(buffer.as_bytes(), 0);
    let language_parser = sequence([digits, char('Z' as u32)]);
    match language_parser(ps) {
        Progress::Parsed(_, v) => println!("found digits {v}"),
        Progress::Failed => println!("parse error!"),
    }
    Ok(())
}
