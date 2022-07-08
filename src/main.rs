use std::env::args;
use std::fs;
use std::str::from_utf8;

pub struct ParseState<'a>(&'a [u8], usize);

pub enum Progress<'a, V> {
    Parsed(ParseState<'a>, V),
    Failed,
}

pub type Parser<V> = fn(ParseState) -> Progress<V>;

pub fn digits<'a>(ps: ParseState<'a>) -> Progress<'a, &str> {
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
    Ok(())
}
