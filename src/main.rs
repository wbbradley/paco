use std::env::args;
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error + 'static>> {
    let args: Vec<String> = args().collect();
    if args.len() != 2 {
        return Ok(());
    }
    let buffer: String = fs::read_to_string(args[1].clone())?;

    for ch in buffer.chars() {
        println!("ch={ch}");
    }
    Ok(())
}
