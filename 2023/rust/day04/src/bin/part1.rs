use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    let file = File::open("./input_test.txt")?;
    let reader = io::BufReader::new(file);

    for line in reader.lines() {
        match line {
            Ok(line_content) => {
                println!("{}", line_content);
                let
            }
            Err(err) => eprintln!("Error reading line: {}", err),
        }
    }

    Ok(())
}

struct Card {
    numbers: Vec<u32>,
    winning: Vec<u32>,
}

impl Card {
    fn get_number_of_wins() {
    }
}
