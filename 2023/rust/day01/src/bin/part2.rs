/*
Your calculation isn't quite right. It looks like some of the digits are actually spelled out with letters: one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".

Equipped with this new information, you now need to find the real first and last digit on each line. For example:

two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen

In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.
*/

use std::collections::BTreeMap;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

fn main() {
    let mut result: u32 = 0;
    // File hosts.txt must exist in the current path
    if let Ok(lines) = read_lines("./input_part1.txt") {
        // Consumes the iterator, returns an (Optional) String
        for line in lines {
            if let Ok(line) = line {
                let digits = extract_digits(&line);
                result += extract_first_and_last_digit(&digits);
            }
        }
    }
    println!("The result is {}", result)
}

// Extract all numbers from every line
fn extract_digits(input: &str) -> String {
    let lowercase_input = input.to_lowercase();
    let mut result = String::new();

    // Define spelled-out digits and their numeric counterparts
    let spelled_out_digits: BTreeMap<&str, &str> = [
        ("one", "1"),
        ("two", "2"),
        ("three", "3"),
        ("four", "4"),
        ("five", "5"),
        ("six", "6"),
        ("seven", "7"),
        ("eight", "8"),
        ("nine", "9"),
    ]
    .iter()
    .cloned()
    .collect();

    // Find start positions of spelled-out digits
    let mut spelled_positions: BTreeMap<usize, &str> = BTreeMap::new();
    for (spelled, numeric) in spelled_out_digits.iter() {
        let mut start = 0;
        while let Some(pos) = lowercase_input[start..].find(spelled) {
            let abs_pos = start + pos;
            spelled_positions.insert(abs_pos, numeric);
            start = abs_pos + spelled.len();
        }
    }

    // Find start positions of actual digits
    let actual_positions: BTreeMap<usize, String> = lowercase_input
        .char_indices()
        .filter_map(|(pos, c)| if c.is_numeric() { Some((pos, c.to_string())) } else { None })
        .collect();

    // Construct the return value by ordering start positions
    let positions: BTreeMap<usize, &str> = spelled_positions
        .into_iter()
        .chain(actual_positions.iter().map(|(&pos, s)| (pos, s.as_str())))
        .collect();

    for (_, digit) in positions.iter() {
        result.push_str(digit);
    }

    result
}

// Index the first and last number in a string of numbers
fn extract_first_and_last_digit(input: &str) -> u32 {
    let digits: String = input.chars().filter(|c| c.is_numeric()).collect();

    let first = digits.chars().nth(0).unwrap().to_digit(10);
    let last = digits.chars().last().unwrap().to_digit(10);

    match (first, last) {
        (Some(f), Some(l)) => f as u32 * 10 + l as u32,
        _ => 0,
    }
}

// The output is wrapped in a Result to allow matching on errors
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where P: AsRef<Path>, {
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
