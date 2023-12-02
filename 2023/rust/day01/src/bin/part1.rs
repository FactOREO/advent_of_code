/* --- Day 1: Trebuchet?! ---
Something is wrong with global snow production, and you've been selected to take a look.
The Elves have even given you a map; on it, they've used stars to mark the top fifty locations that are likely to be having problems.

You've been doing this long enough to know that to restore snow operations, you need to check all fifty stars by December 25th.

Collect stars by solving puzzles.
Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first.
Each puzzle grants one star. Good luck!

You try to ask why they can't just use a weather machine ("not powerful enough") and where they're even sending you ("the sky") and why your map looks mostly blank ("you sure ask a lot of questions") and hang on did you just say the sky ("of course, where do you think snow comes from") when you realize that the Elves are already loading you into a trebuchet ("please hold still, we need to strap you in").

As they're making the final adjustments, they discover that their calibration document (your puzzle input) has been amended by a very young Elf who was apparently just excited to show off her art skills.
Consequently, the Elves are having trouble reading the values on the document.

The newly-improved calibration document consists of lines of text; each line originally contained a specific calibration value that the Elves now need to recover.
On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.

For example:

1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet

In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.

Consider your entire calibration document. What is the sum of all of the calibration values?
*/

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
// For every line, get all the numbers in the input
fn extract_digits(input: &str) -> String {
    let mut result = String::new();

    for c in input.chars() {
        if c.is_numeric() {
            result.push(c);
        }
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
