/*
You and the Elf eventually reach a gondola lift station; he says the gondola lift will take you up to the water source, but this is as far as he can bring you.
You go inside.

It doesn't take long to find the gondolas, but there seems to be a problem: they're not moving.

"Aaah!"

You turn around to see a slightly-greasy Elf with a wrench and a look of surprise.
"Sorry, I wasn't expecting anyone! The gondola lift isn't working right now; it'll still be a while before I can fix it.
You offer to help.

The engineer explains that an engine part seems to be missing from the engine, but nobody can figure out which one.
If you can add up all the part numbers in the engine schematic, it should be easy to work out which part is missing.

The engine schematic (your puzzle input) consists of a visual representation of the engine.
There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum.
(Periods (.) do not count as a symbol.

Here is an example engine schematic:

467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..

In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right).
Every other number is adjacent to a symbol and so is a part number; their sum is 4361.

Of course, the actual engine schematic is much larger.
What is the sum of all of the part numbers in the engine schematic?
*/

/* For every line, we have to capture the start and end position of a number as well as non-period
 * symbols in coordinates. Then, if within a +/- 1 range of the row coordinate there is a match in
 * the column coordinate, e.g. symbol-coordinate column is in a -1 to +1 range to the start or end
 * column coordinate of the number, the number has to be accounted for the total result sum.
 *
 * -> Object which holds the engine schmeatic
 * -> Object which holds a single row, represented as a collection of coordinates
 * --> Coordinates for each start and end point of a found number
 * --> Coordinates for each found non-period symbol
 * --> Function to parse a given string to such a collection of coordinates
 */

use std::collections::HashSet;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::Path;

// Define a struct to represent the content of the file
#[derive(Debug)]
struct FileContent {
    rows: Vec<Row>,
}

impl FileContent {
    // Method to create a new instance of FileContent
    fn new() -> Self {
        FileContent { rows: Vec::new() }
    }

    // Method to read the file and store its content in the object
    fn read_file(&mut self, filename: &str) -> io::Result<()> {
        let file = File::open(filename)?;
        let reader = io::BufReader::new(file);

        for line in reader.lines() {
            if let Ok(line) = line {
                // Parse each line into a Row and add it to the vector
                let row = parse_row(&line, self.rows.len() as u32 + 1);
                self.rows.push(row);
            }
        }

        Ok(())
    }

    // Method to apply the functionality to check if there is a symbol next to any number
    fn check_symbols_next_to_numbers(&self) {
        for (i, row) in self.rows.iter().enumerate() {
            let row_above = self.rows.get(i.wrapping_sub(1));
            let row_below = self.rows.get(i + 1);

            println!("Parse index {} with content  {:?}", i, row);
            // Get a list of numbers in the current row that are next to any symbol in the rows above or below
            // let numbers_next_to_symbol = row.numbers_next_to_symbol(row_above, row_below);

            // Print the result for each row
            // println!("Row {}: {:?}", i + 1, numbers_next_to_symbol);
        }
    }

}

#[derive(Debug)]
struct Row {
    id: u32,
    ranges: Vec<(usize, usize)>,
    non_period_positions: Vec<usize>,
}

impl Row {
    fn numbers_next_to_symbol(&self, row_above: Option<&Row>, row_below: Option<&Row>) -> HashSet<(usize, usize)> {
        let mut result = HashSet::new();

        for (start, end) in &self.ranges {
            // Check horizontal adjacency
            if let Some(row) = row_above {
                if row.non_period_positions.contains(start) || row.non_period_positions.contains(end) {
                    result.insert((*start, *end));
                }
            }

            if let Some(row) = row_below {
                if row.non_period_positions.contains(start) || row.non_period_positions.contains(end) {
                    result.insert((*start, *end));
                }
            }

            // Check vertical adjacency
            if let Some(row) = row_below {
                for &pos in &row.non_period_positions {
                    if pos >= *start && pos <= *end {
                        result.insert((*start, *end));
                        break; // Break to avoid duplicate entries
                    }
                }
            }

            if let Some(row) = row_above {
                for &pos in &row.non_period_positions {
                    if pos >= *start && pos <= *end {
                        result.insert((*start, *end));
                        break; // Break to avoid duplicate entries
                    }
                }
            }

            // Check diagonal adjacency
            if let Some(row) = row_below {
                for &pos in &row.non_period_positions {
                    if pos == *start - 1 || pos == *end + 1 {
                        result.insert((*start, *end));
                        break; // Break to avoid duplicate entries
                    }
                }
            }

            if let Some(row) = row_above {
                for &pos in &row.non_period_positions {
                    if pos == *start - 1 || pos == *end + 1 {
                        result.insert((*start, *end));
                        break; // Break to avoid duplicate entries
                    }
                }
            }
        }

        result
    }
}

fn parse_row(line: &str, id: u32) -> Row {
    let mut ranges = Vec::new();
    let mut non_period_positions = Vec::new();
    let mut current_range: Option<usize> = None;

    for (i, c) in line.chars().enumerate() {
        match c {
            '0'..='9' => {
                if current_range.is_none() {
                    // Start a new range for numbers
                    current_range = Some(i);
                }
            }
            '.' => {
                if let Some(start) = current_range.take() {
                    // End the range for numbers if a period is encountered
                    ranges.push((start, i - 1));
                }
            }
            '#' | '$' | '+' | '*' => {
                // Record the position of non-period symbols
                non_period_positions.push(i);

                // End the range for numbers if it was ongoing
                if let Some(start) = current_range.take() {
                    // End the range for numbers if a period is encountered
                    ranges.push((start, i - 1));
                }
                current_range = None;
            }
            _ => {
                // End the range for numbers if it was ongoing
                current_range = None;
            }
        }
    }

    // Handle the case where a number or symbol continues to the end of the line
    if let Some(start) = current_range {
        ranges.push((start, line.len() - 1));
    }

    Row {
        id,
        ranges,
        non_period_positions,
    }
}

fn main() {
    // Create an instance of FileContent
    let mut file_content = FileContent::new();

    // Specify the path to the input file
    let filename = "./test_input.txt";

    // Read the file and handle any errors
    if let Err(err) = file_content.read_file(filename) {
        eprintln!("Error reading file: {}", err);
        return;
    }

    // Apply the functionality to check if there is a symbol next to any number in each row
    file_content.check_symbols_next_to_numbers();
}
