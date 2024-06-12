fn main() {
    let mut input: Vec<u32> =
        // include_str! is a macro which turns a file into a &'static str
        include_str!("./input.txt")
        // the string is splitted into `Vec<&str>` at `\n\n`
        .split("\n\n")
        .map(
            // For every &str (denoted as `e`) convert this &str
            // into an iterable of strings splitted on `\n`
            // '1000\n2000\n3000\n' => Iterable('1000', '2000', '3000')
            |e| e.lines()
            // Apply a parser for every iterable to an unsigned 32 bit integer (u32)
            .map(
                // Since parse::<u32>() returns an Option, we must unwrap it
                // Finally sum all values to the desired value
                |c| c.parse::<u32>().unwrap()).sum::<u32>())
        .collect::<Vec<u32>>();
    /*
     * Iterator.max() returns Option<Self::Item>
     * hence we receive a reference to a u32 value, e.g. &u32
     * Three options:
     *  1) let part1 = ...              <= Implicite type
     *  2) let part1: &u32 = ...        <= Explicit reference
     *  3) let part1: u32 = *input...   <= Dereferencing
    */
    let part1: u32 = *input.iter().max().unwrap();
    println!("Solution to part 1: {part1}");

    // sort the vector in place
    input.sort_unstable();
    // take the first three elements and sum them
    let part2: u32 = input.iter().rev().take(3).sum::<u32>();
    println!("Solution to part 2: {part2}");
}
