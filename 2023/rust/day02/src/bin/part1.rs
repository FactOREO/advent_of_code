/*
--- Day 2: Cube Conundrum ---

You're launched high into the atmosphere! The apex of your trajectory just barely reaches the surface of a large island floating in the sky.
You gently land in a fluffy pile of leaves.
It's quite cold, but you don't see much snow.
An Elf runs over to greet you.

The Elf explains that you've arrived at Snow Island and apologizes for the lack of snow.
He'll be happy to explain the situation, but it's a bit of a walk, so you have some time.
They don't get many visitors up here; would you like to play a game in the meantime?

As you walk, the Elf shows you a small bag and some cubes which are either red, green, or blue.
Each time you play this game, he will hide a secret number of cubes of each color in the bag, and your goal is to figure out information about the number of cubes.

To get information, once a bag has been loaded with cubes, the Elf will reach into the bag, grab a handful of random cubes, show them to you, and then put them back in the bag.
He'll do this a few times per game.

You play several games and record the information from each game (your puzzle input).
Each game is listed with its ID number (like the 11 in Game 11: ..) followed by a semicolon-separated list of subsets of cubes that were revealed from the bag (like 3 red, 5 green, 4 blue).

For example, the record of a few games might look like this:

Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green

In game 1, three sets of cubes are revealed from the bag (and then put back again).
The first set is 3 blue cubes and 4 red cubes; the second set is 1 red cube, 2 green cubes, and 6 blue cubes; the third set is only 2 green cubes.

The Elf would first like to know which games would have been possible if the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

In the example above, games 1, 2, and 5 would have been possible if the bag had been loaded with that configuration.
However, game 3 would have been impossible because at one point the Elf showed you 20 red cubes at once; similarly, game 4 would also have been impossible because the Elf showed you 15 blue cubes at once.
If you add up the IDs of the games that would have been possible, you get 8.

Determine which games would have been possible if the bag had been loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes.
What is the sum of the IDs of those games?
*/

fn main() {
    // Initialize the true number of cubes for each color
    let true_cubes: Draw = Draw{
        red: 12,
        green: 13,
        blue: 14,
    };
    println!("True Cube distribution: \n{:?}\n", true_cubes);
    // Initialize the final output variables
    let mut output: u32 = 0;
    let mut num_games: u32 = 0;
    // Get a collection of games
    let games_input = [
        "5 red, 1 green; 6 red, 3 blue; 9 red; 1 blue, 1 green, 4 red; 1 green, 2 blue; 2 blue, 1 red",
        "12 red, 2 green, 9 blue; 8 red, 12 blue; 9 red, 1 blue, 2 green; 12 blue, 8 red, 2 green; 4 red, 5 blue; 1 green, 9 blue, 10 red",
        "2 red, 8 blue, 5 green; 10 red, 10 green, 7 blue; 9 green, 13 red, 5 blue",
        "3 red; 1 blue, 3 green, 3 red; 3 blue, 8 green, 5 red; 8 green, 2 blue",
        "8 blue, 1 red, 1 green; 1 blue, 1 red, 7 green; 7 green, 5 blue",
        "2 red, 8 blue; 5 blue, 4 green; 5 blue, 4 red, 3 green; 3 red, 9 blue; 5 green, 9 blue, 7 red; 6 red, 9 blue, 5 green",
        "3 green, 4 blue; 7 green, 3 red, 3 blue; 7 red, 4 green, 6 blue; 4 blue, 1 green, 5 red; 6 blue, 2 red, 7 green; 1 green, 4 blue, 4 red",
        "2 blue, 12 red; 1 green, 2 blue, 10 red; 12 red, 10 blue; 5 red, 1 green, 2 blue; 13 red, 16 blue, 1 green; 2 blue, 18 red",
        "5 green, 2 red, 13 blue; 5 green, 1 red; 7 green, 8 blue, 1 red; 16 blue; 5 blue, 2 green, 3 red",
        "5 blue, 2 red; 13 blue, 3 red, 5 green; 2 red, 3 blue; 6 red, 9 blue, 5 green; 9 blue, 3 green, 3 red",
        "9 red, 4 green, 3 blue; 8 blue, 8 red; 1 green, 13 blue; 11 blue, 8 red, 4 green",
        "1 green; 1 blue, 4 red, 6 green; 1 red, 8 green; 7 red, 1 blue, 3 green; 8 green, 1 blue, 8 red",
        "5 blue, 14 red, 1 green; 10 red, 4 blue, 1 green; 6 red, 1 green, 3 blue; 7 blue, 4 red; 14 red, 2 blue, 1 green",
        "9 green, 1 blue; 6 red, 9 green; 7 green, 3 blue",
        "1 green, 4 blue, 2 red; 8 red, 3 blue; 1 red, 3 blue",
        "6 blue, 10 green, 5 red; 11 blue, 1 red; 3 red, 2 green, 3 blue; 1 green, 4 red, 6 blue; 11 blue, 2 red, 11 green",
        "4 green, 5 blue, 8 red; 6 green, 7 red; 3 green, 8 red, 5 blue; 4 green, 8 red; 5 green, 11 blue, 3 red; 5 green, 15 blue, 1 red",
        "7 green, 17 blue, 14 red; 4 green, 11 red, 7 blue; 13 blue, 8 green; 3 red, 7 green, 11 blue",
        "13 red, 13 blue, 1 green; 7 blue, 1 red, 5 green; 18 blue, 1 green, 11 red; 4 blue, 13 red, 6 green",
        "1 blue, 6 green, 7 red; 5 green, 4 red, 1 blue; 1 blue, 1 red, 4 green",
        "3 red, 5 green, 7 blue; 6 green, 10 blue, 5 red; 9 red, 1 blue; 5 green, 8 blue, 9 red; 2 red, 11 green, 4 blue; 5 red, 6 blue",
        "8 blue, 1 red, 1 green; 10 blue; 13 blue, 1 green, 4 red; 1 green, 8 blue, 4 red; 3 blue, 1 red",
        "2 blue; 3 blue, 5 green; 6 blue, 5 green, 2 red; 1 green",
        "1 red, 13 green, 4 blue; 16 green, 4 blue; 4 red, 5 blue, 11 green; 15 green, 5 red, 10 blue; 16 green, 1 red; 5 red, 3 blue, 10 green",
        "12 red, 7 green, 6 blue; 5 blue, 5 red, 12 green; 12 green, 3 red; 5 green, 5 red, 3 blue",
        "2 green, 11 blue; 5 red, 3 blue; 5 red, 12 blue, 3 green",
        "3 green, 5 red, 12 blue; 3 red, 11 blue, 1 green; 3 red, 4 blue, 3 green; 3 red, 9 green, 9 blue; 14 blue, 1 green, 12 red",
        "8 green, 9 blue; 6 green, 8 red, 1 blue; 7 red, 6 green; 12 red, 2 blue, 2 green",
        "16 red, 4 green, 1 blue; 10 red, 7 green, 2 blue; 3 green, 8 red; 2 blue, 1 red, 4 green; 2 blue, 10 red, 9 green; 7 green, 1 blue, 18 red",
        "13 red, 3 blue; 3 blue, 1 green, 10 red; 15 red, 3 blue; 1 green, 1 red, 1 blue; 16 red, 1 green, 3 blue",
        "5 red, 8 blue, 3 green; 5 green, 7 blue, 13 red; 9 red, 3 green, 10 blue; 15 red, 1 green, 7 blue; 5 green, 12 blue, 2 red; 6 blue, 13 red",
        "2 blue, 8 red, 1 green; 3 green, 2 blue, 11 red; 2 green, 6 red; 13 red, 3 green, 2 blue; 6 red",
        "5 blue, 5 red; 8 red, 1 green, 7 blue; 1 green, 6 red",
        "3 blue; 3 green, 2 blue, 2 red; 3 green, 1 blue, 3 red; 3 blue",
        "4 red, 2 blue, 6 green; 4 blue, 9 red, 10 green; 3 blue, 8 green, 1 red; 3 red, 1 green, 4 blue; 4 green, 7 blue; 8 red, 8 green, 2 blue",
        "1 red, 3 blue, 12 green; 16 green, 7 blue, 1 red; 1 blue, 1 red, 9 green; 2 blue, 2 green; 8 green, 2 red",
        "2 green, 8 red; 3 blue, 1 red, 2 green; 15 red, 7 blue; 1 green, 16 red, 15 blue; 13 red, 9 blue",
        "9 red, 9 blue, 5 green; 3 red, 19 blue, 8 green; 15 blue, 11 red, 6 green; 10 red, 19 blue, 5 green; 8 blue, 7 green, 6 red; 7 red, 6 green, 10 blue",
        "8 blue, 13 green, 2 red; 16 blue, 9 green; 19 blue, 1 green; 1 red, 3 blue, 9 green; 1 green, 18 blue",
        "6 red, 5 blue; 11 green, 15 blue, 7 red; 10 blue, 5 green, 10 red; 13 blue, 9 red, 11 green; 2 green, 14 blue, 12 red; 6 red, 6 green",
        "10 green, 1 red, 1 blue; 1 red, 9 green; 1 green; 6 green, 1 blue, 2 red; 1 blue, 8 green, 1 red; 9 green",
        "13 green, 2 blue; 10 blue, 1 red; 10 red, 10 green, 8 blue; 16 green, 8 blue, 6 red; 9 red, 18 green; 10 red, 15 blue, 1 green",
        "3 red, 16 blue, 6 green; 1 red, 17 blue, 12 green; 19 blue, 2 red, 16 green; 12 green, 7 blue, 1 red; 8 green, 7 blue, 2 red; 12 green, 9 blue",
        "4 red, 3 green, 2 blue; 18 blue, 3 green; 7 red, 7 blue, 4 green",
        "1 green, 3 blue, 7 red; 9 green, 8 red, 2 blue; 5 green, 3 blue, 6 red",
        "18 blue; 4 blue, 1 red, 5 green; 2 red, 15 blue, 7 green",
        "17 red, 4 green, 12 blue; 6 green, 1 red, 2 blue; 3 blue, 13 green, 4 red",
        "11 red, 2 green; 8 red, 3 green; 2 green, 5 blue, 9 red; 3 green, 2 blue, 5 red",
        "2 green, 12 blue, 9 red; 1 green, 12 red; 1 green, 2 blue, 18 red; 8 blue, 19 red; 1 green, 5 blue; 3 blue, 10 red, 1 green",
        "5 green, 1 red; 7 red, 3 blue, 9 green; 15 blue, 4 green, 4 red",
        "12 green, 14 blue; 2 red, 5 green, 16 blue; 4 red, 17 blue, 16 green; 6 blue, 16 green, 2 red; 17 blue, 13 green, 5 red",
        "7 green, 10 red, 2 blue; 6 green, 12 red, 3 blue; 10 red, 3 blue, 8 green; 3 blue, 1 green, 8 red; 6 green, 5 blue, 3 red",
        "8 blue, 3 green; 7 green, 11 blue, 1 red; 1 red, 7 blue, 9 green; 1 blue, 1 green; 4 green, 1 red; 1 red, 8 blue",
        "1 blue, 1 green, 4 red; 1 red, 1 blue, 13 green; 11 red, 11 green, 1 blue",
        "5 blue, 4 red, 11 green; 13 green, 9 blue, 3 red; 3 red, 7 green, 8 blue; 2 red, 20 blue, 2 green; 3 red, 10 blue, 1 green; 12 green, 5 red, 8 blue",
        "17 red, 2 green, 1 blue; 13 blue, 8 green, 6 red; 1 green, 9 blue, 6 red; 2 blue",
        "9 green, 1 red, 9 blue; 15 green, 10 blue, 1 red; 5 blue, 3 red, 10 green",
        "14 red, 2 blue, 14 green; 17 red, 7 blue, 10 green; 4 green, 13 red, 11 blue; 3 green, 13 red, 5 blue; 13 red, 6 blue; 1 red, 7 green, 2 blue",
        "16 blue, 7 red, 2 green; 7 green, 10 red, 12 blue; 4 red, 9 green, 14 blue; 8 blue, 11 green, 1 red; 3 blue, 5 red, 11 green",
        "1 blue; 9 red, 4 green; 3 green, 3 blue, 1 red; 3 red, 1 blue",
        "2 green, 15 red, 12 blue; 9 green, 1 blue, 10 red; 14 blue, 17 red, 2 green; 12 red, 6 blue, 3 green; 8 green, 10 blue, 10 red; 2 green, 10 red, 2 blue",
        "12 red, 6 blue, 1 green; 2 red, 1 green, 4 blue; 10 blue, 12 red, 4 green; 5 green, 8 red, 8 blue",
        "3 green, 3 red; 7 red; 2 green, 1 blue, 7 red; 5 red, 1 green",
        "5 green, 11 red; 4 green, 2 blue, 7 red; 7 red, 11 blue, 3 green; 8 blue, 5 green, 5 red; 8 red, 4 blue",
        "5 red, 5 blue; 15 green, 3 blue; 3 blue, 3 red, 8 green; 1 blue, 3 red, 5 green",
        "8 green, 5 blue, 12 red; 10 red, 5 blue, 11 green; 12 red, 3 blue, 2 green; 5 green, 1 blue, 10 red; 15 red, 5 green, 3 blue; 2 red, 8 blue",
        "12 blue, 3 red; 4 blue, 4 red, 1 green; 9 green, 14 blue, 3 red; 2 red, 13 blue, 6 green; 17 blue, 5 green",
        "1 blue, 4 red, 11 green; 11 green, 4 red, 7 blue; 11 green, 7 blue; 14 green, 2 blue, 1 red; 2 blue, 4 red",
        "4 red, 1 green; 5 red, 2 green, 3 blue; 1 red, 7 blue; 8 red, 6 blue, 1 green; 2 green, 6 red, 1 blue; 6 red, 8 blue, 2 green",
        "6 blue, 2 green, 4 red; 1 green, 5 blue; 1 blue, 3 red; 2 red; 2 red, 17 blue",
        "9 blue, 2 green, 1 red; 7 blue, 2 green, 3 red; 12 red, 13 blue; 15 blue, 1 green, 1 red",
        "15 blue, 16 red, 18 green; 16 red, 12 blue, 14 green; 3 blue, 12 red, 4 green; 8 green, 17 blue, 15 red; 15 blue, 18 green, 4 red; 5 blue, 3 red, 10 green",
        "17 blue, 3 red, 19 green; 10 blue, 15 green, 18 red; 4 green, 15 red; 1 green, 17 blue, 14 red; 16 red, 1 green, 4 blue",
        "6 green, 2 blue, 5 red; 1 blue, 9 green; 5 red, 1 blue, 10 green; 4 green, 11 red, 1 blue",
        "4 blue; 4 green, 6 blue; 2 green, 2 blue, 4 red",
        "5 blue; 5 green, 5 red; 9 red, 1 blue, 1 green; 5 green; 3 green, 6 red, 1 blue",
        "2 red, 10 blue, 6 green; 1 red, 6 blue, 6 green; 9 blue, 2 green, 8 red; 12 blue, 7 green, 18 red",
        "2 red, 5 blue, 2 green; 2 blue, 4 green, 6 red; 4 blue, 4 green, 3 red; 3 red, 5 green; 2 red, 4 green, 4 blue",
        "14 red, 6 blue, 1 green; 6 blue, 18 red, 2 green; 1 green; 8 red, 5 green; 1 blue, 7 red, 6 green; 11 red, 1 blue",
        "5 blue, 4 red; 19 blue, 7 red; 6 red, 1 green, 12 blue; 1 green, 8 red",
        "7 green, 6 red, 9 blue; 14 blue, 8 green; 15 green, 6 red, 4 blue; 1 red, 7 blue, 19 green",
        "1 red, 4 blue; 3 blue, 1 red, 5 green; 3 blue, 5 green, 12 red; 17 red, 2 blue; 4 blue, 1 red",
        "6 blue, 11 green, 18 red; 11 red, 7 blue, 2 green; 13 red, 14 blue, 14 green; 1 red, 3 blue, 16 green",
        "2 red, 5 blue, 3 green; 9 red, 7 blue, 2 green; 4 green, 9 red, 3 blue; 1 blue, 1 green, 5 red",
        "1 red, 10 green, 15 blue; 9 green, 1 red; 1 red, 2 green, 12 blue",
        "1 green, 5 blue, 8 red; 10 green, 8 red, 9 blue; 6 green, 3 red, 8 blue; 9 red, 3 green, 8 blue; 2 red, 6 blue, 1 green",
        "13 red, 12 green; 6 blue, 5 green, 3 red; 10 green, 1 blue, 17 red; 9 green, 6 blue, 2 red; 1 blue, 9 green, 14 red",
        "4 green, 1 blue, 7 red; 6 green, 2 red, 1 blue; 13 red, 7 green",
        "2 blue, 7 green, 10 red; 6 green, 5 blue; 12 red, 4 blue, 5 green; 15 red, 8 blue; 6 blue, 8 red, 3 green; 14 red, 11 green, 16 blue",
        "8 green, 8 red; 5 green, 1 blue, 13 red; 3 blue, 1 green, 3 red; 11 red, 2 green; 9 red, 7 green, 1 blue",
        "4 green, 13 red, 10 blue; 11 blue, 4 red; 10 blue, 9 green; 9 green, 4 blue, 12 red; 7 green, 4 red, 1 blue",
        "6 blue; 10 green, 1 red, 11 blue; 5 blue, 5 green; 6 green, 1 red; 1 red, 6 green",
        "2 red, 15 blue, 4 green; 13 red, 11 green; 6 green, 1 blue, 6 red; 6 red, 5 blue, 10 green; 2 blue, 11 green, 18 red",
        "2 red, 13 blue, 3 green; 15 blue, 4 red, 2 green; 4 green, 9 blue, 7 red; 12 blue, 6 red, 11 green; 20 blue, 13 red, 11 green",
        "6 blue, 1 red, 10 green; 10 red, 5 blue, 7 green; 9 red, 13 green, 10 blue; 11 blue, 9 red, 8 green",
        "2 red, 7 green, 16 blue; 20 green, 2 red, 14 blue; 5 red, 15 green, 15 blue; 4 blue, 6 red, 15 green; 6 green, 6 red, 10 blue",
        "1 red, 1 blue, 14 green; 10 green, 12 red, 1 blue; 10 red, 2 green, 1 blue; 1 blue, 3 green, 14 red; 3 red, 2 blue, 13 green; 1 blue, 3 green, 13 red",
        "13 blue, 1 green; 18 green, 6 red, 3 blue; 11 blue, 7 red, 9 green; 4 red, 6 green, 11 blue; 12 blue, 6 red, 8 green",
        "4 blue; 1 red, 2 green, 11 blue; 12 blue, 1 green, 1 red; 11 blue, 6 green; 1 red, 7 green, 8 blue",
        "10 blue, 5 green; 4 green, 3 red, 6 blue; 2 green, 4 red, 1 blue"
    ];
    
    // Initialize the Game struct, which will contain all the Draw objects per game
    let mut games: Vec<Game> = Vec::new();
    // Now add all the draws per game
    for game_data in &games_input {
        let mut game = Game::new();
        for draw_string in game_data.split(";") {
            let draw = get_draw_from_string(draw_string.trim());
            game.add_draw(draw);
        }
        games.push(game);
    }
    // For each game in games, check if the draws are possible, e.g. every draw is less than the
    // true cubes distribution. If so, add 1 to the possible games counter and the game ID to the
    // output variable, if not skip the game.
    for (game_index, game) in games.iter().enumerate() {
        // println!("Game {}:", game_index + 1);
        let mut possible_game: bool = true;
        for (_, draw) in game.draws.iter().enumerate() {
            // println!("Draw {}: {:?}", draw_index + 1, draw);
            if ! draw.check_draw(&true_cubes) {
                possible_game = false;
            }
        }
        if possible_game {
           output += game_index as u32 + 1;
           num_games += 1;
        }
        println!(); // Add a newline between games
    }
    println!("The number of possible games is {} with a score of {}", num_games, output);
}

// Input: A collection of games.
//        Each game is represented by a number of drawn cubes
//        Those can be of three different colors (red, blue or green)
//        Each draw within a game is separated by a semicolon
//        The number of independent draws is random (it cannot be assumed it is the same amount of
//        draws per game)

// A struct which contains the game data as Draw objects
#[derive(Debug)]
#[derive(Clone)]
struct Game {
    draws: Vec<Draw>,
}

// For each possible game input, we can add a new Draw object from get_draw_from_string to the
impl Game {
    fn new() -> Game {
        Game { draws: Vec::new() }
    }

    fn add_draw(&mut self, draw: Draw) {
        self.draws.push(draw);
    }
}

// A struct containing the amount of drawn red / green / blue balls
#[derive(Debug)]
#[derive(Copy, Clone)]
struct Draw {
    red: u32,
    blue: u32,
    green: u32,
}

impl Draw {
    // Check, if the given draw is possible with respect to the true distribution
    fn check_draw(&self, other: &Draw) -> bool {
        self.red <= other.red && self.green <= other.green && self.blue <= other.blue
    }
}

// A function to convert a given string into a Draw struct
// String format: [[:digit:]]*\s[color] separated by commas
fn get_draw_from_string(string: &str) -> Draw{
    // Split the string at every comma and trim white space
    let draws: Vec<&str> = string.split(',').map(|s| s.trim()).collect();
    // Create (number, color) tuples
    let tuples: Vec<(u32, &str)> = draws
        .iter()
        .map(|part| {
            let mut iter = part.split_whitespace();
            let number = iter.next().unwrap().parse().unwrap_or(0);
            let color = iter.next().unwrap_or("");
            (number, color)
        })
        .collect();
    // Instantiate the Draw struct
    let mut draw = Draw { red: 0, green: 0, blue: 0 };
    // Iterate over the (number, color) tuples
    for (number, color) in tuples {
        match color {
            "red" => draw.red += number,
            "green" => draw.green += number,
            "blue" => draw.blue += number,
            _ => println!("Unknown color: {}", color),
        }
    }
    draw
}
