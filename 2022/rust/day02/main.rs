use std::fs::read_to_string;

#[derive(Debug, PartialEq)]
enum Shape {
    Rock,
    Paper,
    Scissors,
}

#[derive(Debug)]
enum Outcome {
    Win,
    Draw,
    Loss,
}

// PART 2
impl Outcome {
    fn from_str(s: &str) -> Option<Outcome> {
        match s {
            "X" => Some(Outcome::Loss),
            "Y" => Some(Outcome::Draw),
            "Z" => Some(Outcome::Win),
            _ => None,
        }
    }
}

impl Shape {
    // Convert string input to value of type Shape
    fn from_str(s: &str) -> Option<Shape> {
        match s {
            "A" | "X" => Some(Shape::Rock),
            "B" | "Y" => Some(Shape::Paper),
            "C" | "Z" => Some(Shape::Scissors),
            _ => None,
        }
    }

    // Play against another player
    fn play_against(&self, other: &Shape) -> Outcome {
        match (self, other) {
            (Shape::Rock, Shape::Scissors) => Outcome::Win,
            (Shape::Rock, Shape::Paper) => Outcome::Loss,
            (Shape::Rock, Shape::Rock) => Outcome::Draw,
            (Shape::Paper, Shape::Rock) => Outcome::Win,
            (Shape::Paper, Shape::Scissors) => Outcome::Loss,
            (Shape::Paper, Shape::Paper) => Outcome::Draw,
            (Shape::Scissors, Shape::Paper) => Outcome::Win,
            (Shape::Scissors, Shape::Rock) => Outcome::Loss,
            (Shape::Scissors, Shape::Scissors) => Outcome::Draw,
        }
    }

    // PART 2
    fn shape_for_outcome(opponent_shape: &Shape, outcome: &Outcome) -> Shape {
        match (opponent_shape, outcome) {
            (Shape::Rock, Outcome::Draw) => Shape::Rock,
            (Shape::Paper, Outcome::Draw) => Shape::Paper,
            (Shape::Scissors, Outcome::Draw) => Shape::Scissors,

            (Shape::Rock, Outcome::Win) => Shape::Paper,
            (Shape::Paper, Outcome::Win) => Shape::Scissors,
            (Shape::Scissors, Outcome::Win) => Shape::Rock,

            (Shape::Rock, Outcome::Loss) => Shape::Scissors,
            (Shape::Paper, Outcome::Loss) => Shape::Rock,
            (Shape::Scissors, Outcome::Loss) => Shape::Paper,
        }
    }
}

struct Round {
    opponent_shape: Shape,
    shape: Shape,
}

impl Round {
    fn score(&self) -> u64 {
        let shape_score = match self.shape {
            Shape::Rock => 1,
            Shape::Paper => 2,
            Shape::Scissors => 3,
        };

        let outcome_score = match self.shape.play_against(&self.opponent_shape) {
            Outcome::Win => 6,
            Outcome::Draw => 3,
            Outcome::Loss => 0,
        };

        shape_score + outcome_score
    }
}

fn calculate_total_score(strategy_guide: &str, rigged_outcome: bool) -> u64 {
    strategy_guide
        .lines()
        .filter_map(|line| {
            // Parse the shapes
            let shapes: Vec<&str> = line.split_whitespace().collect();
            if shapes.len() != 2 {
                return None;
            }
            let opponent_shape: Shape = Shape::from_str(shapes[0]).unwrap();
            let shape: Shape;
            // PART 2
            if rigged_outcome {
                let outcome: Outcome = Outcome::from_str(shapes[1]).unwrap();
                shape = Shape::shape_for_outcome(&opponent_shape, &outcome);
            } else {
                shape = Shape::from_str(shapes[1]).unwrap();
            }
            // Convert to a game round
            Some(Round {
                    opponent_shape,
                    shape,
                })
        })
        // For each round, calculate the score
        .map(|round| round.score())
        .sum()
}

fn main() {
    let strategy_guide: String = read_to_string("./input.txt").expect("Could not read file");
    let part1: u64 = calculate_total_score(&strategy_guide, false);
    println!("Solution for Part 1: {}", part1);
    let part2: u64 = calculate_total_score(&strategy_guide, true);
    println!("Solution for Part 2: {}", part2);
}
