use anyhow::*;
use std::fs::File;
use std::io::{BufRead, BufReader};
use code_timing_macros::time_snippet;
use const_format::concatcp;
use adv_code_2024::*;

const DAY: &str = "01";
const INPUT_FILE: &str = concatcp!("input/", DAY, ".txt");

const TEST: &str = "\
1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
";

fn extract_two_digit_number(s: &str) -> Option<u32> {
    let chars: Vec<char> = s.chars().collect();
    let first_digit = chars.iter().find(|&&c| c.is_digit(10));
    let last_digit = chars.iter().rev().find(|&&c| c.is_digit(10));

    match (first_digit, last_digit) {
        (Some(&first), Some(&last)) => {
            let tens = first.to_digit(10).unwrap();
            let units = last.to_digit(10).unwrap();
            Some(tens * 10 + units)
        }
        _ => None, // Return None if there are no digits in the string
    }
}

fn main() -> Result<()> {
    start_day(DAY);

    //region Part 1
    println!("=== Part 1 ===");

    fn part1<R: BufRead>(reader: R) -> Result<usize> {
        let answer = reader.lines()
            .filter_map(|line| line.ok().and_then
                (|s|extract_two_digit_number(&s)))
            .reduce(|a, b| a + b);
        answer.map_or(Ok(0), |sum| Ok(sum as usize))
    }

    assert_eq!(142, part1(BufReader::new(TEST.as_bytes()))?);

    let input_file = BufReader::new(File::open(INPUT_FILE)?);
    let result = time_snippet!(part1(input_file)?);
    println!("Result = {}", result);
    //endregion

    //region Part 2
    // println!("\n=== Part 2 ===");
    //
    // fn part2<R: BufRead>(reader: R) -> Result<usize> {
    //     Ok(0)
    // }
    //
    // assert_eq!(0, part2(BufReader::new(TEST.as_bytes()))?);
    //
    // let input_file = BufReader::new(File::open(INPUT_FILE)?);
    // let result = time_snippet!(part2(input_file)?);
    // println!("Result = {}", result);
    //endregion

    Ok(())
}
