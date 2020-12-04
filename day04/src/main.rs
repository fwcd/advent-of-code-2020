use std::{collections::HashMap, io::{BufRead, BufReader}};
use std::fs::File;
use regex::Regex;

fn parse_passport(line: &str) -> HashMap<String, String> {
    let mut passport = HashMap::new();
    let re = Regex::new("(\\S+):(\\S+)").unwrap();
    for pair in re.captures_iter(line) {
        passport.insert(pair[1].to_string(), pair[2].to_string());
    }
    return passport;
}

fn is_valid_passport(pp: HashMap<String, String>) -> bool {
    let mut keys = pp.keys();
    keys.all(|k| ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"].contains(&k.as_str()))
}

fn main() {
    let lines = BufReader::new(File::open("resources/input.txt").unwrap()).lines().map(|l| l.unwrap()).collect::<Vec<_>>();
    let mut valid_count = 0;

    for raw in lines.split(|l| l.is_empty()) {
        let pp = parse_passport(raw.join(" ").as_str());
        if is_valid_passport(pp) {
            valid_count += 1;
        }
    }

    println!("Part 1: {}", valid_count);
}
