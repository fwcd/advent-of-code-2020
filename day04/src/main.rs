use std::{collections::HashMap, io::{BufRead, BufReader}};
use std::fs::File;
use regex::Regex;

fn parse_passport(line: &str) -> HashMap<String, String> {
    let mut passport = HashMap::new();
    let re = Regex::new("(\\S+):(\\S+)").unwrap();
    for pair in re.captures_iter(line) {
        passport.insert(pair[1].to_owned(), pair[2].to_owned());
    }
    passport
}

fn parse_height(raw: &str) -> Option<(i32, String)> {
    let re = Regex::new("(\\d+)(\\w+)").unwrap();
    let cs = re.captures(raw)?;
    Some((cs[1].parse::<i32>().ok()?, cs[2].to_owned()))
}

fn is_hair_color(raw: &str) -> bool {
    Regex::new("#[0-9a-f]{6}").unwrap().is_match(raw)
}

fn is_eye_color(raw: &str) -> bool {
    Regex::new("(?:amb|blu|brn|gry|grn|hzl|oth)").unwrap().is_match(raw)
}

fn is_passport_id(raw: &str) -> bool {
    Regex::new("[0-9]{9}").unwrap().is_match(raw)
}

fn is_valid_part1(pp: &HashMap<String, String>) -> bool {
    let required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];
    required.iter().all(|&r| pp.keys().any(|k| k.as_str() == r))
}

fn is_valid_part2(pp: &HashMap<String, String>) -> bool {
    is_valid_part1(&pp)
        && pp.get("byr").and_then(|raw| raw.parse::<i32>().ok()).filter(|&byr| byr >= 1920 && byr <= 2002).is_some()
        && pp.get("iyr").and_then(|raw| raw.parse::<i32>().ok()).filter(|&byr| byr >= 2010 && byr <= 2020).is_some()
        && pp.get("eyr").and_then(|raw| raw.parse::<i32>().ok()).filter(|&byr| byr >= 2020 && byr <= 2030).is_some()
        && pp.get("hgt").and_then(|raw| parse_height(raw)).filter(|(h, u)| if u == "cm" { *h >= 150 && *h <= 193 } else { *h >= 59 && *h <= 76 }).is_some()
        && pp.get("hcl").filter(|raw| is_hair_color(raw)).is_some()
        && pp.get("ecl").filter(|raw| is_eye_color(raw)).is_some()
        && pp.get("pid").filter(|raw| is_passport_id(raw)).is_some()
}

fn main() {
    let lines = BufReader::new(File::open("resources/input.txt").unwrap())
        .lines()
        .map(|l| l.unwrap()).collect::<Vec<_>>();

    let pps = lines.split(|l| l.is_empty())
        .map(|raw| parse_passport(raw.join(" ").as_str()))
        .collect::<Vec<_>>();

    println!("Part 1: {}", pps.iter().filter(|pp| is_valid_part1(pp)).count());
    println!("Part 2: {}", pps.iter().filter(|pp| is_valid_part2(pp)).count());
}
