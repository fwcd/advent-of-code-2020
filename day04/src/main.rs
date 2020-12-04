use std::io::{BufRead, BufReader};
use std::fs::File;

fn main() {
    let lines = BufReader::new(File::open("resources/input.txt").unwrap()).lines();
    println!("Hello, world!");
}
