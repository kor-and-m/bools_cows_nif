extern crate bools;

use std::env;
use bools::{test_game, random_guess};

fn main() -> () {
	let real: Vec<u8> = match env::args().skip(1).next() {
		Some(g) => parse_guess(g),
		None => random_guess(),
	};
    println!("{:?} {:?}", test_game(&real), real);
}

fn parse_guess(guess: String) -> Vec<u8> {
	let i = guess.chars().collect::<Vec<char>>();
	match i.len() {
		4 => {
		  let map_numbers = i.into_iter().map(|x| x.to_string().parse::<u8>());
		  if map_numbers.clone().any(|x| {
		  	match x {
		  		Err(_) => true,
		  		_ => false,
		  	}
		  }) {
		  	random_guess()
		  } else {
		  	map_numbers.map(|x| x.unwrap()).collect()
		  }
		},
		_ => random_guess(),
	}
}
