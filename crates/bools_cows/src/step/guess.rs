use std::fmt;
use rand::Rng;

const GUESS_COUNT: u32 = 5040;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Guess {
	first: u8,
	second: u8,
	third: u8,
	forth: u8,
}

impl Guess {
	pub fn new(f: u8, s: u8, t: u8, fo: u8) -> Guess {
		Guess {first: f, second: s, third: t, forth: fo}
	}

	pub fn random() -> Guess {
		let x = rand::thread_rng().gen_range(0, GUESS_COUNT as usize - 1);
		init()[x]
    }

	pub fn to_vec(&self) -> Vec<u8> {
		vec![self.first, self.second, self.third, self.forth]
	}

	pub fn is_guess(&self) -> bool {
		init().contains(self)
	}
}

#[test]
fn struct_test_vec() {
	let g = Guess::new(1, 6, 7, 3);
	assert_eq!(g.to_vec(), vec![1, 6, 7, 3]);
}

impl fmt::Display for Guess {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}{}{}{}", self.first, self.second, self.third, self.forth)
	}
}

#[test]
fn init_test_count() {
	assert_eq!(init().len(), GUESS_COUNT as usize);
}

#[test]
fn init_test_first() {
	assert_eq!(init()[0], Guess::new(0, 1, 2, 3));
}

#[test]
fn init_test_last() {
	assert_eq!(init()[5039], Guess::new(9, 8, 7, 6));
}

pub fn init() -> Vec<Guess> {
	let mut result: Vec<Guess> = vec![];
	for x1 in 0..10 {
	  for x2 in 0..10 {
	  	for x3 in 0..10 {
	  	  for x4 in 0..10 {
	  		if x1 != x2 && x1 != x3 && x1 != x4 && x2 != x3 && x2 != x4 && x3 != x4 {
	  		  let g: Guess = Guess::new(x1, x2, x3, x4);
	  		  result.push(g);
	  		}
	  	  }
	    }
	  }
	};
	result
}

#[test]
fn init_chunk_count() {
	assert_eq!(init_chunked(10).len(), 10);
	assert_eq!(init_chunked(100).len(), 101); // Округление приростило 1
}

pub fn init_chunked(size: u32) -> Vec<Vec<Guess>> {
	let all  = init();
	let mut chunks = Vec::new();
	let chunk_size_float: f32 = (GUESS_COUNT as f32) / (size as f32);
	let chunk_size = chunk_size_float.round() as usize;
    for chunk in all.chunks(chunk_size) {
        chunks.push(chunk.to_owned());
    };
    chunks
}