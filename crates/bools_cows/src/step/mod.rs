use std::fmt;
use std::thread;

pub mod guess;
pub mod result;

#[derive(Debug, Copy, Clone)]
pub struct Step {
	pub guess: guess::Guess,
	pub result: result::ResultStep,
}

impl fmt::Display for Step {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
	    write!(f, "(guess: {}, result: {})", self.guess.to_string(), self.result.to_string())
	}
}

impl Step {

	#[allow(dead_code)]
	pub fn new(guess: guess::Guess, result: result::ResultStep) -> Step {
		Step { result: result, guess: guess }
	}

	pub fn calc(guess: &guess::Guess, real: &guess::Guess) -> Step {
		let r = result::ResultStep::calc(guess.to_vec(), real.to_vec());
		Step { result: r, guess: guess.clone() }
	}
}

pub fn guess_game(actual: &guess::Guess) -> Vec<Step> {
	let mut history = vec![];
	loop {
		let next_guess = make_step_concurency(&history, 20);
		let next_step = Step::calc(&next_guess, actual);
		history.push(next_step);
		let Step {guess: _g, result: r} = next_step;
		if r == result::ResultStep::new(4, 0) {
			break
		}
	}
	history
}

#[allow(dead_code)]
pub fn make_step(history: &Vec<Step>) -> guess::Guess {
	if history.len() == 0 {
		return guess::Guess::random();
	}
	let all = guess::init();
	let available = filter_guess(history, &all);
	if available.len() == 1 {
		return available[0];
	}
	let mut guess = &guess::Guess::new(1,2,3,4);
	let mut min_weight: u32 = 10000;
	for elem in &all {
		let max_weight = check_weight(&elem, &available);
		if max_weight < min_weight {
			min_weight = max_weight;
			guess = elem;
		}
	}
	guess.clone()
}

pub fn make_step_concurency(history: &Vec<Step>, s: u32) -> guess::Guess {
	if history.len() == 0 {
		return guess::Guess::random();
	}
	let all = guess::init();
	let available = filter_guess(history, &all);

	if available.len() == 1 {
		return available[0];
	}

	let chunks = guess::init_chunked(s);

    let handles: Vec<_> = chunks.into_iter().map(|elem_list| {
    	let a = available.clone();
        thread::spawn(move || {
			elem_list.into_iter().map(|elem| {
				let w = check_weight(&elem, &a);
                (w, elem)
			}).collect::<Vec<_>>()
        })
    }).collect();

    let (_weight, guess) = handles.into_iter().map(|h| {
    	h.join().unwrap()
    }).flat_map(|x| x).max_by(|x, y| {
	    let (wx, _) = x;
		let (wy, _) = y;
		wy.cmp(wx)
	}).unwrap();

	guess
}


fn check_weight(g: &guess::Guess, available: &Vec<guess::Guess>) -> u32 {
	let mut elems = vec![0; 16];
	for may_be_guess in available {
		let v1 = may_be_guess.to_vec();
		let v2 = g.to_vec();
		let res = result::ResultStep::calc(v1, v2).code() as usize;
		elems[res] += 1;
	}
	elems.into_iter().max().unwrap()
}

fn filter_guess(history: &Vec<Step>, available: &Vec<guess::Guess>) -> Vec<guess::Guess> {
	available.clone().into_iter().filter(|a| {
		history.into_iter().all(|s| {
			let Step { result: r, guess: g } = s;
			let Step { result: r1, guess: _ } = Step::calc(a, &g);
			r1 == *r
		})
	}).collect::<Vec<guess::Guess>>()
}

#[test]
fn filter_guess_test() {
	let g1 = guess::Guess::new(1, 2, 3, 4);
	let r1 = result::ResultStep::new(1, 1);
	let s1 = Step::new(g1, r1);
	let g2 = guess::Guess::new(0, 7, 8, 9);
	let r2 = result::ResultStep::new(0, 0);
	let s2 = Step::new(g2, r2);
	let h = vec![s1, s2];
	let a1 = guess::Guess::new(1, 4, 7, 8); // Не подходит
	let a2 = guess::Guess::new(3, 2, 5, 6); // Подходит
	let a3 = guess::Guess::new(3, 7, 9, 0); // Не подходит
	let a4 = guess::Guess::new(3, 8, 9, 0); // Не подходит
	let a5 = guess::Guess::new(3, 7, 9, 8); // Не подходит
	let a = vec![a1, a2, a3, a4, a5];
	let result = filter_guess(&h, &a);
	assert_eq!(result.len(), 1);
	assert!(result.contains(&a2));
}
