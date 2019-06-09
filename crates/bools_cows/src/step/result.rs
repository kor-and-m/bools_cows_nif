use std::fmt;

#[derive(Debug, Copy, Clone, Hash)]
pub struct ResultStep {
	bools: u8,
	cows: u8,
}

impl PartialEq for ResultStep {
	fn eq(&self, other: &ResultStep) -> bool {
	    self.bools == other.bools && self.cows == other.cows
	}
}

impl Eq for ResultStep {}

impl ResultStep {

	#[allow(dead_code)]
	pub fn new(bools: u8, cows: u8) -> ResultStep {
		ResultStep { bools: bools, cows: cows }
	}

    #[allow(dead_code)]
	fn summary(&self) -> u8 {
	    self.bools + self.cows
	}

	pub fn to_tuple(&self) -> (u8, u8) {
		(self.bools, self.cows)
	}

	pub fn code(&self) -> u8 {
		if self.bools == 4 {
			return 15;
		}
		if self.cows == 4 {
			return 14;
		}
		self.bools * 4 + self.cows
	}

	pub fn calc(gv: Vec<u8>, rv: Vec<u8>) -> ResultStep {
		let mut step = 0;
		let mut bools = 0u8;
		let mut cows = 0u8;
		while step < 4 {
			if gv[step] == rv[step] {
				bools += 1;
			} else if rv.contains(&gv[step]) {
				cows += 1;
			}
			step += 1;
		};
		ResultStep { bools: bools, cows: cows }
	}
}

#[test]
fn calc_test_sum() {
	let r = ResultStep::calc(vec![1, 6, 7, 3], vec![6, 3, 0, 2]);
	assert_eq!(r.summary(), 2);
}

#[test]
fn calc_test_eq() {
	let r = ResultStep::new(1, 1);
	let r1 = ResultStep::calc(vec![1, 6, 7, 3], vec![3, 6, 0, 2]);
	assert_eq!(r1, r);
}

impl fmt::Display for ResultStep {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "(bools: {}, cows: {})", self.bools, self.cows)
	}
}