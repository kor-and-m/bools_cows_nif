#[macro_use] extern crate rustler;

use rustler::{Env, Term, NifResult, Encoder, Decoder};

mod step;

pub fn test_game(a: &Vec<u8>) -> Result<Vec<step::Step>, &'static str> {
    if a.len() == 4 {
      let real = step::guess::Guess::new(a[0], a[1], a[2], a[3]);
      if real.is_guess() {
        Ok(step::guess_game(&real))
      } else {
        Err("numerals_repeat")
      }
    }
    else {
      Err("wrong_length")
    }
}

pub fn random_guess() -> Vec<u8> {
    step::guess::Guess::random().to_vec()
}

mod atoms {
    rustler_atoms! {
        atom ok;
        atom error;
    }
}

rustler_export_nifs!(
    "bools",
    [
      ("guess", 1, guess_real),
      ("guess_sync", 1, guess_sync),
      ("guess_async", 2, guess_async)
    ],
    None
);

impl<'a> Decoder<'a> for step::Step {
    fn decode(term: Term<'a>) -> NifResult<Self> {
        let decoded: (Vec<u8>, (u8, u8)) = term.decode()?;
        let (v, (bools, cows)) = decoded;
        let step = step::Step::new(step::guess::Guess::new(v[0], v[1], v[2], v[3]), step::result::ResultStep::new(bools, cows));
        Ok(step)
    }
}

fn guess_sync<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let history: Vec<step::Step> = args[0].decode()?;
    let guess = step::make_step(&history);
    Ok(guess.to_vec().encode(env))
}

fn guess_async<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let history: Vec<step::Step> = args[0].decode()?;
    let threads_count: u32 = args[1].decode()?;
    let guess = step::make_step_concurency(&history, threads_count);
    Ok(guess.to_vec().encode(env))
}

fn guess_real<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let guess: Vec<u8> = args[0].decode()?;
    match test_game(&guess) {
      Ok(v) => 
        Ok((atoms::ok(), v.into_iter().map(|x| {
          (x.guess.to_vec(), x.result.to_tuple())
        }).collect::<Vec<_>>()).encode(env)),
      Err(e) => Ok((atoms::error(), e).encode(env)),
    }
}
