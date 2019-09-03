
extern crate rand;
use rand::Rng;

// extern crate permutohedron;
// use permutohedron::*;

// use std::cmp::Ordering;
use std::fmt::{self, Formatter, Display};


#[derive(Debug, PartialEq, PartialOrd)]
pub enum Set {
    SetN(Vec<i32>),
    SetC(Vec<Card>),
}


// Do we need colors ... ?
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Color {
    Heart,
    Pik,
    Square,
    Cross,
}


#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Card {
    Ace(Color),
    King(Color),
    Queen(Color),
    Jack(Color),
    Number(i32, Color),
}

#[derive(Debug, PartialEq, PartialOrd)]
pub enum Combo {
    Pair,
    Triple,
    Quadruple,
    FullHouse,
    Street,
}


pub use Card::*;
pub use Color::*;
pub use Set::*;
pub use Combo::*;


/*
What we need is a Metrik for Cards, or rather, a Set of cards (9), to evaluate
exactly in how many cases we would get better cards when discarding four
out of our initial five. Important for this appears to be which card will be
our one consistent one, and which cards combined would give something useful.
we are only looking for pairs, and only if our first hand does not have
anything valuable at all.

Valid reasons for not taking a second hand would be:
    * pair, one or more
    * triple
    * quadruple
    * Full House?
    * street?
    * other variants?

After having taken up the second hand, we only count the number of pairs we
might get. We might need to precompute the amount of pairs resulting from
taking up the new hand for the general case and then apply it for each scenario

Required:
* function to tell us in how many permutations we might get what combinations
    for what hands.
* iterator to get us 'new' 9 cards, order is irrelevant, but we should have
    all 9 different cards together exactly once.

in the end, we will not compute each and every possible permutation even,
since it is definitely not required.

*/


/// generating Random 9 Cards. No Card is included twice.
pub fn get_randomset() -> Set {
    let mut random_cards: Vec<i32>  = Vec::new();
    while random_cards.len() < 9 {
        let new_card = rand::thread_rng().gen_range(0, 52);
    // this ensures to not to include this number if it was generated before
        if !random_cards.contains(&new_card) {
            random_cards.push(new_card);
        }
    }
    SetN(random_cards)
}


/// takes a number from 0 to 52 and returns
/// the corresponding card.
pub fn to_card(n: i32) -> Card {
    let n = n % 52;
    let color = match n % 4 {
        0 => Heart,
        1 => Pik,
        2 => Square,
        _ => Cross,
    };
    match n / 4 {
        0  => Ace(color),
        1  => King(color),
        11 => Jack(color),
        12 => Queen(color),
        m  => Number(m, color),
    }
}


pub fn to_number(c: Card) -> i32 {
    let (m, col) = match c {
        Ace(color) => (0, color),
        King(color) => (1, color),
        Jack(color) => (11, color),
        Queen(color) => (12, color),
        Number(n, color) => (n, color),
    };
    let c = match col {
        Heart => 0,
        Pik => 1,
        Square => 2,
        Cross => 3,
    };
    c * m
}


/// Showing the color as one of
///  H(eart)  P(ik)  S(quare)  C(ross)
impl Display for Color {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            &Heart  => write!(f, "H"),
            &Pik    => write!(f, "P"),
            &Square => write!(f, "S"),
            &Cross  => write!(f, "C"),
        }
    }
}


/// showing the Card with the respective color. Card is one of
/// A(ce)  K(ing)  Q(ueen)  J(ack)  Number     + Color
impl Display for Card {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use Card::*;
        match self {
            &Ace(ref c)       => write!(f, "A{}", c),
            &King(ref c)      => write!(f, "K{}", c),
            &Queen(ref c)     => write!(f, "Q{}", c),
            &Jack(ref c)      => write!(f, "J{}", c),
            &Number(n, ref c) => write!(f, "{}{}", n, c),
        }
    }
}


impl Display for Set {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            &SetN(ref vec) => { write!(f, "[ ")?; for e in vec {
                write!(f, "{} ", e)?
            }; write!(f, "]\n") }
            &SetC(ref vec) => { write!(f, "[ ")?; for e in vec {
                write!(f, "{} ", e)?
            }; write!(f, "]\n") }
        }
    }
}
// */


/*
impl Iterator for Set {
    type Item = Set;
    // this function now needs to generate a new combination of cards which was
    // not yet delivered and is invariant under permutation to all yet
    // delivered, ...
    // 3679075400 such combinations exist, but still,
    // 1335062881152000 is all combinations with permutations.
    fn next(&mut self) -> Option<Set> {
        ()
    }
} // */



impl Set {
    fn to_cards(&self) -> Set {
        match self {
            &SetN(ref v) => SetC(v.iter().map(|i| to_card(*i)).collect()),
            &SetC(ref v) => SetC(v.to_vec()),
        }
    }

    fn to_number(&self) -> Set {
        match self {
            &SetN(ref v) => SetN(v.to_vec()),
            &SetC(ref v) => SetN(v.iter().map(|i| to_number(*i)).collect()),
        }
    }
}




fn main() {
    /*
    for i in 0..52 {
        println!("{}", toCard(i));
    }

    let mut data = [0, 1, 2, 3, 4, 5, 6, 7, 8];
    let mut permutations = Vec::new();

    loop {
        permutations.push(data.to_vec());
        if !data.next_permutation() { break; }
    }
    */
    print!("{}", get_randomset().to_cards());
    print!("{}", get_randomset().to_cards());
    print!("{}", get_randomset().to_cards());
    print!("{}", get_randomset().to_cards());
}


