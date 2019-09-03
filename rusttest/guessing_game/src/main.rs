

/*
extern crate rand;

use std::io;
use rand::Rng;
use std::cmp::Ordering;


fn main() {
    println!("Guess the number!");

    let secret_number = rand::thread_rng().gen_range(1, 101);

//    println!("The secret number is {}", secret_number);

    loop {

        println!("Pleose input your guess: ");
        let mut guess = String::new();
        io::stdin().read_line(&mut guess)
            .expect("Failed to read line");

        println!("You guessed: {}", guess);

        let guess: u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_) => continue,
        };
        //  .expect("Failed to parse number!");

        match guess.cmp(&secret_number) {
            Ordering::Less    => println!("Too small!"),
            Ordering::Greater => println!("Too big!"),
            Ordering::Equal   => {
                println!("you win!");
                break;
            }
        }
    }
}
// */




/*
fn main() {

    let mut last: [(u64, u64); 4] = [(0, 0); 4];

    for i in (0..100) {
        println!("fib_memo({}) = {}", i, fib_memo(i, &mut last));
//        println!("fib_n({}) = {}", i, fib_n(i));
    }

}



// short recursive fibonacci-function
fn fib_n(n: u64) -> u64 {
    match n {
        0 => 0,
        1 => 1,
        n => fib_n(n - 1) + fib_n(n - 2),
    }
}


// memoized fibonacci-function.
// Definitely slower for low values but way faster for bigger ones,
// since the required calculations are not exponential at least.
fn fib_memo(n: u64, memo: &mut [(u64, u64); 4]) -> u64 {

    let mut index = None;

    for (i, &(k, v)) in memo.iter().enumerate() {
        if k == n {
            index = Some(i);
        }
    }

    match index {
        None    => {
            match n {
                0 => 0,
                1 => 1,
                n => {
                for j in (1..3).rev() {
                    memo[j] = memo[j - 1];
                }
                memo[0] = (n, fib_memo(n - 1, memo) + fib_memo(n - 2, memo));
                return memo[0].1;
                }
            }
        }

        Some(0) => return memo[0].1,
        Some(i) => {
            let tmp = memo[i];
            for j in (1..i).rev() {
                memo[j] = memo[j - 1];
            }
            memo[0] = tmp;
            return memo[0].1;
        }
    }
}
// */



// this is a basically iterative version of the fibnoacci-function.
// way faster than the recursive one, since there is no exponentially
// amount of calculation necessary, it thus runs in O(n) instead of O(e^n).
fn fib_n(n: i64) -> i64 {
    let mut a = 0;
    let mut b = 1;
    let mut c = a + b;
    for i in 0..n {
        a = b;
        b = c;
        c = a + b;
    }
    a
}


fn main() {
    for i in 0..150 {
        println!("fib({}) = {}", i, fib_n(i));
    }
}


/*
use std::cmp::PartialOrd;

fn largest<T: PartialOrd>(l: &[T]) -> &T {
    let mut largest = &l[0];
    for item in l {
        if item > largest {
            largest = &item;
        }
    }
    largest
}


fn main() {
    let numbers = vec![34, 50, 25, 100, 65];

    let result = largest(&numbers);
    println!("The largest number is {}", result);

    let chars = vec!['y', 'm', 'a', 'q'];

    let result = largest(&chars);
    println!("The largest char is {}", result);
} // */








