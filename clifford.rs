use std::collections::HashMap;
use std::cell::RefCell;
use std::fmt;
use std::env;
use std::process;

const R: &str = "ℝ";
const C: &str = "ℂ";
const H: &str = "ℍ";

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct Element {
    algebra: String,
    matrix: i32,
    copies: i32,
}

impl fmt::Display for Element {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let k = format!("{}({})", self.algebra, self.matrix);
        let repeated: Vec<String> = vec![k; self.copies as usize];
        write!(f, "{}", repeated.join("⊕"))
    }
}

thread_local! {
    static MEMO: RefCell<HashMap<(i32, i32), Element>> = RefCell::new(HashMap::new());
}

fn otimes(a: &Element, b: &Element) -> Result<Element, String> {
    match a.algebra.as_str() {
        R => Ok(Element {
            algebra: b.algebra.clone(),
            matrix: a.matrix * b.matrix,
            copies: a.copies * b.copies,
        }),
        C => match b.algebra.as_str() {
            R => Ok(Element {
                algebra: C.to_string(),
                matrix: a.matrix * b.matrix,
                copies: a.copies * b.copies,
            }),
            C => Ok(Element {
                algebra: C.to_string(),
                matrix: a.matrix * b.matrix,
                copies: 2 * a.copies * b.copies,
            }),
            H => Ok(Element {
                algebra: C.to_string(),
                matrix: 2 * a.matrix * b.matrix,
                copies: a.copies * b.copies,
            }),
            _ => Err("invalid algebra type in otimes".to_string()),
        },
        H => match b.algebra.as_str() {
            R => Ok(Element {
                algebra: H.to_string(),
                matrix: a.matrix * b.matrix,
                copies: a.copies * b.copies,
            }),
            C => Ok(Element {
                algebra: C.to_string(),
                matrix: 2 * a.matrix * b.matrix,
                copies: a.copies * b.copies,
            }),
            H => Ok(Element {
                algebra: R.to_string(),
                matrix: 4 * a.matrix * b.matrix,
                copies: a.copies * b.copies,
            }),
            _ => Err("invalid algebra type in otimes".to_string()),
        },
        _ => Err("invalid algebra type in otimes".to_string()),
    }
}

fn clifford(s: i32, t: i32) -> Result<Element, String> {
    if s < 0 || t < 0 {
        return Err("s and t must be non-negative".to_string());
    }

    // Check memoization
    if let Ok(memo) = MEMO.try_with(|m| m.borrow().get(&(s, t)).cloned()) {
        if let Some(cached) = memo {
            return Ok(cached);
        }
    }

    let result = match (s, t) {
        (0, 0) => Element {
            algebra: R.to_string(),
            matrix: 1,
            copies: 1,
        },
        (0, 1) => Element {
            algebra: R.to_string(),
            matrix: 1,
            copies: 2,
        },
        (1, 0) => Element {
            algebra: C.to_string(),
            matrix: 1,
            copies: 1,
        },
        (s, t) if s > 0 && t > 0 => {
            let inner = clifford(s - 1, t - 1)?;
            otimes(
                &Element {
                    algebra: R.to_string(),
                    matrix: 2,
                    copies: 1,
                },
                &inner,
            )?
        }
        (0, t) if t > 1 => {
            let inner = clifford(t - 2, 0)?;
            otimes(
                &Element {
                    algebra: R.to_string(),
                    matrix: 2,
                    copies: 1,
                },
                &inner,
            )?
        }
        (s, 0) if s > 1 => {
            let inner = clifford(0, s - 2)?;
            otimes(
                &Element {
                    algebra: H.to_string(),
                    matrix: 1,
                    copies: 1,
                },
                &inner,
            )?
        }
        _ => return Err("unreachable state in clifford".to_string()),
    };

    // Store in memoization
    let _ = MEMO.try_with(|m| {
        m.borrow_mut().insert((s, t), result.clone());
    });

    Ok(result)
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        eprintln!("Usage: {} <s> <t>", args[0]);
        process::exit(1);
    }

    let s: i32 = match args[1].parse() {
        Ok(n) => n,
        Err(e) => {
            eprintln!("Invalid argument for s: {}", e);
            process::exit(1);
        }
    };

    let t: i32 = match args[2].parse() {
        Ok(n) => n,
        Err(e) => {
            eprintln!("Invalid argument for t: {}", e);
            process::exit(1);
        }
    };

    match clifford(s, t) {
        Ok(cl) => println!("Cl({},{}) = {}", args[1], args[2], cl),
        Err(e) => {
            eprintln!("Error: {}", e);
            process::exit(1);
        }
    }
}
