/-- Algebra types: Real (ℝ), Complex (ℂ), Quaternion (ℍ) -/
inductive Algebra : Type where
  | R
  | C
  | H
  deriving DecidableEq, BEq, Repr

instance : ToString Algebra where
  toString
    | .R => "ℝ"
    | .C => "ℂ"
    | .H => "ℍ"

/-- A Clifford algebra element with type, matrix dimension, and number of copies -/
structure Element where
  algebra : Algebra
  matrix : Nat
  copies : Nat
  deriving DecidableEq, BEq, Repr

/-- Display an element as "algebra(matrix) ⊕ algebra(matrix) ⊕ ..." -/
def Element.toString (e : Element) : String :=
  let k := s!"{e.algebra}({e.matrix})"
  let repeated := List.replicate e.copies k
  "⊕".intercalate repeated

instance : ToString Element where
  toString := Element.toString

/-- Tensor product of two algebra elements -/
def otimes (a b : Element) : Element :=
  match a.algebra, b.algebra with
  | .R, _ => 
      ⟨b.algebra, a.matrix * b.matrix, a.copies * b.copies⟩
  | .C, .R => 
      ⟨.C, a.matrix * b.matrix, a.copies * b.copies⟩
  | .C, .C => 
      ⟨.C, a.matrix * b.matrix, 2 * a.copies * b.copies⟩
  | .C, .H => 
      ⟨.C, 2 * a.matrix * b.matrix, a.copies * b.copies⟩
  | .H, .R => 
      ⟨.H, a.matrix * b.matrix, a.copies * b.copies⟩
  | .H, .C => 
      ⟨.C, 2 * a.matrix * b.matrix, a.copies * b.copies⟩
  | .H, .H => 
      ⟨.R, 4 * a.matrix * b.matrix, a.copies * b.copies⟩

/-- Clifford algebra Cl(s,t) - compute the structure -/
partial def clifford : Int → Int → Option Element
  | s, t =>
    if s < 0 || t < 0 then
      none
    else if s == 0 && t == 0 then
      some ⟨.R, 1, 1⟩
    else if s == 0 && t == 1 then
      some ⟨.R, 1, 2⟩
    else if s == 1 && t == 0 then
      some ⟨.C, 1, 1⟩
    else if s > 0 && t > 0 then
      clifford (s - 1) (t - 1) |>.map (otimes ⟨.R, 2, 1⟩)
    else if s == 0 && t > 1 then
      clifford (t - 2) 0 |>.map (otimes ⟨.R, 2, 1⟩)
    else if s > 1 && t == 0 then
      clifford 0 (s - 2) |>.map (otimes ⟨.H, 1, 1⟩)
    else
      none

/-- Parse command-line arguments and run the computation -/
def main (args : List String) : IO UInt32 := do
  match args with
  | [sStr, tStr] =>
    match (sStr.toInt?, tStr.toInt?) with
    | (some s, some t) =>
      match clifford s t with
      | some cl =>
        IO.println s!"Cl({sStr},{tStr}) = {cl}"
        pure 0
      | none =>
        IO.eprintln "Error: invalid arguments to clifford"
        pure 1
    | _ =>
      IO.eprintln s!"Error: could not parse integers from '{sStr}' and '{tStr}'"
      pure 1
  | _ =>
    let prog := args.getD 0 "clifford"
    IO.eprintln s!"Usage: {prog} <s> <t>"
    pure 1
