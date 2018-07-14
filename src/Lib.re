exception Outofrange;

type cell =
  | Low
  | High;

let intOfCell =
  fun
  | Low => 0
  | High => 1;

let cellOfInt =
  fun
  | 0 => Low
  | 1 => High
  | _ => raise(Outofrange);

let charOfCell =
  fun
  | Low => "."
  | High => "@";

let randomCell = () => Random.bool() ? High : Low;

let bitshift_left: (int, int) => int = [%bs.raw "(a,b)=>a<<b"];
let (<<) = bitshift_left;

let bitwise_or: (int, int) => int = [%bs.raw "(a,b)=>a|b"];
let (|||) = bitwise_or;

let bitwise_and: (int, int) => int = [%bs.raw "(a,b)=>a&b"];
let (&) = bitwise_and;

let bitwise_xor: (int, int) => int = [%bs.raw "(a,b)=>a^b"];
let (|^|) = bitwise_xor;

let ruleBit = (b, n) => (n & 1 << b) == 0 ? Low : High;

let makeRule = n => [|
  n |> ruleBit(0),
  n |> ruleBit(1),
  n |> ruleBit(2),
  n |> ruleBit(3),
  n |> ruleBit(4),
  n |> ruleBit(5),
  n |> ruleBit(6),
  n |> ruleBit(7),
|];
