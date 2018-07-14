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

let ruleBit = (n, b) => (n & 1 << b) == 0 ? Low : High;

let makeRule = n => Array.init(8, ruleBit(n));

let logCells = cells =>
  cells |> Array.map(charOfCell) |> Array.fold_left((++), "") |> Js.log;
