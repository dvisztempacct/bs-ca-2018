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

let write: string => unit = [%bs.raw "x=>process.stdout.write(x)"];

let writeSpace = i =>
  for (_ in 1 to i) {
    write(" ");
  };

let oddSelect = (i, a, b) => i mod 2 == 0 ? a : b;

[@bs.module "fs"] external readFileSync : string => Node.buffer = "";

[@bs.module "fs"] external writeFileSync : (string, string) => unit = "";

exception BadCommand;

module Ansi = {
  let reset = "\x1b[0m";
  let gray = "\x1b[30;1m";
  let red = "\x1b[31m";
  let green = "\x1b[32m";
};

let charAtDefault = (default, str, i) =>
  0 <= i && i < String.length(str) ? Js.String.charAt(i, str) : default;

let divRoundUp = (n, d) => n / d + (n mod d == 0 ? 0 : 1);

let arrayChunk = (w, a) =>
  Array.init(divRoundUp(Array.length(a), w), i =>
    a |. Belt.Array.slice(~offset=i * w, ~len=w)
  );

let arrayGetDefault = (~x, ~d, i) =>
  0 <= i && i < Array.length(x) ? x[i] : d;

module Slice = {
  /* TODO hide */
  type t('a) = {
    a: array('a),
    start: int,
    len: int,
  };
  let make = (a, start, len) => {
    if (0 > start || 0 > len || Array.length(a) < start + len) {
      raise(Outofrange);
    };
    {a, start, len};
  };
  let makeWhole = a => make(a, 0, a |. Array.length);
  let getOpt = (s, i) => 0 <= i && i < s.len ? Some(s.a[s.start + i]) : None;
  let getExn = (s, i) =>
    switch (getOpt(s, i)) {
    | Some(s) => s
    | None => raise(Outofrange)
    };
  let put = (s, i, x) =>
    switch (getOpt(s, i)) {
    | Some(_) =>
      s.a[s.start + i] = x;
      true;
    | None => false
    };
  let putExn = (s, i, x) => put(s, i, x) ? () : raise(Outofrange);

  let getDefault = (~s, ~d, i) =>
    switch (getOpt(s, i)) {
    | Some(s) => s
    | None => d
    };

  let slice = (s, start, len) =>
    start >= 0 && len >= 0 && start + len <= s.len ?
      make(s.a, s.start + start, len) : raise(Outofrange);

  let chunksByLen = (s, len) =>
    divRoundUp(s.len, len)
    |. Array.init(i =>
         s |. slice(i * len, Js.Math.min_int(len, s.len - i * len))
       );

  let chunksByCount = (s, num) => {
    let len = s.len / num;
    num
    |. Array.init(i =>
         s |. slice(i * len, Js.Math.min_int(len, s.len - i * len))
       );
  };
};
