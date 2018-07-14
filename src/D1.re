open Lib;

Js.Date.now() |> int_of_float |> Random.init;

let getCell = (cells, index) =>
  0 <= index && index < Array.length(cells) ? cells[index] : Low;

let compute = (cells, rule) =>
  cells
  |. Array.length
  |. Array.init(index => {
       let a = index - 1 |> getCell(cells) |. intOfCell;
       let b = index - 0 |> getCell(cells) |. intOfCell;
       let c = index + 1 |> getCell(cells) |. intOfCell;
       rule[a ||| (b << 1) ||| (c << 2)];
     });

let z = (b, n) => (n & (1 << b)) == 0 ? Low : High;

let makeRule = n => [|
  n |> z(0),
  n |> z(1),
  n |> z(2),
  n |> z(3),
  n |> z(4),
  n |> z(5),
  n |> z(6),
  n |> z(7),
|];

/* wolfram rule #110 */
/* let ruleNumber = abs(int_of_float(Js.Date.now())) mod 256; */
let ruleNumber = 122;
let rule = ruleNumber |. makeRule;

let w = 80;

let logCells = cells => {
  cells |> Array.map(charOfCell) |> Array.fold_left((++), "") |> Js.log
};

let cells = w |. Array.init(_ => randomCell()) |. ref;

let steps = 2600;

for (n in 1 to steps) {
  cells^ |. logCells;
  cells := cells ^ |. compute(rule)
};

Js.log("rule #" ++ string_of_int(ruleNumber));
