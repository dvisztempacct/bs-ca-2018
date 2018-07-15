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

/* wolfram rule #110 */
/* let ruleNumber = abs(int_of_float(Js.Date.now())) mod 256; */
let ruleNumber = 122;
let rule = ruleNumber |. makeRule;

let w = 80;

let cells = w |. Array.init(_ => randomCell()) |. ref;

let steps = 2600;

for (_ in 1 to steps) {
  cells^ |. logCells;
  cells := cells ^ |. compute(rule)
};

Js.log("rule #" ++ string_of_int(ruleNumber));
