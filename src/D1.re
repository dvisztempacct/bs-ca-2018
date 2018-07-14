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

let getCell = (cells, index) =>
  0 <= index && index < Array.length(cells) ? cells[index] : Low;

let bitshift_left: (int, int) => int = [%bs.raw "(a,b)=>a<<b"];
let (<<) = bitshift_left;

let bitwise_or: (int, int) => int = [%bs.raw "(a,b)=>a|b"];
let (|||) = bitwise_or;

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
let rule = [| Low, High, High, High, Low, High, High, Low |];

let w = 80;

let logCells = cells => {
  cells |> Array.map(charOfCell) |> Array.fold_left((++), "") |> Js.log
};

let cells = w |. Array.init(_ => randomCell()) |. ref;

for (n in 1 to 40) {
  cells^ |. logCells;
  cells := cells ^ |. compute(rule)
}


