open Lib;

Js.Date.now() |> int_of_float |> Random.init;

let ruleNumber = 122;
let rule = ruleNumber |. makeRule;

let calcCell = (x, i) => {
  let a = x[i - 1] |. intOfCell;
  let b = x[i - 0] |. intOfCell;
  let c = x[i + 1] |. intOfCell;
  rule[a ||| (b << 1) ||| (c << 2)];
};

let coneLog = (x, i) => {
  let len = x |> Array.length;
  x
  |> Array.mapi((j, xj) => i <= j && j < len - i ? xj |. charOfCell : " ")
  |> Array.fold_left((++), "")
  |> Js.log;
};

let rec bootCone = (x, y, i) => {
  coneLog(x, i);
  let len = x |. Array.length;
  let half = len / 2;
  if (i <= half) {
    for (j in i to len - i - 1) {
      y[j] = calcCell(x, j);
    };
    bootCone(y, x, i + 1);
    ();
  } else {
    ();
  };
};

let w = 80;

let cells = [|w |. Array.init(_ => randomCell()), w |. Array.make(Low)|];

bootCone(cells[0], cells[1], 1);

cells[0] |. logCells;
cells[1] |. logCells;
