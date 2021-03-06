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

let solidConeLog = (x, i) => {
  let w = x |> Array.length;
  writeSpace(i);
  for (i in i to w - i - 1) {
    x[i] |. charOfCell |. write;
  };
  writeSpace(i);
  write("\n");
};

let hollowConeLog = (x0, x1) => {
  let w = x0 |> Array.length;
  let half = w / 2;
  for (i in 0 to half - 1) {
    writeSpace(i);
    oddSelect(i, x0, x1)[i] |. charOfCell |. write;
    writeSpace(w - i * 2 - 2);
    oddSelect(i, x0, x1)[w - i - 1] |. charOfCell |. write;
    writeSpace(i);
    write("\n");
  };
};

let rec bootCone = (x, y, i) => {
  solidConeLog(x, i - 1);
  let w = x |. Array.length;
  let half = w / 2;
  if (i <= half) {
    for (j in i to w - i - 1) {
      y[j] = calcCell(x, j);
    };
    bootCone(y, x, i + 1);
  } else {
    ();
  };
};

let w = 80;

let cells = [|w |. Array.init(_ => randomCell()), w |. Array.make(Low)|];

bootCone(cells[0], cells[1], 1);
Js.log("lol");
hollowConeLog(cells[0], cells[1]);

/*
 cells[0] |. logCells;
 cells[1] |. logCells;
 */
