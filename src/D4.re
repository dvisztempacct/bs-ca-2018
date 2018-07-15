open Lib;

Js.Date.now() |> int_of_float |> Random.init;

let ruleNumber = 122;
let rule = ruleNumber |. makeRule;

let calcCell = (x: Slice.t(cell), i) => {
  let a = (i - 1) |> Slice.getExn(x) |. intOfCell;
  let b = (i - 0) |> Slice.getExn(x) |. intOfCell;
  let c = (i + 1) |> Slice.getExn(x) |. intOfCell;
  rule[a ||| (b << 1) ||| (c << 2)];
};

let solidConeLog = (x: Slice.t(cell), i) => {
  let w = x.len;
  writeSpace(i);
  for (i in i to w - i - 1) {
    x |. Slice.getExn(i) |. charOfCell |. write;
  };
  writeSpace(i);
  write("\n");
};

let hollowConeLog = (x0:Slice.t(cell), x1:Slice.t(cell)) => {
  let w = x0.len;
  let half = w / 2;
  for (i in 0 to half - 1) {
    writeSpace(i);
    oddSelect(i, x0, x1) |. Slice.getExn(i) |. charOfCell |. write;
    writeSpace(w - i * 2 - 2);
    oddSelect(i, x0, x1) |. Slice.getExn(w - i - 1) |. charOfCell |. write;
    writeSpace(i);
    write("\n");
  };
};

let rec bootCone = (x: Slice.t(cell), y: Slice.t(cell), i) => {
  solidConeLog(x, i - 1);
  let w = x.len;
  let half = w / 2;
  if (i <= half) {
    for (j in i to w - i - 1) {
      calcCell(x, j) |> Slice.putExn(y, j);
    };
    bootCone(y, x, i + 1);
  } else {
    ();
  };
};

let w = 80;

let cells = [|
  w |. Array.init(_ => randomCell()) |. Slice.make(0, w),
  w |. Array.make(Low) |. Slice.make(0, w),
|];

bootCone(cells[0], cells[1], 1);
Js.log("lol");
hollowConeLog(cells[0], cells[1]);

/*
 cells[0] |. logCells;
 cells[1] |. logCells;
 */
