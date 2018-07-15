open Lib;

Js.Date.now() |> int_of_float |> Random.init;

let ruleNumber = 122;
let rule = ruleNumber |. makeRule;

let calcCell = (x: Slice.t(cell), i) => {
  let a = i - 1 |> Slice.getExn(x) |. intOfCell;
  let b = i - 0 |> Slice.getExn(x) |. intOfCell;
  let c = i + 1 |> Slice.getExn(x) |. intOfCell;
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

let hollowConeLog = (x0: Slice.t(cell), x1: Slice.t(cell)) => {
  let w = x0.len;
  let half = divRoundUp(w, 2);
  for (i in 0 to half - 1) {
    writeSpace(i);
    oddSelect(i, x0, x1) |. Slice.getExn(i) |. charOfCell |. write;
    writeSpace(w - i * 2 - 2);
    if (w - i - 1 != i) {
      oddSelect(i, x0, x1) |. Slice.getExn(w - i - 1) |. charOfCell |. write;
    };
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

let w = 175;
let chunkSize = 40;
let nChunks = divRoundUp(w, chunkSize);

let cells =
  w
  * 2
  |. Array.init(_ => randomCell())
  |. Slice.makeWhole
  |. Slice.chunksByLen(chunkSize * 2)
  |> Array.map(slice => slice |. Slice.chunksByCount(2));

Js.log2("LOL=", cells[0][0].len);
for (i in 0 to nChunks - 1) {
  bootCone(cells[i][0], cells[i][1], 1);
};

for (i in 0 to nChunks - 1) {
  hollowConeLog(cells[i][0], cells[i][1]);
};

/*
 cells[0] |. logCells;
 cells[1] |. logCells;
 */
