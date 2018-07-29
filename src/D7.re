open Lib;
module C = Canvas.PlainTextCanvas;
type canvas = C.t;

Js.Date.now() |> int_of_float |> Random.init;

let ruleNumber = 122;
let rule = ruleNumber |. makeRule;

let calcCell = (x: Slice.t(cell), i) => {
  let a = i - 1 |> Slice.getExn(x) |. intOfCell;
  let b = i - 0 |> Slice.getExn(x) |. intOfCell;
  let c = i + 1 |> Slice.getExn(x) |. intOfCell;
  rule[a ||| (b << 1) ||| (c << 2)];
};

/* draws a single line */
let solidConeLog = (x: Slice.t(cell), c:C.t, i) => {
  let w = x.len;
  let fn = i => x |. Slice.getExn(i) |. charOfCell;
  let l = forRange(fn, i, w - i);
  let s = l |> List.fold_left((s, ch) => s ++ ch, "");
  c |. C.putDefaultStrClipped(i, i, s);
};

let hollowConeLog = (x0: Slice.t(cell), x1: Slice.t(cell), c:C.t) => {
  let w = x0.len;
  let half = divRoundUp(w, 2);
  let row = i => {
    oddSelect(i, x0, x1) |. Slice.getExn(i) |. charOfCell |> C.putDefaultStrClipped(c, i, i);
    if (w - i - 1 != i) {
      oddSelect(i, x0, x1) |. Slice.getExn(w - i - 1) |. charOfCell |> C.putDefaultStrClipped(c, w - i - 1, i);
    }
  };
  forRange(row, 0, half) |> ignore;
};

let rec bootCone = (x: Slice.t(cell), y: Slice.t(cell), c:C.t, i) => {
  solidConeLog(x, c, i - 1);
  let w = x.len;
  let half = w / 2;
  if (i <= half) {
    for (j in i to w - i - 1) {
      calcCell(x, j) |> Slice.putExn(y, j);
    };
    bootCone(y, x, c, i + 1);
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

let newCanvas = w => C.make(w, divRoundUp(w, 2));

for (i in 0 to nChunks - 1) {
  let cells = cells[i];
  let solidConeCanvas = newCanvas(cells[0].len);
  bootCone(cells[0], cells[1], solidConeCanvas, 1);
  let hollowConeCanvas = newCanvas(cells[0].len);
  hollowConeLog(cells[0], cells[1], hollowConeCanvas);
  let canvas = Canvas.specialDiff(solidConeCanvas, hollowConeCanvas);
  Js.log(canvas |. Canvas.AnsiColorCanvas.stringOfCanvas);
};
