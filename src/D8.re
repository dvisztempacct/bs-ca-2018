open Lib;
open Lib.Infix;
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
let solidConeLog = (cells: Slice.t(cell), c: C.t, x, y) => {
  let w = cells.len;
  let fn = x => cells |. Slice.getExn(x) |. charOfCell;
  let l = forRange(fn, x, w - x);
  let s = l |> List.fold_left((s, ch) => s ++ ch, "");
  c |. C.putDefaultStrClipped(x, y, s);
};

let hollowConeLog = (x0: Slice.t(cell), x1: Slice.t(cell), c: C.t) => {
  let w = x0.len;
  let half = w /^ 2;
  let row = i => {
    oddSelect(i, x0, x1)
    |. Slice.getExn(i)
    |. charOfCell
    |> C.putDefaultStrClipped(c, i, i + w/2);
    if (w - i - 1 != i) {
      oddSelect(i, x0, x1)
      |. Slice.getExn(w - i - 1)
      |. charOfCell
      |> C.putDefaultStrClipped(c, w - i - 1, i + w/2);
    };
  };
  forRange(row, 0, half) |> ignore;
};

let rec calcCone = (x: Slice.t(cell), y: Slice.t(cell), c: C.t, step) => {
  let w = x.len;
  let half = w / 2;
  let wut = step < half ? step - 1 : w mod 2 == 0 ? step - half : step - half - 1;
  solidConeLog(x, c, wut, step - w mod 2);
  if (step <= half) {
    for (i in step to w - step - 1) {
      calcCell(x, i) |> Slice.putExn(y, i);
    };
    calcCone(y, x, c, step + 1);
  }
  else if (step < w) {
    for (i in step - half to w - step + half - 1) {
      calcCell(x, i) |> Slice.putExn(y, i);
    };
    calcCone(y, x, c, step + 1);
  } else {
    ();
  };
};

let w = 175;
let chunkSize = 40;
let nChunks = w /^ chunkSize;

let cells =
  w
  * 2
  |. Array.init(_ => randomCell())
  |. Slice.makeWhole
  |. Slice.chunksByLen(chunkSize * 2)
  |> Array.map(slice => slice |. Slice.chunksByCount(2));

let newCanvas = w => C.make(w, w);

for (i in 0 to nChunks - 1) {
  let cells = cells[i];
  let w = cells[0].len;
  let half = w /^ 2;
  let solidConeCanvas = newCanvas(cells[0].len);
  calcCone(cells[0], cells[1], solidConeCanvas, half);
  let hollowConeCanvas = newCanvas(cells[0].len);
  hollowConeLog(cells[0], cells[1], hollowConeCanvas);
  let canvas = Canvas.specialDiff(solidConeCanvas, hollowConeCanvas);
  Js.log(canvas |. Canvas.AnsiColorCanvas.stringOfCanvas);
};
