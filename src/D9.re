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
    |> C.putDefaultStrClipped(c, i, i + w /- 2);
    if (w - i - 1 != i) {
      oddSelect(i, x0, x1)
      |. Slice.getExn(w - i - 1)
      |. charOfCell
      |> C.putDefaultStrClipped(c, w - i - 1, i + w /- 2);
    };
  };
  forRange(row, 0, half) |> ignore;
};

let rec calcCone = (x: Slice.t(cell), y: Slice.t(cell), c: C.t, step) => {
  let w = x.len;
  let half = w / 2;
  let wut =
    step < half ? step - 1 : w mod 2 == 0 ? step - half : step - half - 1;
  solidConeLog(x, c, wut, step - 1);
  if (step <= half) {
    for (i in step to w - step - 1) {
      calcCell(x, i) |> Slice.putExn(y, i);
    };
    calcCone(y, x, c, step + 1);
  } else if (step < w) {
    for (i in step - half to w - step + half - 1) {
      calcCell(x, i) |> Slice.putExn(y, i);
    };
    calcCone(y, x, c, step + 1);
  } else {
    ();
  };
};

let chunkAndCompute = (w, chunkSize, cells) => {
  let nChunks = w /^ chunkSize;

  let cells =
    cells
    |. Slice.makeWhole
    |. Slice.chunksByLen(chunkSize * 2)
    |> Array.map(slice => slice |. Slice.chunksByCount(2));

  Js.log(String.make(w*2, '='));
  Js.log2("w =", w);
  Js.log2("chunkSize =", chunkSize);
  Js.log2("chunkSizes =", cells |> Array.map(slices => slices[0] |. Slice.length));
  Js.log2("reconstituted w =", cells |> Array.map(slices => slices[0] |. Slice.length) |. Belt.Array.reduce(0, (+)));

  let canvasHeight = chunkSize - (chunkSize + 1) mod 2;
  Js.log2("canvasHeight =", canvasHeight);
  let solidCanvas = C.make(w, canvasHeight);
  let hollowCanvas = C.make(w, canvasHeight);

  for (i in 0 to nChunks - 1) {
    let cells = cells[i];
    let w = cells[0].len;
    let half = w /^ 2;
    let canvasOffsetY = w == chunkSize ? 0 : (chunkSize - w) /- 2 + 1;
    let solidCanvas =
      solidCanvas
      |. C.sliceExn(i * chunkSize, canvasOffsetY, w, canvasHeight);
    let hollowCanvas =
      hollowCanvas
      |. C.sliceExn(i * chunkSize, canvasOffsetY, w, canvasHeight);
    calcCone(cells[0], cells[1], solidCanvas, half);
    hollowConeLog(cells[0], cells[1], hollowCanvas);
  };
  let canvas = Canvas.specialDiff(solidCanvas, hollowCanvas);
  Js.log(canvas |. Canvas.AnsiColorCanvas.stringOfCanvas);
};

let w = 58;
let cells = w * 2 |. Array.init(_ => randomCell());
for (i in 0 to 40 - 5) {
  let chunkSize = 40 - i;
  chunkAndCompute(w, chunkSize, cells |. Array.copy);
};
