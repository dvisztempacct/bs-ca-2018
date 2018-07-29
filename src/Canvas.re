module Slice = Lib.Slice;

module type Impl = {
  type cell;
  let defaultCell : cell;
  let combineCellWithChar : (cell, char) => cell;
  let charOfCell : cell => char;
  let rowToString: Slice.t(cell) => string;
};

module Make = (Impl:Impl) => {
  include Impl;
  type t = {
    w: int,
    h: int,
    x: Slice.t(cell),
  };
  let init = (w, h, f) => {
    w,
    h,
    x: Array.init(w * h, f) |. Slice.makeWhole,
  };
  let make = (w, h) => init(w, h, _ => defaultCell);
  let devNull = [|defaultCell|] |. Slice.makeWhole;
  let calcIndex = (c, x, y) => c.w * y + x;
  let boundsCheck = (c, x, y) => 0 <= x && x < c.w && 0 <= y && y < c.h;
  let getCellOpt = (c, x, y) =>
    boundsCheck(c, x, y) ? Slice.getOpt(c.x, calcIndex(c, x, y)) : None;
  let getCellExn = (c, x, y) =>
    switch (getCellOpt(c, x, y)) {
    | Some(x) => x
    | None => raise(Lib.Outofrange)
    };
  let putBool = (c, x, y, v) => {
    if (boundsCheck(c, x, y)) {
      Slice.put(c.x, calcIndex(c, x, y), v);
    } else {
      false;
    };
  };
  let putExn = (c, x, y, v) => putBool(c, x, y, v) ? () : raise(Lib.Outofrange);
  let putGen = (c, x, y, ~n, ~f) =>
    for (i in 0 to n - 1) {
      f(i) |> putExn(c, x, y);
    };
  let putHomoStrClipped = (c, x, y, cell, v) => {
    let len = v |> String.length;
    let max = Js.Math.min_int(len, c.w - x);
    for (i in 0 to max - 1) {
      let v = combineCellWithChar(cell, v.[i]);
      putBool(c, x + i, y, v) |> ignore
    }
  };
  let putDefaultStrClipped = (c, x, y, v) =>
    putHomoStrClipped(c, x, y, defaultCell, v);
  let stringOfCanvas = c =>
    Slice.chunksByLen(c.x, c.w)
    |> Array.map(rowToString)
    |> Js.Array.joinWith("\n");
};

module PlainTextCanvas = Make({
  type cell = char;
  let defaultCell = ' ';
  let combineCellWithChar = (_cell, char) => char;
  let charOfCell = x => x;
  let rowToString = row => row |> Slice.get |> Array.map(String.make(1)) |> Js.Array.joinWith("");
});

type ansiColorCell = { color: Lib.Ansi.color, char: char };

module AnsiColorCanvas = Make({
  module A = Lib.Ansi;
  type cell = ansiColorCell;
  let defaultCell = { color: A.None, char: ' ' };
  let combineCellWithChar = (_cell, char) => { ..._cell, char };
  let charOfCell = x => x.char;
  let rowToString = row => row |> Slice.get |> Array.map(x => A.reset ++ A.stringOfColor(x.color) ++ String.make(1, x.char)) |> Js.Array.joinWith("");
});

/* diffs two `PlainTextCanvas.t`, producing an `AnsiColorCanvas.t` which colors
 * characters green where it's matched between both inputs, bright black (gray)
 * when one canvas has a space, and red when they differ */
let specialDiff : (PlainTextCanvas.t, PlainTextCanvas.t) => AnsiColorCanvas.t = (a, b) => {
  let w = Js.Math.max_int(a.w, b.w);
  let h = Js.Math.max_int(a.h, b.h);
  let c = AnsiColorCanvas.make(w*2, h);
  for (y in 0 to h-1) {
    for (x in 0 to w-1) {
      let a = a |. PlainTextCanvas.getCellExn(x, y);
      let b = b |. PlainTextCanvas.getCellExn(x, y);
      let same = a == b;
      let space = a == ' ' || b == ' ';
      let char = a == ' ' ? b : a;
      switch ((same, space)) {
      | (_, true) => [| { char, color: Lib.Ansi.Gray } |]
      | (true, _) => [|{ char, color: Lib.Ansi.Green } |]
      | (false, _) => [| { char: a, color: Lib.Ansi.Red }, { char: b, color: Lib.Ansi.BlackOnRed } |]
      }
      |> Array.iteri((i, ch) => AnsiColorCanvas.putExn(c, x*2+i, y, ch));
    }
  };
  c
};
