module C1 = Canvas.PlainTextCanvas;

let canvas = C1.make(20, 4);

for (i in 0 to 3) {
  C1.(canvas |. putDefaultStrClipped(i, i, "hello, world!"));
};

Js.log(canvas |. C1.stringOfCanvas);

module Ansi = Lib.Ansi;
module C2 = Canvas.AnsiColorCanvas;

let canvas = C2.make(20, 4);

for (i in 0 to 3) {
  let color = Ansi.colorOfInt(i);

  C2.(
    canvas
    |. putHomoStrClipped(
         i,
         i,
         Canvas.{color: color, char: ' '},
         "hello, world!",
       )
  );
};

Js.log(canvas |. C2.stringOfCanvas);
