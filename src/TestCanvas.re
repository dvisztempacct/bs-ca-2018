module C1 = Canvas.PlainTextCanvas;

let makeMonochromeHelloWorld = (w, h, x0, xz) => {
  let canvas = C1.make(w, h);

  for (i in x0 to xz-1) {
    C1.(canvas |. putDefaultStrClipped(i, i, "hello, world!"));
  };

  canvas;
};

let helloWorldA = makeMonochromeHelloWorld(20, 4, 0, 4);

Js.log(helloWorldA |. C1.stringOfCanvas);

module Ansi = Lib.Ansi;
module C2 = Canvas.AnsiColorCanvas;

let canvas = C2.make(20, 4);

for (i in 0 to 3) {
  let color = Ansi.colorOfInt(i);

  C2.(
    canvas
    |. putHomoStrClipped(i, i, Canvas.{color, char: ' '}, "hello, world!")
  );
};

Js.log(canvas |. C2.stringOfCanvas);

let helloWorldB = makeMonochromeHelloWorld(20, 4, 1, 3);
C1.(helloWorldB |. putDefaultStrClipped(0, 0, "HELLO, WORLD!"));
let canvas = Canvas.specialDiff(helloWorldA, helloWorldB);
Js.log(canvas |. C2.stringOfCanvas);
