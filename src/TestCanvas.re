module Canvas = Canvas.PlainTextCanvas;

let canvas = Canvas.make(50, 4);

for (i in 0 to 3) {
  Canvas.(
    canvas |. putDefaultStrClipped(i, i, "hello, world!")
  );
}

Js.log(canvas |. Canvas.stringOfCanvas)
