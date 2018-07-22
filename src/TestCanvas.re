module Canvas = Canvas.PlainTextCanvas;

let canvas = Canvas.make(50, 1);

Canvas.(
canvas |. putDefaultStrClipped(0, 0, "hello, world!")
);

Js.log(canvas |. Canvas.stringOfCanvas)
