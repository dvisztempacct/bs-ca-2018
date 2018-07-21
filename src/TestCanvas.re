module Canvas = Lib.Canvas;

let canvas = Canvas.make(50, 1);

Canvas.(
canvas |. putStrBool(0, 0, "hello, world!")
);

Js.log(canvas |. Canvas.stringOfCanvas)
