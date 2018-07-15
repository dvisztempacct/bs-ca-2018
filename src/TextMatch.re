open Lib;
open Ansi;

if (Array.length(Node.Process.argv) != 4) {
  Js.log("please specify two filenames");
  raise(BadCommand);
}

let f = filename => {
  filename |. readFileSync |. Node.Buffer.toString |> Js.String.split("\n");
};

let cmpLine = (a, b) => {
  let w = Js.Math.max_int(String.length(a), String.length(b));
  for (i in 0 to w-1) {
    let a = charAtDefault(" ", a, i);
    let b = charAtDefault(" ", b, i);
    if (a == b) {
      write(green ++ a)
    } else {
      if (a == " ")
        write(gray ++ b)
      else if (b == " ")
        write(gray ++ a)
      else 
        write(red ++ b)
    }
  };
  write("\n")
};

let cmpMultiLine = (a, b) => {
  let h = Js.Math.max_int(Array.length(a), Array.length(b));
  for (i in 0 to h-1) {
    cmpLine(
      i < Array.length(a) ? a[i] : "",
      i < Array.length(a) ? b[i] : "",
    )
  }
};

let a = Node.Process.argv[2] |. f;
let b = Node.Process.argv[3] |. f;
Js.log(a);
Js.log(b);
cmpMultiLine(a, b);
write(reset);
