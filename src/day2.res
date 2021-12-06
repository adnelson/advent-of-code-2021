let _input = `
forward 5
down 5
forward 8
up 3
down 8
forward 2
`

let input = Utils.readInput(2)
type command = [#forward(int) | #up(int) | #down(int)]

let commands: array<command> =
  input
  ->Utils.processInput
  ->Js.Array2.map(line =>
    switch line->Js.String2.split(" ") {
    | ["forward", n] => #forward(n->int_of_string)
    | ["up", n] => #up(n->int_of_string)
    | ["down", n] => #down(n->int_of_string)
    | _ => failwith(`unrecognized command ${line}`)
    }
  )

type posn = {horiz: int, depth: int, aim: int}

let updatePosition: (posn, command) => posn = (posn, command) =>
  switch command {
  | #forward(n) => {...posn, horiz: posn.horiz + n}
  | #up(n) => {...posn, depth: posn.depth - n}
  | #down(n) => {...posn, depth: posn.depth + n}
  }

let updatePosition2: (posn, command) => posn = (posn, command) =>
  switch command {
  | #forward(n) => {...posn, horiz: posn.horiz + n, depth: posn.depth + posn.aim * n}
  | #up(n) => {...posn, aim: posn.aim - n}
  | #down(n) => {...posn, aim: posn.aim + n}
  }

let startingPosn = {horiz: 0, depth: 0, aim: 0}

let finalPosn = commands->Belt.Array.reduce(startingPosn, updatePosition2)

Js.log(finalPosn.horiz * finalPosn.depth)
