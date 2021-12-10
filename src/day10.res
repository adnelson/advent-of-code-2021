let testInput = `
[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
`

@deriving(jsConverter)
type token = [
  | #LParens
  | #RParens
  | #LCurly
  | #RCurly
  | #LAngle
  | #RAngle
  | #LBracket
  | #RBracket
]

type chunk = [
  | #Parens
  | #Curly
  | #Angle
  | #Bracket
]

let part1score = token =>
  switch token {
  | #RParens => Some(3)
  | #RBracket => Some(57)
  | #RCurly => Some(1197)
  | #RAngle => Some(25137)
  | _ => None
  }

let part2score = token =>
  switch token {
  | #RParens => Some(1)
  | #RBracket => Some(2)
  | #RCurly => Some(3)
  | #RAngle => Some(4)
  | _ => None
  }

let tokensToPart2Score = tokens =>
  tokens->Belt.Array.reduce(0->Utils.bigintFromInt, (score, token) => {
    let mult5 = Utils.multBI(score, Utils.bigintFromInt(5))
    switch token {
    | #RParens => mult5->Utils.plusBI(Utils.bigintFromInt(1))
    | #RBracket => mult5->Utils.plusBI(Utils.bigintFromInt(2))
    | #RCurly => mult5->Utils.plusBI(Utils.bigintFromInt(3))
    | #RAngle => mult5->Utils.plusBI(Utils.bigintFromInt(4))
    | _ => score
    }
  })

let tokenFromChar = chr =>
  switch chr {
  | "(" => #LParens
  | ")" => #RParens
  | "{" => #LCurly
  | "}" => #RCurly
  | "<" => #LAngle
  | ">" => #RAngle
  | "[" => #LBracket
  | "]" => #RBracket
  | _ => failwith(`invalid token ${chr}`)
  }

let tokenToChar = token =>
  switch token {
  | #LParens => "("
  | #RParens => ")"
  | #LCurly => "{"
  | #RCurly => "}"
  | #LAngle => "<"
  | #RAngle => ">"
  | #LBracket => "["
  | #RBracket => "]"
  }

let parseTokens = tokens => {
  let chunks = []
  let waitingFor = []
  let badScore = ref(None)
  let completion = ref(None)
  let rec loop = index => {
    let t = tokens->Belt.Array.get(index)
    switch t {
    | None =>
      switch waitingFor {
      | [] => ()
      | _ => {
          completion := Some(waitingFor->Js.Array2.map(fst)->Belt.Array.reverse)
          failwith(`Reached the end of input`)
        }
      }
    | Some(t) => {
        switch t {
        | #LParens => waitingFor->Js.Array2.push((#RParens, #Parens))->ignore
        | #LCurly => waitingFor->Js.Array2.push((#RCurly, #Curly))->ignore
        | #LAngle => waitingFor->Js.Array2.push((#RAngle, #Angle))->ignore
        | #LBracket => waitingFor->Js.Array2.push((#RBracket, #Bracket))->ignore
        | t =>
          switch waitingFor->Js.Array2.pop {
          | Some((t', chunk)) if t' == t => chunks->Js.Array2.push(chunk)->ignore
          | Some((expected, _)) => {
              badScore := part1score(t)
              failwith(`Expected ${tokenToJs(expected)} but got ${tokenToJs(t)}`)
            }
          | None => {
              completion := Some(waitingFor->Js.Array2.map(fst)->Belt.Array.reverse)
              failwith(`Found ${tokenToJs(t)} but wasn't expecting a closing tag`)
            }
          }
        }
        loop(index + 1)
      }
    }
  }
  try {
    loop(0)
    (Some(chunks), None, None)
  } catch {
  | _e => (None, badScore.contents, completion.contents)
  }
}

let parseString = s => s->Js.String2.split("")->Js.Array2.map(tokenFromChar)->parseTokens

let part1 = input => {
  input
  ->Utils.processInput
  ->Js.Array2.map(parseString)
  ->Belt.Array.keepMap(((_, score, _)) => score)
  ->Utils.sumArray
}

let part2 = input => {
  let scores =
    input
    ->Utils.processInput
    ->Js.Array2.map(parseString)
    ->Belt.Array.keepMap(((_, _, completion)) => completion)
    ->Js.Array2.map(tokensToPart2Score)
    ->Utils.sortBigInts

  scores[scores->Js.Array.length / 2]
}

let _ = Utils.readInput(10)->part1->Utils.tapLog("part1")
let _ = Utils.readInput(10)->part2->Utils.tapLog("part2")
//let _ = parseString("(<>{>")->Utils.tapLog("parsed")
