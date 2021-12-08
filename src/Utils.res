let tapLog = (value, description) => {
  Js.log2(description, value)
  value
}

let tapLogMap = (value, description, f) => {
  Js.log2(description, value->f)
  value
}

let tapLogIf = (pred, value, description) => {
  if pred {
    Js.log2(description, value)
  }
  value
}

let optOr = opt1 => opt2 => switch opt1 { | None => opt2 | _ => opt1 }

let readInput = n => Node.Fs.readFileSync(`./input${n->Js.Int.toString}.txt`, #utf8)

@send external sortArray: array<'a> => array<'a> = "sort";

let sortString = s => s->Js.String2.split("")->sortArray->Js.Array2.joinWith("")

let processInput = input =>
  input->Js.String.trim->Js.String2.split("\n")->Js.Array2.filter(s => s != "")

let getColumn: 'a. (array<array<'a>>, int) => array<'a> = (matrix, colIndex) =>
  matrix->Js.Array2.map(row => row[colIndex])

let transpose: 'a. array<array<'a>> => array<array<'a>> = matrix => {
  let transpose = []
  matrix[0]->Js.Array2.forEachi((_, colIndex) =>
    transpose->Js.Array2.push(matrix->getColumn(colIndex))->ignore
  )
  transpose
}

let sumArray: array<int> => int = arr => arr->Belt.Array.reduce(0, (a, b) => a + b)

@val @variadic external max: array<int> => int = "Math.max"
@val @variadic external min: array<int> => int = "Math.min"
@send external flatMap: (array<'a>, 'a => array<'b>) => array<'b> = "flatMap"
@send @variadic external pushMany: (array<'a>, array<'a>) => int = "push"



let flatten: array<array<'a>> => array<'a> = arr => {
  let res = []
  arr->Js.Array2.forEach(row => res->pushMany(row)->ignore)
  res
}

let minBy: 'a. (array<'a>, 'a => int) => option<'a> = (arr, toScore) => {
  let min = ref(None)
  arr->Js.Array2.forEach(x => {
    let score = toScore(x)
    switch min.contents {
    | None => min := Some((x, score))
    | Some((_, minScore)) if score < minScore => min := Some((x, score))
    | _ => ()
    }
  })
  min.contents->Belt.Option.map(fst)
}

let maxBy: 'a. (array<'a>, 'a => int) => option<'a> = (arr, toScore) => {
  let max = ref(None)
  arr->Js.Array2.forEach(x => {
    let score = toScore(x)
    switch max.contents {
    | None => max := Some((x, score))
    | Some((_, maxScore)) if score > maxScore => max := Some((x, score))
    | _ => ()
    }
  })
  max.contents->Belt.Option.map(fst)
}

let splitNums = s => s->Js.String2.split(",")->Js.Array2.map(int_of_string)

@val external parseInt: (string, int) => int = "parseInt"
