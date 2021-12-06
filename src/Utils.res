let tapLog = (value, description) => {
  Js.log2(description, value)
  value
}

let readInput = n => Node.Fs.readFileSync(`./input${n->Js.Int.toString}.txt`, #utf8)

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
