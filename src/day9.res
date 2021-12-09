let testInput = `
2199943210
3987894921
9856789892
8767896789
9899965678
`

let parseInput = (input: string) =>
  input
  ->Utils.processInput
  ->Js.Array2.map(s => s->Js.String2.split("")->Js.Array2.map(int_of_string))

let getRiskLevel = (matrix, ~row, ~col) => {
  let get2 = (row, col) =>
    matrix->Belt.Array.get(row)->Belt.Option.flatMap(row => row->Belt.Array.get(col))
  switch get2(row, col) {
  | None => None
  | Some(value) => {
      let neighbors = [
        get2(row - 1, col),
        get2(row + 1, col),
        get2(row, col - 1),
        get2(row, col + 1),
      ]
      neighbors->Utils.keepSome->Js.Array2.every(x => x > value) ? Some(value + 1) : None
    }
  }
}

let part1 = input => {
  let matrix = input->parseInput
  let numRows = matrix->Js.Array2.length
  let numCols = matrix[0]->Js.Array2.length
  Belt.Array.makeBy(numRows, row =>
    Belt.Array.makeBy(numCols, col => (row, col, matrix->getRiskLevel(~row, ~col)))
  )
  ->Utils.flatMap(r => r->Belt.Array.keepMap(((_, _, rl)) => rl))
  ->Utils.sumArray
}

let toKey = (~row: int, ~col: int) => Obj.magic((row, col))->Js.Json.stringify
let fromKey = (key: string): (int, int) => Obj.magic(key->Js.Json.parseExn)
let basinKey = basin => Obj.magic(basin->JsSet.toArray)->Js.Json.stringify
let basinFromKey = key => Obj.magic(key->Js.Json.parseExn)->JsSet.fromArray

let getBasin = (matrix, ~row, ~col) => {
  let get2 = (~row, ~col) =>
    matrix->Belt.Array.get(row)->Belt.Option.flatMap(row => row->Belt.Array.get(col))

  let seen = JsSet.empty()
  let rec loop = (~row: int, ~col: int) => {
    if !(seen->JsSet.has(toKey(~row, ~col))) {
      switch get2(~row, ~col) {
      | None | Some(9) => ()
      | Some(_) => {
          let _ = seen->JsSet.addMut(toKey(~row, ~col))
          loop(~row=row - 1, ~col)
          loop(~row=row + 1, ~col)
          loop(~row, ~col=col - 1)
          loop(~row, ~col=col + 1)
        }
      }
    }
  }

  loop(~row, ~col)
  seen
}

let getBasins = matrix => {
  let numRows = matrix->Js.Array2.length
  let numCols = matrix[0]->Js.Array2.length
  let basins = JsMap.empty()
  for row in 0 to numRows - 1 {
    for col in 0 to numCols - 1 {
      switch basins->JsMap.get(toKey(~row, ~col)) {
      | Some(_basin) => ()
      | None => {
          let basin = getBasin(matrix, ~row, ~col)
          basin->JsSet.forEach(key => basins->JsMap.setMut(key, basin)->ignore)
        }
      }
    }
  }
  basins
  ->JsMap.valuesArray
  ->Js.Array2.map(basinKey)
  ->JsSet.fromArray
  ->JsSet.toArray
  ->Js.Array2.map(basinFromKey)
  ->Utils.sortByCompareOn(s => -JsSet.size(s))
  ->Js.Array2.slice(~start=0, ~end_=3)
  ->Js.Array2.map(JsSet.size)
  ->Utils.multArray
}

let part2 = input => input->parseInput->getBasins

let _ = Utils.readInput(9)->part1->Utils.tapLog("part1")
let _ = Utils.readInput(9)->part2->Utils.tapLog("part2")
