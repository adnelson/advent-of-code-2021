let testInput = `
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
`

type board = array<array<int>>

let parseInput: string => (array<int>, array<board>) = input => {
  let input = Utils.processInput(input)
  let numbers = input[0]->Js.String2.split(",")->Js.Array2.map(int_of_string)
  let boards = []
  let rest = input->Belt.Array.sliceToEnd(1)
  let rec loop = (startIdx: int) => {
    if startIdx < rest->Js.Array.length {
      let rows = rest->Js.Array2.slice(~start=startIdx, ~end_=startIdx + 5)
      boards
      ->Js.Array2.push(
        rows->Js.Array2.map(row =>
          row
          ->Js.String.trim
          ->Js.String2.splitByRe(%re(`/\s+/g`))
          ->Belt.Array.keepMap(s => s)
          ->Js.Array2.map(int_of_string)
        ),
      )
      ->ignore
      loop(startIdx + 5)
    }
  }
  loop(0)
  (numbers, boards)
}

let hasBingo: (JsSet.t<int>, board) => bool = (calledNumbers, board) => {
  let allCalled = (arr: array<int>): bool => arr->Js.Array2.every(n => calledNumbers->JsSet.has(n))
  let rowHasBingo = board->Js.Array2.some(allCalled)
  let columnHasBingo = board->Utils.transpose->Js.Array2.some(allCalled)
  /* let diagUpperLeftBottomRight = Belt.Array.makeBy(5, idx => board[idx][idx]) */
  /* let diagULBRHasBingo = diagUpperLeftBottomRight->allCalled */
  /* let diagLowerLeftTopRight = Belt.Array.makeBy(5, idx => board[4 - idx][idx]) */
  /* let diagLLTRHasBingo = diagLowerLeftTopRight->allCalled */

  rowHasBingo || columnHasBingo // || diagULBRHasBingo || diagLLTRHasBingo
}

let getBingoScore: (array<int>, board) => option<{"score": int, "index": int}> = (
  numbers,
  board,
) => {
  let calledNumbers = JsSet.empty()
  let rec loop = index => {
    switch numbers->Belt.Array.get(index) {
    | Some(number) => {
        calledNumbers->JsSet.addMut(number)->ignore
        if hasBingo(calledNumbers, board) {
          let sumUncalledNumbers =
            board->Belt.Array.reduce(0, (sum, row) =>
              sum + row->Js.Array2.filter(n => !(calledNumbers->JsSet.has(n)))->Utils.sumArray
            )
          Some({"score": sumUncalledNumbers * number, "index": index})
        } else {
          loop(index + 1)
        }
      }
    | None => None
    }
  }
  loop(0)
}

let getFirstBingoScore: (array<int>, array<board>) => option<int> = (numbers, boards) => {
  let scoringBoards =
    boards->Belt.Array.keepMap(board => getBingoScore(numbers, board));
  scoringBoards->Utils.minBy(board => board["index"])->Belt.Option.map(board => board["score"])
}

let getLastBingoScore: (array<int>, array<board>) => option<int> = (numbers, boards) => {
  let scoringBoards =
    boards->Belt.Array.keepMap(board => getBingoScore(numbers, board));
  scoringBoards->Utils.maxBy(board => board["index"])->Belt.Option.map(board => board["score"])
}

let (numbers, boards) = Utils.readInput(4)->parseInput
//Js.log(getFirstBingoScore(numbers, boards))

//let (numbers, boards) = testInput->parseInput
Js.log(getLastBingoScore(numbers, boards))
