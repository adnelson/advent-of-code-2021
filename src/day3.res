let testInput = `
00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
`

let parse10 = s =>
  switch s {
  | "1" => 1
  | "0" => 0
  | _ => failwith(`invalid number ${s}`)
  }

let parseInput: string => array<array<int>> = input =>
  input->Utils.processInput->Js.Array2.map(row => row->Js.String2.split("")->Js.Array2.map(parse10))

let getColumn: (array<array<int>>, int) => array<int> = (matrix, colIndex) =>
  matrix->Js.Array2.map(row => row[colIndex])

let mostCommonDigit: array<int> => int = digits => {
  let zeros = ref(0)
  let ones = ref(0)
  digits->Js.Array2.forEach(digit =>
    switch digit {
    | 0 => zeros := zeros.contents + 1
    | 1 => ones := ones.contents + 1
    | _ => failwith("impossible")
    }
  )

  zeros.contents > ones.contents ? 0 : 1
}

let leastCommonDigit: array<int> => int = digits =>
  switch mostCommonDigit(digits) {
  | 1 => 0
  | _ => 1
  }

let binaryToInt: array<int> => int = digits => digits->Js.Array2.joinWith("")->Utils.parseInt(2)

let gammaDigits: array<array<int>> => array<int> = matrix => {
  let len = matrix[0]->Js.Array2.length
  Belt.Array.makeBy(len, colIndex => matrix->getColumn(colIndex)->mostCommonDigit)
}

let epsilonDigits: array<array<int>> => array<int> = matrix => {
  let len = matrix[0]->Js.Array2.length
  Belt.Array.makeBy(len, colIndex => matrix->getColumn(colIndex)->leastCommonDigit)
}

let powerConsumption: array<array<int>> => int = matrix => {
  let gamma = matrix->gammaDigits->binaryToInt
  let epsilon = matrix->epsilonDigits->binaryToInt
  gamma * epsilon
}

let oxygenGeneratorRating: array<array<int>> => array<int> = matrix => {
  let rec loop = (colIndex, rows) => {
    let mostCommon = rows->getColumn(colIndex)->mostCommonDigit
    let matchingRows = rows->Js.Array2.filter(row => row[colIndex] == mostCommon)
    switch matchingRows {
    | [row] => row
    | rows => loop(colIndex + 1, rows)
    }
  }
  loop(0, matrix)
}

let co2ScrubberRating: array<array<int>> => array<int> = matrix => {
  let rec loop = (colIndex, rows) => {
    let leastCommon = rows->getColumn(colIndex)->leastCommonDigit
    let matchingRows = rows->Js.Array2.filter(row => row[colIndex] == leastCommon)
    switch matchingRows {
    | [row] => row
    | rows => loop(colIndex + 1, rows)
    }
  }
  loop(0, matrix)
}

let lifeSupportRating: array<array<int>> => int = matrix => {
  let oxy = matrix->oxygenGeneratorRating->binaryToInt
  let co2 = matrix->co2ScrubberRating->binaryToInt
  oxy * co2
}

// Js.log(Utils.readInput(3)->parseInput->lifeSupportRating)
//Js.log(testInput->parseInput->lifeSupportRating)
