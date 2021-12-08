let testInput = `
  3,4,3,1,2
`

type bigint

@val external bigintFromInt: int => bigint = "BigInt"
let plusBI: (bigint, bigint) => bigint = (a, b) => {
  let _ = (a, b)
  %raw(`a + b`)
}
let incBigInt: bigint => bigint = bi => plusBI(bi, bigintFromInt(1))

type counts = {
  zero: bigint,
  one: bigint,
  two: bigint,
  three: bigint,
  four: bigint,
  five: bigint,
  six: bigint,
  seven: bigint,
  eight: bigint,
}

let total: counts => bigint = counts =>
  counts.zero
  ->plusBI(counts.one)
  ->plusBI(counts.two)
  ->plusBI(counts.three)
  ->plusBI(counts.four)
  ->plusBI(counts.five)
  ->plusBI(counts.six)
  ->plusBI(counts.seven)
  ->plusBI(counts.eight)

let tick: counts => counts = counts => {
  eight: counts.zero,
  seven: counts.eight,
  six: plusBI(counts.seven, counts.zero),
  five: counts.six,
  four: counts.five,
  three: counts.four,
  two: counts.three,
  one: counts.two,
  zero: counts.one,
}

let countsFromArray: array<int> => counts = nums =>
  nums->Belt.Array.reduce(
    {
      zero: bigintFromInt(0),
      one: bigintFromInt(0),
      two: bigintFromInt(0),
      three: bigintFromInt(0),
      four: bigintFromInt(0),
      five: bigintFromInt(0),
      six: bigintFromInt(0),
      seven: bigintFromInt(0),
      eight: bigintFromInt(0),
    },
    (counts, n) =>
      switch n {
      | 0 => {...counts, zero: counts.zero->incBigInt}
      | 1 => {...counts, one: counts.one->incBigInt}
      | 2 => {...counts, two: counts.two->incBigInt}
      | 3 => {...counts, three: counts.three->incBigInt}
      | 4 => {...counts, four: counts.four->incBigInt}
      | 5 => {...counts, five: counts.five->incBigInt}
      | 6 => {...counts, six: counts.six->incBigInt}
      | 7 => {...counts, seven: counts.seven->incBigInt}
      | _ => {...counts, eight: counts.eight->incBigInt}
      },
  )

let parseInput = input =>
  input->Js.String2.trim->Js.String2.split(",")->Js.Array2.map(int_of_string)->countsFromArray

let tickAll: (counts, int) => counts = (initialCounts, days) => {
  let counts = ref(initialCounts)
  for _i in 0 to days - 1 {
    counts := counts.contents->tick
  }
  counts.contents
}

let _ = Js.log(Utils.readInput(6)->parseInput->tickAll(256)->total)

//Js.log(testInput->parseInput->tick->tick->tick);
