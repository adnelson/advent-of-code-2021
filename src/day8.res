let testInput = `
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |fgae cfgab fg bagce
`

@new external charSet: string => JsSet.t<string> = "Set"
let charsInCommon = (s1, s2) => charSet(s1)->JsSet.intersection(charSet(s2))
let numCharsInCommon = (s1, s2) => charsInCommon(s1, s2)->JsSet.size

let parseSigPatterns = sigPatterns => {
  let patterns = sigPatterns->Js.String2.split(" ")
  // Start with three patterns we can find by length alone
  let one = patterns->Js.Array2.find(s => s->Js.String.length == 2)->Belt.Option.getExn
  let four = patterns->Js.Array2.find(s => s->Js.String.length == 4)->Belt.Option.getExn
  let seven = patterns->Js.Array2.find(s => s->Js.String.length == 3)->Belt.Option.getExn

  // Find six, it is a pattern with length 6 and it has only one in common with 1
  let six =
    patterns
    ->Js.Array2.find(s => s->Js.String.length == 6 && numCharsInCommon(s, one) == 1)
    ->Belt.Option.getExn

  // Zero has six letters, two in common with 1, and 3 in common with 4
  let zero =
    patterns
    ->Js.Array2.find(s =>
      s->Js.String.length == 6 && numCharsInCommon(s, one) == 2 && numCharsInCommon(s, four) == 3
    )
    ->Belt.Option.getExn

  // Nine has 6 letters and contains 4
  let nine =
    patterns
    ->Js.Array2.find(s => s->Js.String.length == 6 && numCharsInCommon(s, four) == 4)
    ->Belt.Option.getExn

  // Three has length 5 and two in common with 1
  let three =
    patterns
    ->Js.Array2.find(s => s->Js.String.length == 5 && numCharsInCommon(s, one) == 2)
    ->Belt.Option.getExn

  // Five has 5 letters and all are contained in 6
  let five =
    patterns
    ->Js.Array2.find(s => s->Js.String.length == 5 && numCharsInCommon(s, six) == 5)
    ->Belt.Option.getExn

  // Two has 5 letters, one in common with 1 and is not five :|
  let two =
    patterns
    ->Js.Array2.find(s =>
      s->Js.String.length == 5 &&
      numCharsInCommon(s, one) == 1 &&
      s->Utils.sortString != five->Utils.sortString
    )
    ->Belt.Option.getExn
  // Eight has 7 letters
  let eight = patterns->Js.Array2.find(s => s->Js.String.length == 7)->Belt.Option.getExn

  // Return a dictionary with each pattern sorted as a key, mapping to the digit
  Js.Dict.fromArray([
    (zero->Utils.sortString, "0"),
    (one->Utils.sortString, "1"),
    (two->Utils.sortString, "2"),
    (three->Utils.sortString, "3"),
    (four->Utils.sortString, "4"),
    (five->Utils.sortString, "5"),
    (six->Utils.sortString, "6"),
    (seven->Utils.sortString, "7"),
    (eight->Utils.sortString, "8"),
    (nine->Utils.sortString, "9"),
  ])
}

let decodeOutputValue = (patterns, output) =>
  output
  ->Js.String2.split(" ")
  ->Js.Array2.map(s =>
    switch patterns->Js.Dict.get(s->Utils.sortString) {
    | Some(digit) => digit
    | None => {
        Js.log2(patterns, s)
        failwith(`no pattern matching ${s}`)
      }
    }
  )
  ->Js.Array2.joinWith("")

let decodeEntry1 = entry =>
  switch entry->Js.String2.split("|")->Js.Array2.map(Js.String.trim) {
  | [_, output] => output->Js.String2.split(" ")
  | _ => failwith(`invalid entry ${entry}`)
  }

let decodeInput1 = input => input->Utils.processInput->Js.Array2.map(decodeEntry1)

let decodeEntry2 = entry =>
  switch entry->Js.String2.split("|")->Js.Array2.map(Js.String.trim) {
  | [sigPatterns, output] => {
      let patterns = parseSigPatterns(sigPatterns) // ->Utils.tapLog("sigPatterns")
      decodeOutputValue(patterns, output)->Utils.parseInt(10)
    }
  | _ => failwith(`invalid entry ${entry}`)
  }

let decodeInput2 = input => input->Utils.processInput->Js.Array2.map(decodeEntry2)

let part1 = () =>
  Utils.readInput(8)
  ->decodeInput1
  ->Utils.flatMap(arr =>
    arr->Js.Array2.filter(s => [2, 4, 3, 7]->Js.Array2.includes(s->Js.String.length))
  )
  ->Js.Array.length

let part2 = () => Utils.readInput(8)->decodeInput2->Utils.sumArray

let _ = part1()->Utils.tapLog("part1")
let _ = part2()->Utils.tapLog("part2")
