let input = Node.Fs.readFileSync("./input1.txt", #utf8)
let numbers: array<int> =
  input->Js.String2.split("\n")->Js.Array2.filter(s => s != "")->Js.Array2.map(int_of_string)

let getWindows: array<int> => array<int> = numbers => {
  let windows = []
  numbers->Js.Array2.forEachi((n, index) => {
    let back1 = numbers->Belt.Array.get(index - 1)
    let back2 = numbers->Belt.Array.get(index - 2)
    switch (back1, back2) {
    | (Some(back1), Some(back2)) => windows->Js.Array2.push(n + back1 + back2)->ignore
    | _ => ()
    }
  })

  windows
}

let getNumIncreases: array<int> => int = numbers => {
  let numIncreases = ref(0)
  let prevNum = ref(None)
  numbers->Js.Array2.forEach(n => {
    switch prevNum.contents {
    | Some(prev) if n > prev => numIncreases := numIncreases.contents + 1
    | _ => ()
    }
    prevNum := Some(n)
  })
  numIncreases.contents
}

let _ = numbers->getWindows->Utils.tapLog("windows")->getNumIncreases->Utils.tapLog("increases")
