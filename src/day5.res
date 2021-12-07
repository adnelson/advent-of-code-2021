let testInput = `
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
`

type point = {
  x: int,
  y: int,
}

type segment = {
  start: point,
  end_: point,
}
let parsePoint = pointStr =>
  switch pointStr->Js.String2.split(",") {
  | [x, y] => {x: x->int_of_string, y: y->int_of_string}
  | _ => failwith(`invalid point format: ${pointStr}`)
  }

let parseInput = input => {
  input
  ->Utils.processInput
  ->Js.Array2.map(line =>
    switch line->Js.String2.split(" -> ") {
    | [startPoint, endPoint] => {start: startPoint->parsePoint, end_: endPoint->parsePoint}
    | _ => failwith(`invalid line ${line}`)
    }
  )
}

let getPointsInSegment: segment => array<point> = ({start, end_}) => {
  if start.x == end_.x {
    // vert
    let minY = Utils.min([end_.y, start.y])
    Belt.Array.makeBy(Js.Math.abs_int(end_.y - start.y) + 1, y => {x: start.x, y: minY + y})
  } else if start.y == end_.y {
    // horiz
    let minX = Utils.min([end_.x, start.x])
    Belt.Array.makeBy(Js.Math.abs_int(end_.x - start.x) + 1, x => {x: minX + x, y: start.y})
  } else if start.x < end_.x && start.y < end_.y {
    // down-right
    let res = Belt.Array.makeBy(end_.x - start.x + 1, offset => {
      x: start.x + offset,
      y: start.y + offset,
    })
    res
  } else if start.x < end_.x && start.y > end_.y {
    // up-right
    let res = Belt.Array.makeBy(end_.x - start.x + 1, offset => {
      x: start.x + offset,
      y: start.y - offset,
    })
    res
  } else if start.x > end_.x && start.y < end_.y {
    // down-left
    let res = Belt.Array.makeBy(start.x - end_.x + 1, offset => {
      x: start.x - offset,
      y: start.y + offset,
    })
    res
  } else {
    // up-left
    let res = Belt.Array.makeBy(start.x - end_.x + 1, offset => {
      x: start.x - offset,
      y: start.y - offset,
    })
    res
  }
}

let isPointInSegment: (point, segment) => bool = (point, segment) => {
  segment->getPointsInSegment->Js.Array2.some(({x, y}) => x == point.x && y == point.y)
}

let numRows: array<segment> => int = segments =>
  segments->Utils.flatMap(seg => [seg.start.y, seg.end_.y])->Utils.max

let numColumns: array<segment> => int = segments =>
  segments->Utils.flatMap(seg => [seg.start.x, seg.end_.x])->Utils.max

let printGrid = grid => {
  grid
  ->Js.Array2.map(row =>
    row
    ->Js.Array2.map(count =>
      switch count {
      | 0 => "."
      | n => n->string_of_int
      }
    )
    ->Js.Array2.joinWith("")
  )
  ->Js.Array2.joinWith("\n")
}

let getGridMap = segments => {
  let rows: JsMap.t<int, JsMap.t<int, int>> = JsMap.empty()
  segments
  ->Utils.flatMap(getPointsInSegment)
  ->Js.Array2.forEach(point => {
    let row = rows->JsMap.get(point.y)
    switch row {
    | None => rows->JsMap.setMut(point.y, JsMap.fromArray([(point.x, 1)]))->ignore
    | Some(col) =>
      switch col->JsMap.get(point.x) {
      | None => col->JsMap.setMut(point.x, 1)->ignore
      | Some(count) => col->JsMap.setMut(point.x, count + 1)->ignore
      }
    }
  })

  rows
}

let getGrid = segments => {
  let (rows, cols, grid) = (numRows(segments), numColumns(segments), getGridMap(segments))
  Belt.Array.makeBy(rows + 1, rowId =>
    Belt.Array.makeBy(cols + 1, colId =>
      grid
      ->JsMap.get(rowId)
      ->Belt.Option.flatMap(row => row->JsMap.get(colId))
      ->Belt.Option.getWithDefault(0)
    )
  )
}

let _ = Js.log(
  Utils.readInput(5)
  ->parseInput
  ->getGrid
  ->Utils.flatten
  ->Js.Array2.filter(n => n > 1)
  ->Js.Array2.length,
)
