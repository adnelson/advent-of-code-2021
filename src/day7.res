let testInput = `
  16,1,2,0,4,2,7,1,2,14
`;

let parseInput = input => input->Js.String.trim->Utils.splitNums;

let getRange = nums => Belt.Array.range(nums->Utils.min, nums->Utils.max)

let getFuelCost = (nums, pos) => nums->Js.Array2.map(n => Js.Math.abs_int(n - pos))->Utils.sumArray

let getRealCost: (int, int) => int = (start, end_) => {
  let distance = Js.Math.abs_int(start - end_);
  distance * (distance + 1) / 2;
}

let getRealFuelCost = (nums, pos) => nums->Js.Array2.map(n => getRealCost(n, pos))->Utils.sumArray

let getMinFuel = nums => {
  let range = nums->getRange;
  let pos = range->Utils.minBy(pos => getRealFuelCost(nums, pos))->Belt.Option.getExn;
  getRealFuelCost(nums, pos);
}

let _ = Js.log(Utils.readInput(7)->parseInput->getMinFuel)
