let rec readlines () =
    seq {
        let line = System.Console.ReadLine()

        if line <> null then
            yield line
            yield! readlines ()
    }

type Grid = bool array array
type Pos = (int * int)
type Trajectory = (int * int)

let create_grid (lines : seq<string>) =
  lines 
  |> Array.ofSeq 
  |> Array.map (fun line -> line |> Array.ofSeq |> Array.map(fun ch -> ch = '#') )

let rec trees_on_trajectory 
  (grid: Grid) (pos: Pos) (trajectory: Trajectory) state : int =
    let (x, y) = pos
    let (dx, dy) = trajectory
    let width = (Array.length grid[0])
    let height = Array.length grid
    let next_x = (x + dx) % width
    let next_y = y + dy
    let next_state = if grid[y][x] then state + 1 else state

    if next_y >= height then
        next_state
    else
      trees_on_trajectory grid (next_x, next_y) trajectory next_state
      
let all_trees_on_trajectory grid trj =
  int64 (trees_on_trajectory grid (0,0) trj 0)

[<EntryPoint>]
let main _ =
  let input = readlines ()
  let grid = create_grid input
  let tree_count_3_1 = all_trees_on_trajectory grid (3,1)
  printfn "part 1: %d\n" tree_count_3_1

  let tree_count_1_1 = all_trees_on_trajectory grid (1,1)
  let tree_count_5_1 = all_trees_on_trajectory grid (5,1)
  let tree_count_7_1 = all_trees_on_trajectory grid (7,1)
  let tree_count_1_2 = all_trees_on_trajectory grid (1,2)

  let total_product = tree_count_1_1 * tree_count_3_1 * tree_count_5_1 * tree_count_7_1 * tree_count_1_2
  printfn "part 2: %d\n" total_product
  0
