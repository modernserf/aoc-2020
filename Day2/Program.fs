open FSharp.Core

let rec readlines () =
  seq {
    let line = System.Console.ReadLine()

    if line <> null then
      yield line
      yield! readlines ()
  }

type PasswordLine =
    { Min: int
      Max: int
      Letter: char
      Password: string }



let parse_line (line: string) =
  let sections = line.Split [|' '|]
  let minmax = sections[0].Split [|'-'|]
  let min = int minmax[0]
  let max = int minmax[1]
  let letter = sections[1] |> List.ofSeq |> List.head
  let password = sections[2]
  { PasswordLine.Min = min
    Max = max
    Letter = letter
    Password = password }

let check_line (line: PasswordLine) =
  let char_count = line.Password |> Seq.filter (fun ch -> ch = line.Letter) |> Seq.length
  char_count >= line.Min && char_count <= line.Max

let xor a b =
  (a && not b) || (not a && b)

let check_line_2 (line: PasswordLine) =
  let char_at_min = line.Password[line.Min - 1] = line.Letter
  let char_at_max = line.Password[line.Max - 1] = line.Letter
  xor char_at_min char_at_max


[<EntryPoint>]
let main _ =
    let input = readlines () |> Seq.map parse_line |> List.ofSeq
    let valid_lines = input |> List.filter check_line
    printf "part 1: %d\n" (List.length valid_lines)

    let valid_lines_2 = input |> List.filter check_line_2
    printf "part 2: %d\n" (List.length valid_lines_2)
    0
