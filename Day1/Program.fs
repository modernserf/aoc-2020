let parse_input ins =
    ins |> List.map System.Int32.Parse |> Set.ofList

let rec readlines () =
    seq {
        let line = System.Console.ReadLine()

        if line <> null then
            yield line
            yield! readlines ()
    }


let find_pair set sum =
    set
    |> Set.toList
    |> List.map (fun value -> (value, sum - value))
    |> List.tryFind (fun (_, r) -> set |> Set.contains r)


exception NotFound

let rec find_triple list set =
    match list with
    | [] -> raise NotFound
    | item :: rest ->
        let remaining = 2020 - item

        match find_pair set remaining with
        | None -> find_triple rest set
        | Some (l, r) -> (item, l, r)



[<EntryPoint>]
let main _ =
    let input = readlines ()

    let numbers =
        input |> Seq.map System.Int32.Parse |> List.ofSeq

    let set = numbers |> Set.ofList

    match find_pair set 2020 with
    | Some (l, r) -> printfn "part 1: %d" (l * r)
    | None -> raise NotFound

    let (a, b, c) = find_triple numbers set
    printfn "part 2 %d" (a * b * c)

    0
