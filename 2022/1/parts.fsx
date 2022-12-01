open System.IO

module aoc =
    
    // let fn = "test_input.txt"
    let fn = "input.txt"

    let readfile filePath = File.ReadAllText(filePath);
    let agglist vs =
        let rec loop vs acc =
            match vs with
            | v::[] -> 
                acc + (v |> int)
            | v::vs ->
                loop vs acc + (v |> int)
            | _ -> acc
        loop vs 0

    let aggnested vs =
        let rec loop vs acc = 
            match vs with
            | v:string :: [] -> 
                let vtr = v.Trim()
                if vtr.Contains("\r\n") then
                    (vtr.Split("\r\n") |> Array.toList |> agglist) :: acc
                else
                    (v.Split("") |> Array.toList |> agglist) :: acc
            | v:string :: vs -> 
                loop vs ((v.Split("\r\n") |> Array.toList |> agglist) :: acc)
            | _ -> acc
        loop vs []

    let text = readfile(fn)
    let sums = text.Split("\r\n\r\n") |> Array.toList |> aggnested

    let p1val = sums |> List.sortByDescending (fun x -> x) |> List.take 1 |> List.sum
    printfn $"Part1: %d{p1val}"

    let p2val = sums |> List.sortByDescending (fun x -> x) |> List.take 3 |> List.sum
    printfn $"Part2: %d{p2val}"