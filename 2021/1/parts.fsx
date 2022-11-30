#!/usr/bin/env dotnet fsi

module aoc =
    
    // let fn = "test_input.txt"
    let fn = "input.txt"

    let readLines filePath = System.IO.File.ReadAllLines(filePath);
    let parseInt line = line |> int
    let values = readLines(fn)
    let parsedVals = Array.map parseInt values
    
    let countdept vs =
        let rec loop vs acc = 
            match vs with
            | v::[] -> acc
            | v::vs -> if v < vs[0] then loop vs (acc + 1) else loop vs acc
        loop vs 0

    let countdeeper vs = 
        let rec loop vs acc =
            match vs with
            | v::vs -> 
                if vs.Length < 3 then acc
                else
                    if v < vs[2] then loop vs (acc + 1) else loop vs acc
            | _ -> acc
        loop vs 0

    let depth1 = parsedVals |> Array.toList |> countdept
    printfn $"part 1 is {depth1}"

    let depth2 = parsedVals |> Array.toList |> countdeeper
    printfn $"part 2 is {depth2}"
