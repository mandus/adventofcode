open System.IO

module aoc =

    let fn = "input.txt"    
    let readfile fp = File.ReadAllLines(fp)

    let data = readfile(fn)

    let calc func vs =
        vs |> Array.fold func 0

    let chrval c =
        (58 + (int c-96)) % 58

    let prio1 acc (line:string) =
        let half = line.Length / 2
        let left, right  = Set(line[..half-1]), Set(line[half..])
        let lprio = Set.intersect left right |> Set.toList
        acc + chrval lprio[0]

    let prio2 x y z =
        let sx, sy, sz = Set(x), Set(y), Set(z)
        let lprio = Set.intersect (Set.intersect sx sy) sz |> Set.toList
        chrval lprio[0]

    let gathertriple vs =
        let rec loop vs acc = 
            match vs with
            | x::y::z::vs ->
                loop vs (acc + prio2 x y z)
            |_ -> acc
        loop vs 0

    printfn "p1: %d" (calc prio1 data)
    printfn "p2: %d" (gathertriple (data |> Array.toList))
    