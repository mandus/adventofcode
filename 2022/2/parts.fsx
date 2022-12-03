// Rock, Paper, Scissors
// idea:
// map R, P, S (aka A B C) to (0, 1 2)
// 
// For part 1, map X, Y, Z onto the same scale - and use 
// (1 + (m-e)) % 3) (m: me, e: elf) to find who won.
// Multiply with 3 and then add (m + 1) to get the full score.

open System.IO

module aoc = 

    let fn = "input.txt"
    let readfile filePath = File.ReadAllLines(filePath)
    let a = "ABC"

    let score1 (l:string) = 
        let x = "XYZ"
        let ls = l.Split(" ")
        let e_pos = a.IndexOf(ls[0])
        let m_pos = x.IndexOf(ls[1])
        1 + m_pos + 3 * ((1 + (3 + m_pos - e_pos)) % 3) // need to add 3 in pos-subtraction to ensure positive

    let score2 (l:string) = 
        // X: loose, Y: draw, Z: win
        // Y: m_pos = e_pos
        // Z: m_pos = e_pos + 1
        // X: m_pos = e_pos + 2
        // so; just rotate the lookup-string:
        let x = "YZX"

        let ls = l.Split(" ")
        let e_pos = a.IndexOf(ls[0])
        let me_res = x.IndexOf(ls[1])
        let m_pos = (e_pos + me_res) % 3
        1 + m_pos + 3 * ((1 + me_res) % 3) // we know from the expected result what the score will be


    let play funk vs = 
        let rec loop vs acc = 
            match vs with
            | v:string::[] ->
                acc + (funk v)
            | v:string::vs -> 
                loop vs acc + (funk v)
            | _ -> acc
        loop vs 0

    let listlines = readfile(fn)
    let lines = listlines |> Array.toList

    // Replace "play" above with proper fold; move acc to the score funcs
    let fplay funk vs =
        vs |> Array.fold funk 0

    let fscore1 acc (l:string) =
        let x = "XYZ"
        let ls = l.Split(" ")
        let e_pos = a.IndexOf(ls[0])
        let m_pos = x.IndexOf(ls[1])
        acc + 1 + m_pos + 3 * ((1 + (3 + m_pos - e_pos)) % 3) // need to add 3 in pos-subtraction to ensure positive

    let fscore2 acc (l:string) =
        let x = "YZX"
        let ls = l.Split(" ")
        let e_pos = a.IndexOf(ls[0])
        let me_res = x.IndexOf(ls[1])
        let m_pos = (e_pos + me_res) % 3
        acc + 1 + m_pos + 3 * ((1 + me_res) % 3) // we know from the expected result what the score will be

    let p1 = play score1 lines
    printfn "part1: %d" p1
    let p2 = play score2 lines
    printfn "part2: %d" p2

    printfn "fold part1: %d" (fplay fscore1 listlines)
    printfn "fold part2: %d" (fplay fscore2 listlines)
