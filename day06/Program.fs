// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

let fold1 f zs = match Seq.toList zs with
                 | x :: xs -> Seq.fold f x xs
                 | [] -> raise (System.ArgumentException "fold1 needs non-empty list!")

[<EntryPoint>]
let main argv =
    let lns = File.ReadAllText("resources/input.txt")
    let groups = lns.Split("\n\n") |> Seq.map ((fun g -> g.Split("\n") |> Seq.map Set.ofSeq |> Seq.filter (Seq.isEmpty >> not)))
    let part1 = groups |> Seq.map (fold1 Set.union >> Seq.length)
    let part2 = groups |> Seq.map (fold1 Set.intersect >> Seq.length)
    printfn "Part 1: %d" (Seq.sum part1)
    printfn "Part 2: %d" (Seq.sum part2)
    0 // return an integer exit code
