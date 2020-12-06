// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

[<EntryPoint>]
let main argv =
    let lns = File.ReadAllText("resources/input.txt")
    let groups = lns.Split("\n\n")
    let customs = groups |> Seq.map ((fun g -> g.Replace("\n", "")) >> Seq.distinct >> Seq.length)
    printfn "Part 1: %d" (Seq.sum customs)
    0 // return an integer exit code
