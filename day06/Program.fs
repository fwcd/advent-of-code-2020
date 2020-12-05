// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

[<EntryPoint>]
let main argv =
    let lns = File.ReadAllLines("resources/input.txt")
    printfn "%s" (String.concat " " lns)
    0 // return an integer exit code
