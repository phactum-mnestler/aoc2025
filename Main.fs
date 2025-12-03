namespace AOC2025

open System
open System.Text.Json

module Program =
    let solutions =
        Map
            [ "01", (AOC01.solveA, AOC01.solveB)
              "02", (AOC02.solveA, AOC02.solveB)
              "03", (AOC03.solveA, AOC03.solveB) ]

    [<EntryPoint>]
    let main args =
        printfn "Running example-%s" args[0]
        let solveA, solveB = solutions[args[0]]
        let solve = if args[1] = "a" then solveA else solveB
        let exampleLines = System.IO.File.ReadAllLines($"inputs/example-{args[0]}.txt")
        printfn "Result: %s" (solve exampleLines)
        Console.ReadKey(true) |> ignore
        printfn "Running inputs-%s" args[0]
        let inputLines = System.IO.File.ReadAllLines($"inputs/input-{args[0]}.txt")
        printfn "Result: %s" (solve inputLines)
        0
