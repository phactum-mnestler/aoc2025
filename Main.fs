namespace AOC2025

open System

module Program =
    let solutions =
        Map
            [ "01", (AOC01.solveA, AOC01.solveB)
              "02", (AOC02.solveA, AOC02.solveB)
              "03", (AOC03.solveA, AOC03.solveB)
              "04", (AOC04.solveA, AOC04.solveB)
              "05", (AOC05.solveA, AOC05.solveB)
              "06", (AOC06.solveA, AOC06.solveB)
              "07", (AOC07.solveA, AOC07.solveB)
              "08", (AOC08.solveA, AOC08.solveB)
              "09", (AOC09.solveA, AOC09.solveB)
              "10", (AOC10.solveA, AOC10.solveB) ]

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
