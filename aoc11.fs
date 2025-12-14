namespace AOC2025

module AOC11 =
    open System.Collections.Generic

    type Device = { id: string; outputs: string array }

    let parseLine (line: string) : Device =
        let segments = line.Split ": "
        let id = segments[0]
        let outputs = segments[1].Split " "
        { id = id; outputs = outputs }

    let rec findPaths (current: string) (target: string) (devices: Device array) =
        if current = target then
            1
        else
            devices
            |> Array.find (fun device -> device.id = current)
            |> _.outputs
            |> Array.map (fun output -> findPaths output target devices)
            |> Array.sum

    let solveA (lines: string array) =
        lines |> Array.map parseLine |> findPaths "you" "out" |> string

    let rec findPathsMemoized
        (current: string)
        (target: string)
        (known: Dictionary<string, int64>)
        (devices: Device array)
        =
        if current = target then
            int64 1
        else if known.ContainsKey current then
            known[current]
        else
            devices
            |> Array.find (fun device -> device.id = current)
            |> _.outputs
            |> Seq.ofArray
            |> Seq.map (fun output ->
                let result = findPathsMemoized output target known devices
                known.TryAdd(output, result) |> ignore
                result)
            |> Seq.sum

    let solveB (lines: string array) =
        let devices =
            lines
            |> Array.map parseLine
            |> Array.append [| { id = "out"; outputs = Array.empty } |]

        let fromDacToOut = devices |> findPathsMemoized "dac" "out" (Dictionary())
        let fromFftToDac = devices |> findPathsMemoized "fft" "dac" (Dictionary())
        let fromDacToFft = devices |> findPathsMemoized "dac" "fft" (Dictionary())
        assert (fromDacToFft = 0)
        let fromSvrToFft = devices |> findPathsMemoized "svr" "fft" (Dictionary())
        string (fromDacToOut * fromFftToDac * fromSvrToFft)
