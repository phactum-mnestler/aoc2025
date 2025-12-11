namespace AOC2025

module AOC08 =
    type Coordinate = (int64 * int64 * int64)

    // 0 19 7 14
    // 2 13 8 18 17

    let parseLine (line: string) =
        let segments = line.Split(",") |> Array.map int64
        Coordinate(segments[0], segments[1], segments[2])

    let equals (a, b) =
        let (aX, aY, aZ) = a
        let (bX, bY, bZ) = b
        aX = bX && aY = bY && aZ = bZ

    let equals2 a b = equals (a, b)

    let distanceSq ((aX, aY, aZ), (bX, bY, bZ)) =
        (bX - aX) * (bX - aX) + (bY - aY) * (bY - aY) + (bZ - aZ) * (bZ - aZ)

    let generatePairs coordinates =
        coordinates
        |> Array.mapi (fun i from -> coordinates |> Array.skip (i + 1) |> Array.map (fun toCoord -> from, toCoord))
        |> Array.concat
        |> Array.sortBy distanceSq

    let invertPair (from, toCoord) = toCoord, from

    let chooseNetwork
        (networks: int ref array)
        (coordinates: Coordinate array)
        (neighbors: Map<Coordinate, Coordinate array>)
        i
        =
        if not (Map.containsKey coordinates[i] neighbors) then
            i, Array.empty
        else
            let result =
                neighbors[coordinates[i]]
                |> Seq.ofArray
                |> Seq.map (fun neighbor -> Array.findIndex (equals2 neighbor) coordinates)
                |> Seq.indexed
                |> Seq.tryFind (fun (i, j) -> i <> j)

            let allNeighboringNetworks =
                neighbors[coordinates[i]]
                |> Array.map (fun neighbor -> Array.findIndex (equals2 neighbor) coordinates)
                |> Array.map (fun i -> int (networks[i].Value))

            match result with
            | Some(_, x) -> x, allNeighboringNetworks
            | None -> i, allNeighboringNetworks

    let printNetwork (networks: int ref array) =
        networks |> Array.iteri (fun i x -> printfn "  %d %d" i x.Value)

    let overrideNetwork newNetwork (toOverride: int array) (networks: int ref array) =
        for i in [ 0 .. networks.Length - 1 ] do
            if Seq.contains networks[i].Value toOverride then
                networks[i].Value <- newNetwork

    let buildNetworks uniquePairs limit =
        let allNeighbors =
            uniquePairs
            |> Array.take limit
            |> Array.fold
                (fun map (from, toCoord) ->
                    map
                    |> Map.change from (fun coords ->
                        match coords with
                        | Some c -> Some(Array.append c [| toCoord |])
                        | None -> Some [| toCoord |])
                    |> Map.change toCoord (fun coords ->
                        match coords with
                        | Some c -> Some(Array.append c [| from |])
                        | None -> Some [| from |]))
                Map.empty

        allNeighbors
        |> Map.toSeq
        |> Seq.indexed
        |> Seq.fold
            (fun net (i, (coord, neighbors)) ->
                let network = if Map.containsKey coord net then net[coord] else i

                neighbors
                |> Array.fold
                    (fun net2 neighbor ->
                        if Map.containsKey neighbor net2 then
                            let toReplace = net2[neighbor]
                            net2 |> Map.map (fun _ n -> if n = toReplace then network else n)
                        else
                            Map.add neighbor network net2)
                    net)
            Map.empty

    let calculateNetworkSizes networks =
        networks
        |> Map.toArray
        |> Array.groupBy (fun (_, network) -> network)
        |> Array.map (fun (_, all) -> all.Length)
        |> Array.sortDescending

    let solveA (lines: string array) =
        let coordinates = lines |> Array.map parseLine
        let uniquePairs = generatePairs coordinates
        let limit = if lines.Length <= 20 then 10 else 1000
        let networks = buildNetworks uniquePairs limit

        calculateNetworkSizes networks |> Array.take 3 |> Array.reduce (*) |> string

    let rec findLimit limitStart limitEnd uniquePairs (coordinates: Coordinate array) =
        printfn "Searching from %d to %d" limitStart limitEnd

        if limitStart = limitEnd then
            limitStart
        else
            let probe = (limitEnd - limitStart) / 2 + limitStart
            let networks = buildNetworks uniquePairs probe

            if networks.Count <> coordinates.Length then
                findLimit (probe + 1) limitEnd uniquePairs coordinates
            else
                let groups = calculateNetworkSizes networks

                if groups.Length = 1 then
                    findLimit limitStart probe uniquePairs coordinates
                else
                    findLimit (probe + 1) limitEnd uniquePairs coordinates

    let solveB (lines: string array) =
        let limitStart = if lines.Length <= 20 then 10 else 1000
        let coordinates = lines |> Array.map parseLine
        let uniquePairs = generatePairs coordinates
        let limit = findLimit limitStart (uniquePairs.Length / 4) uniquePairs coordinates
        printfn "Limit: %d" limit
        let ((fx, _, _), (tx, _, _)) = uniquePairs[limit - 1]
        string (fx * tx)
