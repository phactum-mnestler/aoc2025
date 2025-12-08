namespace AOC2025

module AOC06 =

    let parseOperator s : int64 -> int64 -> int64 = if s = "*" then (*) else (+)

    let parseNumbers (line: string) =
        line.Split(" ") |> Array.filter (fun x -> x.Length > 0) |> Array.map int64

    let applyOps ops prev next =
        Array.zip3 prev next ops |> Array.map (fun (a, b, op) -> op a b)

    let solveA (lines: string array) =
        let operators =
            Array.last lines
            |> _.Split(" ")
            |> Array.filter (fun x -> x.Length > 0)
            |> Array.map parseOperator

        let values =
            lines
            |> Array.removeAt (lines.Length - 1)
            |> Array.map parseNumbers
            |> Array.reduce (applyOps operators)

        values |> Array.sum |> string

    let reverseString (str: string) =
        str.ToCharArray() |> Array.rev |> System.String

    let extractOperators (lines: string array) =
        let temp =
            (Array.last lines).ToCharArray()
            |> Array.mapi (fun i c -> i, c)
            |> Array.filter (fun (i, c) -> c = '*' || c = '+')
            |> Array.pairwise

        let firstOp, _ = temp[0]
        Array.append [| ((-2, '+'), firstOp) |] temp

    let parseColumn (lines: string array) (index: int) =
        lines
        |> Array.removeAt (lines.Length - 1)
        |> Array.map (fun line -> line[index])
        |> Array.filter System.Char.IsDigit
        |> Array.map System.Char.GetNumericValue
        |> Array.map int64
        |> Array.fold (fun acc next -> acc * int64 (10) + next) (int64 0)

    let parseValues (fromIndex: int) (toIndex: int) (lines: string array) =
        [ fromIndex..toIndex ] |> List.map (parseColumn lines)

    let applyOperator (op: char) (values: int64 list) : int64 =
        let operator = if op = '*' then (*) else (+)
        values |> List.reduce (fun acc next -> operator acc next)

    let calcOpResult lines ((prevI, _), (i, op)) =
        parseValues (prevI + 2) i lines |> applyOperator op

    let solveB (lines: string array) =
        let flipped = Array.map reverseString lines

        extractOperators flipped
        |> Array.map (calcOpResult flipped)
        |> Array.sum
        |> string
