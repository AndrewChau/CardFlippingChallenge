let debug = match fsi.CommandLineArgs.Length with
            | args when args > 1 -> (Array.get fsi.CommandLineArgs 1) = "debug"
            | _ -> false

let printLine() = printfn "-------------------------------------------------------"

let getPrefix (challenge: string) (index: int): string = if index > 0 then challenge.Substring(0, index) else ""

let getSuffix (challenge: string) (index: int): string = if index < (challenge.Length - 1) then challenge.Substring(index + 1) else ""

let flip (challenge: string) (index: int): string =
    let flipResult (currentSide: char) = if currentSide = '0' then '1' elif currentSide = '1' then '0' else '.'
    let flipped = flipResult challenge.[index]
    (getPrefix challenge index) + (string flipped) + (getSuffix challenge index)

let pickup (challenge: string) (index: int): string =
    if debug then
        printfn "%i\t%s" index challenge
    let prefix = getPrefix challenge index
    let updatedPrefix = if (prefix.Length > 0) then flip prefix (prefix.Length - 1) else ""
    let suffix = getSuffix challenge index
    let updatedSuffix = if (suffix.Length > 0) then flip suffix 0 else ""
    updatedPrefix + "." + updatedSuffix

let allZeroesBefore (challenge: string) (index: int) =
    let prefix = getPrefix challenge index
    prefix |> Seq.forall (fun c -> c = '0' || c = '.')

let allZeroesAfter (challenge: string) (index: int) =
    let suffix = getSuffix challenge index
    suffix |> Seq.forall (fun c -> c = '0' || c = '.')

let isSolved (challenge: string) = challenge |> Seq.forall (fun c -> c = '.')

let rec pickupNextCard (challenge: string) (currentChoices: string) =
    let pickupAt =
        // All 0s or . before 0[1]
        if (challenge.IndexOf "01") >= 0 && (allZeroesBefore challenge (challenge.IndexOf "01")) then
            (challenge.IndexOf "01") + 1
        // All 0s or . after [1]0
        elif (challenge.LastIndexOf "10") >= 0 && (allZeroesAfter challenge (challenge.LastIndexOf "10")) then
            (challenge.LastIndexOf "10")
            
        // .[1].
        elif (challenge.IndexOf ".1.") >= 0 then
            (challenge.IndexOf ".1.") + 1

        // Starts with [1].
        elif (challenge.IndexOf "1.") = 0 then
            0
        // Ends with .[1]
        elif (challenge.LastIndexOf ".1") = (challenge.Length - 2) then
            challenge.Length - 1

        // 0[1].
        elif (challenge.IndexOf "01.") >= 0 then
            (challenge.IndexOf "01.") + 1
        // .[1]0
        elif (challenge.IndexOf ".10") >= 0 then
            (challenge.IndexOf ".10") + 1

        // .[1]1
        elif (challenge.IndexOf ".11") >= 0 then
            challenge.IndexOf ".11" + 1
        // 1[1].
        elif (challenge.LastIndexOf "11.") >= 0 then
            (challenge.LastIndexOf "11.") + 1

        // [1]00
        elif (challenge.IndexOf "100") >= 0 then
            challenge.IndexOf "100"
        // 00[1]
        elif (challenge.LastIndexOf "001") >= 0 then
            (challenge.LastIndexOf "001") + 2

        // Starts with [1]0
        elif (challenge.IndexOf "10") = 0 then
            0
        // Ends with 0[1]
        elif (challenge.LastIndexOf "01") = (challenge.Length - 2) then
            challenge.Length - 1

        // 0[1]0
        elif (challenge.IndexOf "010") >= 0 then
            (challenge.IndexOf "010") + 1

        // .[1]
        elif (challenge.IndexOf ".1") >= 0 then
            (challenge.IndexOf ".1") + 1
        // [1].
        elif (challenge.IndexOf "1.") >= 0 then
            challenge.IndexOf "1."

        // Starts with [1]
        elif (challenge.IndexOf "1") = 0 then
            0
        // Ends with [1]
        elif (challenge.IndexOf "1") = (challenge.Length - 1) then
            challenge.Length - 1

        // [1] anywhere
        elif (challenge.IndexOf "1") >= 0 then
            challenge.IndexOf "1"
        else
            -1
    if (pickupAt >= 0) then
        let nextChallenge = pickup challenge pickupAt
        pickupNextCard nextChallenge (currentChoices + (string pickupAt) + ", ")
    else
        if debug then
            printfn "Final: %s" challenge
        if (isSolved challenge) then
            printfn "Answer: %s" currentChoices
        else
            printfn "Answer: UNSOLVABLE"

let tryChallenge (challenge: string) =
    if not debug then
        printfn "Challenge: %s" challenge
    pickupNextCard challenge ""
    printLine()

printLine()



tryChallenge "0100110"
tryChallenge "01001100111" //unsolvable
tryChallenge "100001100101000"
tryChallenge "001011011101001001000"
tryChallenge "1010010101001011011001011101111" // unsolvable
tryChallenge "1101110110000001010111011100110"
tryChallenge "010111111111100100101000100110111000101111001001011011000011000"