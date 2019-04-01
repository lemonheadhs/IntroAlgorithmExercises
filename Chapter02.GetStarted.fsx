#load "./Common.fsx"

open Common

(*
Algorithm 

Description - pseudo code
Proof       - Loop invariants, Mathematical induction
Analyse     - single core, RAM model


Algorithm design methods

incremental
divide & conquer



*)

let InsertionSort (data: int []) =
    let sorted = Array.zeroCreate data.Length
    match data.Length with
    | 0 -> sorted
    | 1 ->
        sorted.[0] <- data.[0]
        sorted
    | n ->
        sorted.[0] <- data.[0]
        for j = 1 to (n - 1) do
            let key = data.[j]
            let mutable i = j - 1
            while i >= 0 && sorted.[i] > key do
                sorted.[i + 1] <- sorted.[i]
                i <- i - 1
            sorted.[i + 1] <- key
        sorted   

let data = readFromBinaryFile<int []> "5mInt.data"

#time "on"
InsertionSort data
#time "off"

