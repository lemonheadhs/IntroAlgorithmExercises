#load "./Common.fsx"

open Common

let data = readFromBinaryFile<int []> "5mInt.data"

(*
Algorithm 

Description - pseudo code
Proof       - Loop invariants, Mathematical induction (Initialization, Maintenance, Termination)
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

#time "on"
InsertionSort data
#time "off"


Exercise "2.1-2 pre"
type SortDirection = | Asc | Desc

Exercise "2.1-2 ans" (
    let InsertionSort direction (data: int []) =
        let compare =
            match direction with
            | Asc -> (>)
            | Desc -> (<)
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
                while i >=0 && compare sorted.[i] key do
                    sorted.[i + 1] <- sorted.[i]
                    i <- i - 1
                sorted.[i + 1] <- key
            sorted
    let testData = [|5;7;4;9;0;2;3|]
    testData |> InsertionSort Asc
    testData |> InsertionSort Desc
)

Exercise "2.1-3" (
    let findV (data: int []) v =
        let mutable result = None
        let mutable n = 0
        while n < data.Length && result = None do        
            if data.[n] = v then
                result <- Some n
            n <- n + 1
        result
    (*
    Proof:
      Init: no prev value had been examined, so current result would be None
      Maint: prev (n - 1) values yield None; examine the current n-th value, 
             if not eq v, then None for this time; combined with prev result, it's stil None
             if eq v, resulting Some n
      Terminate: ...
    *)
    let findV' (data: int []) v =
        let rec tryNext (data: int []) n v =
            match n < data.Length, Lazy (data.[n] = v) with
            | false, _ -> None
            | true, Lazy true -> Some n
            | _ -> tryNext data (n + 1) v
        tryNext data 0 v
    ()
)

Exercise "2.1-4 pre" 
type BinaryPrime = | Zero | One
type BinaryNum = BinaryPrime []

Exercise "2.1-4 ans" (
    let add (a: BinaryNum) (b: BinaryNum) =
        // assume a, b have same length
        let n = a.Length
        let c = Array.zeroCreate (n + 1)
        let mutable carry = Zero
        for j = 0 to (n - 1) do
            let res =
                [ a.[j]; b.[j]; carry ]
                |> List.sumBy (function | Zero -> 0 | One -> 1)
                |> function
                | 0 ->
                    carry <- Zero
                    Zero
                | 1 ->
                    carry <- Zero
                    One
                | _ ->
                    carry <- One
                    One            
            c.[j] <- res
        c.[n] <- carry
    ()
)

Exercise "2.2-2" (
    let SelectionSort (data: int []) =
        let sorted = Array.copy data
        match data.Length with
        | 0 -> sorted
        | n ->
            for j = 0 to (n - 1) do
                let mutable leastPos = j
                let mutable i = j
                while i < n - 1 do
                    i <- i + 1
                    if sorted.[leastPos] > sorted.[i] then
                        leastPos <- i
                let mutable least = sorted.[leastPos]
                sorted.[leastPos] <- sorted.[j]
                sorted.[j] <- least
            sorted
    (*
    Proof:
      Init: imagine there is a (-1 .. -1) empty sub list at the very beginning of 
            the original list; it has no element, so can be viewed as sorted; whole list can be 
            viewed as one 0-elements sub list, which is sorted and contains the most least 0 
            elements of the whole list, concatenates the rest of the original list
      Maint: prev step results in a n-elements sorted sub list, which contains the most least n 
             elements of the original list, followed by a list contains the rest elements;
             at the current step, find out the least element from the unsorted sub list, that element
             would naturally be the n+1 th least element of the whole list; append the element after
             that prev sorted sub list, we get a new (n+1) elements sorted sub list which contains the most least (n+1)
             elements of the original list; rest elements forms an unsorted sub list
      Terminate: if the unsorted sub list only exists 2 elements; once we find the least between the 2,
                 the remained element naturally would be the greatest among the original list, it's should be put to
                 the end of sorted list
    *)
    ()
)

