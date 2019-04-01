#load "./Common.fsx"

open System
open Common

let groupCount = 8
let totalCount = 5000000
let seedGen = Random()
let seeds = List.replicate groupCount () |> List.map seedGen.Next

#time
let groups =
    seeds
    |> List.map (fun s ->
        let rGen = Random s
        async {
            let mutable n = 0
            let max = totalCount / groupCount
            return [
                while n < max do
                    yield rGen.Next()
                    n <- n + 1
            ]
        })
    |> Async.Parallel

async {
    let! gs = groups
    let rsarr = ResizeArray(totalCount)
    for g in gs do
        rsarr.AddRange g
    rsarr.ToArray()
    |> writeToBinaryFile "5mInt.data"
} |> Async.Start
#time
