// Learn more about F# at http://fsharp.org
open System
open System.Collections.Generic

let swap (arr : 'a []) a b =
    let newArr = Array.copy arr
    let tmp = newArr.[a]
    newArr.[a] <- newArr.[b]
    newArr.[b] <- tmp
    newArr

let bigSum (arr : int []) =
    let rec loop (i : int) (sum : int64) =
        if i < 0 then sum
        else loop (i - 1) (sum + int64 arr.[i])
    loop (arr.Length - 1) (int64 0)

let factorial (num : int) =
    let rec loop acc num =
        match num with
        | 1 -> acc
        | _ -> num - 1 |> loop (acc * num)
    loop 1 num

let binarySearch (arr : 'a []) (key : 'a) =
    let rec loop (arr : 'a []) (key : 'a) (minIndex : int) (maxIndex : int) =
        if maxIndex < minIndex then -1
        else
            let midIndex = minIndex + (maxIndex - minIndex) / 2
            if arr.[midIndex] > key then loop arr key minIndex (midIndex - 1)
            elif arr.[midIndex] < key then loop arr key (midIndex + 1) maxIndex
            else midIndex
    loop arr key 0 arr.Length

type Heap() =
    let mutable data : List<int> = new List<int>()
    member this.Data = data

    member this.Push(el : int) =
        data.Add(el)
        let rec loop (data : List<int>) (nowIndex : int) =
            if nowIndex = 0 then data
            else
                let isLeft = nowIndex % 2 = 1

                let maxChild =
                    match isLeft with
                    | true -> data.[nowIndex]
                    | false -> max data.[nowIndex] data.[nowIndex - 1]

                let parentIndex =
                    match isLeft with
                    | true -> nowIndex / 2
                    | false -> (nowIndex - 1) / 2

                if maxChild > data.[parentIndex] then
                    let swap = data.[nowIndex]
                    data.[nowIndex] <- data.[parentIndex]
                    data.[parentIndex] <- swap
                    loop data parentIndex
                else data
        data <- loop data (data.Count - 1)

    member this.Pop() =
        let popped = data.[0]
        let lastIndex = data.Count - 1
        let swap = data.[0]
        data.[0] <- data.[lastIndex]
        data.[lastIndex] <- swap
        data.RemoveAt(lastIndex)
        let newLastIndex = lastIndex - 1

        let rec loop (data : List<int>) (nowIndex : int) =
            if newLastIndex < 0 then data
            else
                let leftChildIndex =
                    let index =
                        if nowIndex = 0 then 1
                        else nowIndex * 2 + 1
                    if index <= newLastIndex then index
                    else -1

                let rightChildIndex =
                    let index =
                        if nowIndex = 0 then 2
                        else nowIndex * 2 + 2
                    if index <= newLastIndex then index
                    else -1

                let maxChildIndex =
                    if (if leftChildIndex = -1 then 0
                        else data.[leftChildIndex]) > (if rightChildIndex = -1 then
                                                           0
                                                       else
                                                           data.[rightChildIndex])
                    then leftChildIndex
                    else rightChildIndex
                    |> fun index ->
                        match index with
                        | -1 -> nowIndex
                        | _ -> index

                if data.[nowIndex] < data.[maxChildIndex] then
                    let swap = data.[nowIndex]
                    data.[nowIndex] <- data.[maxChildIndex]
                    data.[maxChildIndex] <- swap
                    loop data maxChildIndex
                else data
        data <- loop data 0
        popped

[<EntryPoint>]
let main _argv =
    let [| _; m |] = stdin.ReadLine().Split(' ') |> Array.map int
    let aList = stdin.ReadLine().Split(' ') |> Array.map int
    let heap = new Heap()
    for a in aList do
        heap.Push(a)
    for _ in 1..m do
        let a = heap.Pop()
        heap.Push(a / 2)
    bigSum (heap.Data.ToArray()) |> printfn "%d"
    0 // return an int exit code
