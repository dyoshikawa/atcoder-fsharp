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
        else sum + int64 arr.[i] |> loop (i - 1)
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
        if lastIndex > 0 then
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
    let [| n; m |] = stdin.ReadLine().Split(' ') |> Array.map int
    let aList = stdin.ReadLine().Split(' ') |> Array.map int
    let mutable heap = Set.ofArray (Array.zip aList [|0..n-1|])
    for _ in 1..m do
        let (x, y) as a = Set.maxElement heap
        heap <- Set.remove a heap
        heap <- Set.add (x / 2, y) heap
    Set.fold (fun s (x, _) -> s + int64 x) 0L heap |> printfn "%d"
    0 // return an int exit code
