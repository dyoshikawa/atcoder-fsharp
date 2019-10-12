// Learn more about F# at http://fsharp.org
open System
open System.Collections.Generic

let swap (arr : 'a []) a b =
    let newArr = Array.copy arr
    let tmp = newArr.[a]
    newArr.[a] <- newArr.[b]
    newArr.[b] <- tmp
    newArr

//let sqrt64 (n : int64) =
//    let rec loop (n : float) =
//        let newN = n - (n + n - 2) / (x * 2)
        

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

let makeGcd (a : int) (b : int) =
    let rec loop (a : int) (b : int) =
        if b = 0 then
            a
        else
            loop b (a % b)
    if a < b then
        loop b a
    else
        loop a b

let makeGcd64 (a : int64) (b : int64) =
    let rec loop (a : int64) (b : int64) =
        if b = int64 0 then
            a
        else
            loop b (a % b)
    if a < b then
        loop b a
    else
        loop a b

let makeDivs (n : int) =
    let divs = new List<int>()
    let rec loop (i : int) =
        if i*i > n then
            ()
        else
            if n % i = 0 then
                divs.Add(i)
                if i <> 1 && i*i <> n then
                    divs.Add(n / i)
            loop (i + 1)
    loop 1
    divs.Sort()
    divs.ToArray()

let makeDivs64 (n : int64) =
    let divs = new List<int64>()
    let rec loop (i : int64) =
        if i*i > int64 n then
            ()
        else
            if n % i = int64 0 then
                divs.Add(i)
                if i <> int64 1 && i*i <> n then
                    divs.Add(n / i)
            loop (i + int64 1)
    loop (int64 1)
    divs.Sort()
    divs.ToArray()

let makePrimes (n : int) =
    let primes = new List<int>()
    let rec loop (i : int) (nums : int[]) =
        if i * i > n then
            for i in nums do
                primes.Add(i)
            ()
        else
            primes.Add(i)
            let newNums = Array.filter (fun num -> num % i <> 0) nums
            loop newNums.[0] newNums
    loop 2 [| 2..n |]
    primes.ToArray()

let makePrimes64 (n : int64) =
    let primes = new List<int64>()
    let rec loop (i : int64) (nums : int64[]) =
        if i * i > n then
            for i in nums do
                primes.Add(i)
            ()
        else
            primes.Add(i)
            let newNums = Array.filter (fun num -> num % i <> int64 0) nums
            loop newNums.[0] newNums
    loop (int64 2) [| int64 2..n |]
    primes.ToArray()

let primeFact (n : int) =
    let primes = new List<int>()
    let rec loop (i : int) (n : int) =
        if i > n then
            ()
        else
            if n % i = 0 then
                primes.Add(i)
                let newN = n / i
                loop 2 newN
            else
                loop (i + 1) n
    loop 2 n
    primes.ToArray()

let isPrime64 (n : int64) =
    if n = int64 1 then false
    else
        let rec loop (i : int64) =
            if i > n then true
            else
                if n % i = int64 0 then false
                else
                    loop (i + int64 1)
        loop (int64 (Math.Sqrt (float n)))

let trialDiv64 (n : int64) =
    let factor = new List<int64>()
    let max = sqrt (float n) |> int64
    let rec loop (i : int64) (n : int64) =
        if i > max then
            n
        else
            if n % i = int64 0 then
                factor.Add(i)
                loop i (n / i)
            else
                loop (i + int64 1) n
    let lastN = loop (int64 2) n
    if lastN <> int64 1 then
        factor.Add(lastN)
    factor.ToArray()
    
[<EntryPoint>]
let main _argv =
    let [| n; q |] = stdin.ReadLine().Split(' ') |> Array.map int
    

    0 // return an int exit code
