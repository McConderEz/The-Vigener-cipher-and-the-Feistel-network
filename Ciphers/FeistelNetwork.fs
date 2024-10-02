module FeistelNetwork
open System.Text
open System.Collections
open System


let stringToBitArray (input: string) : BitArray =
    let bytes = Encoding.UTF8.GetBytes(input)
    let bitArray = new BitArray(bytes)
    bitArray


let generateKey (length: int) : string =
    let random = Random()
    let letters = "abcdefghijklmnopqrstuvwxyz1234567890"
    let word = 
        [ for _ in 1 .. length do
            yield letters[random.Next(letters.Length)] ]
    String.Concat(word)

let padBitArray (bitArray: BitArray) =
    let currentLength = bitArray.Length
    let newLength = ((currentLength + 7) / 8) * 8 
    if newLength > currentLength then
        let paddedArray = new BitArray(newLength)
        paddedArray.SetAll(false)
        let tempArray = Array.create (newLength / 8) 0uy 
        bitArray.CopyTo(tempArray, 0)
        paddedArray.CopyTo(tempArray, 0)
        paddedArray
    else
        bitArray

let bitArrayToByteArray (bitArray: bool array) =
    let byteCount = (bitArray.Length + 7) / 8
    let byteArray = Array.create byteCount 0uy
    for i = 0 to bitArray.Length - 1 do
        if bitArray[i] then
            byteArray[i / 8] <- byteArray[i / 8] ||| (1uy <<< (i % 8))
    byteArray

let byteArrayToString (byteArray: byte array) = 
    let result = Encoding.UTF8.GetString(byteArray)
    result

let funcActivation (thread: bool array, key : string) : bool array =
   let keyBits = stringToBitArray key
   let result = Array.create thread.Length false

   for i = 0 to thread.Length - 1 do
        result[i] <- thread[i] <> keyBits[i % keyBits.Length]
   result

let passAllThreadThroughActivation(block: bool array array) = 
    let mutable result = block[0]
    for i = 0 to block.Length - 2 do
        for j = 0 to block[i].Length - 1 do
            if i = 0 then 
                 result <- funcActivation (result, generateKey(12))
            else
                 let next = funcActivation (block[i], generateKey(12))
                 result <- funcActivation(result,byteArrayToString(bitArrayToByteArray(next)))
    result

let partitionDataIntoBlocks (data: string) = 
    let dataToBitArray = stringToBitArray data
    let numBlock = (dataToBitArray.Length + 127) / 128
    let partitions =
        Array.init numBlock (fun i ->
            let start = i * 128
            let length = min 128 (dataToBitArray.Length - start)
            let block = Array.create length false
            for j in 0 .. (length - 1) do
                block.[j] <- dataToBitArray.[start + j]
            block)
    partitions

let partitionData (data: string) = 
    let dataToBitArray = stringToBitArray data
    let numBlock = (dataToBitArray.Length + 127) / 128
    let partitions =
        Array.init numBlock (fun i ->
            let start = i * 128
            let length = min 128 (dataToBitArray.Length - start)
            let block = Array.create 128 false
            for j in 0 .. (length - 1) do
                block.[j] <- dataToBitArray.[start + j]
            let numThread = (length + 31) / 32
            Array.init numThread (fun x -> 
                let streamStart = x * 32
                let streamLength = min 32 (length - streamStart)
                let thread = Array.create 32 false
                for y in 0 .. (streamLength - 1) do 
                    thread.[y] <- block.[streamStart + y]
                thread
            ))
    partitions

let swapThreads(threads: bool array array) : bool array array =
    let mutable x_temp = threads[0]
    threads[0] <- threads[threads.Length - 1]
    for i = threads.Length - 1 downto 1 do
         threads[i] <- threads[i - 1]
    threads[1] <- x_temp
    threads

let partitionsToString(partitions: bool array array array) =
    let bitArray = 
        partitions 
        |> Array.concat 
        |> Array.concat 
    let byteArray = bitArrayToByteArray bitArray
    byteArrayToString byteArray

let feistelNetwork(partitions: bool array array array, roundNumber: int) =
    for i = 0 to roundNumber do 
        for j = 0 to partitions.Length - 1 do
            let resultFuncActivation = passAllThreadThroughActivation partitions[j]
            partitions[j][partitions[j].Length - 1] <- funcActivation(partitions[j][partitions[j].Length - 1] ,byteArrayToString(bitArrayToByteArray(resultFuncActivation)))
            partitions[j] <- swapThreads partitions[j]
    partitionsToString partitions


