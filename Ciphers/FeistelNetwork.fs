module FeistelNetwork
open System.Text
open System.Collections
open System


let stringToBitArray (input: string) : bool array =
    let bytes = Encoding.Unicode.GetBytes(input)
    let bits = new BitArray(bytes)
    let bitsArray = Array.create bits.Length false
    bits.CopyTo(bitsArray, 0)
    bitsArray

let generateKey (length: int) : string =
    let random = Random()
    let letters = "abcdefghijklmnopqrstuvwxyz1234567890"
    let word = 
        [ for _ in 1 .. length do
            yield letters[random.Next(letters.Length)] ]
    String.Concat(word)

let padBitArray (bitArray: bool array) =
    let currentLength = bitArray.Length
    let newLength = ((currentLength + 7) / 8) * 8 
    if newLength > currentLength then
        let paddedArray = Array.create newLength false
        bitArray.CopyTo(paddedArray, 0)
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
    Encoding.Unicode.GetString(byteArray)

let funcActivation (thread: bool array, key : string) : bool array =
    let keyBits = stringToBitArray key
    let result = Array.create thread.Length false

    for i = 0 to thread.Length - 1 do
        result[i] <- thread[i] <> keyBits[i % keyBits.Length]
    result

let generateKeys(roundNumber: int) =
    Array.init (roundNumber) (fun _ -> generateKey(12))

let passAllThreadThroughActivation(block: bool array array, keys: string array, round: int) = 
    let mutable result = block[0]
    for i = 0 to block.Length - 2 do
        if i = 0 then 
            result <- funcActivation (result, keys.[round])
        else
            let next = funcActivation (block[i], keys.[round])
            result <- funcActivation(result, byteArrayToString(bitArrayToByteArray(next)))
    result

let passAllThreadThroughActivationDecrypt(block: bool array array, keys: string array, round: int) = 
    let mutable result = block[block.Length - 1]
    for i = block.Length - 1 downto 1 do
        if i = block.Length - 1 then 
            result <- funcActivation(result, keys.[round])
        else
            let next = funcActivation(block[i - 1], keys.[round])
            result <- funcActivation(result, byteArrayToString(bitArrayToByteArray(next)))
    result

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
            let threads = 
                Array.init numThread (fun x -> 
                    let streamStart = x * 32
                    let streamLength = min 32 (length - streamStart)
                    let thread = Array.create 32 false
                    for y in 0 .. (streamLength - 1) do 
                        thread.[y] <- block.[streamStart + y]
                    thread
                )
            if i = numBlock - 1 && threads.Length < 2 then
                let newThread = Array.create 32 false
                Array.append threads [| newThread |]
            else
                threads
        )
    partitions


let swapThreads(threads: bool array array) : bool array array =
    let mutable x_temp = threads[0]
    threads[0] <- threads[threads.Length - 1]
    for i = threads.Length - 1 downto 1 do
        threads[i] <- threads[i - 1]
    threads[1] <- x_temp
    threads

let swapThreadsBack(threads: bool array array) : bool array array =
    let mutable x_temp = threads[1]
    threads[1] <- threads[0]
    for i = 1 to threads.Length - 2 do
        threads[i] <- threads[i + 1]
    threads[threads.Length - 1] <- x_temp
    threads

let swapThreadsReverse(threads: bool array array) : bool array array =
    let first = threads[0]
    for i = 0 to threads.Length - 2 do
        threads[i] <- threads[i + 1]
    threads[threads.Length - 1] <- first
    threads

let partitionsToString(partitions: bool array array array) =
    let bitArray = 
        partitions 
        |> Array.concat 
        |> Array.concat 
    let byteArray = bitArrayToByteArray (bitArray)
    byteArrayToString byteArray

let feistelNetwork(partitions: bool array array array, keys: string array, roundNumber: int) =
    for i = 0 to roundNumber - 1 do 
        for j = 0 to partitions.Length - 1 do
            let resultFuncActivation = passAllThreadThroughActivation(partitions.[j], keys, i)
            let activatedValue = funcActivation(partitions.[j].[partitions.[j].Length - 1], byteArrayToString(bitArrayToByteArray(resultFuncActivation)))
            partitions.[j].[partitions.[j].Length - 1] <- activatedValue
            partitions[j] <- swapThreads partitions[j]
    (partitions, keys) 

let decryptFeistelNetwork(encryptedPartitions: bool array array array, keys: string array, roundNumber: int) =
    let partitions = encryptedPartitions
    for i = roundNumber - 1 downto 0 do
        for j = 0 to partitions.Length - 1 do
            let resultFuncActivation = passAllThreadThroughActivation(partitions.[j], keys, i)
            let activatedValue = funcActivation(partitions.[j].[partitions.[j].Length - 1], byteArrayToString(bitArrayToByteArray(resultFuncActivation)))
            partitions.[j].[0] <- activatedValue
            

            partitions[j] <- swapThreadsReverse(partitions[j]) 
    partitions


let assembleStringFromBoolArray (data: bool array array array) : string =
    let flattenedPartitions = 
        data 
        |> Array.concat 
        |> Array.concat 
    let byteArray = bitArrayToByteArray flattenedPartitions
    byteArrayToString byteArray

