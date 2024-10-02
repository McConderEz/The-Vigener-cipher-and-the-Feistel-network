module VigenerCipher

open System.Text
open System

let charToAsciiCode (symbol: char) : int =
    int symbol

let AsciiCodeToChar(code: int) : char =
    char code

let keyCodeOffset (keyCode: int) (offset: int) : int =
       if keyCode + 1 > 122 then
           97
       else
           keyCode + 1
    
let convertToAlphabet(dataCode: int, keyCode: int) : char =
    let mutable newCode =
         if (dataCode + keyCode > 122) then
             (dataCode + keyCode) - 122
         else
            dataCode + (keyCode-96)
    if newCode < 97 then
        newCode <- newCode + (122 - 96)
    char newCode

let dataToVijenerCipher(data: string) (keyCodes: seq<int>) : string =
    let mutable result: StringBuilder = new StringBuilder()
    let mutable indexKey: int = 0
    let keyCodeLength = Seq.length keyCodes
    for i = 0 to data.Length - 1 do
        if indexKey = keyCodeLength then
            indexKey <- 0         
        result.Append(convertToAlphabet(charToAsciiCode data[i], Seq.item indexKey keyCodes)) |> ignore
        indexKey <- indexKey + 1
    result.ToString()

let keyEncrypt (key: string) (offset: int) : string =
    let keyCodes = [for n in 0 .. key.Length - 1 -> char (int key[n])]
    let keyCodeLength = List.length keyCodes
    let mutable encryptedKey = []   
    for i in 0 .. keyCodeLength - 1 do
        let newCode = keyCodeOffset (int keyCodes[i]) offset
        encryptedKey <- encryptedKey @ [char newCode]
    String.Concat(encryptedKey)

let generateSalt (length: int) : string =
    let random = Random()
    let letters = "abcdefghijklmnopqrstuvwxyz"
    let word = 
        [ for _ in 1 .. length do
            yield letters[random.Next(letters.Length)] ]
    String.Concat(word)

let rec multiStepEncrypt(step: int) (data: string) (keyCodes: seq<int>) : string =
    if step = 0 then
        data
    else
        let dataEncrypted = dataToVijenerCipher data keyCodes
        multiStepEncrypt (step - 1) dataEncrypted keyCodes

let reverseString (s: string) =
    s.ToCharArray() |> Array.rev |> String


let convertToAlphabetDecrypt(dataCode: int, keyCode: int) : char =
    let mutable newCode = dataCode - (keyCode - 96)
    if newCode < 97 then
        newCode <- newCode + (122 - 96)
    char newCode

let dataToVigenereDecrypt(data: string) (keyCodes: seq<int>) : string =
    let mutable result: StringBuilder = new StringBuilder()
    let mutable indexKey: int = 0
    let keyCodeLength = Seq.length keyCodes
    for i = 0 to data.Length - 1 do
        if indexKey = keyCodeLength then
            indexKey <- 0         
        result.Append(convertToAlphabetDecrypt(charToAsciiCode data[i], Seq.item indexKey keyCodes)) |> ignore
        indexKey <- indexKey + 1
    result.ToString()

let rec multiStepDecrypt(step: int) (data: string) (keyCodes: seq<int>) : string =
    if step = 0 then
        data
    else
        let dataDecrypted = dataToVigenereDecrypt data keyCodes
        multiStepDecrypt (step - 1) dataDecrypted keyCodes