open System
open System.IO
open System.Text
open System.Threading
open System.Threading.Tasks
open FeistelNetwork
open VigenerCipher


let createFile(path:string) : unit  =
    try
        let fullPath = Path.GetFullPath(path)
        let fileExtension = Path.GetExtension(path)
        let directory = Path.GetDirectoryName(fullPath)
        
        if not (Directory.Exists(directory)) || String.IsNullOrEmpty(fileExtension) then
            raise (DirectoryNotFoundException $"Directory does not exist: %s{directory}")
        else    
            File.Create(path) |> ignore
    with
        | _ -> printf "Create file is failed\n"
    
    
let writeFile(path:string, data: string) : Task<unit>  =
    async {
        try
            let fullPath = Path.GetFullPath(path)
            let fileExtension = Path.GetExtension(path)
            let directory = Path.GetDirectoryName(fullPath)
          
            if not (Directory.Exists(directory)) || String.IsNullOrEmpty(fileExtension) then
                raise (DirectoryNotFoundException $"Directory does not exist: %s{directory}")
            else    
                use writer = new StreamWriter(path, false)
                do! writer.WriteLineAsync(data)
                    |> Async.AwaitTask
        with
            | _ -> printf "Failed to write to file\n"
        } |> Async.StartAsTask

let readFile(path: string) : string =
    try
        let fullPath = Path.GetFullPath(path)
        let fileExtension = Path.GetExtension(path)
        let directory = Path.GetDirectoryName(fullPath)
        
        if not (Directory.Exists(directory)) || String.IsNullOrEmpty(fileExtension) then
            raise (DirectoryNotFoundException $"Directory does not exist: %s{directory}")
        else    
            File.ReadAllText(path)
    with
        | _ -> printf "Failed to read file\n"; null



printfn "Введите данные"

let data: string = Console.ReadLine()

let mutable partitions = partitionData data

let mutable result = feistelNetwork (partitions, 16)

writeFile("D:\\text.txt", result) |> ignore

printfn $"\ns{result}"

Console.ReadKey() |> ignore



//printfn "Введи текст для шифрования"
//let mutable enterData: string = Console.ReadLine()
//printfn "Введи ключевое слово"
//let mutable key: string = Console.ReadLine()

//enterData <- reverseString enterData

//let keyCodes = seq {for n = 0 to key.Length - 1 do charToAsciiCode(key[n])}

//let salt = generateSalt 6

//let mutable keyEncrypted = keyEncrypt key 1
//keyEncrypted <- salt + keyEncrypted

//let keyEncryptedKeys = seq {for n = 0 to keyEncrypted.Length - 1 do charToAsciiCode(keyEncrypted[n])}

//let result = multiStepEncrypt 1 enterData keyEncryptedKeys

//printfn $"%s{result}"


//let mutable decryptedResult = multiStepDecrypt 1 result keyEncryptedKeys

//decryptedResult <- reverseString decryptedResult

//printfn $"Дешифрованный текст: %s{decryptedResult}"

