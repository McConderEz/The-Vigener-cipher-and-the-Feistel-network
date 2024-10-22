module Program

open System
open System.IO
open System.Text
open System.Threading
open System.Threading.Tasks
open VigenerCipher
open FeistelNetwork


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
            File.ReadAllText(path).TrimEnd('\r', '\n')
    with
        | _ -> printf "Failed to read file\n"; null


let processFile(path: string, action: string, key: string, cipher: string, keys: string array) =
    match action with
    | "read" ->
        let data = readFile path
        printfn "Содержимое файла:\n%s" data
    | "encrypt" ->
        let data = readFile path
        let encryptedData =
            match cipher with
            | "vigenere" -> multiStepEncrypt 1 data (seq { for c in key -> charToAsciiCode c })
            | "feistel" ->
                let partitions = partitionData data
                let (encryptedPartitions, keys) = feistelNetwork(partitions, keys, 16)
                partitionsToString encryptedPartitions
            | _ -> failwith "Unknown cipher"
        
        let encryptedFilePath = Path.GetFileNameWithoutExtension(path) + "_encrypt" + Path.GetExtension(path)
        writeFile(encryptedFilePath, encryptedData) |> ignore
        printfn "Encrypted data written to %s" encryptedFilePath
    | "decrypt" ->
        let data = readFile path
        let decryptedData =
            match cipher with
            | "vigenere" -> multiStepDecrypt 1 data (seq { for c in key -> charToAsciiCode c })
            | "feistel" ->
                let partitions = partitionData data
                let decryptedPartitions = decryptFeistelNetwork(partitions, keys, 16)
                partitionsToString decryptedPartitions
            | _ -> failwith "Unknown cipher"
        
        let decryptedFilePath = Path.GetFileNameWithoutExtension(path) + "_decrypt" + Path.GetExtension(path)
        writeFile(decryptedFilePath, decryptedData.Trim()) |> ignore
        printfn "Decrypted data written to %s" decryptedFilePath
    | _ -> printfn "Unknown action. Please specify 'encrypt', 'decrypt', or 'read'."



let keys = generateKeys 16
while true do
    printfn "Введите путь к файлу:"
    let filePath = Console.ReadLine()
    
    printfn "Введите действие (encrypt/decrypt/read):"
    let action = Console.ReadLine().ToLower()
    
    printfn "Введите ключевое слово:"
    let key = Console.ReadLine()
    
    printfn "Выберите шифр (vigenere/feistel):"
    let cipher = Console.ReadLine().ToLower()
    
    processFile(filePath, action, key, cipher, keys)
    
    Console.ReadKey() |> ignore


