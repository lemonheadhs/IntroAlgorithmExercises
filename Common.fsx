open System.IO

let writeToBinaryFile filePath data =
    use stream = File.Open(filePath, FileMode.Create)
    let bformatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
    bformatter.Serialize(stream, data)

let readFromBinaryFile<'T> filePath =
    use stream = File.Open(filePath, FileMode.Open)
    let bformatter = System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
    bformatter.Deserialize(stream) :?> 'T

let Exercise (name: string) expr = ()

