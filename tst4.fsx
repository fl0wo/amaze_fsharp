#r "System.Runtime.Serialization"

open System.IO
open System.Text
open System.Runtime.Serialization
open System.Runtime.Serialization.Json

// [<field:DataMember(Name = "name")>] per bindare con un nome diverso

[<DataContract>]
type User =
    { name: string

      x: int

      y: int

      color: int }


let decode (s: string) =
    let json = DataContractJsonSerializer(typeof<User>)
    let byteArray = Encoding.UTF8.GetBytes(s)
    let stream = new MemoryStream(byteArray)
    json.ReadObject(stream) :?> User

let tw = "{
    \"name\":\"Flo\",
    \"x\":1,
    \"y\":1,
    \"color\":0
    }"


let v = decode tw // val v : geo = {t = "Point"; coordinates = [|-7.002648; 110.449961|];}

printfn "%A" v
