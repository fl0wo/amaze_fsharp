#r "System.Runtime.Serialization"

open System.IO
open System.Text
open System.Runtime.Serialization
open System.Runtime.Serialization.Json

// [<field:DataMember(Name = "name")>] per bindare con un nome diverso
// {" nome " : "lobby_nameflo "  ,
// " creator " : { "nome" : "flo", "x" : 12, "y" : 12, "colore" : 0},
// "r":21, " c " : 21, " m " : "010 " ,
// " partecipants "  : [{ "nome" : "flo", "x" : 12, "y" : 12, "colore" : 0}]}


[<DataContract>]
type User =
    { [<field:DataMember(Name = "nome")>]
      nome: string
      [<field:DataMember(Name = "x")>]
      x: int
      [<field:DataMember(Name = "y")>]
      y: int
      [<field:DataMember(Name = "colore")>]
      colore: int }

[<DataContract>]
type Maze =
    { [<field:DataMember(Name = "nome")>]
      nome: string
      [<field:DataMember(Name = "creator")>]
      creator: User
      [<field:DataMember(Name = "r")>]
      r: int
      [<field:DataMember(Name = "c")>]
      c: int
      [<field:DataMember(Name = "m")>]
      m: string
      [<field:DataMember(Name = "partecipants")>]
      partecipants: array<User> }


let decode (s: string) =
    let json = DataContractJsonSerializer(typeof<Maze>)
    let byteArray = Encoding.UTF8.GetBytes(s)
    let stream = new MemoryStream(byteArray)
    json.ReadObject(stream) :?> Maze

// let xx =
//     "{\"nome\":\"lobby_nameflo\",\"creator\":{\"nome\":\"flo\",\"x\":4,\"y\":7,\"colore\":0},\"r\":21,\"c\":21,\"m\":\"1111111111111111111111111111111111111111111110101010000010100010111010101010111010101011100000101000101010001110111011111010111010111010000010100010001011111111101010111110111110000000100000101010111110114011101110101011100000100010000010001110101111101010101011111010101000001010000011101110101010111011101110101000101000101000111010101110101011101011101000100010100010101111111110101011101010111000000010101000101011111111111111111111111111111111111111111111111111111111111111111111\",\"partecipants\":[{\"nome\":\"flo\",\"x\":4,\"y\":7,\"colore\":0}]}"


// printfn "%A" (decode xx)
