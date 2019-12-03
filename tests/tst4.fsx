open System.IO
open System.Text
open System.Runtime.Serialization
open System.Runtime.Serialization.Json

#load "./../models/parser.fsx"

open Parser

let tw = "{
    \"name\":\"Flo\",
    \"x\":1,
    \"y\":1,
    \"color\":0
    }"

let v = Parser.decode tw // val v : geo = {t = "Point"; coordinates = [|-7.002648; 110.449961|];}

printfn "%A" v
// #r "System.Runtime.Serialization"
// open System.IO
// open System.Runtime.Serialization.Json
// open System.Runtime.Serialization
// [<DataContract>]
// [<CLIMutable>]
// type Person =
//     { [<DataMember(Name = "Name")>]
//       entityName: string
//       [<DataMember(Name = "Type")>]
//       entityType: string }
// let person =
//     { entityName = "ENTITY"
//       entityType = "TYPE" }
// let toJson<'t> (myObj: 't) =
//     let fs = new FileStream(@"C:\tmp\test.json", FileMode.Create)
//     (new DataContractJsonSerializer(typeof<'t>)).WriteObject(fs, myObj)
// toJson<Person> person
