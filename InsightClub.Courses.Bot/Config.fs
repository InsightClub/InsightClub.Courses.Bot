namespace InsightClub.Courses.Bot.Config

open System
open System.IO
open Microsoft.FSharpLu.Json

module Json = Compact.Strict


// Types
type Server =
  { Address: String
    Listens: String }

type Database =
  { Host: String
    Database: String
    Username: String
    Password: String
    Port: Int32 }

type Config =
  { Token: String
    Server: Server
    Database: Database }

// Values
module Config =
  let load =
    File.ReadAllText >> Json.deserialize<Config>
