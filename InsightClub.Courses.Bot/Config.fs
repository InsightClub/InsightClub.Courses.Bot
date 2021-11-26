namespace InsightClub.Courses.Bot.Config

open System
open System.IO
open Microsoft.FSharpLu.Json

module Json = Compact.Strict


// Types
type Server =
  { Address: Uri
    Listens: Uri }

type Database =
  { Host: Uri
    Database: String
    Username: String
    Password: String
    Port: Int32 }

type Config =
  { Token: String
    Server: Server
    Database: Database }

// Functions
module Config =
  let load :string -> Config =
    File.ReadAllText >> Json.deserialize
