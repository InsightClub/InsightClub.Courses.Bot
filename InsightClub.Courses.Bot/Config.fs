namespace InsightClub.Courses.Bot.Config

open System
open System.IO
open InsightClub.Courses.Bot
open Chiron


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

// Deserializers
type Server with
  static member FromJson (_: Server) = json {
    let! address = Json.read "Address"
    let! listens = Json.read "Listens"

    let addressError =
      "«Address» field must contain correct http or https url"
    let address = Uri.createHttp id (Defer.func failwith addressError) address

    let listensError =
      "«Listens» field must contain correct http or https url"
    let listens = Uri.createHttp id (Defer.func failwith listensError) listens

    return
      { Address = address
        Listens = listens } }

type Database with
  static member FromJson(_: Database) = json {
    let! host = Json.read "Host"
    let! database = Json.read "Database"
    let! username = Json.read "Username"
    let! password = Json.read "Password"
    let! port = Json.read "Port"

    let hostError =
      "«Host» field must contain correct http or https url"
    let host = Uri.createHttp id (Defer.func failwith hostError) host

    let defaultPort = 5432 // Default postgres port

    let port = Option.defaultValue defaultPort port

    let maxPortNumber = 65535

    if port < 0 then
      failwith "«Port» can not be negative"

    if port > maxPortNumber then
      failwith $"«Port» can not be larger than {maxPortNumber}"

    return
      { Host = host
        Database = database
        Username = username
        Password = password
        Port = port } }

type Config with
  static member FromJson(_: Config) = json {
    let! token = Json.read "Token"
    let! server = Json.read "Server"
    let! database = Json.read "Database"

    if token = String.Empty then
      failwith "«Token» can not be empty"

    return
      { Token = token
        Server = server
        Database = database } }

// Functions
module Config =
  let load :string -> Config =
    File.ReadAllText >> Json.parse >> Json.deserialize
