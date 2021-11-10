namespace InsightClub.Courses.Bot.Config

open System
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
