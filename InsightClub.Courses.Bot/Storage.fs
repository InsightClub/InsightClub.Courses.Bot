module InsightClub.Courses.Bot.Storage

open System.IO


let getFile storagePath fileId =
  File.OpenRead(Path.Combine([| storagePath; fileId |]))
  :> Stream
