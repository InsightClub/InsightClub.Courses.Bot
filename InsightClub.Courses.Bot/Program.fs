module InsightClub.Courses.Bot.Program

open InsightClub.Courses.Bot.Config


[<EntryPoint>]
let main _ =
  let config = Config.load "Config.json"

  0 // return an integer exit code
