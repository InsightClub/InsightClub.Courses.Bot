module InsightClub.Courses.Bot.Services

open Core


let get () =
  let callback state effect =
    Async.singleton (state, effect)

  { callback = callback }
