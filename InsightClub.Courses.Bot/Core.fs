module InsightClub.Courses.Bot.Core


// Types
module Inactive =
  type Command = Start

type BotState =
  | Inactive
  | Idle

type GetCommand<'Command> = unit -> 'Command option

type BotCommands =
  { getInactive: GetCommand<Inactive.Command> }

type BotServices<'Effect, 'Result> =
  { callback: BotState -> 'Effect option -> 'Result }

// Values
/// Initial state
let initial = Inactive

let private updateInactive callback = function
| Some Inactive.Start ->
  callback Idle None

| None ->
  callback Inactive None

// Stub
let private updateIdle callback =
  callback Idle None

let update services commands =
  let s = services
  function
  | Inactive ->
    commands.getInactive ()
    |> updateInactive s.callback

  | Idle ->
    updateIdle s.callback
