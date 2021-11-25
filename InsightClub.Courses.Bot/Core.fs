module InsightClub.Courses.Bot.Core


// Types
module Inactive =
  type Command = Start

module Idle =
  type Command = Help

  type Msg =
    | Started
    | Helping
    | Error

type BotState =
  | Inactive
  | Idle of Idle.Msg

type GetCommand<'Command> = unit -> 'Command option

type BotCommands =
  { getInactive: GetCommand<Inactive.Command>
    getIdle: GetCommand<Idle.Command> }

type BotServices<'Effect, 'Result> =
  { callback: BotState -> 'Effect option -> 'Result }

// Values
/// Initial state
let initial = Inactive

let private updateInactive callback = function
| Some Inactive.Start ->
  callback (Idle Idle.Started) None

| None ->
  callback Inactive None

// Stub
let private updateIdle callback = function
| Some Idle.Help ->
  callback (Idle Idle.Helping) None

| None ->
  callback (Idle Idle.Error) None

let update services commands =
  let s = services
  function
  | Inactive ->
    commands.getInactive ()
    |> updateInactive s.callback

  | Idle _ ->
    commands.getIdle()
    |> updateIdle s.callback
