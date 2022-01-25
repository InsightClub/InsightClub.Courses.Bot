module InsightClub.Courses.Bot.Customer

open Microsoft.FSharpLu.Json


module Json = Compact.Strict

type User = Funogram.Telegram.Types.User

type State =
  /// Id of the last message sent with the inline keyboard
  { LastId: int64 option
    State: Core.BotState }

let create lastId state =
  { LastId = lastId
    State = state }

let initialJson =
  create None Core.initial
  |> Json.serialize

let getOrCreate (user: User) connection =
  Repo.getOrCreateCustomer
    user.Id user.FirstName user.LastName user.Username initialJson connection
  |> Async.map
    ( fun (customerId, stateJson) ->
        let { LastId = lastId; State = state } =
          Json.deserialize<State> stateJson

        customerId, lastId, state )

let update customerId (user: User) lastId state connection =
  let state =
    create lastId state
    |> Json.serialize

  Repo.updateCustomer
    customerId user.FirstName user.LastName user.Username state connection
