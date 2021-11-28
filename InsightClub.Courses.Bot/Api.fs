module InsightClub.Courses.Bot.Api

open System
open Funogram
open Funogram.Telegram
open Funogram.Telegram.Bot
open Funogram.Telegram.Types


let private inlineMarkup =
  Option.map
    ( fun markup ->
        { InlineKeyboard = List.map Seq.ofList markup } )

let private markup =
  inlineMarkup
  >> Option.map InlineKeyboardMarkup

let private removeKeyboard config userId messageId = async {
  do!
    Api.editMessageReplyMarkupBase
      (Some <| Int userId) (Some messageId) None None
    |> Api.api config
    |> Async.StartChild
    |> Async.Ignore }

let private sendMessage config userId text keyboard = async {
  return!
    Api.sendMessageBase
      (Int userId) text None None None None (markup keyboard)
    |> Api.api config
    |> Async.map
      ( function
        | Ok m    -> Option.map (always m.MessageId) m.ReplyMarkup
        | Error _ -> None ) }

let onUpdate getConnection ctx = async {
  use connection = getConnection ()
  let config = ctx.Config

  match ctx.Update with
  // Message updates
  | { Message = Some ({ From = Some user } as message) } ->
    let! creatorId, lastId, state = State.get connection user.Id

    let services = Services.get ()
    let commands = Commands.onMessage message
    let! state, _ = Core.update services commands state

    let! text, keyboard = Render.state user state

    match lastId with
    | Some messageId ->
      do! removeKeyboard config user.Id messageId

    | None ->
      ()

    let! lastId =
      if text <> String.Empty then
        sendMessage config user.Id text keyboard
      else
        Async.singleton None

    do! State.update connection creatorId lastId state

  | _ ->
    () }
