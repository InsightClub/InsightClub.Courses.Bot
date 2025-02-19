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

let answerCallbackQuery config queryId text =
  Api.answerCallbackQueryBase
    (Some queryId) text None None None
  |> Api.api config
  |> Async.StartChild
  |> Async.Ignore

let editMessage config messageId userId text keyboard = async {
  let id = Some (Int userId)
  let messageId = Some messageId
  let keyboard = inlineMarkup keyboard
  do!
    Api.editMessageTextBase
      id messageId None text None None keyboard
    |> Api.api config
    |> Async.StartChild
    |> Async.Ignore }

let sendContent config userId storagePath content = async {
  let makeFile fileId =
    FileToSend.File <| (fileId, Storage.getFile storagePath fileId)

  match content with
  | Core.Text text ->
    do!
      Api.sendMessage userId text
      |> Api.api config
      |> Async.Ignore

  | Core.Photo fileId ->
    do!
      Api.sendChatAction userId ChatAction.UploadPhoto
      |> Api.api config
      |> Async.Ignore

    do!
      Api.sendPhoto userId (makeFile fileId) ""
      |> Api.api config
      |> Async.Ignore

  | Core.Audio fileId ->
    do!
      Api.sendChatAction userId ChatAction.UploadAudio
      |> Api.api config
      |> Async.Ignore

    do!
      Api.sendAudio userId (makeFile fileId) "" None None
      |> Api.api config
      |> Async.Ignore

  | Core.Video fileId ->
    do!
      Api.sendChatAction userId ChatAction.UploadVideo
      |> Api.api config
      |> Async.Ignore

    do!
      Api.sendVideo userId (makeFile fileId) ""
      |> Api.api config
      |> Async.Ignore

  | Core.Voice fileId ->
    do!
      Api.sendChatAction userId ChatAction.UploadAudio
      |> Api.api config
      |> Async.Ignore

    do!
      Api.sendVoice userId (makeFile fileId) ""
      |> Api.api config
      |> Async.Ignore

  | Core.Document fileId ->
    do!
      Api.sendChatAction userId ChatAction.UploadDocument
      |> Api.api config
      |> Async.Ignore

    do!
      Api.sendDocument userId (makeFile fileId) ""
      |> Api.api config
      |> Async.Ignore

  | Core.VideoNote fileId ->
    do!
      Api.sendChatAction userId ChatAction.UploadVideoNote
      |> Api.api config
      |> Async.Ignore

    do!
      Api.sendVideoNote userId (makeFile fileId)
      |> Api.api config
      |> Async.Ignore }

let onUpdate getConnection storagePath ctx = async {
  use connection = getConnection ()
  let config = ctx.Config

  match ctx.Update with
  // Message updates
  | { Message = Some ({ From = Some user } as message) } ->
    let! customerId, lastId, state = Customer.getOrCreate connection user

    let services = Services.get connection customerId

    let commands = Commands.onMessage message

    let! state, _ = Core.update services commands state

    let renderServices : Render.Services =
      { getMyCourses = Repo.getMyCourses connection customerId
        getAllCourses = Repo.getAllCourses connection
        getCourseData = Repo.getCourseData connection
        getCurrentBlockTitle =
          Repo.getCurrentBlockTitle connection customerId }

    let! text, keyboard =
      Render.state renderServices user state

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

    do! Customer.update connection customerId user lastId state

  // Inline keyboard updates
  | { CallbackQuery = Some ({ From = user; Message = Some message } as query) } ->
    let! customerId, _, state = Customer.getOrCreate connection user

    let services = Services.get connection customerId

    let commands = Commands.onQuery query

    let! state, effect = Core.update services commands state

    let effectContents, queryAnswer = Render.queryEffect effect

    let renderServices : Render.Services =
      { getMyCourses = Repo.getMyCourses connection customerId
        getAllCourses = Repo.getAllCourses connection
        getCourseData = Repo.getCourseData connection
        getCurrentBlockTitle =
          Repo.getCurrentBlockTitle connection customerId }

    let! text, keyboard =
      Render.state renderServices user state

    do! answerCallbackQuery config query.Id queryAnswer

    let! lastId = async {
      match effectContents with
      | [ ] ->
        if text <> String.Empty then
          do! editMessage config message.MessageId user.Id text keyboard
          return Option.map (always message.MessageId) keyboard
        else
          return Some message.MessageId

      | contents ->
        do! removeKeyboard config user.Id message.MessageId

        do!
          contents
          |> List.map (sendContent config user.Id storagePath)
          |> Async.Sequential
          |> Async.Ignore

        if text <> String.Empty && not <| List.isEmpty contents then
          return! sendMessage config user.Id text keyboard
        else
          return None  }

    do! Customer.update connection customerId user lastId state

  | _ ->
    () }
