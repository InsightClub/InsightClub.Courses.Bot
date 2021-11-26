module InsightClub.Courses.Bot.Commands

open Core
open System
open Funogram.Telegram


type Message = Types.Message
type CallbackQuery = Types.CallbackQuery

let start = "/start"
let help = "/help"

let private (|Command|_|) command = function
| { Message.Text = Some text }
  when text = command -> Some ()
| _                   -> None

let private (|Text|_|) = function
| { Message.Text = Some text } -> Some text
| _                            -> None

let private (|Photo|_|) = function
| { Message.Photo = Some sizes } -> Some <| (Seq.head sizes).FileId
| _                              -> None

let private (|Audio|_|) = function
| { Message.Audio = Some audio } -> Some audio.FileId
| _                              -> None

let private (|Video|_|) = function
| { Message.Video = Some video } -> Some video.FileId
| _                              -> None

let private (|Voice|_|) = function
| { Message.Voice = Some voice } -> Some voice.FileId
| _                              -> None

let private (|Document|_|) = function
| { Message.Document = Some document } -> Some document.FileId
| _                                    -> None

let private (|VideoNote|_|) = function
| { Message.VideoNote = Some note } -> Some note.FileId
| _                                 -> None

let private (|CommandQ|_|) command = function
| { CallbackQuery.Data = Some text }
  when text = command -> Some ()
| _                   -> None

let private (|ParamQ|_|) command = function
| { CallbackQuery.Data = Some text }
  when text.StartsWith(command + " ") ->
  let start = String.length command + 1
  tryParseWith Int32.TryParse text.[ start .. ]

| _ -> None

let onMessage message : BotCommands =
  let getInactive () =
    match message with
    | Command start -> Some Inactive.Start
    | _             -> None

  let getIdle () =
    match message with
    | Command help -> Some Idle.Help
    | _            -> None

  { getInactive = getInactive
    getIdle = getIdle }
