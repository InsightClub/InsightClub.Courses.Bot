module InsightClub.Courses.Bot.Commands

open Core
open System
open Funogram.Telegram


type Message = Types.Message
type CallbackQuery = Types.CallbackQuery

type QueryEffect =
  | InformMin
  | InformMax
  | ShowBlock of Content list

let start = "/start"
let help = "/help"
let select = "/select"
let prev = "/prev"
let next = "/next"
let exit = "/exit"
let show = "/show"
let close = "/close"

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

let onMessage message : BotCommands<unit> =
  let getInactive () =
    match message with
    | Command start -> Some Inactive.Start
    | _             -> None

  let getIdle () =
    match message with
    | Command help   -> Some Idle.Help
    | Command start  -> Some <| Idle.Start 5
    | _              -> None

  let getListingCourses () = None

  let getViewingCourse () = None

  let getStudyingCourse () = None

  { getInactive = getInactive
    getIdle = getIdle
    getListingMyCourses = getListingCourses
    getViewingCourse = getViewingCourse
    getStudyingCourse = getStudyingCourse }

let onQuery query =
  let getInactive () = None

  let getIdle () = None

  let getListingCourses () =
    match query with
    | ParamQ select id -> Some <| ListingMyCourses.Select id
    | CommandQ prev    -> Some <| ListingMyCourses.Prev QueryEffect.InformMin
    | CommandQ next    -> Some <| ListingMyCourses.Next QueryEffect.InformMax
    | CommandQ exit    -> Some ListingMyCourses.Exit
    | _                -> None

  let getViewingCourse () =
    match query with
    | CommandQ start -> Some ViewingCourse.Start
    | CommandQ exit  -> Some ViewingCourse.Exit
    | _              -> None

  let getStudyingCourse () =
    match query with
    | CommandQ show  -> Some <| StudyingCourse.ShowInfo QueryEffect.ShowBlock
    | CommandQ prev  -> Some <| StudyingCourse.Prev QueryEffect.InformMin
    | CommandQ next  -> Some <| StudyingCourse.Next QueryEffect.InformMax
    | CommandQ close -> Some StudyingCourse.Close
    | CommandQ exit  -> Some StudyingCourse.Exit
    | _              -> None

  { getInactive = getInactive
    getIdle = getIdle
    getListingMyCourses = getListingCourses
    getViewingCourse = getViewingCourse
    getStudyingCourse = getStudyingCourse }
