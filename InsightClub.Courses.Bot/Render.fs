module InsightClub.Courses.Bot.Render

open Core
open System
open Funogram.Telegram
open System.Text.RegularExpressions


type User = Types.User
type Button = Types.InlineKeyboardButton

let private c s = Regex("\n[ ]+").Replace(s, "\n")
let private random = Random()
let randomEmoji () =
  let emojis =
    [| "🤷‍♂️"; "😵‍💫"; "🙄"; "🤪"; "🙀"
       "😭"; "😣"; "🥺"; "😑"; "💩" |]

  emojis.[ random.Next(emojis.Length) ]

let private idleMsg (user: User) = function
| Idle.Started ->
  let lastName =
    user.LastName
    |> Option.map ((+) " ")
    |> Option.defaultValue ""

  c$"Добро пожаловать в InsightClub.Courses.Bot, {user.FirstName} \
    {lastName}! ✨ С помощью этого бота Вы можете проходить курсы \
    InsightClub! 😎

    Отправьте /help для получения помощи 👀"

| Idle.Helping ->
  c$"Добро пожаловать в справку InsightClub.Courses.Bot! 🤖

    Этот бот имеет несколько режимов 🧞‍♂️ На данный момент он находится \
    в режиме ожидания. Все остальные режимы имеют вспомогательные \
    клавиатуры, которые помогут Вам легко разобраться в функционале.

    {Commands.select} - Выбрать курс ⚡️
    {Commands.help} - Получить помощь (Вы сейчас здесь) 👀

    Если Вы ещё не оплатили ни одного курса, Ваш список курсов будет пуст. \
    Для того, чтоб приорбрести курсы, свяжитесь пожалуйста с ХХХ. Затем, \
    после проверки оплаты, Вам будет предоставлен доступ к оплаченному курсу. \
    Вы сможете найти его по комманде {Commands.select}.

    Учитывайте, что команда {Commands.help} работает только в режиме ожидания. \
    В остальных режимах она не распознаётся, ибо их интерфейс поможет \
    Вам легко разобраться 🔥"

| Idle.NoCourses ->
  c$"У Вас пока нет доступных курсов {randomEmoji ()}
    Для получения справки отправьте {Commands.help} 🤹‍♂️"

| Idle.SelectCanceled ->
  "Выбор курса отменён 👌"

| Idle.Error ->
  c$"Неизвестная команда {randomEmoji ()}
    Отправьте {Commands.help} для получения помощи 👀"

let private listingCoursesMsg page count courseCount msg =
  let m s =
    match msg with
    | ListingCourses.Started ->
      s

    | ListingCourses.Error ->
      c$"Неизвестная команда. {randomEmoji ()}

        {s}"

  let min = page * count + 1
  let max = page * count + courseCount

  if min = max
  then $"Курс № {min}"
  else $"Курсы с № {min} по № {max}"
  |> m
  |> c

module private Button =
  let cancel = "Отмена ❌"
  let exit = "Выход 🚪"
  let prev = "⬅️"
  let next = "➡️"

let private button text command : Button =
  { Text = text
    CallbackData = Some command
    Url = None
    Pay = None
    LoginUrl = None
    CallbackGame = None
    SwitchInlineQuery = None
    SwitchInlineQueryCurrentChat = None }

let state getCourses user state = async {
  match state with
  | Inactive ->
    return String.Empty, None

  | Idle msg ->
    return idleMsg user msg, None

  | ListingCourses (page, count, msg) ->
    let! courses = getCourses page count

    return
      listingCoursesMsg page count (List.length courses) msg,
      Some
        [ for (id, title) in courses do
            yield [ button title $"{Commands.select} {id}" ]

            yield [ button Button.prev Commands.prev
                    button Button.next Commands.next ]

            yield [ button Button.exit Commands.exit ] ]

  // Stub
  | ViewingCourse _ ->
    return "", None

  // Stub
  | StudyingCourse _ ->
    return "", None }
