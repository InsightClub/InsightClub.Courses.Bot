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

    {Commands.start} - Начать курс ⚡️
    {Commands.help} - Получить помощь (Вы сейчас здесь) 👀

    Если Вы ещё не оплатили ни одного курса, Ваш список курсов будет пуст. \
    Для того, чтоб приорбрести курсы, свяжитесь пожалуйста с ХХХ. Затем, \
    после проверки оплаты, Вам будет предоставлен доступ к оплаченному курсу. \
    Вы сможете найти его по комманде {Commands.start}.

    Учитывайте, что команда {Commands.help} работает только в режиме ожидания. \
    В остальных режимах она не распознаётся, ибо их интерфейс поможет \
    Вам легко разобраться 🔥"

| Idle.Error ->
  c$"Неизвестная команда {randomEmoji ()}
    Отправьте {Commands.help} для получения помощи 👀"

let private button text command : Button =
  { Text = text
    CallbackData = Some command
    Url = None
    Pay = None
    LoginUrl = None
    CallbackGame = None
    SwitchInlineQuery = None
    SwitchInlineQueryCurrentChat = None }

let state user state = async {
  match state with
  | Inactive ->
    return String.Empty, None

  | Idle msg ->
    return idleMsg user msg, None }
