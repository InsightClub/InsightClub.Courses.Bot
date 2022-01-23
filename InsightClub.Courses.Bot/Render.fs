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

let private commands =
  c$"{Commands.select} - Выбрать курс ⚡️
    {Commands.help} - Получить помощь 👀"

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

    {commands}

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
  c$"Выбор курса отменён 👌

    {commands}"

| Idle.Exited ->
  c$"Как прикажете 🧞‍♂️

    {commands}"

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

let private viewingCourseMsg data = function
| ViewingCourse.Started ->
  data

| ViewingCourse.CourseEmpty ->
  c$"Курс не имеет ни одного блока, то есть пуст {randomEmoji ()}

    {data}"

| ViewingCourse.Closed ->
  c$"Курс закрыт 👌
    Вы в любой момент можете начать его с помощью кнопки ниже.

    {data}"

| ViewingCourse.Error ->
  c$"Неизвестная команда {randomEmoji ()}

    {data}"

let private studyingCourseMsg title = function
| StudyingCourse.Studying ->
  title

| StudyingCourse.Error ->
  c$"Неизвестная команда {randomEmoji ()}

    {title}"

module private Button =
  let exit = "Выход 🚪"
  let prev = "⬅️"
  let next = "➡️"
  let start = "Начать ⚡️"
  let close = "Закрыть 🔓"
  let show = "Читать 👁‍🗨"

let private button text command : Button =
  { Text = text
    CallbackData = Some command
    Url = None
    Pay = None
    LoginUrl = None
    CallbackGame = None
    SwitchInlineQuery = None
    SwitchInlineQueryCurrentChat = None }

let state getCourses getCourseData getCurrentBlockTitle user state = async {
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

  | ViewingCourse (courseId, msg) ->
    let! title, desc = getCourseData courseId
    let data =
      c$"{title}

      {desc}"

    return
      viewingCourseMsg data msg,
      Some
        [ [ button Button.start Commands.start ]
          [ button Button.exit Commands.exit ] ]

  | StudyingCourse (courseId, msg) ->
    let! title = getCurrentBlockTitle courseId
    return
      studyingCourseMsg title msg,
      Some
        [ [ button Button.prev Commands.prev
            button Button.show Commands.show
            button Button.next Commands.next ]
          [ button Button.close Commands.close ]
          [ button Button.exit Commands.exit ] ] }
