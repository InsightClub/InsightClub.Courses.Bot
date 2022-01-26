module InsightClub.Courses.Bot.Render

open Core
open System
open Funogram.Telegram
open System.Text.RegularExpressions


type User = Types.User
type Button = Types.InlineKeyboardButton

type Services =
  { getMyCourses: Page -> Count -> Async<(int * string) list>
    getAllCourses: Page -> Count -> Async<(int * string) list>
    getCourseData: CourseId -> Async<string * string>
    getCurrentBlockTitle: CourseId -> Async<string> }

let private c s = Regex("\n[ ]*").Replace(s, "\n")
let private random = Random()
let randomEmoji () =
  let emojis =
    [| "🤷‍♂️"; "😵‍💫"; "🙄"; "🤪"; "🙀"
       "😭"; "😣"; "🥺"; "😑"; "💩" |]

  emojis.[ random.Next(emojis.Length) ]

let private commands =
  c$"{Commands.mycourses} - Мои курсы ⭐️
    {Commands.courses} - Доступные курсы 📄
    {Commands.help} - Получить помощь 👀"

let private idleMsg (user: User) = function
| Idle.Started ->
  let lastName =
    user.LastName
    |> Option.map ((+) " ")
    |> Option.defaultValue ""

  c$"Добро пожаловать в InsightClub.Courses.Bot, {user.FirstName} \
    {lastName}! ✨

    С помощью этого бота Вы можете проходить курсы \
    InsightClub! 😎

    Отправьте {Commands.help} для получения помощи 👀"

| Idle.Helping ->
  c$"Добро пожаловать в справку InsightClub.Courses.Bot! 🤖

    Этот бот имеет несколько режимов 🧞‍♂️ На данный момент он находится \
    в режиме ожидания. Все остальные режимы имеют вспомогательные \
    клавиатуры, которые помогут Вам легко разобраться в функционале.

    {commands}

    Для начала Вы можете ознакомиться со списком доступных курсов по команде \
    {Commands.courses}. Из этого меню Вы можете выбрать бесплатные курсы \
    и добавить их в свой список. Платные курсы станут Вам доступны после \
    оплаты и проверки факта оплаты автором курса. Каждый курс содержит \
    информацию о его содержании, контакты автора, информацию об оплате. \
    После добавления курса в свой список он становится доступен в меню \
    {Commands.mycourses}.

    Учитывайте, что команда {Commands.help} работает только в режиме \
    ожидания. В остальных режимах она не распознаётся. Их интерфейсы \
    помогут Вам легко разобраться.

    Приятного личностного роста! 🔥"

| Idle.NoCoursesAdded ->
  c$"Вы пока не добавили ни одного курса в свой список {randomEmoji ()}

    Для просмотра доступных курсов, отправьте {Commands.courses}. \
    Для получения справки отправьте {Commands.help} 🤹‍♂️"

| Idle.NoCourses ->
  c$"К сожалению, на платформе ещё нет ни одного курса {randomEmoji ()}

    В скором времени, здесь будет много полезной информации. \
    Ну а пока, просим Вас набраться терпения ❤️

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

let private listingCoursesMsg (innerState: ListingCourses.State) courseCount =
  let m s =
    match innerState.Msg with
    | ListingCourses.Started ->
      s

    | ListingCourses.Error ->
      c$"Неизвестная команда. {randomEmoji ()}

        {s}"

  let min = innerState.Page * innerState.Count + 1
  let max = innerState.Page * innerState.Count + courseCount

  let context =
    match innerState.Context with
    | ListingCourses.My ->
      "Мои курсы ⭐️"

    | ListingCourses.All ->
      "Доступные курсы 📄"

  if min = max
  then $"{context}\n\n(№ {min})"
  else $"{context}\n\n(№ {min} - № {max})"
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

let state services user state = async {
  match state with
  | Inactive ->
    return String.Empty, None

  | Idle msg ->
    return idleMsg user msg, None

  | ListingCourses innerState ->
    let! courses =
      match innerState.Context with
      | ListingCourses.My ->
        services.getMyCourses innerState.Page innerState.Count

      | ListingCourses.All ->
        services.getAllCourses innerState.Page innerState.Count

    return
      listingCoursesMsg innerState (List.length courses),
      Some
        [ for (id, title) in courses do
            yield [ button title $"{Commands.select} {id}" ]

          yield [ button Button.prev Commands.prev
                  button Button.next Commands.next ]

          yield [ button Button.exit Commands.exit ] ]

  | ViewingCourse (courseId, msg) ->
    let! title, desc = services.getCourseData courseId

    let desc =
      if desc <> String.Empty
      then desc
      else "{Без описания}"

    let data =
      c$"{title}

      {desc}"

    return
      viewingCourseMsg data msg,
      Some
        [ [ button Button.start Commands.start ]
          [ button Button.exit Commands.exit ] ]

  | StudyingCourse (courseId, msg) ->
    let! title = services.getCurrentBlockTitle courseId
    return
      studyingCourseMsg title msg,
      Some
        [ [ button Button.prev Commands.prev
            button Button.show Commands.show
            button Button.next Commands.next ]
          [ button Button.close Commands.close ]
          [ button Button.exit Commands.exit ] ] }

let queryEffect = function
| Some Commands.InformMin ->
  [ ], Some "Вы дошли до начала."

| Some Commands.InformMax ->
  [ ], Some "Вы дошли до конца."

| Some (Commands.ShowBlock contents) ->
  contents, None

| None ->
  [ ], None
