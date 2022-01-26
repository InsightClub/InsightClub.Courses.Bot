module InsightClub.Courses.Bot.Core


// Types
type CourseId = int
type BlockId = int
type Page = int
type Count = int
type Text = string
type FileId = string

type Content =
  | Text of Text
  | Photo of FileId
  | Audio of FileId
  | Video of FileId
  | Voice of FileId
  | Document of FileId
  | VideoNote of FileId

module Inactive =
  type Command = Start

module Idle =
  type Command =
    | Help
    | Start

  type Msg =
    | Started
    | Helping
    | NoCourses
    | SelectCanceled
    | Exited
    | Error

module ListingCourses =
  type Command<'Effect> =
    | Select of CourseId
    | Prev of 'Effect
    | Next of 'Effect
    | Exit

  type Msg =
    | Started
    | Error

  type Context =
    | Added
    | All

  type State =
    { Context: Context
      Page: Page
      Count: Count
      Msg: Msg }

module ViewingCourse =
  type Command =
    | Start
    | Exit

  type Msg =
    | Started
    | CourseEmpty
    | Closed
    | Error

module StudyingCourse =
  type Command<'Effect> =
    | ShowInfo of (Content list -> 'Effect)
    | Prev of 'Effect
    | Next of 'Effect
    | Close
    | Exit

  type Msg =
    | Studying
    | Error

type BotState =
  | Inactive
  | Idle of Idle.Msg
  | ListingCourses of ListingCourses.State
  | ViewingCourse of CourseId * ViewingCourse.Msg
  | StudyingCourse of CourseId * StudyingCourse.Msg

type GetCommand<'Command> = unit -> 'Command option

type BotCommands<'Effect> =
  { getInactive: GetCommand<Inactive.Command>
    getIdle: GetCommand<Idle.Command>
    getListingCourses: GetCommand<ListingCourses.Command<'Effect>>
    getViewingCourse: GetCommand<ViewingCourse.Command>
    getStudyingCourse: GetCommand<StudyingCourse.Command<'Effect>> }

type Service<'Param, 'Result> = ('Param -> 'Result) -> 'Result
type Service<'Result> = Service<unit, 'Result>

type BotServices<'Effect, 'Result> =
  { callback: BotState -> 'Effect option -> 'Result
    checkAnyCourses: Service<bool, 'Result>
    getCoursesCount: Service<Count, 'Result>
    checkCourseStarted: CourseId -> Service<bool, 'Result>
    getFirstBlockId: CourseId -> Service<BlockId option, 'Result>
    setCurrentBlock: CourseId -> BlockId option -> Service<'Result>
    getPrevBlockId: CourseId -> Service<BlockId option, 'Result>
    getNextBlockId: CourseId -> Service<BlockId option, 'Result>
    getCurrentBlockContent: CourseId -> Service<Content list, 'Result> }

// Values
/// Initial state
let initial = Inactive

let private coursesPerPage = 5

let private updateInactive callback = function
| Some Inactive.Start ->
  callback (Idle Idle.Started) None

| None ->
  callback Inactive None

let private updateIdle callback checkAnyCourses = function
| Some Idle.Help ->
  callback (Idle Idle.Helping) None

| Some Idle.Start ->
  checkAnyCourses <|
    fun any ->
      if any then
        let state : ListingCourses.State =
          { Context = ListingCourses.Added
            Page = 0
            Count = coursesPerPage
            Msg = ListingCourses.Started }

        callback (ListingCourses state) None
      else
        callback (Idle Idle.NoCourses) None

| None ->
  callback (Idle Idle.Error) None

let private updateListingCourses
  callback
  getCoursesCount
  checkCourseStarted
  (innerState : ListingCourses.State) = function
| Some (ListingCourses.Select courseId) ->
  checkCourseStarted courseId <|
    fun started ->
      if started then
        callback (StudyingCourse (courseId, StudyingCourse.Studying)) None
      else
        callback (ViewingCourse (courseId, ViewingCourse.Started)) None

| Some (ListingCourses.Prev informMin) ->
  let state, effect =
    if innerState.Page = 0 then
      let newInnerState =
        { innerState with
            Msg = ListingCourses.Started }

      ListingCourses newInnerState, Some informMin
    else
      let newInnerState =
        { innerState with
            Page = innerState.Page - 1
            Msg = ListingCourses.Started }

      ListingCourses newInnerState, None

  callback state effect

| Some (ListingCourses.Next informMax) ->
  getCoursesCount <|
    fun coursesCount ->
      let state, effect =
        if (innerState.Page + 1) * innerState.Count >= coursesCount then
          let newInnerState =
            { innerState with
                Msg = ListingCourses.Started }

          ListingCourses newInnerState, Some informMax
        else
          let newInnerState =
            { innerState with
                Page = innerState.Page + 1
                Msg = ListingCourses.Started }

          ListingCourses newInnerState, None

      callback state effect

| Some ListingCourses.Exit ->
  callback (Idle Idle.SelectCanceled) None

| None ->
  let newInnerState =
    { innerState with
        Msg = ListingCourses.Error }

  callback (ListingCourses newInnerState) None

let private updateViewingCourse
  callback getFirstBlockId setCurrentBlock courseId = function
| Some ViewingCourse.Start ->
  getFirstBlockId courseId <|
    function
    | Some blockId ->
      setCurrentBlock courseId (Some blockId) <|
        fun () ->
          callback (StudyingCourse (courseId, StudyingCourse.Studying)) None

    | None ->
      callback (ViewingCourse (courseId, ViewingCourse.CourseEmpty)) None

| Some ViewingCourse.Exit ->
  callback (Idle Idle.Exited) None

| None ->
  callback (ViewingCourse (courseId, ViewingCourse.Error)) None

let private updateStudyingCourse
  { callback = callback
    getCurrentBlockContent = getCurrentBlockContent
    getPrevBlockId = getPrevBlockId
    getNextBlockId = getNextBlockId
    setCurrentBlock = setCurrentBlock } courseId = function
| Some (StudyingCourse.ShowInfo show) ->
  getCurrentBlockContent courseId <|
    fun contents ->
      callback
        (StudyingCourse (courseId, StudyingCourse.Studying))
        (Some (show contents))

| Some (StudyingCourse.Prev informMin) ->
  getPrevBlockId courseId <|
    function
    | Some blockId ->
      setCurrentBlock courseId (Some blockId) <|
        fun () ->
          callback (StudyingCourse (courseId, StudyingCourse.Studying)) None

    | None ->
      callback
        (StudyingCourse (courseId, StudyingCourse.Studying)) (Some informMin)

| Some (StudyingCourse.Next informMax) ->
  getNextBlockId courseId <|
    function
    | Some blockId ->
      setCurrentBlock courseId (Some blockId) <|
        fun () ->
          callback (StudyingCourse (courseId, StudyingCourse.Studying)) None

    | None ->
      callback
        (StudyingCourse (courseId, StudyingCourse.Studying)) (Some informMax)

| Some StudyingCourse.Close ->
  setCurrentBlock courseId None <|
    fun () ->
      callback (ViewingCourse (courseId, ViewingCourse.Closed)) None

| Some StudyingCourse.Exit ->
  callback (Idle Idle.Exited) None

| None ->
  callback (StudyingCourse (courseId, StudyingCourse.Error)) None

let update services commands =
  let s = services
  function
  | Inactive ->
    commands.getInactive ()
    |> updateInactive s.callback

  | Idle _ ->
    commands.getIdle()
    |> updateIdle s.callback s.checkAnyCourses

  | ListingCourses innerState ->
    commands.getListingCourses ()
    |> updateListingCourses
      s.callback s.getCoursesCount s.checkCourseStarted innerState

  | ViewingCourse (courseId, _) ->
    commands.getViewingCourse ()
    |> updateViewingCourse
      s.callback s.getFirstBlockId s.setCurrentBlock courseId

  | StudyingCourse (courseId, _) ->
    commands.getStudyingCourse ()
    |> updateStudyingCourse s courseId
