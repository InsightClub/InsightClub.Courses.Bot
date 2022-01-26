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
    | MyCourses

  type Msg =
    | Started
    | Helping
    | NoAddedCourses
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
    | My
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
    checkMyCourses: Service<bool, 'Result>
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

let private updateInactive services = function
| Some Inactive.Start ->
  services.callback (Idle Idle.Started) None

| None ->
  services.callback Inactive None

let private updateIdle services = function
| Some Idle.Help ->
  services.callback (Idle Idle.Helping) None

| Some Idle.MyCourses ->
  services.checkMyCourses <|
    fun any ->
      if any then
        let state : ListingCourses.State =
          { Context = ListingCourses.My
            Page = 0
            Count = coursesPerPage
            Msg = ListingCourses.Started }

        services.callback (ListingCourses state) None
      else
        services.callback (Idle Idle.NoAddedCourses) None

| None ->
  services.callback (Idle Idle.Error) None

let private updateListingCourses services (innerState: ListingCourses.State)
  = function
| Some (ListingCourses.Select courseId) ->
  services.checkCourseStarted courseId <|
    fun started ->
      if started then
        let newState =
          StudyingCourse (courseId, StudyingCourse.Studying)

        services.callback newState None
      else
        let newState =
          ViewingCourse (courseId, ViewingCourse.Started)

        services.callback newState None

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

  services.callback state effect

| Some (ListingCourses.Next informMax) ->
  services.getCoursesCount <|
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

      services.callback state effect

| Some ListingCourses.Exit ->
  services.callback (Idle Idle.SelectCanceled) None

| None ->
  let newInnerState =
    { innerState with
        Msg = ListingCourses.Error }

  services.callback (ListingCourses newInnerState) None

let private updateViewingCourse services courseId = function
| Some ViewingCourse.Start ->
  services.getFirstBlockId courseId <|
    function
    | Some blockId ->
      services.setCurrentBlock courseId (Some blockId) <|
        fun () ->
          services.callback (StudyingCourse (courseId, StudyingCourse.Studying)) None

    | None ->
      services.callback (ViewingCourse (courseId, ViewingCourse.CourseEmpty)) None

| Some ViewingCourse.Exit ->
  services.callback (Idle Idle.Exited) None

| None ->
  services.callback (ViewingCourse (courseId, ViewingCourse.Error)) None

let private updateStudyingCourse services courseId = function
| Some (StudyingCourse.ShowInfo show) ->
  services.getCurrentBlockContent courseId <|
    fun contents ->
      services.callback
        (StudyingCourse (courseId, StudyingCourse.Studying))
        (Some (show contents))

| Some (StudyingCourse.Prev informMin) ->
  services.getPrevBlockId courseId <|
    function
    | Some blockId ->
      services.setCurrentBlock courseId (Some blockId) <|
        fun () ->
          services.callback (StudyingCourse (courseId, StudyingCourse.Studying)) None

    | None ->
      services.callback
        (StudyingCourse (courseId, StudyingCourse.Studying)) (Some informMin)

| Some (StudyingCourse.Next informMax) ->
  services.getNextBlockId courseId <|
    function
    | Some blockId ->
      services.setCurrentBlock courseId (Some blockId) <|
        fun () ->
          services.callback (StudyingCourse (courseId, StudyingCourse.Studying)) None

    | None ->
      services.callback
        (StudyingCourse (courseId, StudyingCourse.Studying)) (Some informMax)

| Some StudyingCourse.Close ->
  services.setCurrentBlock courseId None <|
    fun () ->
      services.callback (ViewingCourse (courseId, ViewingCourse.Closed)) None

| Some StudyingCourse.Exit ->
  services.callback (Idle Idle.Exited) None

| None ->
  services.callback (StudyingCourse (courseId, StudyingCourse.Error)) None

let update services commands = function
| Inactive ->
  commands.getInactive ()
  |> updateInactive services

| Idle _ ->
  commands.getIdle()
  |> updateIdle services

| ListingCourses innerState ->
  commands.getListingCourses ()
  |> updateListingCourses services innerState

| ViewingCourse (courseId, _) ->
  commands.getViewingCourse ()
  |> updateViewingCourse services courseId

| StudyingCourse (courseId, _) ->
  commands.getStudyingCourse ()
  |> updateStudyingCourse services courseId
