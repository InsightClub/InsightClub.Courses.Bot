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
    | AllCourses

  type Msg =
    | Started
    | Helping
    | NoCoursesAdded
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
    | Continue
    | Add
    | Exit

  type Msg =
    | Started
    | CourseEmpty
    | Closed
    | Error

  type Context =
    | NotAvailable
    | Available
    | Added
    | Launched

  type State =
    { CourseId: CourseId
      Context: Context
      Msg: Msg }

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
  | ViewingCourse of ViewingCourse.State
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
    checkAllCourses: Service<bool, 'Result>
    getMyCoursesCount: Service<Count, 'Result>
    getAllCoursesCount: Service<Count, 'Result>
    getFirstBlockId: CourseId -> Service<BlockId option, 'Result>
    setCurrentBlock: CourseId -> BlockId option -> Service<'Result>
    getPrevBlockId: CourseId -> Service<BlockId option, 'Result>
    getNextBlockId: CourseId -> Service<BlockId option, 'Result>
    getCurrentBlockContent: CourseId -> Service<Content list, 'Result>
    getCourseAvailability: CourseId ->
      Service<ViewingCourse.Context, 'Result>
    addCourseToMy: CourseId -> Service<'Result> }

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
        services.callback (Idle Idle.NoCoursesAdded) None

| Some Idle.AllCourses ->
  services.checkAllCourses <|
    fun any ->
      if any then
        let state : ListingCourses.State =
          { Context = ListingCourses.All
            Page = 0
            Count = coursesPerPage
            Msg = ListingCourses.Started }

        services.callback (ListingCourses state) None
      else
        services.callback (Idle Idle.NoCourses) None

| None ->
  services.callback (Idle Idle.Error) None

let private updateListingCourses services (innerState: ListingCourses.State)
  = function
| Some (ListingCourses.Select courseId) ->
  services.getCourseAvailability courseId <|
    fun ctx ->
      let innerState : ViewingCourse.State =
        { CourseId = courseId
          Context = ctx
          Msg = ViewingCourse.Started }

      services.callback (ViewingCourse innerState) None

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
  let getCoursesCount =
    match innerState.Context with
    | ListingCourses.My ->
      services.getMyCoursesCount

    | ListingCourses.All ->
      services.getAllCoursesCount

  getCoursesCount <|
    fun coursesCount ->
      let nextCount =
        (innerState.Page + 1) * innerState.Count

      let state, effect =
        if nextCount >= coursesCount then
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

let private updateViewingCourse services (innerState: ViewingCourse.State)
  = function
| Some ViewingCourse.Start ->
  services.getFirstBlockId innerState.CourseId <|
    function
    | Some blockId ->
      services.setCurrentBlock innerState.CourseId (Some blockId) <|
        fun () ->
          let newState =
            StudyingCourse (innerState.CourseId, StudyingCourse.Studying)

          services.callback newState None

    | None ->
      let newInnerState =
        { innerState with
            Msg = ViewingCourse.CourseEmpty }

      services.callback (ViewingCourse newInnerState) None

| Some ViewingCourse.Continue ->
  let newState =
    StudyingCourse (innerState.CourseId, StudyingCourse.Studying)

  services.callback newState None

| Some ViewingCourse.Add ->
  services.addCourseToMy innerState.CourseId <|
    fun () ->
      let newInnerState =
        { innerState with
            Context = ViewingCourse.Added }

      services.callback (ViewingCourse newInnerState) None

| Some ViewingCourse.Exit ->
  services.callback (Idle Idle.Exited) None

| None ->
  let newInnerState =
    { innerState with
        Msg = ViewingCourse.Error }

  services.callback (ViewingCourse newInnerState) None

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
          let newState =
            StudyingCourse (courseId, StudyingCourse.Studying)

          services.callback newState None

    | None ->
      let newState =
        StudyingCourse (courseId, StudyingCourse.Studying)

      services.callback newState (Some informMin)

| Some (StudyingCourse.Next informMax) ->
  services.getNextBlockId courseId <|
    function
    | Some blockId ->
      services.setCurrentBlock courseId (Some blockId) <|
        fun () ->
          let newState =
            StudyingCourse (courseId, StudyingCourse.Studying)

          services.callback newState None

    | None ->
      let newState =
        StudyingCourse (courseId, StudyingCourse.Studying)

      services.callback newState (Some informMax)

| Some StudyingCourse.Close ->
  services.setCurrentBlock courseId None <|
    fun () ->
      let innerState : ViewingCourse.State =
        { CourseId = courseId
          Context = ViewingCourse.Added
          Msg = ViewingCourse.Started }

      services.callback (ViewingCourse innerState) None

| Some StudyingCourse.Exit ->
  services.callback (Idle Idle.Exited) None

| None ->
  let newState =
    StudyingCourse (courseId, StudyingCourse.Error)

  services.callback newState None

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

| ViewingCourse innerState ->
  commands.getViewingCourse ()
  |> updateViewingCourse services innerState

| StudyingCourse (courseId, _) ->
  commands.getStudyingCourse ()
  |> updateStudyingCourse services courseId
