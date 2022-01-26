module InsightClub.Courses.Bot.Services

open Core


let private mapContent = function
| text, "text"         -> Text text
| fileId, "photo"      -> Photo fileId
| fileId, "audio"      -> Audio fileId
| fileId, "video"      -> Video fileId
| fileId, "voice"      -> Voice fileId
| fileId, "document"   -> Document fileId
| fileId, "video_note" -> VideoNote fileId
| fileId, contentType  ->
  failwith $"Unknown content type: {contentType}! FileId: {fileId}"

let get connection customerId =
  let callback state effect =
    Async.singleton (state, effect)

  let checkMyCourses callback = async {
    let! any =
      Repo.checkMyCourses connection customerId

    return! callback any }

  let checkAllCourses callback = async {
    let! any =
      Repo.checkAllCourses connection

    return! callback any }

  let getMyCoursesCount callback = async {
    let! count =
      Repo.getMyCoursesCount connection customerId

    return! callback count }

  let getAllCoursesCount callback = async {
    let! count =
      Repo.getAllCoursesCount connection

    return! callback count }

  let checkCourseStarted courseId callback = async {
    let! started =
      Repo.checkCourseStarted connection customerId courseId

    return! callback started }

  let getFirstBlockId courseId callback = async {
    let! started =
      Repo.getFirstBlockId connection courseId

    return! callback started }

  let setCurrentBlock courseId blockId callback = async {
    do!
      Repo.setCurrentBlock connection customerId courseId blockId

    return! callback () }

  let getPrevBlockId courseId callback = async {
    let! blockId =
      Repo.getNextOrPrevBlockId connection customerId courseId false

    return! callback blockId }

  let getNextBlockId courseId callback = async {
    let! blockId =
      Repo.getNextOrPrevBlockId connection customerId courseId true

    return! callback blockId }

  let getCurrentBlockContent courseId callback = async {
    let! contents =
      Repo.getCurrentBlockContent connection customerId courseId

    let contents = contents |> List.map mapContent

    return! callback contents }

  { callback = callback
    checkMyCourses = checkMyCourses
    checkAllCourses = checkAllCourses
    getMyCoursesCount = getMyCoursesCount
    getAllCoursesCount = getAllCoursesCount
    checkCourseStarted = checkCourseStarted
    getFirstBlockId = getFirstBlockId
    setCurrentBlock = setCurrentBlock
    getPrevBlockId = getPrevBlockId
    getNextBlockId = getNextBlockId
    getCurrentBlockContent = getCurrentBlockContent }
