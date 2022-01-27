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

    let contents =
      contents |> List.map mapContent

    return! callback contents }

  let getCourseAvailability courseId callback = async {
    let! available =
      Repo.checkCourseAvailable connection customerId courseId

    if not available
    then return! callback ViewingCourse.NotAvailable
    else
      let! added =
        Repo.checkCourseAddedToMy connection customerId courseId

      if not added
      then return! callback ViewingCourse.Available
      else
        let! launched =
          Repo.checkCourseLaunched connection customerId courseId

        if not launched
        then return! callback ViewingCourse.Added
        else return! callback ViewingCourse.Launched }

  let addCourseToMy courseId callback = async {
    do!
      Repo.addCourseToMy connection customerId courseId

    return! callback () }

  { callback = callback
    checkMyCourses = checkMyCourses
    checkAllCourses = checkAllCourses
    getMyCoursesCount = getMyCoursesCount
    getAllCoursesCount = getAllCoursesCount
    getFirstBlockId = getFirstBlockId
    setCurrentBlock = setCurrentBlock
    getPrevBlockId = getPrevBlockId
    getNextBlockId = getNextBlockId
    getCurrentBlockContent = getCurrentBlockContent
    getCourseAvailability = getCourseAvailability
    addCourseToMy = addCourseToMy }
