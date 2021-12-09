module InsightClub.Courses.Bot.Services

open Core


let get connection customerId =
  let callback state effect =
    Async.singleton (state, effect)

  let checkAnyCourses callback = async {
    let! any =
      Repo.checkAnyCourses connection customerId

    return! callback any }

  let getCoursesCount callback = async {
    let! count =
      Repo.getCoursesCount connection customerId

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

    return! callback contents }

  { callback = callback
    checkAnyCourses = checkAnyCourses
    getCoursesCount = getCoursesCount
    checkCourseStarted = checkCourseStarted
    getFirstBlockId = getFirstBlockId
    setCurrentBlock = setCurrentBlock
    getPrevBlockId = getPrevBlockId
    getNextBlockId = getNextBlockId
    getCurrentBlockContent = getCurrentBlockContent }
