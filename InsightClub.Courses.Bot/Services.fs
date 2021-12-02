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

  { callback = callback
    checkAnyCourses = checkAnyCourses
    getCoursesCount = getCoursesCount }
