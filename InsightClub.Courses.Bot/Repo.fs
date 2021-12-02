module InsightClub.Courses.Bot.Repo

open Npgsql.FSharp


let getState initialState connection telegramId =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "WITH i AS(
      INSERT INTO customers (telegram_id, telegram_state)
      VALUES (@telegram_id, @initial_state)
      ON CONFLICT(telegram_id)
      DO NOTHING
      RETURNING customer_id, telegram_state
    )
    SELECT * FROM i
    UNION
    SELECT customer_id, telegram_state
    FROM customers
    WHERE telegram_id = @telegram_id"
  |> Sql.parameters
    [ "telegram_id", Sql.int64 telegramId
      "initial_state", Sql.string initialState ]
  |> Sql.executeRowAsync
    ( fun read ->
        read.int "customer_id",
        read.string "telegram_state" )
  |> Async.AwaitTask

let updateState connection customerId state =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "UPDATE customers
    SET telegram_state = @new_state
    WHERE customer_id = @customer_id"
  |> Sql.parameters
    [ "new_state", Sql.string state
      "customer_id", Sql.int customerId ]
  |> Sql.executeNonQueryAsync
  |> Async.AwaitTask
  |> Async.Ignore

let checkAnyCourses connection customerId =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "SELECT EXISTS(
      SELECT 1
      FROM customer_courses
      WHERE customer_id = @customer_id
    ) as any"
  |> Sql.parameters
    [ "customer_id", Sql.int customerId ]
  |> Sql.executeRowAsync (fun read -> read.bool "any")
  |> Async.AwaitTask

let getCoursesCount connection customerId =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "SELECT COUNT(*) as count
    FROM customer_courses
    WHERE customer_id = @customer_id"
  |> Sql.parameters
    [ "customer_id", Sql.int customerId ]
  |> Sql.executeRowAsync (fun read -> read.int "count")
  |> Async.AwaitTask

let getCourses connection customerId page count =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "SELECT course_id, course_title
    FROM courses
    WHERE course_id
    IN (
      SELECT course_id
      FROM customer_courses
      WHERE customer_id = @customer_id
    )
    ORDER BY course_id
    LIMIT @limit
    OFFSET @offset"
  |> Sql.parameters
    [ "customer_id", Sql.int customerId
      "limit", Sql.int count
      "offset", Sql.int (page * count) ]
  |> Sql.executeAsync
    ( fun read ->
        read.int "course_id",
        read.string "course_title" )
  |> Async.AwaitTask
