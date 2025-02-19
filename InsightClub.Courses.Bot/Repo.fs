module InsightClub.Courses.Bot.Repo

open Npgsql.FSharp


module Queries =
  let checkCourseAvailable =
    "(SELECT telegram_id
    FROM customers
    WHERE customer_id = @customer_id)
    =
    (SELECT telegram_id
    FROM creators
    WHERE creator_id = (
      SELECT creator_id
      FROM courses
      WHERE course_id = @course_id
    ))"

let getOrCreateCustomer
  connection telegramId firstName lastName username initialState =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "WITH i AS(
      INSERT INTO customers (
        telegram_id,
        first_name,
        last_name,
        username,
        telegram_bot_state
      )
      VALUES (
        @telegram_id,
        @first_name,
        @last_name,
        @username,
        @initial_state)
      ON CONFLICT(telegram_id)
      DO NOTHING
      RETURNING customer_id, telegram_bot_state
    )
    SELECT * FROM i
    UNION
    SELECT customer_id, telegram_bot_state
    FROM customers
    WHERE telegram_id = @telegram_id"
  |> Sql.parameters
    [ "telegram_id", Sql.int64 telegramId
      "first_name", Sql.string firstName
      "last_name", Sql.stringOrNone lastName
      "username", Sql.stringOrNone username
      "initial_state", Sql.string initialState ]
  |> Sql.executeRowAsync
    ( fun read ->
        read.int "customer_id",
        read.string "telegram_bot_state" )
  |> Async.AwaitTask

let updateCustomer connection customerId firstName lastName username state =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "UPDATE customers
    SET
      first_name = @first_name,
      last_name = @last_name,
      username = @username,
      telegram_bot_state = @state
    WHERE customer_id = @customer_id"
  |> Sql.parameters
    [ "first_name", Sql.string firstName
      "last_name", Sql.stringOrNone lastName
      "username", Sql.stringOrNone username
      "state", Sql.string state
      "customer_id", Sql.int customerId ]
  |> Sql.executeNonQueryAsync
  |> Async.AwaitTask
  |> Async.Ignore

let checkMyCourses connection customerId =
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

let checkAllCourses connection =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "SELECT EXISTS(
      SELECT 1
      FROM courses
    ) as any"
  |> Sql.executeRowAsync (fun read -> read.bool "any")
  |> Async.AwaitTask

let getMyCoursesCount connection customerId =
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

let getAllCoursesCount connection =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "SELECT COUNT(*) as count
    FROM courses"
  |> Sql.executeRowAsync (fun read -> read.int "count")
  |> Async.AwaitTask

let getMyCourses connection customerId page count =
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

let getAllCourses connection page count =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "SELECT course_id, course_title
    FROM courses
    LIMIT @limit
    OFFSET @offset"
  |> Sql.parameters
    [ "limit", Sql.int count
      "offset", Sql.int (page * count) ]
  |> Sql.executeAsync
    ( fun read ->
        read.int "course_id",
        read.string "course_title" )
  |> Async.AwaitTask

let checkCourseLaunched connection customerId courseId =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "SELECT block_id IS NOT NULL as started
    FROM customer_courses
    WHERE customer_id = @customer_id
    AND course_id = @course_id"
  |> Sql.parameters
    [ "customer_id", Sql.int customerId
      "course_id", Sql.int courseId ]
  |> Sql.executeRowAsync (fun read -> read.bool "started")
  |> Async.AwaitTask

let getFirstBlockId connection courseId =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "SELECT COALESCE((
      SELECT block_id
      FROM blocks
      WHERE course_id = @course_id
      AND block_index = (
        SELECT MIN(block_index)
        FROM blocks
        WHERE course_id = @course_id
      )),
      NULL
    ) as block_id"
  |> Sql.parameters
    [ "course_id", Sql.int courseId ]
  |> Sql.executeRowAsync (fun read -> read.intOrNone "block_id")
  |> Async.AwaitTask

let setCurrentBlock connection customerId courseId blockId =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "UPDATE customer_courses
    SET block_id = @block_id
    WHERE customer_id = @customer_id
    AND course_id = @course_id"
  |> Sql.parameters
    [ "customer_id", Sql.int customerId
      "course_id", Sql.int courseId
      "block_id", Sql.intOrNone blockId ]
  |> Sql.executeNonQueryAsync
  |> Async.AwaitTask
  |> Async.Ignore

let getCourseData connection courseId =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "SELECT course_title, course_description
    FROM courses
    WHERE course_id = @course_id"
  |> Sql.parameters
    [ "course_id", Sql.int courseId ]
  |> Sql.executeRowAsync
    ( fun read ->
        read.string "course_title",
        read.string "course_description" )
  |> Async.AwaitTask

let getNextOrPrevBlockId connection customerId courseId getNext =
  let sign = if getNext then '>' else '<'
  let order = if getNext then "ASC" else "DESC"
  connection
  |> Sql.existingConnection
  |> Sql.query
    $"SELECT COALESCE((
      SELECT block_id
      FROM blocks
      WHERE course_id = @course_id
      AND block_index {sign} (
        SELECT block_index
        FROM blocks
        WHERE course_id = @course_id
        AND block_id = (
          SELECT block_id
          FROM customer_courses
          WHERE customer_id = @customer_id
          AND course_id = @course_id
        )
      )
      ORDER BY block_index {order}
      LIMIT 1),
      NULL
    ) as block_id"
  |> Sql.parameters
    [ "customer_id", Sql.int customerId
      "course_id", Sql.int courseId ]
  |> Sql.executeRowAsync (fun read -> read.intOrNone "block_id")
  |> Async.AwaitTask

let getCurrentBlockContent connection customerId courseId =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "SELECT content, content_type
    FROM contents
    WHERE block_id = (
      SELECT block_id
      FROM customer_courses
      WHERE customer_id = @customer_id
      AND course_id = @course_id
    )
    ORDER BY content_index"
  |> Sql.parameters
    [ "customer_id", Sql.int customerId
      "course_id", Sql.int courseId ]
  |> Sql.executeAsync
    ( fun read ->
        read.string "content",
        read.string "content_type" )
  |> Async.AwaitTask

let getCurrentBlockTitle connection customerId courseId =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "SELECT COALESCE((
      SELECT block_title
      FROM blocks
      WHERE block_id = (
        SELECT block_id
        FROM customer_courses
        WHERE customer_id = @customer_id
        AND course_id = @course_id
      )),
      ''
    ) as block_title"
  |> Sql.parameters
    [ "customer_id", Sql.int customerId
      "course_id", Sql.int courseId ]
  |> Sql.executeRowAsync (fun read -> read.string "block_title")
  |> Async.AwaitTask

let checkCourseAvailable connection customerId courseId =
  connection
  |> Sql.existingConnection
  |> Sql.query
    $"SELECT ({Queries.checkCourseAvailable}) as available"
  |> Sql.parameters
    [ "customer_id", Sql.int customerId
      "course_id", Sql.int courseId ]
  |> Sql.executeRowAsync (fun read -> read.bool "available")
  |> Async.AwaitTask

let checkCourseAddedToMy connection customerId courseId =
  connection
  |> Sql.existingConnection
  |> Sql.query
    "SELECT EXISTS(
      SELECT 1
      FROM customer_courses
      WHERE customer_id = @customer_id
      AND course_id = @course_id
    ) as added"
  |> Sql.parameters
    [ "customer_id", Sql.int customerId
      "course_id", Sql.int courseId ]
  |> Sql.executeRowAsync (fun read -> read.bool "added")
  |> Async.AwaitTask

let addCourseToMy connection customerId courseId =
  connection
  |> Sql.existingConnection
  |> Sql.query
    $"INSERT INTO customer_courses (
      customer_id,
      course_id
    )
    SELECT @customer_id, @course_id
    WHERE ({Queries.checkCourseAvailable})"
  |> Sql.parameters
    [ "customer_id", Sql.int customerId
      "course_id", Sql.int courseId ]
  |> Sql.executeNonQueryAsync
  |> Async.AwaitTask
  |> Async.Ignore
