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
