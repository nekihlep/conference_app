test_that("check_login возвращает корректную структуру", {
  # Мокаем функции работы с БД
  mock_conn <- structure(list(), class = "DBIConnection")
  mock_user <- data.frame(
    user_id = 1,
    username = "testuser",
    password_hash = sodium::password_store("password123"),
    role = "user"
  )
  
  with_mock(
    get_db_connection = function() mock_conn,
    dbGetQuery = function(conn, query, params) mock_user,
    dbDisconnect = function(conn) NULL,
    {
      result <- check_login("testuser", "password123")
      expect_true(result$success)
      expect_equal(result$user_id, 1)
      expect_equal(result$role, "user")
    }
  )
})

test_that("check_login возвращает FALSE при неверном пароле", {
  mock_conn <- structure(list(), class = "DBIConnection")
  mock_user <- data.frame(
    user_id = 1,
    username = "testuser",
    password_hash = sodium::password_store("correct_password"),
    role = "user"
  )
  
  with_mock(
    get_db_connection = function() mock_conn,
    dbGetQuery = function(conn, query, params) mock_user,
    dbDisconnect = function(conn) NULL,
    {
      result <- check_login("testuser", "wrong_password")
      expect_false(result$success)
    }
  )
})