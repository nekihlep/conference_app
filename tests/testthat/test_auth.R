test_that("check_login возвращает корректную структуру", {
  library(mockery)
  
  # Мокаем функции
  stub(check_login, "get_db_connection", 
       function() structure(list(), class = "DBIConnection"))
  
  stub(check_login, "dbGetQuery", 
       function(conn, query, params) {
         data.frame(
           user_id = 1,
           username = "testuser",
           password_hash = sodium::password_store("password123"),
           role = "user"
         )
       })
  
  stub(check_login, "dbDisconnect", 
       function(conn) NULL)
  
  result <- check_login("testuser", "password123")
  expect_true(result$success)
  expect_equal(result$user_id, 1)
  expect_equal(result$role, "user")
})

test_that("check_login возвращает FALSE при неверном пароле", {
  library(mockery)
  
  stub(check_login, "get_db_connection", 
       function() structure(list(), class = "DBIConnection"))
  
  stub(check_login, "dbGetQuery", 
       function(conn, query, params) {
         data.frame(
           user_id = 1,
           username = "testuser",
           password_hash = sodium::password_store("correct_password"),
           role = "user"
         )
       })
  
  stub(check_login, "dbDisconnect", 
       function(conn) NULL)
  
  result <- check_login("testuser", "wrong_password")
  expect_false(result$success)
})