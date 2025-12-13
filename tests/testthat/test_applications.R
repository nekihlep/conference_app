test_that("check_conference_limit корректно проверяет лимиты", {
  # Используем mockery напрямую
  library(mockery)
  
  # Мокаем функции
  stub(check_conference_limit, "get_db_connection", 
       function() structure(list(), class = "DBIConnection"))
  
  stub(check_conference_limit, "dbGetQuery", 
       function(conn, query, params) {
         if (grepl("max_participants", query)) {
           return(data.frame(max_participants = 100))
         } else {
           return(data.frame(count = 75))
         }
       })
  
  stub(check_conference_limit, "dbDisconnect", 
       function(conn) NULL)
  
  result <- check_conference_limit(1)
  expect_equal(result$max_participants, 100)
  expect_equal(result$current_approved, 75)
  expect_true(result$has_free_places)
})

test_that("check_conference_limit определяет отсутствие мест", {
  library(mockery)
  
  stub(check_conference_limit, "get_db_connection", 
       function() structure(list(), class = "DBIConnection"))
  
  stub(check_conference_limit, "dbGetQuery", 
       function(conn, query, params) {
         if (grepl("max_participants", query)) {
           return(data.frame(max_participants = 100))
         } else {
           return(data.frame(count = 100))
         }
       })
  
  stub(check_conference_limit, "dbDisconnect", 
       function(conn) NULL)
  
  result <- check_conference_limit(1)
  expect_false(result$has_free_places)
})

test_that("validate_application_data разрешает новую заявку", {
  library(mockery)
  
  stub(validate_application_data, "get_db_connection", 
       function() structure(list(), class = "DBIConnection"))
  
  stub(validate_application_data, "dbGetQuery", 
       function(conn, query, params) data.frame())
  
  stub(validate_application_data, "dbDisconnect", 
       function(conn) NULL)
  
  result <- validate_application_data(1, "speaker", 1)
  expect_true(result)
})