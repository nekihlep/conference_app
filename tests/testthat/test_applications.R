test_that("check_conference_limit корректно проверяет лимиты", {
  mock_conn <- structure(list(), class = "DBIConnection")
  
  with_mock(
    get_db_connection = function() mock_conn,
    dbGetQuery = function(conn, query, params) {
      if (grepl("max_participants", query)) {
        return(data.frame(max_participants = 100))
      } else {
        return(data.frame(count = 75))
      }
    },
    dbDisconnect = function(conn) NULL,
    {
      result <- check_conference_limit(1)
      expect_equal(result$max_participants, 100)
      expect_equal(result$current_approved, 75)
      expect_true(result$has_free_places)
    }
  )
})

test_that("check_conference_limit определяет отсутствие мест", {
  mock_conn <- structure(list(), class = "DBIConnection")
  
  with_mock(
    get_db_connection = function() mock_conn,
    dbGetQuery = function(conn, query, params) {
      if (grepl("max_participants", query)) {
        return(data.frame(max_participants = 100))
      } else {
        return(data.frame(count = 100))
      }
    },
    dbDisconnect = function(conn) NULL,
    {
      result <- check_conference_limit(1)
      expect_false(result$has_free_places)
    }
  )
})

test_that("validate_application_data разрешает новую заявку", {
  mock_conn <- structure(list(), class = "DBIConnection")
  
  with_mock(
    get_db_connection = function() mock_conn,
    dbGetQuery = function(conn, query, params) {
      return(data.frame())  # Нет существующих заявок
    },
    dbDisconnect = function(conn) NULL,
    {
      result <- validate_application_data(1, "speaker", 1)
      expect_true(result)
    }
  )
})