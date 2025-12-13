
source("R/db_functions.R")
check_login <- function(username, password) {
  conn <- get_db_connection()
  
  user_data <- dbGetQuery(conn, 
                          "SELECT * FROM users WHERE username = ?",
                          params = list(username))
  
  dbDisconnect(conn)
  
  if (nrow(user_data) == 1) {
    password_correct <- sodium::password_verify(user_data$password_hash, password)
    
    if (password_correct) {
      return(list(
        success = TRUE, 
        user_id = user_data$user_id,
        username = user_data$username,
        role = user_data$role
      ))
    }
  }
  
  return(list(success = FALSE))
}

register_user <- function(username, password, email, full_name, institution) {
  conn <- get_db_connection()
  
  existing_user <- dbGetQuery(conn, 
                              "SELECT * FROM users WHERE username = ?", 
                              params = list(username))
  
  if (nrow(existing_user) > 0) {
    dbDisconnect(conn)
    return(FALSE)
  }
  
  hashed_password <- sodium::password_store(password)
  
  dbExecute(conn, 
            "INSERT INTO users (username, password_hash, email, full_name, institution, role) 
     VALUES (?, ?, ?, ?, ?, 'user')",
            params = list(username, hashed_password, email, full_name, institution))
  
  dbDisconnect(conn)
  return(TRUE)
}

# UI компоненты аутентификации
auth_ui <- function(show_register = FALSE) {
  if (!show_register) {
    wellPanel(
      style = "max-width: 400px; margin: 50px auto; padding: 20px;",
      h3("🔐 Вход в систему", style = "text-align: center;"),
      
      textInput("login_username", "Имя пользователя", value = "admin"),
      passwordInput("login_password", "Пароль", value = "admin"),
      actionButton("login_btn", "Войти", class = "btn-primary", style = "width: 100%;"),
      
      br(), br(),
      p(style = "text-align: center;",
        "Вы ещё не зарегистрированы? ",
        actionLink("go_to_register", "Зарегистрироваться")
      )
    )
  } else {
    wellPanel(
      style = "max-width: 400px; margin: 50px auto; padding: 20px;",
      h3("👤 Регистрация", style = "text-align: center;"),
      
      textInput("reg_username", "Имя пользователя *"),
      passwordInput("reg_password", "Пароль *"),
      textInput("reg_email", "Email *"),
      textInput("reg_full_name", "ФИО *"),
      textInput("reg_institution", "Место работы/учёбы *"),
      
      actionButton("register_btn", "Зарегистрироваться", class = "btn-success", style = "width: 100%;"),
      
      br(), br(),
      p(style = "text-align: center;",
        "Вы уже зарегистрированы? ",
        actionLink("go_to_login", "Войти")
      )
    )
  }
}