# app.R
library(shiny)
library(shinyjs)
source("R/db_functions.R")
ui <- fluidPage(
  useShinyjs(),  # для показа/скрытия элементов
  titlePanel("🎤 Система подачи заявок на конференции"),
  uiOutput("main_ui")
)

server <- function(input, output, session) {
  
  # Реактивное значение для хранения информации о пользователе
  user <- reactiveValues(
    logged_in = FALSE, 
    username = "", 
    role = "",
    user_id = NULL
  )
  
  # Реактивное значение для переключения между входом и регистрацией
  show_register <- reactiveVal(FALSE)
  
  # Функция для проверки логина
  check_login <- function(username, password) {
    conn <- get_db_connection()
    
    # Ищем пользователя по username
    user_data <- dbGetQuery(conn, 
                            "SELECT * FROM users WHERE username = ?",
                            params = list(username)
    )
    
    dbDisconnect(conn)
    
    if (nrow(user_data) == 1) {
      # ПРАВИЛЬНО проверяем пароль
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
    
    # Проверяем, нет ли уже такого пользователя
    existing_user <- dbGetQuery(conn, 
                                "SELECT * FROM users WHERE username = ?", 
                                params = list(username)
    )
    
    if (nrow(existing_user) > 0) {
      dbDisconnect(conn)
      return(FALSE) # Пользователь уже существует
    }

    hashed_password <- sodium::password_store(password)
    
    # Сохраняем нового пользователя
    dbExecute(conn, 
              "INSERT INTO users (username, password_hash, email, full_name, institution, role) 
       VALUES (?, ?, ?, ?, ?, 'user')",
              params = list(
                username, 
                hashed_password,
                email,
                full_name,
                institution
              )
    )
    
    dbDisconnect(conn)
    return(TRUE)
  }
  
  # Главный интерфейс
  output$main_ui <- renderUI({
    if (!user$logged_in) {
      if (!show_register()) {
        # ФОРМА ВХОДА
        wellPanel(
          style = "max-width: 400px; margin: 50px auto; padding: 20px;",
          h3("🔐 Вход в систему", style = "text-align: center;"),
          
          textInput("login_username", "Имя пользователя", value = "admin"),
          passwordInput("login_password", "Пароль", value = "admin"),
          actionButton("login_btn", "Войти", class = "btn-primary", style = "width: 100%;"),
          
          br(), br(),
          
          # Ссылка на регистрацию
          p(style = "text-align: center;",
            "Вы ещё не зарегистрированы? ",
            actionLink("go_to_register", "Зарегистрироваться", style = "cursor: pointer; color: #007bff;")
          )
        )
      } else {
        # ФОРМА РЕГИСТРАЦИИ
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
          
          # Ссылка на вход
          p(style = "text-align: center;",
            "Вы уже зарегистрированы? ",
            actionLink("go_to_login", "Войти", style = "cursor: pointer; color: #007bff;")
          )
        )
      }
    } else {
      # ИНТЕРФЕЙС ПОСЛЕ ВХОДА
      fluidRow(
        column(12,
               wellPanel(
                 h3(paste("👋 Добро пожаловать,", user$username, "!")),
                 p(strong("Роль:"), user$role),
                 p(strong("ID пользователя:"), user$user_id),
                 
                 br(),
                 
                 # Кнопка выхода
                 actionButton("logout_btn", "🚪 Выйти", class = "btn-warning"),
                 
                 # Для админов - просмотр пользователей
                 if (user$role == "admin") {
                   tagList(
                     br(), br(),
                     h4("👨‍💼 Панель администратора"),
                     actionButton("show_users_btn", "Показать всех пользователей"),
                     tableOutput("users_table")
                   )
                 }
               )
        )
      )
    }
  })
  
  # Переход к регистрации
  observeEvent(input$go_to_register, {
    show_register(TRUE)
  })
  
  # Переход ко входу
  observeEvent(input$go_to_login, {
    show_register(FALSE)
  })
  
  # Обработчик кнопки ВХОДА
  observeEvent(input$login_btn, {
    req(input$login_username, input$login_password)
    
    # Показываем загрузку
    showNotification("Проверяем данные...", duration = NULL, id = "loading")
    
    result <- check_login(input$login_username, input$login_password)
    
    # Убираем уведомление о загрузке
    removeNotification("loading")
    
    if (result$success) {
      user$logged_in <- TRUE
      user$username <- result$username
      user$role <- result$role
      user$user_id <- result$user_id
      
      showNotification(paste("✅ Успешный вход! Добро пожаловать,", user$username), type = "message")
      
      # Очищаем поля
      updateTextInput(session, "login_username", value = "")
      updateTextInput(session, "login_password", value = "")
      
    } else {
      showNotification("❌ Неверное имя пользователя или пароль!", type = "error")
    }
  })
  
  # Обработчик кнопки РЕГИСТРАЦИИ
  observeEvent(input$register_btn, {
    req(input$reg_username, input$reg_password, input$reg_email, 
        input$reg_full_name, input$reg_institution)
    
    # Проверяем что все поля заполнены
    if (any(c(input$reg_username, input$reg_password, input$reg_email, 
              input$reg_full_name, input$reg_institution) == "")) {
      showNotification("❌ Заполните все обязательные поля!", type = "error")
      return()
    }
    
    success <- register_user(
      input$reg_username,
      input$reg_password,
      input$reg_email,
      input$reg_full_name,
      input$reg_institution
    )
    
    if (success) {
      showNotification("✅ Пользователь успешно зарегистрирован! Теперь вы можете войти.", type = "message")
      
      # Переключаем на форму входа
      show_register(FALSE)
      
      # Очищаем поля регистрации
      updateTextInput(session, "reg_username", value = "")
      updateTextInput(session, "reg_password", value = "")
      updateTextInput(session, "reg_email", value = "")
      updateTextInput(session, "reg_full_name", value = "")
      updateTextInput(session, "reg_institution", value = "")
      
    } else {
      showNotification("❌ Пользователь с таким именем уже существует!", type = "error")
    }
  })
  
  # Обработчик кнопки ВЫХОДА
  observeEvent(input$logout_btn, {
    user$logged_in <- FALSE
    user$username <- ""
    user$role <- ""
    user$user_id <- NULL
    showNotification("👋 Вы вышли из системы", type = "message")
  })
  
  # Показ всех пользователей (для админа)
  observeEvent(input$show_users_btn, {
    conn <- get_db_connection()
    users_data <- dbGetQuery(conn, "SELECT user_id, username, email, full_name, role FROM users")
    dbDisconnect(conn)
    
    output$users_table <- renderTable({
      users_data
    })
  })
}

# Запуск приложения
shinyApp(ui, server)