source("R/db_functions.R")
source("R/auth.R")

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .shiny-notification {
        position: fixed;
        top: 20px;
        right: 20px;
        z-index: 10000;
      }
    "))
  ),
  titlePanel("🎤 Система подачи заявок на конференции"),
  uiOutput("main_ui")
)

server <- function(input, output, session) {
  
  user <- reactiveValues(logged_in = FALSE, username = "", role = "", user_id = NULL)
  show_register <- reactiveVal(FALSE)
  show_users_table <- reactiveVal(FALSE)
  show_applications_table <- reactiveVal(FALSE)
  
  # Реактивные данные
  conferences_data <- reactiveVal(data.frame())
  user_applications <- reactiveVal(data.frame())
  all_applications_data <- reactiveVal(data.frame())
  
  # Загрузка данных при входе
  observeEvent(user$logged_in, {
    if (user$logged_in) {
      # Загружаем конференции
      conn <- get_db_connection()
      conferences <- dbGetQuery(conn, "SELECT * FROM conferences WHERE status = 'active'")
      dbDisconnect(conn)
      conferences_data(conferences)
      
      # Загружаем заявки пользователя
      if (user$role == "user") {
        load_user_applications()
      }
      
      # Для админа загружаем все заявки
      if (user$role == "admin") {
        load_all_applications()
      }
    }
  })
  
  # Функция загрузки заявок пользователя
  load_user_applications <- function() {
    conn <- get_db_connection()
    applications <- dbGetQuery(conn, 
                               "SELECT a.*, c.title as conference_title 
       FROM applications a 
       JOIN conferences c ON a.conference_id = c.conference_id 
       WHERE a.user_id = ?",
                               params = list(user$user_id))
    dbDisconnect(conn)
    user_applications(applications)
  }
  
  # Функция загрузки всех заявок для админа
  load_all_applications <- function() {
    conn <- get_db_connection()
    applications <- dbGetQuery(conn,
                               "SELECT a.application_id, u.username, c.title, 
            a.participation_type, a.topic, a.status,
            a.applied_at
     FROM applications a
     JOIN users u ON a.user_id = u.user_id
     JOIN conferences c ON a.conference_id = c.conference_id
     ORDER BY a.applied_at DESC")    # ← Сначала новые заявки!
    dbDisconnect(conn)
    all_applications_data(applications)
  }
  
  # Главный UI
  output$main_ui <- renderUI({
    if (!user$logged_in) {
      auth_ui(show_register())
    } else {
      if (user$role == "admin") {
        # ИНТЕРФЕЙС АДМИНА
        fluidRow(
          column(12,
                 wellPanel(
                   h3(paste("👋 Добро пожаловать,", user$username, "! (Администратор)")),
                   
                   actionButton("admin_logout_btn", "🚪 Выйти", class = "btn-warning"),
                   
                   br(), br(),
                   h4("👨‍💼 Панель администратора"),
                   
                   fluidRow(
                     column(6,
                            actionButton("show_users_btn", "👥 Показать пользователей", 
                                         class = "btn-info", style = "width: 100%; margin-bottom: 10px;"),
                            actionButton("show_all_applications_btn", "📋 Все заявки", 
                                         class = "btn-info", style = "width: 100%;")
                     ),
                     column(6,
                            actionButton("add_conference_btn", "➕ Добавить конференцию", 
                                         class = "btn-success", style = "width: 100%; margin-bottom: 10px;"),
                            actionButton("manage_conferences_btn", "🎤 Управление конференциями", 
                                         class = "btn-info", style = "width: 100%;")
                     )
                   ),
                   
                   # Таблицы
                   uiOutput("admin_tables")
                 )
          )
        )
      } else {
        # ИНТЕРФЕЙС ОБЫЧНОГО ПОЛЬЗОВАТЕЛЯ
        tabsetPanel(
          id = "user_tabs",
          type = "tabs",
          
          tabPanel("🏠 Главная",
                   wellPanel(
                     h3(paste("👋 Добро пожаловать,", user$username, "!")),
                     
                     fluidRow(
                       column(6,
                              wellPanel(
                                h4("📊 Статистика заявок"),
                                plotOutput("applications_pie")
                              )
                       ),
                       column(6,
                              wellPanel(
                                h4("📈 Информация"),
                                p(strong("Активных конференций:"), textOutput("active_conferences", inline = TRUE)),
                                p(strong("Ваших заявок:"), textOutput("user_applications_count", inline = TRUE)),
                                p(strong("Ближайшая конференция:"), textOutput("nearest_conference", inline = TRUE))
                              )
                       )
                     )
                   )
          ),
          
          tabPanel("🎤 Конференции",
                   wellPanel(
                     h3("Доступные конференции"),
                     
                     # Список конференций
                     uiOutput("conferences_list"),
                     
                     br(),
                     
                     # Форма подачи заявки
                     wellPanel(
                       h4("📝 Подать заявку на участие"),
                       
                       selectInput("selected_conference", "Выберите конференцию:", 
                                   choices = c("Выберите конференцию..." = "")),
                       
                       radioButtons("participation_type", "Тип участия:",
                                    choices = c("👂 Слушатель" = "listener", 
                                                "🎤 Докладчик" = "speaker"),
                                    selected = "listener"),
                       
                       uiOutput("speaker_fields"),
                       
                       actionButton("submit_application_btn", "📤 Подать заявку", 
                                    class = "btn-success")
                     )
                   )
          ),
          
          tabPanel("📝 Мои заявки",
                   wellPanel(
                     h3("Мои заявки на конференции"),
                     
                     DTOutput("my_applications_table"),
                     
                     br(),
                     
                     fluidRow(
                       column(4, valueBoxOutput("pending_apps", width = 12)),
                       column(4, valueBoxOutput("approved_apps", width = 12)),
                       column(4, valueBoxOutput("total_apps", width = 12))
                     )
                   )
          ),
          
          tabPanel("🚪 Выйти",
                   wellPanel(
                     h3("Выход из системы"),
                     p("Вы уверены, что хотите выйти?"),
                     actionButton("confirm_logout_btn", "✅ Да, выйти", class = "btn-warning"),
                     actionButton("cancel_logout_btn", "❌ Отмена", class = "btn-secondary")
                   )
          )
        )
      }
    }
  })
  
  # UI для админских таблиц
  output$admin_tables <- renderUI({
    tagList(
      if (show_users_table()) {
        tagList(
          h4("📊 Таблица пользователей"),
          DTOutput("users_table"),
          br()
        )
      },
      if (show_applications_table()) {
        tagList(
          h4("📋 Все заявки"),
          DTOutput("all_applications_table"),
          br()
        )
      }
    )
  })
  
  # ОБНОВЛЕНИЕ ВЫБОРА КОНФЕРЕНЦИЙ
  observe({
    conferences <- conferences_data()
    if (nrow(conferences) > 0) {
      choices <- setNames(conferences$conference_id, conferences$title)
      updateSelectInput(session, "selected_conference", 
                        choices = c("Выберите конференцию..." = "", choices))
    } else {
      updateSelectInput(session, "selected_conference", 
                        choices = c("Нет доступных конференций" = ""))
    }
  })
  
  # ПОЛЯ ДЛЯ ДОКЛАДЧИКОВ
  output$speaker_fields <- renderUI({
    if (input$participation_type == "speaker") {
      tagList(
        textInput("presentation_topic", "Тема доклада:", placeholder = "Введите тему вашего доклада"),
        fileInput("qualification_file", "Файл с материалами доклада (опционально):",
                  accept = c(".pdf", ".doc", ".docx", ".ppt", ".pptx"))
      )
    }
  })
  
  # ОБРАБОТЧИКИ СОБЫТИЙ
  
  # Аутентификация
  observeEvent(input$login_btn, {
    req(input$login_username, input$login_password)
    
    result <- check_login(input$login_username, input$login_password)
    
    if (result$success) {
      user$logged_in <- TRUE
      user$username <- result$username
      user$role <- result$role
      user$user_id <- result$user_id
      
      showNotification(paste("✅ Успешный вход,", user$username, "!"), type = "message")
    } else {
      showNotification("❌ Неверный логин или пароль!", type = "error")
    }
  })
  
  observeEvent(input$register_btn, {
    req(input$reg_username, input$reg_password, input$reg_email, 
        input$reg_full_name, input$reg_institution)
    
    success <- register_user(
      input$reg_username,
      input$reg_password,
      input$reg_email,
      input$reg_full_name,
      input$reg_institution
    )
    
    if (success) {
      showNotification("✅ Регистрация успешна! Теперь войдите в систему.", type = "message")
      show_register(FALSE)
    } else {
      showNotification("❌ Пользователь с таким именем уже существует!", type = "error")
    }
  })
  
  # Подача заявки
  observeEvent(input$submit_application_btn, {
    req(input$selected_conference, input$participation_type)
    
    if (input$selected_conference == "") {
      showNotification("❌ Выберите конференцию!", type = "error")
      return()
    }
    
    conn <- get_db_connection()
    
    # Проверяем существующую заявку
    existing_application <- dbGetQuery(conn,
                                       "SELECT * FROM applications WHERE user_id = ? AND conference_id = ?",
                                       params = list(user$user_id, input$selected_conference)
    )
    
    if (nrow(existing_application) > 0) {
      showNotification("❌ Вы уже подавали заявку на эту конференцию!", type = "error")
      dbDisconnect(conn)
      return()
    }
    
    # Сохраняем заявку
    tryCatch({
      if (input$participation_type == "speaker") {
        dbExecute(conn,
                  "INSERT INTO applications (user_id, conference_id, participation_type, topic) 
           VALUES (?, ?, ?, ?)",
                  params = list(user$user_id, input$selected_conference, "speaker", 
                                ifelse(is.null(input$presentation_topic) || input$presentation_topic == "", 
                                       "Тема не указана", input$presentation_topic))
        )
      } else {
        dbExecute(conn,
                  "INSERT INTO applications (user_id, conference_id, participation_type) 
           VALUES (?, ?, ?)",
                  params = list(user$user_id, input$selected_conference, "listener")
        )
      }
      
      dbDisconnect(conn)
      showNotification("✅ Заявка успешно подана!", type = "message")
      
      # Обновляем данные
      load_user_applications()
      
      # Очищаем форму
      updateSelectInput(session, "selected_conference", selected = "")
      updateRadioButtons(session, "participation_type", selected = "listener")
      
    }, error = function(e) {
      dbDisconnect(conn)
      showNotification(paste("❌ Ошибка при подаче заявки:", e$message), type = "error")
    })
  })
  
  # Админские кнопки
  observeEvent(input$show_users_btn, {
    show_users_table(TRUE)
    show_applications_table(FALSE)
  })
  
  observeEvent(input$show_all_applications_btn, {
    show_applications_table(TRUE)
    show_users_table(FALSE)
    load_all_applications()
  })
  
  observeEvent(input$add_conference_btn, {
    showNotification("📝 Функция добавления конференции в разработке", type = "message")
  })
  
  observeEvent(input$manage_conferences_btn, {
    showNotification("🎤 Функция управления конференциями в разработке", type = "message")
  })
  
  # Выход
  observeEvent(input$admin_logout_btn, {
    user$logged_in <- FALSE
    user$username <- ""
    user$role <- ""
    user$user_id <- NULL
    show_users_table(FALSE)
    show_applications_table(FALSE)
    showNotification("👋 Вы вышли из системы", type = "message")
  })
  
  observeEvent(input$confirm_logout_btn, {
    user$logged_in <- FALSE
    user$username <- ""
    user$role <- ""
    user$user_id <- NULL
    showNotification("👋 Вы вышли из системы", type = "message")
  })
  
  observeEvent(input$cancel_logout_btn, {
    showNotification("✅ Выход отменён", type = "message")
  })
  
  # Переключение форм
  observeEvent(input$go_to_register, { show_register(TRUE) })
  observeEvent(input$go_to_login, { show_register(FALSE) })
  
  # ВЫХОДНЫЕ ДАННЫЕ
  
  # Для админа
  output$users_table <- renderDT({
    conn <- get_db_connection()
    users <- dbGetQuery(conn, "SELECT user_id, username, email, full_name, institution, role, created_at FROM users")
    dbDisconnect(conn)
    datatable(users, options = list(pageLength = 10))
  })
  
  output$all_applications_table <- renderDT({
    applications <- all_applications_data()
    if (nrow(applications) > 0) {
      datatable(applications, options = list(pageLength = 10))
    }
  })
  
  # Для пользователя
  output$active_conferences <- renderText({ 
    nrow(conferences_data())
  })
  
  output$user_applications_count <- renderText({
    nrow(user_applications())
  })
  
  output$nearest_conference <- renderText({
    conferences <- conferences_data()
    if (nrow(conferences) > 0) {
      min(conferences$date)
    } else {
      "Нет активных конференций"
    }
  })
  
  output$applications_pie <- renderPlot({
    apps <- user_applications()
    if (nrow(apps) > 0) {
      status_counts <- table(apps$status)
      pie(status_counts, 
          labels = paste(names(status_counts), "\n", status_counts),
          col = c("#ffc107", "#28a745", "#dc3545"),
          main = "Статус ваших заявок")
    } else {
      plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE)
      text(0, 0, "Нет данных о заявках", cex = 1.5)
    }
  })
  
  output$my_applications_table <- renderDT({
    apps <- user_applications()
    if (nrow(apps) > 0) {
      display_data <- apps[, c("conference_title", "participation_type", "topic", "status")]
      display_data$participation_type <- ifelse(display_data$participation_type == "speaker", "Докладчик", "Слушатель")
      display_data$status <- ifelse(display_data$status == "pending", "На рассмотрении",
                                    ifelse(display_data$status == "approved", "Одобрено", "Отклонено"))
      datatable(display_data, 
                colnames = c("Конференция", "Тип участия", "Тема доклада", "Статус"),
                options = list(pageLength = 5))
    }
  })
  
  output$conferences_list <- renderUI({
    conferences <- conferences_data()
    if (nrow(conferences) > 0) {
      tagList(
        lapply(1:nrow(conferences), function(i) {
          wellPanel(
            h4(conferences$title[i]),
            p(strong("Описание:"), conferences$description[i]),
            p(strong("Дата:"), conferences$date[i]),
            p(strong("Место:"), conferences$location[i]),
            p(strong("Макс. участников:"), conferences$max_participants[i])
          )
        })
      )
    } else {
      p("Нет активных конференций")
    }
  })
  
  output$pending_apps <- renderValueBox({
    apps <- user_applications()
    count <- if (nrow(apps) > 0) sum(apps$status == "pending") else 0
    valueBox(count, "На рассмотрении", icon = icon("clock"), color = "yellow")
  })
  
  output$approved_apps <- renderValueBox({
    apps <- user_applications()
    count <- if (nrow(apps) > 0) sum(apps$status == "approved") else 0
    valueBox(count, "Одобрено", icon = icon("check"), color = "green")
  })
  
  output$total_apps <- renderValueBox({
    apps <- user_applications()
    count <- if (nrow(apps) > 0) nrow(apps) else 0
    valueBox(count, "Всего заявок", icon = icon("list"), color = "blue")
  })
}

shinyApp(ui, server)