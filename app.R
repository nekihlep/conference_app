library(shiny)
library(shinyjs)
library(DBI)
library(RSQLite)
library(sodium)
library(DT)
library(shinydashboard)
library(shinyjs)
library(ggplot2)
source("R/db_functions.R")
source("R/auth.R")
source("R/logic.R")

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
  titlePanel("💻 Система подачи заявок на конференции"),
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
  
  # Настройка шрифтов для русского языка
  setup_russian_fonts <- function() {
    # Устанавливаем шрифт для русского текста
    par(family = "sans")  # Используем стандартный шрифт
    
    # Если есть проблемы, пробуем другие варианты
    if (.Platform$OS.type == "windows") {
      try(par(family = "Arial"), silent = TRUE)
    } else {
      try(par(family = "Helvetica"), silent = TRUE)
    }
  }
  # Функция загрузки заявок на рассмотрение для админа
  load_pending_applications <- function() {
    conn <- get_db_connection()
    applications <- dbGetQuery(conn, "
    SELECT 
      a.application_id, 
      u.username, 
      u.full_name,
      c.title as conference_title, 
      a.participation_type, 
      a.topic, 
      a.status,
      af.file_name,
      af.file_path,
      a.applied_at
    FROM applications a
    JOIN users u ON a.user_id = u.user_id
    JOIN conferences c ON a.conference_id = c.conference_id
    LEFT JOIN application_files af ON a.application_id = af.application_id
    WHERE a.status = 'pending'
    ORDER BY a.applied_at DESC
  ")
    dbDisconnect(conn)
    return(applications)
  }
  
  # Функции для обновления статуса заявок
  approve_application <- function(application_id) {
    conn <- get_db_connection()
    dbExecute(conn,
              "UPDATE applications SET status = 'approved' WHERE application_id = ?",
              params = list(application_id))
    dbDisconnect(conn)
  }
  
  reject_application <- function(application_id) {
    conn <- get_db_connection()
    dbExecute(conn,
              "UPDATE applications SET status = 'rejected' WHERE application_id = ?",
              params = list(application_id))
    dbDisconnect(conn)
  }
  
  # Функция для получения ID выбранных заявок
  get_selected_application_ids <- function(selected_rows) {
    applications <- load_pending_applications()
    if (length(selected_rows) > 0 && nrow(applications) > 0) {
      return(applications$application_id[selected_rows])
    }
    return(NULL)
  }
  # Функция добавления новой конференции
  # Функция добавления новой конференции
  add_new_conference <- function(title, description, date, location, max_participants) {
    conn <- get_db_connection()
    success <- tryCatch({
      # Преобразуем дату в правильный формат
      if (is.numeric(date)) {
        # Если дата пришла как число (Excel формат)
        date <- as.Date(as.numeric(date), origin = "1899-12-30")
      } else {
        # Если дата в строковом формате
        date <- as.Date(date)
      }
      
      dbExecute(conn,
                "INSERT INTO conferences (title, description, date, location, max_participants, status) 
               VALUES (?, ?, ?, ?, ?, 'active')",
                params = list(title, description, as.character(date), location, max_participants))
      TRUE
    }, error = function(e) {
      cat("Ошибка при добавлении конференции:", e$message, "\n")
      FALSE
    }, finally = {
      dbDisconnect(conn)
    })
    return(success)
  }
  
  # Функция загрузки всех конференций
  load_all_conferences <- function() {
    conn <- get_db_connection()
    conferences <- dbGetQuery(conn, "SELECT * FROM conferences WHERE status = 'active' ORDER BY date DESC")
    dbDisconnect(conn)
    return(conferences)
  }

  # RENDERUI ДЛЯ ВЫБОРА КОНФЕРЕНЦИЙ
  output$conference_selector <- renderUI({
    conferences <- conferences_data()
    
    if (nrow(conferences) > 0) {
      choices <- setNames(conferences$conference_id, conferences$title)
      selectInput("selected_conference", "Выберите конференцию:", 
                  choices = c("Выберите конференцию..." = "", choices))
    } else {
      selectInput("selected_conference", "Выберите конференцию:", 
                  choices = c("Нет доступных конференций" = ""))
    }
  })
  # Добавьте эту функцию после load_all_applications()
  check_conference_limit <- function(conference_id) {
    conn <- get_db_connection()
    # Получаем максимальное количество участников для конференции
    conference <- dbGetQuery(conn, 
                             "SELECT max_participants FROM conferences WHERE conference_id = ?",
                             params = list(conference_id)
    )
    
    # Считаем сколько уже одобренных заявок (слушателей)
    approved_count <- dbGetQuery(conn,
                                 "SELECT COUNT(*) as count FROM applications 
     WHERE conference_id = ? AND status = 'approved' AND participation_type = 'listener'",
                                 params = list(conference_id)
    )
    
    dbDisconnect(conn)
    
    return(list(
      max_participants = conference$max_participants,
      current_approved = approved_count$count,
      has_free_places = approved_count$count < conference$max_participants
    ))
  }
  
  
  # Главный UI
  output$main_ui <- renderUI({
    if (!user$logged_in) {
      auth_ui(show_register())
    } else {
      if (user$role == "admin") {
        fluidPage(
          # Кнопка выхода в правом верхнем углу
          div(style = "position: absolute; top: 10px; right: 10px;",
              actionButton("logout_btn", "🚪 Выйти", class = "btn-warning")
          ),
          
          tabsetPanel(
            id = "admin_tabs",
            type = "tabs",
            tabPanel("📋 Заявки на рассмотрение",
                     wellPanel(
                       h3("Заявки, требующие решения"),
                       p("Здесь будут заявки докладчиков на рассмотрении"),
                       DTOutput("pending_applications_table"),
                       br(),
                       fluidRow(
                         column(6,
                                actionButton("approve_selected_btn", "✅ Одобрить выбранные", 
                                             class = "btn-success", style = "width: 100%;")
                         ),
                         column(6,
                                actionButton("reject_selected_btn", "❌ Отклонить выбранные", 
                                             class = "btn-danger", style = "width: 100%;")
                         )
                       ),
                       br(),
                       uiOutput("file_download_ui")
                     )
            ),
            
            tabPanel("👥 Пользователи",
                     wellPanel(
                       h3("Управление пользователями"),
                       DTOutput("users_table")
                     )
            ),
            
            tabPanel("📊 Отчеты",
                     wellPanel(
                       # Заголовок с кнопкой скачивания
                       fluidRow(
                         column(6,
                                h3("Отчеты по заявкам")
                         ),
                         column(6, 
                                style = "text-align: left; margin-top: 10px;",
                                actionButton("download_report_btn", "📥 Скачать отчет", 
                                             class = "btn-primary")
                         )
                      
                       ),
                       downloadLink("download_report", label = NULL, style = "display: none;"),
                       
                       fluidRow(
                         column(6,
                                wellPanel(
                                  h4(HTML("<strong><em>Динамика заявок по дням</em></strong>")),
                                  p("График покажет количество заявок по дням"),
                                  plotOutput("applications_trend_plot")
                                )
                         ),
                         column(6,
                                wellPanel(
                                  h4(HTML("<strong><em>Статусы заявок</em></strong>")),
                                  p("Круговая диаграмма статусов заявок"),
                                  plotOutput("applications_status_plot")
                                )
                         )
                       ),
                       
                       fluidRow(
                         column(6,
                                wellPanel(
                                  h4(HTML("<strong><em>Типы участников</em></strong>")),
                                  p("Соотношение докладчиков и слушателей"),
                                  plotOutput("participation_type_plot")
                                )
                         ),
                         column(6,
                                wellPanel(
                                  h4(HTML("<strong><em>Конференции по популярности</em></strong>")),
                                  p("Диаграмма рассеивания показывает заполняемость конференций"),
                                  plotOutput("conferences_popularity_plot")
                                )
                         )
                       )
                     )
            ),
            
            tabPanel("🎤 Конференции",
                     wellPanel(
                       h3("Управление конференциями"),
                       
                       # Форма добавления конференции
                       wellPanel(
                         h4("➕ Добавить новую конференцию"),
                         textInput("new_conf_title", "Название конференции:"),
                         textAreaInput("new_conf_description", "Описание:", rows = 3),
                         dateInput("new_conf_date", "Дата проведения:"),
                         textInput("new_conf_location", "Место проведения:"),
                         numericInput("new_conf_max_participants", "Макс. участников:", value = 100, min = 1),
                         actionButton("add_conference_btn", "Добавить конференцию", class = "btn-success")
                       ),
                       
                       br(),
                       
                       # Список конференций
                       h4("Существующие конференции"),
                       DTOutput("conferences_table")
                     )
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
                     wellPanel(
                       h4("📝 Подать заявку на участие"),
                       
                       uiOutput("conference_selector"),
                       
                       radioButtons("participation_type", "Тип участия:",
                                    choices = c("👂 Слушатель" = "listener", 
                                                "🎤 Докладчик" = "speaker"),
                                    selected = "listener"),
                       
                       uiOutput("speaker_fields"),
                       
                       actionButton("submit_application_btn", "📤 Подать заявку", 
                                    class = "btn-success")
                     ),
                     
                     br(),

                     h4("📋 Подробный список конференций"),
                     uiOutput("conferences_list")
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
    )
  })
  
  # ОБНОВЛЕНИЕ ВЫБОРА КОНФЕРЕНЦИЙ
  observe({
    conferences <- conferences_data()
    
    if (nrow(conferences) > 0) {
      choices <- setNames(conferences$conference_id, conferences$title)
      cat("Choices:", paste(names(choices), collapse = ", "), "\n")
      
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
        fileInput("qualification_file", "Файл с подтверждением квалификации*:",
                  accept = c(".pdf", ".doc", ".docx", ".ppt", ".pptx"))
      )
    }
  })
  
  # ОБРАБОТЧИКИ СОБЫТИЙ
  output$download_qualification_file <- downloadHandler(
    filename = function() {
      # Получаем имя файла из выбранной заявки
      selected_rows <- input$pending_applications_table_rows_selected
      if (length(selected_rows) > 0) {
        applications <- load_pending_applications()
        file_name <- applications$file_name[selected_rows[1]]
        return(file_name)
      }
      return("qualification_file")
    },
    content = function(file) {
      selected_rows <- input$pending_applications_table_rows_selected
      if (length(selected_rows) > 0) {
        applications <- load_pending_applications()
        file_path <- applications$file_path[selected_rows[1]]
        
        # Копируем файл для скачивания
        if (file.exists(file_path)) {
          file.copy(file_path, file)
        }
      }
    }
  )
  # В server.R добавь:
  observeEvent(input$download_file, {
    application_id <- input$download_file
    # Запускаем скачивание
    session$sendCustomMessage("downloadFile", application_id)
  })
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
      # Для СЛУШАТЕЛЯ - автоматическое одобрение если есть места
      if (input$participation_type == "listener") {
        # Проверяем есть ли свободные места используя то же соединение
        conference <- dbGetQuery(conn,
                                 "SELECT max_participants FROM conferences WHERE conference_id = ?",
                                 params = list(input$selected_conference)
        )
        
        approved_count <- dbGetQuery(conn,
                                     "SELECT COUNT(*) as count FROM applications 
                                 WHERE conference_id = ? AND status = 'approved' AND participation_type = 'listener'",
                                     params = list(input$selected_conference)
        )
        
        has_free_places <- approved_count$count < conference$max_participants
        
        if (has_free_places) {
          # Есть места - автоматически одобряем
          dbExecute(conn,
                    "INSERT INTO applications (user_id, conference_id, participation_type, status) 
                 VALUES (?, ?, ?, 'approved')",
                    params = list(user$user_id, input$selected_conference, "listener")
          )
          showNotification("✅ Заявка одобрена! Свободные места есть.", type = "message")
        } else {
          # Мест нет - отклоняем
          dbExecute(conn,
                    "INSERT INTO applications (user_id, conference_id, participation_type, status) 
                 VALUES (?, ?, ?, 'rejected')",
                    params = list(user$user_id, input$selected_conference, "listener")
          )
          showNotification("❌ Заявка отклонена. Все места заняты.", type = "error")
        }
        
      } else {
        # Для ДОКЛАДЧИКА - всегда на рассмотрение
        dbExecute(conn,
                  "INSERT INTO applications (user_id, conference_id, participation_type, topic, status) 
               VALUES (?, ?, ?, ?, 'pending')",
                  params = list(user$user_id, input$selected_conference, "speaker", 
                                ifelse(is.null(input$presentation_topic) || input$presentation_topic == "", 
                                       "Тема не указана", input$presentation_topic))
        )
        new_application_id <- dbGetQuery(conn, "SELECT last_insert_rowid() AS id")$id
        
        if (!is.null(input$qualification_file)) {
          file_info <- input$qualification_file
          # Создаем папку uploads если ее нет
          if (!dir.exists("uploads")) dir.create("uploads")
          new_file_path <- file.path("uploads", paste0("qualification_", new_application_id, "_", file_info$name))
          file.copy(file_info$datapath, new_file_path)
          
          # Сохраняем в новую таблицу
          dbExecute(conn, "
          INSERT INTO application_files (application_id, file_name, file_path)
          VALUES (?, ?, ?)
        ", params = list(new_application_id, file_info$name, new_file_path))
        }
        showNotification("📝 Заявка отправлена на рассмотрение администратору", type = "message")
      }
      
      dbDisconnect(conn)
      
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
    req(input$new_conf_title, input$new_conf_description, input$new_conf_date, 
        input$new_conf_location, input$new_conf_max_participants)
    
    # Проверяем, что все поля заполнены
    if (input$new_conf_title == "" || input$new_conf_description == "" || 
        input$new_conf_location == "") {
      showNotification("❌ Заполните все поля!", type = "error")
      return()
    }
    
    # Добавляем конференцию в БД
    success <- add_new_conference(
      input$new_conf_title,
      input$new_conf_description,
      input$new_conf_date,
      input$new_conf_location,
      input$new_conf_max_participants
    )
    
    if (success) {
      showNotification("✅ Конференция успешно добавлена!", type = "message")
      
      # Очищаем форму
      updateTextInput(session, "new_conf_title", value = "")
      updateTextAreaInput(session, "new_conf_description", value = "")
      updateTextInput(session, "new_conf_location", value = "")
      updateNumericInput(session, "new_conf_max_participants", value = 100)
      
      # Обновляем данные конференций
      conferences_data(load_all_conferences())
      
    } else {
      showNotification("❌ Ошибка при добавлении конференции", type = "error")
    }
  })
  
  # Вывод таблицы конференций
  output$conferences_table <- renderDT({
    conferences <- conferences_data()
    if (nrow(conferences) > 0) {
      # Исправляем формат дат
      conferences$date <- sapply(conferences$date, function(d) {
        if (is.numeric(d)) {
          format(as.Date(as.numeric(d), origin = "1899-12-30"), "%Y-%m-%d")
        } else {
          as.character(d)
        }
      })
      
      display_data <- conferences[, c("conference_id", "title", "description", "date", "location", "max_participants")]
      datatable(display_data, 
                options = list(pageLength = 10),
                colnames = c("ID", "Название", "Описание", "Дата", "Место", "Макс. участников"),
                rownames = FALSE)
    } else {
      datatable(data.frame(Сообщение = "Нет активных конференций"))
    }
  })
  
  # Обработчик кнопки "Одобрить выбранные"
  observeEvent(input$approve_selected_btn, {
    selected_rows <- input$pending_applications_table_rows_selected
    application_ids <- get_selected_application_ids(selected_rows)
    
    if (length(application_ids) > 0) {
      # Одобряем каждую выбранную заявку
      for (app_id in application_ids) {
        approve_application(app_id)
      }
      
      showNotification(paste("✅ Одобрено заявок:", length(application_ids)), type = "message")
      
      # ОБНОВЛЯЕМ ДАННЫЕ ПОЛЬЗОВАТЕЛЕЙ АВТОМАТИЧЕСКИ
      # Обновляем таблицу заявок на рассмотрение
      # таблицу заявок для админа
      output$pending_applications_table <- renderDT({
        applications <- load_pending_applications()
        if (nrow(applications) > 0) {
          datatable(
            applications[, c("username", "full_name", "conference_title", "participation_type", "topic")],
            options = list(
              pageLength = 10,
              selection = 'multiple'
            ),
            rownames = FALSE,
            colnames = c('Логин', 'ФИО', 'Конференция', 'Тип участия', 'Тема доклада')
          )
        } else {
          datatable(data.frame(Сообщение = "Нет заявок на рассмотрении"))
        }
      })
      
      # Обновляем заявки пользователей (если они залогинены)
      if (user$logged_in && user$role == "user") {
        user_applications(load_user_applications(user$user_id))
      }
      
    } else {
      showNotification("❌ Выберите заявки для одобрения", type = "error")
    }
  })
  
  # Обработчик кнопки "Отклонить выбранные"
  observeEvent(input$reject_selected_btn, {
    selected_rows <- input$pending_applications_table_rows_selected
    application_ids <- get_selected_application_ids(selected_rows)
    
    if (length(application_ids) > 0) {
      # Отклоняем каждую выбранную заявку
      for (app_id in application_ids) {
        reject_application(app_id)
      }
      
      showNotification(paste("❌ Отклонено заявок:", length(application_ids)), type = "message")
      
      # Обновляем таблицу заявок на рассмотрение
      output$pending_applications_table <- renderDT({
        applications <- load_pending_applications()
        if (nrow(applications) > 0) {
          datatable(applications, 
                    options = list(
                      pageLength = 10,
                      selection = 'multiple'
                    ),
                    rownames = FALSE)
        } else {
          datatable(data.frame(Сообщение = "Нет заявок на рассмотрении"))
        }
      })
      
      # Обновляем заявки пользователей (если они залогинены)
      if (user$logged_in && user$role == "user") {
        user_applications(load_user_applications(user$user_id))
      }
      
    } else {
      showNotification("❌ Выберите заявки для отклонения", type = "error")
    }
  })

  observeEvent(input$admin_tabs, {
    if (input$admin_tabs == "📋 Заявки на рассмотрение" && user$role == "admin") {
      # Принудительно обновляем таблицу при открытии вкладки
      output$pending_applications_table <- renderDT({
        applications <- load_pending_applications()
        if (nrow(applications) > 0) {
          datatable(applications, 
                    options = list(
                      pageLength = 10,
                      selection = 'multiple'
                    ),
                    rownames = FALSE)
        } else {
          datatable(data.frame(Сообщение = "Нет заявок на рассмотрении"))
        }
      })
    }
  })
  
  # Обработчик для новой кнопки выхода
  observeEvent(input$logout_btn, {
    user$logged_in <- FALSE
    user$username <- ""
    user$role <- ""
    user$user_id <- NULL
    show_users_table(FALSE)
    show_applications_table(FALSE)
    showNotification("👋 Вы вышли из системы", type = "message")
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
  # В server.R добавь:
  output$download_report <- downloadHandler(
    filename = function() {
      paste("отчет-конференции-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Создаем временный PDF файл
      pdf(file, paper = "a4")
      
      # Заголовок отчета
      grid::grid.text(paste("Отчет по заявкам на конференции\n", Sys.Date()), 
                      gp = grid::gpar(fontsize = 16, fontface = "bold"))
      grid::grid.newpage()
      
      # График 1: Статусы заявок
      print(ggplot(...) + ggtitle("Статусы заявок"))  # твой график статусов
      
      grid::grid.newpage()
      
      # График 2: Типы участников  
      print(ggplot(...) + ggtitle("Типы участников"))  # твой график типов
      
      grid::grid.newpage()
      
      # Таблица сводной статистики
      grid::grid.table(summary_table)
      
      dev.off()
    }
  )

  # таблицу заявок для админа
  output$pending_applications_table <- renderDT({
    applications <- load_pending_applications()
    if (nrow(applications) > 0) {
      # Создаем русские названия колонок
      display_data <- data.frame(
        'Логин' = applications$username,
        'ФИО' = applications$full_name,
        'Конференция' = applications$conference_title,
        'Тип участия' = ifelse(applications$participation_type == "speaker", "🎤 Докладчик", "👂 Слушатель"),
        'Тема доклада' = applications$topic
      )
      
      datatable(
        display_data,
        options = list(
          pageLength = 10,
          selection = 'multiple'
        ),
        rownames = FALSE
      )
    } else {
      datatable(data.frame(Сообщение = "Нет заявок на рассмотрении"))
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
          # Проверяем свободные места для каждой конференции
          limit_info <- check_conference_limit(conferences$conference_id[i])
          free_places <- limit_info$max_participants - limit_info$current_approved
          
          wellPanel(
            h4(conferences$title[i]),
            p(strong("Описание:"), conferences$description[i]),
            p(strong("Дата:"), conferences$date[i]),
            p(strong("Место:"), conferences$location[i]),
            p(strong("Мест свободно:"), free_places, "из", limit_info$max_participants),
            if (free_places == 0) {
              p(strong("⚠️ Все места заняты"), style = "color: red;")
            }
          )
        })
      )
    } else {
      p("Нет активных конференций")
    }
  })
  # Создаем папку для отчетов если ее нет
  reports_dir <- "reports/generated"
  if (!dir.exists(reports_dir)) {
    dir.create(reports_dir, recursive = TRUE)
  }
  # ОТЧЕТ!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # Обработчик кнопки скачивания PDF отчета - ФИНАЛЬНАЯ ВЕРСИЯ
  observeEvent(input$download_report_btn, {
    showNotification("📊 Генерируем PDF отчет...", type = "message")
    
    file_name <- paste0("report-conference-", format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".pdf")
    file_path <- file.path(reports_dir, file_name)
    
    tryCatch({
      # Создаем PDF
      pdf(file_path, paper = "a4", width = 8.3, height = 11.7)
      
      # Устанавливаем шрифты
      par(family = "Helvetica", mar = c(3, 3, 3, 2))
      
      # Получаем все данные
      conn <- get_db_connection()
      
      total_apps <- dbGetQuery(conn, "SELECT COUNT(*) as count FROM applications")$count
      total_users <- dbGetQuery(conn, "SELECT COUNT(*) as count FROM users")$count
      total_conferences <- dbGetQuery(conn, "SELECT COUNT(*) as count FROM conferences WHERE status = 'active'")$count
      
      status_data <- dbGetQuery(conn, "
      SELECT status, COUNT(*) as count 
      FROM applications 
      GROUP BY status
    ")
      
      type_data <- dbGetQuery(conn, "
      SELECT participation_type, COUNT(*) as count 
      FROM applications 
      GROUP BY participation_type
    ")
      
      conf_stats <- dbGetQuery(conn, "
      SELECT 
        c.title,
        c.date,
        COUNT(a.application_id) as total_apps,
        COUNT(CASE WHEN a.status = 'approved' THEN 1 END) as approved,
        COUNT(CASE WHEN a.status = 'pending' THEN 1 END) as pending,
        COUNT(CASE WHEN a.status = 'rejected' THEN 1 END) as rejected
      FROM conferences c
      LEFT JOIN applications a ON c.conference_id = a.conference_id
      WHERE c.status = 'active'
      GROUP BY c.conference_id, c.title, c.date
      ORDER BY c.date DESC
    ")
      
      dbDisconnect(conn)
      
      # СТРАНИЦА 1: ОСНОВНАЯ СТАТИСТИКА (без рамки)
      plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE, 
           xlim = c(0, 10), ylim = c(0, 10))
      
      # Заголовок
      text(5, 9, "CONFERENCE APPLICATION SYSTEM", cex = 1.3, font = 2)
      text(5, 8.5, "STATISTICS REPORT", cex = 1.1)
      text(5, 8, format(Sys.Date(), "%Y-%m-%d"), cex = 0.9)
      
      # Основная статистика (простой текст)
      text(5, 6.5, "SUMMARY", font = 2, cex = 1.0)
      text(3, 6, paste("Total applications:", total_apps), adj = 0, cex = 0.9)
      text(3, 5.5, paste("Total users:", total_users), adj = 0, cex = 0.9)
      text(3, 5, paste("Active conferences:", total_conferences), adj = 0, cex = 0.9)
      
      # СТРАНИЦА 2: Application Status с легендой и процентами
      plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE, 
           xlim = c(0, 10), ylim = c(0, 10))
      text(5, 9.5, "APPLICATION STATUS", font = 2, cex = 1.2)
      
      if (nrow(status_data) > 0) {
        par(new = TRUE, mar = c(1, 1, 3, 1))
        
        # Считаем проценты
        total <- sum(status_data$count)
        status_data$percent <- round(status_data$count / total * 100, 1)
        
        status_labels <- c("Pending", "Approved", "Rejected")
        colors <- c("#ffc107", "#28a745", "#dc3545")
        
        # Круговая диаграмма с процентами на секторах
        pie(status_data$count, 
            labels = paste0(status_data$percent, "%"),
            col = colors,
            main = "",
            radius = 0.8,
            cex = 1.0)
        legend(0.5, -0.2, 
               legend = paste0(c("Pending", "Approved", "Rejected"), ": ", status_data$count),
               fill = colors,
               cex = 0.9,
               bty = "n",
               xjust = 0.5,
               horiz = TRUE)
      }
      
      # СТРАНИЦА 3: Participant Types
      plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE, 
           xlim = c(0, 10), ylim = c(0, 10))
      text(5, 9.5, "PARTICIPANT TYPES", font = 2, cex = 1.2)
      
      if (nrow(type_data) > 0) {
        par(new = TRUE, mar = c(5, 4, 4, 2))
        
        type_labels <- ifelse(type_data$participation_type == "speaker", "Speakers", "Listeners")
        
        # Столбчатая диаграмма
        bp <- barplot(type_data$count,
                      names.arg = type_labels,
                      col = c("#17a2b8", "#6f42c1"),
                      main = "",
                      ylab = "Count",
                      border = NA,
                      ylim = c(0, max(type_data$count) * 1.2),
                      cex.names = 0.9,
                      cex.axis = 0.8)
        
        # Цифры на столбцах
        text(bp, type_data$count + max(type_data$count) * 0.05, 
             type_data$count, 
             cex = 1.0, 
             font = 2)
      }
      
      # СТРАНИЦА 4: Conference Statistics с английскими названиями
      plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE, 
           xlim = c(0, 10), ylim = c(0, 12))
      text(5, 11.5, "CONFERENCE STATISTICS", font = 2, cex = 1.2)
      
      if (nrow(conf_stats) > 0) {
        # Заменяем русские названия на английские
        conf_stats$display_title <- sapply(conf_stats$title, function(title) {
          # Простые замены для часто встречающихся русских слов
          eng_title <- title
          eng_title <- gsub("конференц", "Conference", eng_title, ignore.case = TRUE)
          eng_title <- gsub("семинар", "Seminar", eng_title, ignore.case = TRUE)
          eng_title <- gsub("форум", "Forum", eng_title, ignore.case = TRUE)
          eng_title <- gsub("симпозиум", "Symposium", eng_title, ignore.case = TRUE)
          eng_title <- gsub("съезд", "Congress", eng_title, ignore.case = TRUE)
          eng_title <- gsub("встреча", "Meeting", eng_title, ignore.case = TRUE)
          eng_title <- gsub("совещание", "Conference", eng_title, ignore.case = TRUE)
          
          # Если остались русские буквы, заменяем на "Conference X"
          if (grepl("[А-Яа-я]", eng_title)) {
            return(paste("Conference", sample(1000:9999, 1)))
          }
          return(eng_title)
        })
        
        # Заголовки таблицы
        y_pos <- 10.5
        text(0.5, y_pos, "Conference", font = 2, adj = 0, cex = 0.7)
        text(5.5, y_pos, "Date", font = 2, cex = 0.7)
        text(7, y_pos, "Total", font = 2, cex = 0.7)
        text(7.8, y_pos, "Appr", font = 2, cex = 0.7)
        text(8.6, y_pos, "Pend", font = 2, cex = 0.7)
        text(9.4, y_pos, "Rej", font = 2, cex = 0.7)
        
        # Разделительная линия
        segments(0.3, 10.3, 9.7, 10.3)
        
        # Данные таблицы
        for(i in 1:min(18, nrow(conf_stats))) {
          y_pos <- y_pos - 0.35
          
          # Обрезаем длинные названия
          conf_name <- ifelse(nchar(conf_stats$display_title[i]) > 30, 
                              paste0(substr(conf_stats$display_title[i], 1, 27), "..."), 
                              conf_stats$display_title[i])
          
          # Форматируем дату (только год-месяц-день)
          conf_date <- ifelse(!is.na(conf_stats$date[i]), 
                              as.character(conf_stats$date[i]), 
                              "N/A")
          
          text(0.5, y_pos, conf_name, adj = 0, cex = 0.6)
          text(5.5, y_pos, conf_date, cex = 0.6)
          text(7, y_pos, conf_stats$total_apps[i], cex = 0.6)
          text(7.8, y_pos, conf_stats$approved[i], cex = 0.6)
          text(8.6, y_pos, conf_stats$pending[i], cex = 0.6)
          text(9.4, y_pos, conf_stats$rejected[i], cex = 0.6)
        }
        
        if (nrow(conf_stats) > 18) {
          text(5, y_pos - 0.6, paste("... and", nrow(conf_stats) - 18, "more conferences"), 
               cex = 0.6, font = 3)
        }
      }
      
      dev.off()
      
      showNotification(paste("✅ PDF report saved:", file_name), type = "message")
      
      # Настраиваем скачивание
      output$download_report <- downloadHandler(
        filename = function() {
          file_name
        },
        content = function(file) {
          file.copy(file_path, file)
        }
      )
      
      session$sendCustomMessage('downloadFile', 'download_report')
      
    }, error = function(e) {
      showNotification(paste("❌ Error creating report:", e$message), type = "error")
    })
  })
  # Кнопка скачивания файла для админа
  output$file_download_ui <- renderUI({
    selected_rows <- input$pending_applications_table_rows_selected
    if (length(selected_rows) > 0) {
      applications <- load_pending_applications()
      
      # Берем первую выбранную заявку
      app <- applications[selected_rows[1], ]
      
      if (!is.na(app$file_name) && app$file_name != "") {
        div(
          style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #eee;",
          h5("📎 Файл квалификации:"),
          downloadButton("download_qualification_file", 
                         paste("Скачать", app$file_name),
                         class = "btn-info"),
          br()
        )
      } else {
        div(
          style = "margin-top: 15px; padding-top: 15px; border-top: 1px solid #eee;",
          p("❌ В выбранной заявке нет файла", style = "color: gray;")
        )
      }
    }
  })
  # ГРАФИКИ
#График покажет количество заявок по дням
  output$applications_trend_plot <- renderPlot({
    conn <- get_db_connection()
    
    trend_data <- dbGetQuery(conn, "
    SELECT 
      DATE(created_at) as date,
      COUNT(*) as daily_count
    FROM applications 
    WHERE created_at IS NOT NULL
    GROUP BY DATE(created_at)
    ORDER BY date
  ")
    
    dbDisconnect(conn)
    
    if (nrow(trend_data) > 0 && sum(trend_data$daily_count) > 0) {
      trend_data$date <- as.Date(trend_data$date)

      plot(trend_data$date, trend_data$daily_count, 
           type = "o",  # линия с точками
           lwd = 2,
           col = "#007bff",
           pch = 19,
           ylim = c(0, 100),
           xlab = "Дата",
           ylab = "Количество заявок")
      
      grid()
      text(trend_data$date, trend_data$daily_count,
           labels = trend_data$daily_count,
           pos = 3, cex = 0.9, col = "darkblue")
      
    } else {
      plot(0, 0, type = "n", main = "Динамика заявок по дням")
      text(0, 0, "Нет данных")
    }
  })
  # Круговая диаграмма для пользователя (главная страница)
  output$applications_pie <- renderPlot({
    apps <- user_applications()
    if (nrow(apps) > 0) {
      status_counts <- table(apps$status)
      pie(status_counts,
          labels = paste(c("На рассмотрении", "Одобрено", "Отклонено"), "\n", status_counts),
          col = c("#ffc107", "#28a745", "#dc3545"),
          main = "Статус ваших заявок")
    } else {
      plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE)
      text(0, 0, "Нет данных о заявках", cex = 1.5)
    }
  })
#Статусы заявок  
  output$applications_status_plot <- renderPlot({
    conn <- get_db_connection()
    
    status_data <- dbGetQuery(conn, "
    SELECT status, COUNT(*) as count 
    FROM applications 
    GROUP BY status
  ")
    
    dbDisconnect(conn)
    
    if (nrow(status_data) > 0) {
      status_data$status_ru <- factor(status_data$status,
                                      levels = c("pending", "approved", "rejected"),
                                      labels = c("На рассмотрении", "Одобрено", "Отклонено"))
      total <- sum(status_data$count)
      status_data$percent <- round(status_data$count / total * 100, 1)
      
      colors <- c("На рассмотрении" = "#ffc107", "Одобрено" = "#28a745", "Отклонено" = "#dc3545")
      
      ggplot(status_data, aes(x = "", y = count, fill = status_ru)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        # Показываем проценты только если сегмент больше 5%
        geom_text(aes(label = ifelse(percent > 5, paste0(percent, "%"), "")), 
                  position = position_stack(vjust = 0.5),
                  size = 6, 
                  color = "white",
                  fontface = "bold") +
        scale_fill_manual(values = colors) +
        labs(fill = "Статус") +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5, size = 16),
              legend.position = "bottom",
              legend.text = element_text(size = 12))
    } else {
      ggplot() +
        annotate("text", x = 1, y = 1, label = "Нет данных о заявках", size = 6) +
        theme_void()
    }
  })
 # Типы участников 
  output$participation_type_plot <- renderPlot({
    conn <- get_db_connection()
    
    type_data <- dbGetQuery(conn, "
    SELECT participation_type, COUNT(*) as count 
    FROM applications 
    GROUP BY participation_type
  ")
    
    dbDisconnect(conn)
    
    if (nrow(type_data) > 0) {
      type_data$type_ru <- ifelse(type_data$participation_type == "speaker", 
                                  "Докладчики", "Слушатели")
    
      barplot(type_data$count,
              names.arg = type_data$type_ru,
              col = c("#17a2b8", "#6f42c1"),
              border = NA,
              ylab = "Количество заявок",
              ylim = c(0, max(type_data$count) * 1.2))
      
      # Добавляем числа на столбцы
      text(1:2, type_data$count, 
           labels = paste0(type_data$count),
           pos = 3, cex = 1.2, font = 2)
      
    } else {
      plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE,
           main = "Типы участников")
      text(0, 0, "Нет данных", cex = 1.2)
    }
  })
  #Диаграмма рассеивания
  output$conferences_popularity_plot <- renderPlot({
    conn <- get_db_connection()
    
    conf_data <- dbGetQuery(conn, "
    SELECT 
      c.conference_id,
      c.title,
      c.date,
      c.max_participants,
      COUNT(a.application_id) as application_count,
      COUNT(CASE WHEN a.status = 'approved' THEN 1 END) as approved_count
    FROM conferences c
    LEFT JOIN applications a ON c.conference_id = a.conference_id
    GROUP BY c.conference_id, c.title, c.date, c.max_participants
  ")
    
    dbDisconnect(conn)
    
    if (nrow(conf_data) > 0) {
      # Преобразуем даты
      conf_data$date <- as.Date(conf_data$date)
      
      # Считаем процент заполнения
      conf_data$fill_percentage <- ifelse(conf_data$max_participants > 0,
                                          conf_data$approved_count / conf_data$max_participants * 100,
                                          0)
      
      # Цвета по проценту заполнения
      conf_data$color <- ifelse(conf_data$fill_percentage > 80, "#dc3545",
                                ifelse(conf_data$fill_percentage > 50, "#ffc107", 
                                       "#28a745"))
      
      # Автоматическое определение границ дат
      date_range <- range(conf_data$date)
      date_span <- as.numeric(difftime(date_range[2], date_range[1], units = "days"))
      
      # Определяем интервал для меток в зависимости от диапазона дат
      if (date_span > 365) {
        # Больше года - показываем каждый квартал
        date_breaks <- seq(from = as.Date(paste0(format(date_range[1], "%Y"), "-01-01")),
                           to = as.Date(paste0(format(date_range[2], "%Y"), "-12-31")),
                           by = "3 months")
        date_labels <- "%b\n%Y"
      } else if (date_span > 180) {
        # Полгода-год - показываем каждый месяц
        date_breaks <- seq(from = as.Date(paste0(format(date_range[1], "%Y-%m"), "-01")),
                           to = as.Date(paste0(format(date_range[2], "%Y-%m"), "-01")),
                           by = "month")
        date_labels <- "%b\n%Y"
      } else if (date_span > 90) {
        # 3-6 месяцев - показываем каждые 2 недели
        date_breaks <- seq(from = date_range[1], to = date_range[2], by = "2 weeks")
        date_labels <- "%d %b"
      } else {
        # Меньше 3 месяцев - показываем каждую неделю
        date_breaks <- seq(from = date_range[1], to = date_range[2], by = "week")
        date_labels <- "%d %b"
      }
      
      # Увеличиваем отступы для легенды
      par(mar = c(5, 4, 4, 10), xpd = TRUE)
      
      # Основной график с автоматическими пределами
      plot(conf_data$date, 
           conf_data$application_count,
           pch = 19,
           cex = conf_data$max_participants / max(conf_data$max_participants, na.rm = TRUE) * 3 + 1,
           col = conf_data$color,
           xlab = "Дата конференции",
           ylab = "Количество заявок",
           xaxt = "n",
           xlim = date_range + c(-10, 10))  # Добавляем небольшие отступы по краям
      
      # Добавляем метки дат
      axis(1, at = date_breaks, labels = format(date_breaks, date_labels), cex.axis = 0.8)
      
      grid()
      
      # Легенда для заполняемости (справа за пределами графика)
      legend_x <- par("usr")[2] + 0.05 * (par("usr")[2] - par("usr")[1])
      legend(legend_x,
             par("usr")[4],
             legend = c(">80% заполнена", "50-80% заполнена", "<50% заполнена"),
             pch = 19,
             col = c("#dc3545", "#ffc107", "#28a745"),
             title = "Заполняемость",
             bty = "n",
             cex = 0.8)
      
      # Легенда для размеров
      legend_sizes <- c(50, 100, 200)
      legend(legend_x,
             par("usr")[4] - 0.4 * (par("usr")[4] - par("usr")[3]),
             legend = paste("Лимит:", legend_sizes),
             pch = 19,
             pt.cex = legend_sizes / max(conf_data$max_participants, na.rm = TRUE) * 3 + 1,
             col = "gray",
             title = "Размер точки",
             bty = "n",
             cex = 1.1)
      
    } else {
      plot(0, 0, type = "n", xlab = "", ylab = "", axes = FALSE, 
           main = "Диаграмма рассеивания конференций")
      text(0, 0, "Нет данных для построения графика", cex = 1.2)
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