# Функция проверки лимита конференции
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

# Функция загрузки заявок пользователя
load_user_applications <- function(user_id) {
  conn <- get_db_connection()
  applications <- dbGetQuery(conn, 
                             "SELECT a.*, c.title as conference_title 
   FROM applications a 
   JOIN conferences c ON a.conference_id = c.conference_id 
   WHERE a.user_id = ?",
                             params = list(user_id))
  dbDisconnect(conn)
  return(applications)
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
   ORDER BY a.applied_at DESC")
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

# Вспомогательные функции для проверки данных
validate_application_data <- function(conference_id, participation_type, user_id) {
  # Проверка на существующую заявку
  conn <- get_db_connection()
  existing <- dbGetQuery(conn,
                         "SELECT * FROM applications WHERE user_id = ? AND conference_id = ?",
                         params = list(user_id, conference_id))
  dbDisconnect(conn)
  
  return(nrow(existing) == 0)
}