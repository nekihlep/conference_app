library(DBI)
library(RSQLite)
library(dplyr)

get_db_connection <- function() {
  if (!exists("db_conn", envir = .GlobalEnv) || !dbIsValid(db_conn)) {
    .GlobalEnv$db_conn <- dbConnect(RSQLite::SQLite(), "data/database.sqlite")
    
    if (!dbExistsTable(.GlobalEnv$db_conn, "users")) {
      initialize_database()
    }
  }
  return(.GlobalEnv$db_conn)
}

initialize_database <- function() {
  conn <- get_db_connection()
  
  tryCatch({
    # Таблица пользователей
    dbExecute(conn, "
      CREATE TABLE IF NOT EXISTS users (
        user_id INTEGER PRIMARY KEY AUTOINCREMENT,
        username TEXT UNIQUE NOT NULL,
        password_hash TEXT NOT NULL,
        email TEXT,
        full_name TEXT,
        institution TEXT,  -- Место работы/учёбы
        role TEXT NOT NULL CHECK(role IN ('admin', 'user')),
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP
      )
    ")
    
    # Таблица конференций
    dbExecute(conn, "
      CREATE TABLE IF NOT EXISTS conferences (
        conference_id INTEGER PRIMARY KEY AUTOINCREMENT,
        title TEXT NOT NULL,
        description TEXT,
        date TEXT,
        location TEXT,
        max_participants INTEGER,
        status TEXT DEFAULT 'active' CHECK(status IN ('active', 'completed', 'cancelled')),
        created_by INTEGER,
        created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY(created_by) REFERENCES users(user_id)
      )
    ")
    
    # Таблица заявок 
    dbExecute(conn, "
      CREATE TABLE IF NOT EXISTS applications (
        application_id INTEGER PRIMARY KEY AUTOINCREMENT,
        user_id INTEGER NOT NULL,
        conference_id INTEGER NOT NULL,
        participation_type TEXT NOT NULL CHECK(participation_type IN ('speaker', 'listener')),
        
        -- Данные для ДОКЛАДЧИКА
        topic TEXT,              -- Тема доклада
        qualification_file TEXT, -- Путь к файлу (подтверждение квалификации)
        
        -- Статус заявки (автоматически определяется по типу участия)
        status TEXT NOT NULL DEFAULT 'pending' CHECK(status IN ('pending', 'approved', 'rejected')),

        FOREIGN KEY(user_id) REFERENCES users(user_id),
        FOREIGN KEY(conference_id) REFERENCES conferences(conference_id)
      )
    ")
    
    # Таблица для файлов
    dbExecute(conn, "
      CREATE TABLE IF NOT EXISTS application_files (
        file_id INTEGER PRIMARY KEY AUTOINCREMENT,
        application_id INTEGER NOT NULL,
        file_name TEXT NOT NULL,
        file_path TEXT NOT NULL,
        uploaded_at DATETIME DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY(application_id) REFERENCES applications(application_id) ON DELETE CASCADE
      )
    ")
    
    migrate_existing_files()

    hashed_admin_password <- sodium::password_store("admin")
    dbExecute(conn, "
      INSERT OR IGNORE INTO users (username, password_hash, email, full_name, institution, role)
      VALUES ('admin', ?, 'admin@conference.ru', 'Администратор Системы', 'БГУИР', 'admin')
    ", params = list(hashed_admin_password))
    
    hashed_user_password <- sodium::password_store("user123")
    dbExecute(conn, "
      INSERT OR IGNORE INTO users (username, password_hash, email, full_name, institution, role)
      VALUES ('user1', ?, 'user1@mail.ru', 'Иванов Иван', 'БГУИР', 'user')
    ", params = list(hashed_user_password))

    dbExecute(conn, "
      INSERT OR IGNORE INTO conferences (title, description, date, location, max_participants, created_by)
      VALUES 
      ('Научная конференция по ИИ', 'Современные тенденции в искусственном интеллекте', '2025-06-15', 'Минск', 200, 1),
      ('Бизнес-форум 2025', 'Инновации в бизнесе и управлении', '2025-07-20', 'Москва', 150, 1)
    ")
    
    cat("База данных инициализирована успешно!\n")
    cat("Тестовый администратор: admin / admin\n")
    cat("Тестовый пользователь: user1 / user123\n")
    
  }, error = function(e) {
    cat("Ошибка при инициализации БД:", e$message, "\n")
  })
}
