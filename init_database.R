# init_database.R - Standalone database setup script
# Run: Rscript init_database.R

message("Setting up Conference Management System database...")

# 1. Install missing packages
required_packages <- c("DBI", "RSQLite", "dplyr", "sodium")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages, repos = "https://cloud.r-project.org")
  message("Installed packages: ", paste(new_packages, collapse = ", "))
}

# 2. Load libraries
library(DBI)
library(RSQLite)
library(dplyr)
library(sodium)

# 3. Remove old database if exists
if (file.exists("data/database.sqlite")) {
  file.remove("data/database.sqlite")
  message("Old database removed.")
}

# Create data directory if doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}

# 4. Create database connection
conn <- dbConnect(RSQLite::SQLite(), "data/database.sqlite")

# 5. Create tables (копия вашего кода, но БЕЗ зависимостей)
tryCatch({
  # Таблица пользователей
  dbExecute(conn, "
    CREATE TABLE users (
      user_id INTEGER PRIMARY KEY AUTOINCREMENT,
      username TEXT UNIQUE NOT NULL,
      password_hash TEXT NOT NULL,
      email TEXT,
      full_name TEXT,
      institution TEXT,
      role TEXT NOT NULL CHECK(role IN ('admin', 'user')),
      created_at DATETIME DEFAULT CURRENT_TIMESTAMP
    )
  ")
  
  # Таблица конференций
  dbExecute(conn, "
    CREATE TABLE conferences (
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
    CREATE TABLE applications (
      application_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER NOT NULL,
      conference_id INTEGER NOT NULL,
      participation_type TEXT NOT NULL CHECK(participation_type IN ('speaker', 'listener')),
      topic TEXT,
      qualification_file TEXT,
      status TEXT NOT NULL DEFAULT 'pending' CHECK(status IN ('pending', 'approved', 'rejected')),
      FOREIGN KEY(user_id) REFERENCES users(user_id),
      FOREIGN KEY(conference_id) REFERENCES conferences(conference_id)
    )
  ")
  
  # Таблица для файлов
  dbExecute(conn, "
    CREATE TABLE application_files (
      file_id INTEGER PRIMARY KEY AUTOINCREMENT,
      application_id INTEGER NOT NULL,
      file_name TEXT NOT NULL,
      file_path TEXT NOT NULL,
      uploaded_at DATETIME DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY(application_id) REFERENCES applications(application_id) ON DELETE CASCADE
    )
  ")
  
  # 6. Insert test data
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
  
  message("\n✅ Database 'data/database.sqlite' created successfully!")
  message("Test accounts:")
  message("  Admin:    admin / admin")
  message("  User:     user1 / user123")
  
}, error = function(e) {
  message("❌ Error creating database: ", e$message)
}, finally = {
  dbDisconnect(conn)
})
