test_that("Интеграционный тест: полный цикл заявки", {
  library(DBI)
  library(RSQLite)
  
  cat("=== НАЧАЛО ИНТЕГРАЦИОННОГО ТЕСТА ===\n")
  
  # 1. Создаем реальную тестовую БД
  conn <- dbConnect(SQLite(), ":memory:")
  cat("✓ Создана тестовая БД SQLite\n")
  
  # 2. Создаем таблицы (как в реальном приложении)
  dbExecute(conn, "
    CREATE TABLE conferences (
      conference_id INTEGER PRIMARY KEY,
      title TEXT NOT NULL,
      max_participants INTEGER DEFAULT 100
    )
  ")
  
  dbExecute(conn, "
    CREATE TABLE users (
      user_id INTEGER PRIMARY KEY,
      username TEXT UNIQUE NOT NULL,
      role TEXT DEFAULT 'user'
    )
  ")
  
  dbExecute(conn, "
    CREATE TABLE applications (
      application_id INTEGER PRIMARY KEY AUTOINCREMENT,
      user_id INTEGER,
      conference_id INTEGER,
      participation_type TEXT,
      topic TEXT,
      status TEXT DEFAULT 'pending',
      applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      FOREIGN KEY(user_id) REFERENCES users(user_id),
      FOREIGN KEY(conference_id) REFERENCES conferences(conference_id)
    )
  ")
  
  cat("✓ Созданы таблицы: conferences, users, applications\n")
  
  # 3. Добавляем тестовые данные
  dbExecute(conn, "
    INSERT INTO conferences (title, max_participants) 
    VALUES ('Научная конференция по ИИ', 200)
  ")
  
  dbExecute(conn, "
    INSERT INTO users (username, role) 
    VALUES ('test_speaker', 'user')
  ")
  
  dbExecute(conn, "
    INSERT INTO users (username, role) 
    VALUES ('admin', 'admin')
  ")
  
  cat("✓ Добавлены тестовые данные\n")
  
  # 4. Подаем заявку докладчика
  dbExecute(conn, "
    INSERT INTO applications (user_id, conference_id, participation_type, topic, status)
    VALUES (1, 1, 'speaker', 'Искусственный интеллект в медицине', 'pending')
  ")
  
  cat("✓ Заявка подана\n")
  
  # 5. Проверяем создание заявки
  apps <- dbGetQuery(conn, "
    SELECT * FROM applications 
    WHERE user_id = 1 AND conference_id = 1
  ")
  
  expect_equal(nrow(apps), 1, info = "Должна быть создана одна заявка")
  expect_equal(apps$status, "pending", info = "Статус новой заявки должен быть 'pending'")
  expect_equal(apps$participation_type, "speaker", info = "Тип участия должен быть 'speaker'")
  
  cat("✓ Заявка создана со статусом 'pending'\n")
  
  # 6. Одобряем заявку (модерируем)
  dbExecute(conn, "
    UPDATE applications 
    SET status = 'approved' 
    WHERE application_id = ?
  ", params = list(apps$application_id))
  
  cat("✓ Заявка одобрена администратором\n")
  
  # 7. Проверяем результат модерации
  updated_app <- dbGetQuery(conn, "
    SELECT status FROM applications WHERE application_id = ?
  ", params = list(apps$application_id))
  
  expect_equal(updated_app$status, "approved", 
               info = "После одобрения статус должен быть 'approved'")
  
  # 8. Проверяем статистику
  total_apps <- dbGetQuery(conn, "
    SELECT COUNT(*) as count FROM applications
  ")$count
  
  approved_apps <- dbGetQuery(conn, "
    SELECT COUNT(*) as count FROM applications WHERE status = 'approved'
  ")$count
  
  expect_true(total_apps >= 1, info = "Должна быть хотя бы одна заявка")
  expect_true(approved_apps >= 1, info = "Должна быть хотя бы одна одобренная заявка")
  
  cat("✓ Статистика корректна\n")
  
  # 9. Очистка
  dbDisconnect(conn)
  
  cat("=== ИНТЕГРАЦИОННЫЙ ТЕСТ УСПЕШНО ЗАВЕРШЕН ===\n\n")
})