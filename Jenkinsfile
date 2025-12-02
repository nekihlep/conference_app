pipeline {
    agent any
    
    stages {
        stage('Тесты R') {
            steps {
                bat '''
                "D:\\PROGRA~1\\R\\R-45~1.1\\bin\\x64\\Rscript.exe" -e "
                # Устанавливаем пакеты (тихо)
                install.packages(c('testthat','DBI','RSQLite','mockery','sodium'), 
                                 repos='https://cloud.r-project.org', quiet=TRUE)
                
                # Загружаем функции
                source('R/db_functions.R')
                source('R/auth.R') 
                source('R/logic.R')
                
                # Запускаем ВСЕ тесты
                testthat::test_dir('tests/testthat')
                "
                '''
            }
        }
    }
    
    post {
        success {
            echo '✅ Все тесты прошли успешно!'
        }
        failure {
            echo '❌ Обнаружены ошибки в тестах'
        }
    }
}
