pipeline {
    agent any
    
    stages {
        stage('Запуск тестов R') {
            steps {
                echo '🚀 Запускаем тесты системы...'
                
                bat '''
                rem Устанавливаем пакеты
                "D:\\PROGRA~1\\R\\R-45~1.1\\bin\\x64\\Rscript.exe" -e "install.packages(c('testthat','DBI','RSQLite','mockery','sodium'), repos='https://cloud.r-project.org')"

                rem Запускаем тесты С ЗАГРУЗКОЙ ФУНКЦИЙ
                "D:\\PROGRA~1\\R\\R-45~1.1\\bin\\x64\\Rscript.exe" -e "
                source('R/db_functions.R')
                source('R/auth.R')
                source('R/logic.R')
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
