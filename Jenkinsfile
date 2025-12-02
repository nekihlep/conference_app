pipeline {
    agent any
    
    stages {
        stage('Запуск тестов R') {
            steps {
                echo '🚀 Запускаем тесты системы...'
                
                bat '''
                rem Устанавливаем пакеты по одному
                "D:\\PROGRA~1\\R\\R-45~1.1\\bin\\x64\\Rscript.exe" -e "if(!require('testthat')) install.packages('testthat', repos='https://cloud.r-project.org')"
                "D:\\PROGRA~1\\R\\R-45~1.1\\bin\\x64\\Rscript.exe" -e "if(!require('DBI')) install.packages('DBI', repos='https://cloud.r-project.org')"
                "D:\\PROGRA~1\\R\\R-45~1.1\\bin\\x64\\Rscript.exe" -e "if(!require('RSQLite')) install.packages('RSQLite', repos='https://cloud.r-project.org')"
                "D:\\PROGRA~1\\R\\R-45~1.1\\bin\\x64\\Rscript.exe" -e "if(!require('mockery')) install.packages('mockery', repos='https://cloud.r-project.org')"
                "D:\\PROGRA~1\\R\\R-45~1.1\\bin\\x64\\Rscript.exe" -e "if(!require('sodium')) install.packages('sodium', repos='https://cloud.r-project.org')"

                rem Запускаем тесты
                "D:\\PROGRA~1\\R\\R-45~1.1\\bin\\x64\\Rscript.exe" -e "testthat::test_dir('tests/testthat')"
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
