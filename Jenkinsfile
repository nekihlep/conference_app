pipeline {
    agent any
    
    stages {
        stage('Запуск тестов R') {
            steps {
                echo '🚀 Запускаем тесты системы...'
                
                bat '''
                # Проверяем где Rscript
                where Rscript
                
                # Указываем полный путь (обычно здесь)
                "C:\\Program Files\\R\\R-4.2.0\\bin\\Rscript.exe" -e "if(!require('testthat')) install.packages('testthat', repos='https://cloud.r-project.org')"
                "C:\\Program Files\\R\\R-4.2.0\\bin\\Rscript.exe" -e "if(!require('DBI')) install.packages('DBI', repos='https://cloud.r-project.org')"
                "C:\\Program Files\\R\\R-4.2.0\\bin\\Rscript.exe" -e "if(!require('RSQLite')) install.packages('RSQLite', repos='https://cloud.r-project.org')"
                
                # Запускаем тесты
                "C:\\Program Files\\R\\R-4.2.0\\bin\\Rscript.exe" -e "testthat::test_dir('tests/testthat')"
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
