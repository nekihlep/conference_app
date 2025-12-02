pipeline {
    agent any
    
    stages {
        stage('Запуск тестов R') {
            steps {
                echo '🚀 Запускаем тесты системы...'
                
                // ИСПРАВЛЕНО: используем bat для Windows
                bat '''
                # Устанавливаем пакеты если нужно
                Rscript -e "if(!require('testthat')) install.packages('testthat', repos='https://cloud.r-project.org')"
                Rscript -e "if(!require('DBI')) install.packages('DBI', repos='https://cloud.r-project.org')"
                Rscript -e "if(!require('RSQLite')) install.packages('RSQLite', repos='https://cloud.r-project.org')"
                
                # Запускаем тесты
                Rscript -e "testthat::test_dir('tests/testthat')"
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