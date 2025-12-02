pipeline {
    agent any
    
    stages {
        stage('Запуск тестов R') {
            steps {
                echo '🚀 Запускаем тесты системы...'
                
                bat '''
                rem Устанавливаем ВСЕ нужные пакеты
                "D:\\PROGRA~1\\R\\R-45~1.1\\bin\\x64\\Rscript.exe" -e "
                packages <- c('testthat', 'DBI', 'RSQLite', 'mockery', 'sodium')
                for (pkg in packages) {
                  if (!require(pkg, character.only = TRUE)) {
                    cat('Устанавливаю:', pkg, '\\n')
                    install.packages(pkg, repos = 'https://cloud.r-project.org')
                  }
                }
                cat('✅ Все пакеты установлены\\n')
                "

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
