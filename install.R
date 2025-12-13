# install.R - Install required packages
packages <- c(
  "shiny",
  "shinyjs", 
  "DBI",
  "RSQLite",
  "sodium",
  "DT",
  "shinydashboard",
  "ggplot2",
  "plotly"
)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

cat("✅ All packages installed successfully.\n")
