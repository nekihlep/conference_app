A full-stack web application that automates the entire workflow of academic conference management — from paper submission to final decision.
The application optimizes the submission and review process by handling different participant types, provides real-time status updates and statistics, and simplifies overall administration by replacing manual work with automated workflows.

[Dashboard Preview](https://prnt.sc/EpOJldoK9aHK) 

## 🚀 Features

### 👥 Multi-Role Platform
- **Admins**: Create conferences, moderate submissions, view analytics
- **Participants**: Submit applications, track status, upload documents
- **Role-Based Access**: Secure separation of concerns

### 📊 Interactive Analytics Dashboard
- Real-time visualizations (scatter plots, bar charts, timelines)
- Conference popularity vs. date analysis
- Live submission status tracking
- Participant demographics breakdown

### 🔒 Security & Workflow
- Secure authentication with password hashing (sodium)
- File upload and management system
- Automated application status updates
- Complete audit trail

## 🛠 Tech Stack

- **Backend**: R, Shiny, RSQLite
- **Frontend**: HTML, CSS, JavaScript, Shiny UI
- **Database**: SQLite with normalized 4-table schema
- **Security**: Password hashing, session management
- **Visualization**: ggplot2, Plotly, DT

## ⚡ Quick Start

### Prerequisites
- R (≥ 4.0.0)
- RStudio (recommended for development)

### One-Command Setup
```bash
# Clone and setup in one command
git clone https://github.com/nekihlep/conference_app.git
cd conference_app
Rscript install.R        # Install dependencies
Rscript init_database.R  # Create database with sample data
R -e "shiny::runApp('app.R')"  # Launch application
```

### Defolt Test Accounts
Admin logs: admin\admin
Participient: user1\user123
