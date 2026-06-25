library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(dplyr)
library(DT)

# Read data
df <- read.csv("BankLoanDefaultDataset.csv", check.names = TRUE)

# Convert Default to factor
df$Default <- factor(df$Default,
                     levels = c(0, 1),
                     labels = c("No Default", "Default"))

# ---------------- UI ----------------
ui <- dashboardPage(
  
  dashboardHeader(
    title = "Bank Loan Default",
    
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://raw.githubusercontent.com/ChingyiLam/STA-553-Project-5/refs/heads/main/app.R",
        target = "_blank",
        icon("code"),
        " Source Code",
        style = "color: white; font-size: 16px;"
      )
    ),
    
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://chingyilam.github.io/STA-553-Project-5/",
        target = "_blank",
        icon("file-alt"),
        " Report",
        style = "color: white; font-size: 16px;"
      )
    ),
    
    tags$li(
      class = "dropdown",
      tags$a(
        href = "https://raw.githubusercontent.com/ChingyiLam/STA-553-Project-5/refs/heads/main/BankLoanDefaultDataset.csv",
        target = "_blank",
        icon("database"),
        " Data Source",
        style = "color: white; font-size: 16px;"
      )
    )
  ),
  
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
        .skin-blue .main-header .logo {
          background-color: #6E3061;
        }
        .skin-blue .main-header .navbar {
          background-color: #177AAD;
        }
        .skin-blue .main-sidebar {
          background-color: #177AAD;
        }
        .skin-blue .sidebar-menu > li.active > a {
          border-left-color: #6E3061;
        }
        .content-wrapper, .right-side {
          background-color: #ffffff;
        }
        .box.box-solid.box-primary>.box-header {
          color: #fff;
          background: #6E3061;
        }
        .box.box-solid.box-primary {
          border-color: #6E3061;
        }
      "))
    ),
    
    sidebarMenu(
      id = "sidebar",
      menuItem("Data Explorer", tabName = "dashboard", icon = icon("dashboard")),
      
      sliderInput("credit", "Credit Score:",
                  min = min(df$Credit_score, na.rm = TRUE),
                  max = max(df$Credit_score, na.rm = TRUE),
                  value = c(min(df$Credit_score, na.rm = TRUE),
                            max(df$Credit_score, na.rm = TRUE))),
      
      sliderInput("amount", "Loan Amount:",
                  min = min(df$Amount, na.rm = TRUE),
                  max = max(df$Amount, na.rm = TRUE),
                  value = c(min(df$Amount, na.rm = TRUE),
                            max(df$Amount, na.rm = TRUE))),
      
      selectInput("gender", "Gender:",
                  choices = c("All", unique(df$Gender)),
                  selected = "All"),
      
      selectInput("emp", "Employment Status:",
                  choices = c("All", unique(df$Emp_status)),
                  selected = "All")
    )
  ),
  
  dashboardBody(
    
    # Value boxes
    fluidRow(
      valueBoxOutput("borrowersBox", width = 3),
      valueBoxOutput("creditBox", width = 3),
      valueBoxOutput("amountBox", width = 3),
      valueBoxOutput("defaultBox", width = 3)
    ),
    
    # First row of charts
    fluidRow(
      column(
        6,
        box(
          title = "Number of Borrowers by Default Status",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput("plot1", height = "350px")
        )
      ),
      
      column(
        6,
        box(
          title = "Credit Score by Default Status",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput("plot2", height = "350px")
        )
      )
    ),
    
    # Second row of charts
    fluidRow(
      column(
        6,
        box(
          title = "Loan Amount Distribution by Default Status",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput("plot3", height = "350px")
        )
      ),
      
      column(
        6,
        box(
          title = "Default Rate by Employment Status",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput("plot4", height = "350px")
        )
      )
    ),
    
    # Data table
    fluidRow(
      column(
        12,
        box(
          title = "Filtered Borrower Records",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          DTOutput("table")
        )
      )
    )
  )
)

# ---------------- Server ----------------
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data <- df %>%
      filter(Credit_score >= input$credit[1],
             Credit_score <= input$credit[2],
             Amount >= input$amount[1],
             Amount <= input$amount[2])
    
    if (input$gender != "All") {
      data <- data %>% filter(Gender == input$gender)
    }
    
    if (input$emp != "All") {
      data <- data %>% filter(Emp_status == input$emp)
    }
    
    data
  })
  
  # Value boxes
  output$borrowersBox <- renderValueBox({
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "Number of Borrowers",
      icon = icon("users"),
      color = "purple"
    )
  })
  
  output$creditBox <- renderValueBox({
    valueBox(
      value = round(mean(filtered_data()$Credit_score, na.rm = TRUE), 2),
      subtitle = "Average Credit Score",
      icon = icon("credit-card"),
      color = "blue"
    )
  })
  
  output$amountBox <- renderValueBox({
    valueBox(
      value = paste0("$", round(mean(filtered_data()$Amount, na.rm = TRUE), 2)),
      subtitle = "Average Loan Amount",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$defaultBox <- renderValueBox({
    valueBox(
      value = paste0(round(mean(filtered_data()$Default == "Default", na.rm = TRUE) * 100, 2), "%"),
      subtitle = "Default Rate",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  # Plot 1: Default count
  output$plot1 <- renderPlotly({
    data <- filtered_data() %>%
      count(Default)
    
    plot_ly(data,
            x = ~Default,
            y = ~n,
            type = "bar",
            text = ~n,
            textposition = "auto") %>%
      layout(
        xaxis = list(title = "Default Status"),
        yaxis = list(title = "Number of Borrowers")
      )
  })
  
  # Plot 2: Credit score boxplot
  output$plot2 <- renderPlotly({
    plot_ly(filtered_data(),
            x = ~Default,
            y = ~Credit_score,
            type = "box",
            color = ~Default) %>%
      layout(
        xaxis = list(title = "Default Status"),
        yaxis = list(title = "Credit Score"),
        showlegend = FALSE
      )
  })
  
  # Plot 3: Loan amount histogram
  output$plot3 <- renderPlotly({
    plot_ly(filtered_data(),
            x = ~Amount,
            color = ~Default,
            type = "histogram",
            opacity = 0.7) %>%
      layout(
        xaxis = list(title = "Loan Amount"),
        yaxis = list(title = "Count"),
        barmode = "overlay"
      )
  })
  
  # Plot 4: Default rate by employment status
  output$plot4 <- renderPlotly({
    data <- filtered_data() %>%
      group_by(Emp_status) %>%
      summarise(
        Default_Rate = mean(Default == "Default", na.rm = TRUE) * 100,
        Count = n()
      )
    
    plot_ly(data,
            x = ~Emp_status,
            y = ~Default_Rate,
            type = "bar",
            text = ~paste0(round(Default_Rate, 2), "%"),
            textposition = "auto") %>%
      layout(
        xaxis = list(title = "Employment Status"),
        yaxis = list(title = "Default Rate (%)")
      )
  })
  
  # Data table
  output$table <- renderDT({
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
}

# Run app
shinyApp(ui = ui, server = server)