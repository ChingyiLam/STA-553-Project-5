library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(dplyr)
library(DT)

df <- read.csv("BankLoanDefaultDataset.csv", check.names = TRUE)

df$Default <- factor(df$Default,
                     levels = c(0, 1),
                     labels = c("No Default", "Default"))

ui <- dashboardPage(
  
  dashboardHeader(
    title = "Bank Loan Default Dashboard",
    
    tags$li(
      class = "dropdown",
      tags$a(
        href = "YOUR_GITHUB_SOURCE_CODE_LINK",
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
                  selected = "All")
    )
  ),
  
  dashboardBody(
    fluidRow(
      column(
        6,
        box(
          title = "Dashboard Summary",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          verbatimTextOutput("summary")
        )
      ),
      
      column(
        6,
        box(
          title = "Number of Borrowers by Default Status",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput("plot1", height = "350px")
        )
      )
    ),
    
    fluidRow(
      column(
        6,
        box(
          title = "Credit Score by Default Status",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput("plot2", height = "350px")
        )
      ),
      
      column(
        6,
        box(
          title = "Loan Amount Distribution by Default Status",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotlyOutput("plot3", height = "350px")
        )
      )
    ),
    
    fluidRow(
      column(
        12,
        box(
          title = "Filtered Bank Loan Data",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          DTOutput("table")
        )
      )
    )
  )
)

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
    
    data
  })
  
  output$summary <- renderPrint({
    data <- filtered_data()
    
    cat("Number of borrowers:", nrow(data), "\n")
    cat("Average credit score:", round(mean(data$Credit_score, na.rm = TRUE), 2), "\n")
    cat("Average loan amount:", round(mean(data$Amount, na.rm = TRUE), 2), "\n")
    cat("Default rate:", round(mean(data$Default == "Default", na.rm = TRUE) * 100, 2), "%\n")
  })
  
  output$plot1 <- renderPlotly({
    data <- filtered_data() %>%
      count(Default)
    
    plot_ly(data,
            x = ~Default,
            y = ~n,
            type = "bar") %>%
      layout(
        xaxis = list(title = "Default Status"),
        yaxis = list(title = "Count")
      )
  })
  
  output$plot2 <- renderPlotly({
    plot_ly(filtered_data(),
            x = ~Default,
            y = ~Credit_score,
            type = "box") %>%
      layout(
        xaxis = list(title = "Default Status"),
        yaxis = list(title = "Credit Score")
      )
  })
  
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
  
  output$table <- renderDT({
    datatable(
      filtered_data(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)