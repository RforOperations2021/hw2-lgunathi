library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)


# Loading dataset 
df <- read.csv("policedata.csv", nrows = 10000)

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "St.Louis Crime")


# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar()


# Dashboard body ----------------------------------------------
body <- dashboardBody(
          # Value Boxes ----------------------------------------------
            infoBoxOutput("info_box1")
)



ui <- dashboardPage(header, sidebar, body)


# Server Function ---------------------------------------------------
server <- function(input, output) {
  # A plot showing the mass of characters -----------------------------

  
  # Mass mean info box ----------------------------------------------
  output$info_box1 <- renderInfoBox({
    infoBox("Total Number of Crimes is", nrow(distinct(df,complaint)))
  })
  

}
# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
