library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(stringr)

# Loading dataset 
df <- read.csv("policedata.csv", nrows = 10000)
df_murder <- df %>% filter(description=="HOMICIDE")
df_car <- df %>% filter(str_detect(description, "^ROBBERY CARJACKING"))


# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "St.Louis Crime")



# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
        sliderInput("timeofday",
                    "Time (24-hour clock)",
                    min = min(df$timeofday,na.rm = T),
                    max = max(df$timeofday,na.rm = T),
                    value = c(min(df$timeofday,na.rm = T),max(df$timeofday,na.rm = T)),
                    step =1)      

)


# Dashboard body ----------------------------------------------
body <- dashboardBody(
          # Value Boxes ----------------------------------------------
            infoBoxOutput("info_box1"),
            infoBoxOutput("info_box2"),
            infoBoxOutput("info_box3")
)



ui <- dashboardPage(header, sidebar, body)


# Server Function ---------------------------------------------------
server <- function(input, output) {
  #  Reactive data function-----------------------------
  crimedatainput <- reactive({
    crimedf <- df %>%
      filter(timeofday >= input$timeofday[1] & timeofday<= input$timeofday[2])
    
  })
  
  homdatainput <- reactive({
  homdf <- df_murder %>%
      filter(timeofday >= input$timeofday[1] & timeofday<= input$timeofday[2])
  })
  
  carjackinput <- reactive({
    cardf <- df_car %>%
      filter(timeofday >= input$timeofday[1] & timeofday<= input$timeofday[2])
  })
  
  
  # Mass mean info box ----------------------------------------------
  output$info_box1 <- renderInfoBox({
    cd <- crimedatainput()
    infoBox("Total Number of Crimes is", nrow(distinct(cd,complaint)))
  })
  
  output$info_box2 <- renderInfoBox({
    cd <- homdatainput()
    infoBox("Total Number of Homicides is", nrow(distinct(cd,complaint)),color = "purple")
  })
  
  output$info_box3 <- renderInfoBox({
    cd <- carjackinput()
    infoBox("Total Number of Carjackings is", nrow(distinct(cd,complaint)),color = "red")
  })
  
  
}
# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
