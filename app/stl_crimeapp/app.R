library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(stringr)
library(plotly)


# Loading dataset 
df <- read.csv("violent_crimes.csv")
df_murder <- df %>% filter(description=="HOMICIDE")
df_car <- df %>% filter(str_detect(description, "^ROBBERY CARJACKING"))


# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "St.Louis Crime")



# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
  
  # Inputs: select variables to plot ----------------------------------------------
    selectInput("monthSelect",
              "Months:",
              choices = sort(unique(df$Month)),
              multiple = TRUE,
              selectize = TRUE,
              selected = c("Jan", "Feb")),
  
  
        sliderInput("timeofday",
                    "Time (24-hour clock)",
                    min = min(df$timeofday,na.rm = T),
                    max = max(df$timeofday,na.rm = T),
                    value = c(min(df$timeofday,na.rm = T),max(df$timeofday,na.rm = T)),
                    step =1)      

))


# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  tabItem("plot",
          # Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("info_box1"),
            infoBoxOutput("info_box2"),
            infoBoxOutput("info_box3")
          ),
          
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Crime Type", plotlyOutput("plot_type")),
                   tabPanel("Crime In Parks", plotlyOutput("plot_park")))
          
          )
)))





ui <- dashboardPage(header, sidebar, body)


# Server Function ---------------------------------------------------
server <- function(input, output) {
  #  Reactive data function-----------------------------
  crimedatainput <- reactive({
    crimedf <- df %>%
      filter(timeofday >= input$timeofday[1] & timeofday<= input$timeofday[2])
      # Month Filter ----------------------------------------------
    if (length(input$monthSelect) > 0 ) {
      crimedf <- subset(crimedf, Month %in% input$monthSelect)
    }
    
    # Return dataframe ----------------------------------------------
    return(crimedf)
  })
  
  homdatainput <- reactive({
  homdf <- df_murder %>%
      filter(timeofday >= input$timeofday[1] & timeofday<= input$timeofday[2])
  })
  
  carjackinput <- reactive({
    cardf <- df_car %>%
      filter(timeofday >= input$timeofday[1] & timeofday<= input$timeofday[2])
  })
  
  # Plots -------------------------------------------------------------
  # A plot showing crimetypes
  output$plot_type <- renderPlotly({
    dat <- crimedatainput()
    ggplot(data = dat)+geom_bar(aes(x = UCR_casetype, y = stat(prop),group=1))
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
