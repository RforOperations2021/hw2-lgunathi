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
df_park <- df %>% filter(neighborhood %in% c(80:88))
df_park<- df_park[grepl("PARK",df_park$ileads_street),]
df_park <- df_park%>%group_by(ileads_street,coded_year,Month)%>%summarize(n=n())
df_district<- df %>% group_by(district,coded_year,Month)%>%summarize(n=n())%>%filter(district!=0)


# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Violent Crime in St.Louis",titleWidth = 300)




# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table",  badgeColor = "green"),
  
  # Inputs: select variables to plot ----------------------------------------------
  checkboxGroupInput(inputId="yearselect",
                     label= "Years",
                     choices= sort(unique(df$coded_year)),
                     selected=c(2018)),  
  
  selectInput("monthSelect",
              "Months:",
              choices = sort(unique(df$Month)),
              multiple = TRUE,
              selectize = TRUE,
              selected = c("May", "Aug","Mar")),
  
  
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
                   tabPanel("Crime In Parks", plotlyOutput("plot_park")),
                   tabPanel("Crime In Police District", plotlyOutput("plot_district"))
          )
)),
# Data Table Page ----------------------------------------------
tabItem("table",
        fluidPage(
          box(title = "Violent Crime Data", DT::dataTableOutput("table"), width = 12))

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
    
    if (length(input$yearselect) > 0 ) {
      crimedf <- subset(crimedf, coded_year %in% input$yearselect)
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
    ggplot(data = dat)+geom_bar(aes(x = UCR_casetype, y = stat(prop),group=1))+
      xlab("Type of Case")+
      ylab("Proportion of Cases")+theme_light()
  })
  
  # Park barplot 
  output$plot_park <- renderPlotly({
    dat <- crimedatainput()
    dat <-dat %>% filter(neighborhood %in% c(80:88)) 
    dat <- dat[grepl("PARK",dat$ileads_street),]
    dat <- dat%>%group_by(ileads_street,coded_year,Month)%>%summarize(n=n())
    ggplot(data = dat) +
    geom_bar(mapping = aes(x = ileads_street, y = n), stat = "identity")+coord_flip()+
    xlab("Park Name")+
    ylab("Number of Crimes")+theme_light()
  })
  
  output$plot_district <- renderPlotly({
    dat <- crimedatainput()
    dat <- dat%>%group_by(district,coded_year,Month)%>%summarize(n=n())%>%filter(district!=0)
    ggplot(dat, aes(x=as.factor(district), y=n)) + 
      geom_boxplot(aes(fill=as.factor(district)))+xlab("Police Districts Number")+
      ylab("Violent Crime in St. Louis")+scale_fill_discrete(name = "Police District")
  })
  

  # Mass mean info box ----------------------------------------------
  output$info_box1 <- renderInfoBox({
    cd <- crimedatainput()
    infoBox("Total Number of Crimes", nrow(distinct(cd,complaint)))
  })
  
  output$info_box2 <- renderInfoBox({
    cd <- crimedatainput()
    cd <- cd%>%filter(description=="HOMICIDE")
    infoBox("Total Number of Homicides", nrow(distinct(cd,complaint)),color = "purple")
  })
  
  output$info_box3 <- renderInfoBox({
    cd <- crimedatainput()
    cd <- cd%>%filter(str_detect(description, "^ROBBERY CARJACKING"))
    infoBox("Total Number of Carjackings", nrow(distinct(cd,complaint)),color = "red")
  })
  
  
  # Data table of crimes ----------------------------------------------
  output$table <- DT::renderDataTable({
    subset(crimedatainput(), select = c(complaint, coded_year,coded_month,UCR_casetype))
  })
  
  
}
# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
