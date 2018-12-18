#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Load Libraries
library(shiny)
library(tidyverse)
library(shinythemes)

#load data and do some adjustments
payments <- read_csv("./Payments.csv") %>% 
  filter(`Check Date` %in% c("2014","2015")) %>%
  rename(vendor = `VENDOR NAME`)

payments$`Check Date` <- as.numeric(payments$`Check Date`)

#prepare the inputs
vendor_inputs <- sort(unique(payments$vendor))

#ui
ui <- fluidPage(theme=shinytheme("slate"),
  
  # Application title
  titlePanel("Chicago PI"),
  #tab bar
  navbarPage(
    title = "Tools",
    tabPanel("About", 
             h1("Chicago Payment Investigator"),
             p("This application allows the user to explore the payments database
               from the City of Chicago. This logs payments from 2014 - 2015."),
             h2("Tabs:"),
             strong("Explore - "),
             span("Allows you to view and filter the raw data."),
             br(),
             strong("Payments by Vendor - "),
             span("Allow you to explore payments by vendor and graph payments."),
             br(),
             strong("Vendor Info - "),
             span("Gives some information about the vendor"),
             h2("Credits: "),
             span("Data from the "),
             a("City of Chicago data portal", href = "https://data.cityofchicago.org/Administration-Finance/Payments/s4vu-giwb"),
             br(),
             span("Created by Paul \""),
             a("Gummywormz", href="https://gummywormz.github.io"),
             span("\"Alves")
             ),
              
    tabPanel("Explore",     DT::dataTableOutput('explore')),
    tabPanel("Payments by Vendor",
             sidebarLayout
             (
               sidebarPanel
               (
                 selectInput(inputId = "pbv_vendor",
                             label = "Vendor",
                             choices = vendor_inputs,
                             multiple = TRUE),
                 radioButtons("plot_pbv","Plot Type", choices = 
                                c("Boxplot" = "box", "Point Graph" = "line"))
                 
               ),
               mainPanel(plotOutput("boxplot"),tableOutput("table_pbv"))
               
             )
    ),
    
    tabPanel("Vendor Info",
             sidebarLayout
             (
               sidebarPanel
               (
                 selectInput(inputId = "vi_vendor",
                             label = "Vendor",
                             choices = vendor_inputs,
                             multiple = FALSE)
               ),
               mainPanel(tableOutput("vi_info"),uiOutput("google"))
               
             )
    )
    
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #main data table
  output$explore <- DT::renderDataTable(
    DT::datatable(payments, options = list(pageLength = 25))
  )
  #filter for vendors
  filter_vendor <- reactive({
      payments_r <- payments
      filter(payments_r, vendor %in% input$pbv_vendor)
    })
  #same as above for the vendor info
  filter_vendor2 <- reactive({
    payments_r <- payments
    filter(payments_r, vendor %in% input$vi_vendor)
  })
  #generate vendor info table
  get_table <- reactive({
    
    info <- filter_vendor2()
    #messy fun part. let's join a bunch of data tables for the values wow
    info_calc <- data.frame("Name" = as.character(), "Value" = as.character(),stringsAsFactors = FALSE)
    
    info_calc <- bind_rows(info_calc, data.frame("Name"= "Name","Value"= input$vi_vendor,stringsAsFactors = FALSE))
    info_calc <- bind_rows(info_calc, data.frame("Name"= "Number of Payments","Value"= as.character(nrow(info)),stringsAsFactors = FALSE))
    info_calc <- bind_rows(info_calc, data.frame("Name"= "Mean Payment","Value"= as.character(mean(info$AMOUNT)),stringsAsFactors = FALSE))
    info_calc <- bind_rows(info_calc, data.frame("Name"= "Min Payment","Value"= as.character(min(info$AMOUNT)),stringsAsFactors = FALSE))
    info_calc <- bind_rows(info_calc, data.frame("Name"= "Max Payment","Value"= as.character(max(info$AMOUNT)),stringsAsFactors = FALSE))
  })
  
  #generate link
  get_link <- reactive({
    basestr <- str_to_title(input$vi_vendor)
    basestr <- gsub(" ","+",basestr)
    link <- str_c("https://www.google.com/search?q=",basestr)
  })
  
  #generate the plot for payments by vendor
  output$boxplot <- renderPlot({
    
      if(input$plot_pbv == "box" || is.null(input$plot_pbv))
      {
        ggplot(filter_vendor(),mapping = aes(y = AMOUNT,x = vendor)) + 
          geom_boxplot() + coord_flip()
      }else
      {
        ggplot(filter_vendor(),mapping = aes(y = AMOUNT,x = `Check Date`)) + 
          geom_point(alpha = 0.2)
        }
      
  })
    #table for payments by vendor
  output$table_pbv <- renderTable({filter_vendor()}) 
  #table for vendor info
  output$vi_info <- renderTable({
    get_table()
  })
  #google link
  output$google <- renderUI({
    a("Google vendor",href = get_link())
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

