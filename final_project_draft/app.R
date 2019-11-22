#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#load packages
library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)

#load data

cash <- read_csv("merged.csv")

# Define UI (user interface) for application, which is the formating/way the interface will look to users
ui <- fluidPage(theme = shinytheme("flatly"),
                
  #used navbarpage to create a navbar shiny app set up, used tab panel to create the tabs within my shiny app, within the first tab map I called imageoutput referencing map, which I then specified in the server section below, inside the second tab, about I used h4 to set a header and p to specify a paragraph of text that I entered 
  navbarPage("Exploring Consumer Cash Usage Data",
             tabPanel("Who Uses Cash?",
                       sidebarPanel(
                 selectInput("educ", "Select an education level:",
                            choices = c("High School" = 9,
                                          "Bachelors" = 13,
                                          "Masters" = 14,
                                           "PhD" = 16)
               )),
               mainPanel(
                 plotOutput("plot_1")
               )),
             tabPanel("What Factors Predict Cash Usage",
                      htmlOutput("regression")
             ),
             tabPanel("About",
                      h1("About Section"),
                      h2("Background/Research Questions"),
                      p("Technology has rapidly transformed almost every aspect of our lives - including the way we carry out payments. Within recent years, the devlopment of PayPal, Ripple, Venmo among other mobile money payment systesm have acclerated the presence and access of cashless technologies across societies. But, despite this growth in technology, cash continues to persist both for use in transactions and a store of value. In order to answer this question we must understand: Who is using cash? Who is holding cash? What types of transactions is it used for? And, how often is it used?"),
                      h3("The Data"),
                      p("To answer this question I used data from the Federal Reserve's Diary of Consumer Payment Choice (DCPC). The DCPC is a survey of consumer payment behavior run in conjunction with the University of Southern California’s Understanding America Study (UAS). Respondents were randomly assigned a three-day period and asked to track all of their payments using an online questionnaire. I plan to combine data from the 2017, 2016 and 2015 data sets as well as use both the individual level and transaction level data."),
                      h4("About Me"),
                      p("My name is Jessica, I'm a senior economics concentrator and I am writing my thesis on a similar topic analyzing the changes in cash demand across countries and across time. I'm really excited to be using data science to observe trends that I can potentially include in my larger inquiry into cash in  my thesis.")
             )))


# Define server logic, which is what is required to create the data output (in this case showing our gif), made sure to set deletefile = FALSE so it does not delete our gif, output$ is set to map to corespond to map in the imageoutput specified in the UI above 
server <- function(input, output) {
  
  datareact1 <- reactive({
    cash %>%
      filter(highest_education == input$educ) %>%
      filter(pi %in% c(1,2,3,4,5,6,7)) %>%
      group_by(pi) %>%
      count()
  })
  output$plot_1 <- renderPlot({
   
  datareact1() %>%
      ggplot(aes(x=factor(pi), y=n)) +  geom_col() + labs(title = "Frequency of Payment Methods Used", x= "Payment Method", y = "Count") + scale_x_discrete(labels=c("1" = "Cash", "2" = "Check", "3" = "Credit Card", "4" = "Debit Card", "5" = "Prepaid/Gift", "6" = "Bank Account", "7" = "Online Payment")) + theme(axis.text.x=element_text(angle=45, hjust=1))
  
  }) 
  
  getPage <- function() {
    return(includeHTML("regression.html"))
  }
  
  output$regression <- renderUI({
    
    getPage()})
  
}

# Run the application 
shinyApp(ui = ui, server = server)
