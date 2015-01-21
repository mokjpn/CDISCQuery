
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Search CDISC Terminology & Standards & Validation Rules"),

  # Sidebar with a slider input for number of bins
    inputPanel(
      textInput("term",
                  "Search term:",
                  value = "")
      #submitButton(text = "Search")
    ),

    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
    tabPanel('Terminology',
              tableOutput('searchResult1')),
    tabPanel('Standard', 
              tableOutput('searchResult2')),
    tabPanel('Validation', 
             tableOutput('searchResult3'))
    )
  , width=12))
)
