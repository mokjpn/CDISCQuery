
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
                  NULL,
                  value = ""),
      submitButton(text = "Search")
    ),

    # Show a plot of the generated distribution
    mainPanel(p(textOutput('searchTerm')),tabsetPanel(
    tabPanel('Terminology',
              dataTableOutput('searchResult1')),
    tabPanel('Standard', 
              dataTableOutput('searchResult2')),
    tabPanel('Validation', 
             dataTableOutput('searchResult3'))
    )
  , width=12))
)
