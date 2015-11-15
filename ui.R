
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Create Templates for Mikey!"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dates",
                     label= 'Please select your date range.',
                     start=Sys.Date(),end=Sys.Date()
      ),
      submitButton('Submit')
              
    ),

    # Show a plot of the generated distribution
    mainPanel(
      h2('Hi Michael, how are you this fine day?'),
      span("From the days between ",textOutput('first',inline=T), 'and', textOutput('second',inline=T),
           'you received',textOutput('n',inline=T),'orders.'),
      tableOutput("styles"),
      tableOutput('location')
    )
  )
))
