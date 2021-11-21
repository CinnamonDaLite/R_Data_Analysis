#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
library(shinydashboard)
dashboardPage(

    # Application title
    dashboardHeader(title = "Carcrash Data"),

    # create input for items
    dashboardSidebar(
        sidebarMenu(
            menuItem("Motivation", tabName = 'goal', icon = icon('car-crash')), # goal: why do we care?
            menuItem("Graphs", tabName = 'Graphs', icon = icon('chart-bar')),
            menuItem("Data", tabName = 'data', icon = icon("database"))
        ),
            selectizeInput('vehicle_type', label = 'Vehicle', choices = NULL),
            selectizeInput('Categories', label = 'Category', choices = NULL),
            selectizeInput('week', label = 'Day of Week', choices = NULL),
            selectizeInput('hour', label = 'Hour of day', choices = NULL)
        ),

        # Show a plot of the generated distribution
    dashboardBody(
        tabItems(
            tabItem(tabName = "Graphs",
                     fluidRow(
                         column(12, plotOutput('rates'))
                     )),
            tabItem(tabName = 'data', DTOutput('table')),
            tabItem(tabName = 'goal', 'Our goal is to 
            investigate which kind of crashes are more likely to happen
            and which are less likely to happen, and to also investigate 
            what factors cause these crashes to happen so that we may reduce
            these crashes from happening.')
            )
    )
)
