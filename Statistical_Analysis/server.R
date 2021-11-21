#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    updateSelectInput(session, 'vehicle_type', choices = unique(carcrash$vehicle_type))
    
    updateSelectInput(session, 'Categories', choices = unique(carcrash$Categories))
    
    updateSelectInput(session, 'week', choices = unique(carcrash$week))
    
    updateSelectInput(session, 'hour', choices = unique(carcrash$hour))
    
    crash_deadliness <- reactive({
        carcrash %>%
            filter(vehicle_type == input$vehicle_type,
                   Categories == input$Categories,
                   week == input$week,
                   hour == input$hour) %>%
            group_by(Factors) %>%
            summarise(
                death_rate = mean(killed),
                injured_rate = mean(injured))
    })
    
    
    output$rates <- renderPlot(
        crash_deadliness() %>% pivot_longer(
            death_rate:injured_rate,
            names_to = 'type',
            values_to = 'Rate') %>% 
            ggplot() + 
            geom_col(aes(x = Factors, y = Rate, fill = type), 
                     position = 'dodge') + 
            ggtitle('Accident Injury and Death Rates') +
            coord_flip()
            
        )
    
    output$table <- renderDataTable(carcrash %>%
            filter(vehicle_type == input$vehicle_type & 
                       Categories == input$Categories &
                       week == input$week & hour == input$hour))
    

})