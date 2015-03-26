library(shiny)

# server.R
flights <- tbl_df(read.csv("flights.csv", stringsAsFactors = FALSE))
flights$date <- as.Date(flights$date)


shinyServer(
  function(input, output) {
    

    output$plot_2 <- renderPlot ({
      flights %>% 
        group_by(dest, carrier) %>%
        summarise(average_arr_delay = mean(arr_delay, na.rm = TRUE), average_dep_delay = 
                    mean(dep_delay, na.rm =TRUE), n = n(), flight_time = mean(time, na.rm = TRUE)) %>%
        filter( n > input$vols) %>%
        ggplot(aes(x = average_arr_delay, y = dest, size  = average_dep_delay, color = carrier)) + 
        geom_point() +
        ggtitle("Dep/Arriv delays") + labs(x = "Arrival delay [min]", y = "Destination")
      
    })
      
  }
)