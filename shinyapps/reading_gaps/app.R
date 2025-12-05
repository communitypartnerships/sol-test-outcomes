# SOL Reading Gaps App
# Last Updated 12/5/25
# Deployed to virginiaequitycenter.shinyapps.io/reading_gaps/
# Last Deployed: 12/5/25

# Libraries
library(tidyverse)
library(shiny)
library(bslib)
library(plotly)

# Read in data ----
app_data <- read_csv("reading_gap_data.csv")

division_names <- as.list(unique(app_data$division_name[order(app_data$division_name)]))
test_years <- as.list(unique(app_data$test_year[order(app_data$test_year, decreasing=TRUE)]))

# UI ----
ui <- page_fillable(
  layout_column_wrap(
    width = 1/3,
    fill = FALSE,
    heights_equal = "row",
    selectInput("selected_division",
                label = "Select Highlighted School Division:",
                choices = division_names,
                selected = "Albemarle County"),
    selectInput("selected_group",
                label = "Select Demographic Group:",
                choices = c("Race", "Economic Status", "English Learner Status"),
                selected = "Race"),
    selectInput("selected_year",
                label = "Select SOL Test Year:",
                choices = test_years,
                selected = test_years[1])
  ),
  plotlyOutput("tornadoPlot", height='800px')
)

# Server ----
server <- function(input, output) {
  rvs = reactiveValues()
  
  observeEvent(c(input$selected_year, input$selected_group), {
    rvs$plt_dat = app_data %>% 
      filter(group == input$selected_group, test_year == input$selected_year) %>% 
      mutate(division_name = fct_reorder(division_name, desc(rank)))
  })
  
  output$tornadoPlot <- renderPlotly({
    
    tornado_plot <- ggplot(rvs$plt_dat, aes(y = division_name, x = rate, group = division_name, color = label,
                                            text = paste0("Divison: ", division_name, 
                                                          "\nDemographic: ", label,
                                                          "\nPass Rate: ", scales::percent(rate, accuracy = 0.1)))) +
      geom_path(color = "grey") +
      geom_path(data = filter(rvs$plt_dat, division_name == input$selected_division), color = "black", size = 1) +
      geom_point() +
      labs(x ="", y = "School Division") +  
      theme_classic() +
      theme(axis.text.y = element_text(size = 9)) +
      labs(colour = NULL) +
      scale_x_continuous(labels = scales::percent) +
      scale_color_manual(values=c("#E69F00", "#56B4E9")) +
      geom_vline(xintercept = c(0.4, 0.6, 0.8), linetype = "dotted", color = "light grey")
    
    ggplotly(tornado_plot, height = 2000, tooltip = "text") %>% 
      config(displayModeBar = FALSE,
             showLink = FALSE,
             scrollZoom = FALSE) %>% 
      layout(xaxis = list(fixedrange = TRUE), 
             yaxis = list(fixedrange = TRUE),
             legend = list(orientation = 'h', title = FALSE,
                           xanchor = "center", yanchor = "bottom",
                           x = 0.5, y = 1,
                           font = list(size = 14)),
             margin = list(t = 80),
             title = paste0("Gaps in Reading SOL Test Pass Rates by ", input$selected_group)) %>% 
      style(hoverlabel = list(
        font = list(size = 14))
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)