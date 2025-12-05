# SOL Pass Rates App
# Last Updated 12/5/25
# Deployed to virginiaequitycenter.shinyapps.io/pass_rates/
# Last Deployed: 12/5/25


# Libraries
library(tidyverse)
library(shiny)
library(bslib)
library(shinyWidgets)
library(scales)
library(plotly)
library(DT)

# Read in data ----
app_data <- read_csv("pass_rates_data.csv")

reading_all_grades <- app_data %>% filter(subject == "English:Reading")
math_all_grades <- app_data %>% filter(subject == "Mathematics") %>% filter(grade <= 6)

division_names <- as.list(unique(app_data$division_name))

reading_grade_levels <- as.list(unique(reading_all_grades$test_level))
math_grade_levels <- as.list(unique(math_all_grades$test_level))

# UI ----
ui <- page_fillable(
  # card(
  #   card_header("SOL Test Pass Rates"),
  #   card_body(
      layout_column_wrap(
        width = 1/2,
        fill = FALSE,
        heights_equal = "row",
        selectInput("selected_division",
                    label = "Select Highlighted School Division:",
                    choices = division_names,
                    selected = "Albemarle County"),
        pickerInput("benchmarks",
                    label = "Select Benchmark School Divisions:",
                    choices = division_names,
                    selected = division_names,
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
        )
      ),
      navset_pill(
        nav_panel(
          h5(icon("book-open-reader"), "Reading SOL Test Pass Rates"),
          # icon = icon("book"),
          br(),
          layout_column_wrap(
            width = 1/3,
            fill = FALSE,
            heights_equal = "row",
            class = "align-items-end",
            selectInput("reading_grade_level",
                        label = "Select Reading SOL Test Grade Level:",
                        choices = reading_grade_levels,
                        selected = "Grade 3"),
            "",
            tags$a(href="#jumpReading", class="btn btn-default", icon("table"), "Go to Data Table"),
          ),
          plotlyOutput("reading_plot", height = "550px"),
          br(),
          uiOutput("jumpReading", inline = TRUE),
          DTOutput("sol_table"),
          downloadButton("downloadBtn", "Download Selected Division Table")
        ),
        nav_panel(
          h5(icon("calculator"), "Math SOL Test Pass Rates"),
          # icon = icon("calculator")
          br(),
          layout_column_wrap(
            width = 1/3,
            fill = FALSE,
            heights_equal = "row",
            class = "align-items-end",
            selectInput("math_grade_level",
                        label = "Select Math SOL Test Grade Level:",
                        choices = math_grade_levels,
                        selected = "Grade 6"),
            "",
            tags$a(href="#jumpMath", class="btn btn-default", icon("table"), "Go to Data Table"),
          ),
          plotlyOutput("math_plot", height = "550px"),
          br(),
          uiOutput("jumpMath", inline = TRUE),
          DTOutput("sol_table_math"),
          downloadButton("downloadBtnMath", "Download Selected Division Table")
        ),
        nav_spacer(),
        nav_panel(
          h5(shiny::icon("circle-info")),
          br(),
          markdown("The Virginia Standards of Learning (SOL) assessments establish the expectations for student learning at the end of each grade in core subjects. Students in Virginia take these exams in May from 3rd to 12th grade. We have compiled pass rates for SOL assessments for Reading (Grades 3-8) and Math (Grades 3-6).
          
          As detailed in an [Annie E. Casey report](https://www.aecf.org/resources/early-warning-why-reading-by-the-end-of-third-grade-matters), third grade reading is a critical educational milestone marking a transition from 'learning to read' to 'reading to learn' and low proficiency by the end of third grade has a detrimental impact on a child's future. As such, SOL test scores in third grade are of particular importance for educators and administrators.
          
          After the 6th grade, students place into different math classes (Math 7, Algebra I, Geometry, etc.) in accordance with their retention of previous years' concepts as understood through grades and SOL test scores.
          
          Pass rates are shown by school division and for Virginia students overall. VDOE adopted revised standards and other changes to the Reading (2013) and Math (2012) SOL tests, making results not directly comparable to those from prior years. For information on other grades, data can be collected from the [Virginia Department of Education's website](https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=306).")
        )
      )
    # )
  # )
  

)

# Server ----
server <- function(input, output) {
  
  
  df <- reactive({
    d <- app_data %>% filter(division_name %in% input$benchmarks)
    
  })
  
  selected <- reactive({
    d <- app_data %>% filter(division_name == input$selected_division)
    
    # d <- d %>% filter(test_level == input$reading_grade_level)
  })
  
  state <- reactive({
    d <- app_data %>% filter(division_name == "VA Overall")
    
    # d <- d %>% filter(test_level == input$reading_grade_level)
  })
  
  df_reading <- reactive({
    d <- df() %>% filter(subject == "English:Reading") %>% filter(test_level == input$reading_grade_level)
  })
  selected_reading <- reactive({
    d <- selected() %>% filter(subject == "English:Reading") %>% filter(test_level == input$reading_grade_level)
  })
  state_reading <- reactive({
    d <- state() %>% filter(subject == "English:Reading") %>% filter(test_level == input$reading_grade_level)
  })
  
  dt_reading <- reactive({
    d <- rbind(selected_reading(), state_reading()) %>% 
      select(school_year, division_name, test_level, total_count, pass_rate, subject)
  })
  
  output$jumpReading <- renderUI({
    tagList(h5(paste0("Data Table: ", input$reading_grade_level, " Reading SOL Test Pass Rates")),
            p("Selected Division and State-Wide Average"))
  })
  
  # Math reactives ----
  df_math <- reactive({
    d <- df() %>% filter(subject == "Mathematics") %>% filter(test_level == input$math_grade_level)
  })
  selected_math <- reactive({
    d <- selected() %>% filter(subject == "Mathematics") %>% filter(test_level == input$math_grade_level)
  })
  state_math <- reactive({
    d <- state() %>% filter(subject == "Mathematics") %>% filter(test_level == input$math_grade_level)
  })
  
  dt_math <- reactive({
    d <- rbind(selected_math(), state_math()) %>% 
      select(school_year, division_name, test_level, total_count, pass_rate, subject)
  })
  
  output$jumpMath <- renderUI({
    tagList(h5(paste0("Data Table: ", input$math_grade_level, " Math SOL Test Pass Rates")),
            p("Selected Division and State-Wide Average"))
  })
  
  # Line chart functions ----
  
  chart_func <- function(dat, selected_dat, state_dat){
    if (is.null(input$benchmarks)) {
      chart <- ggplot(selected_dat, aes(x = as.character(test_year), y = round(pass_rate,1), group = division_name, color = division_name,
                                                     text = paste0("School Year: ", school_year, "<br>Division: ", division_name, "<br>Pass Rate: ", round(pass_rate,1), "%"))) +
        geom_vline(xintercept = 15, linetype = 2, size = 0.3, color = "#333333") +
        annotate("text", x = 15, y = 10, size = 3, hjust = 0, color = "#333333",
                 label =
                   "SOLs not conducted
in 2020 due to the
COVID-19 Pandemic") +
        geom_line(data = state_dat, linewidth = 0.75) +
        geom_point(data = state_dat) +
        geom_line(linewidth = 0.75) +
        geom_point() +
        scale_y_continuous(limits = c(0, 100),
                           labels = label_percent(scale = 1),
                           name = "SOL Test Pass Rate") +
        scale_x_discrete(name = "Test Year (Spring)") +
        theme_minimal()

    } else {
      chart <- ggplot(dat, aes(x = as.character(test_year), y = round(pass_rate,1), group = division_name, color = division_name, 
                                      text = paste0("School Year: ", school_year, "<br>Division: ", division_name, "<br>Pass Rate: ", round(pass_rate,1), "%"))) +
        geom_line(color = "#c4c0c2", size = 0.4) +
        geom_vline(xintercept = 15, linetype = 2, size = 0.3, color = "#333333") +
        annotate("text", x = 15, y = 10, size = 3, hjust = 0, color = "#333333", 
                 label = 
                   "SOLs not conducted
in 2020 due to the 
COVID-19 Pandemic") +
        geom_line(data = state_dat, linewidth = 0.75) +
        geom_point(data = state_dat) +
        geom_line(data = selected_dat, linewidth = 0.75) +
        geom_point(data = selected_dat) +
        scale_y_continuous(limits = c(0, 100),
                           labels = label_percent(scale = 1), 
                           name = "SOL Test Pass Rate") + 
        scale_x_discrete(name = "Test Year (Spring)") +
        theme_minimal()
    }
    
    chart
    
  }
  
  chart_config <- function(plot){
    plot %>% 
      config(displayModeBar = FALSE,
             showLink = FALSE,
             scrollZoom = FALSE) %>% 
      layout(xaxis = list(fixedrange = TRUE), 
             yaxis = list(fixedrange = TRUE),
             legend = list(orientation = 'h', title = FALSE,
                           xanchor = "center", yanchor = "bottom",
                           x = 0.5, y = 1,
                           font = list(size = 14)),
             margin = list(t = 80)) %>% 
      style(hoverlabel = list(
                              # bgcolor = "white",
                              # bordercolor = "#77777",
                              font = list(size = 14))
            ) %>% 
      highlight(on='plotly_hover', off='plotly_doubleclick', dynamic=FALSE, color = "#333333",
                opacityDim = 1, persistent = FALSE) %>%
    htmlwidgets::onRender("
    function(el) {
      el.on('plotly_hover', function(data) {
        var pointIndex = data.points[0].pointIndex;
        var curveNumber = data.points[0].curveNumber; // Index of the hovered trace (line)
        var traceName = data.points[0].data.name; // Name of the hovered trace
        console.log('Hovered over Line:', traceName, 'at curveNumber:', curveNumber, 'pointIndex:', pointIndex);
      });
      el.on('plotly_unhover', function(data) {
        console.log('Unhovered from a line');
        var curveNumber = data.points[0].curveNumber; // Index of the hovered trace (line)
        Plotly.restyle(el.id, {'line.color': '#c4c0c2'}, [curveNumber]);
      });
    }
  ")
    
    
  }
  
  
  # Reading Chart----
  output$reading_plot <- renderPlotly({

    dat <- highlight_key(df_reading(), key=~division_name)
    
    reading_plot <- chart_func(dat, selected_reading(), state_reading())
    
    reading_plot <- reading_plot + 
              geom_vline(xintercept = 8, linetype = 2, size = 0.3, color = "#333333") +
              annotate("text", x = 8, y = 10, size = 3, hjust = 0, color = "#333333",
                       label =
                         "Implementation of
      revised reading standards")
    
    ggplotly(reading_plot, tooltip = c("text")) %>%
      chart_config %>%
      layout(title = paste0(input$reading_grade_level, " Reading SOL Test Pass Rates"))

  })
  
  
  # Math chart ----
  output$math_plot <- renderPlotly({
    
    dat <- highlight_key(df_math(), key=~division_name)
    
    math_plot <- chart_func(dat, selected_math(), state_math())
    
    math_plot <- math_plot + 
      geom_vline(xintercept = 7, linetype = 2, size = 0.3, color = "#333333") +
      annotate("text", x = 7, y = 10, size = 3, hjust = 0, color = "#333333", 
               label = 
                 "Implementation of 
revised math standards")
    
    ggplotly(math_plot, tooltip = c("text")) %>% 
      chart_config %>% 
      layout(title = paste0(input$math_grade_level, " Math SOL Test Pass Rates"))
    
  })
  
  
  # Data table function ----
  
  table_func <- function(dat){
    table <- datatable(dat,
                       rownames = FALSE,
                       caption = "Source: Virginia Department of Education (VDOE). \"Annual Statewide Assessment Result Build-A-Table.\"",
                       class = 'display nowrap',
                       filter = 'top',
                       options = list(searchHighlight = TRUE,
                                      scrollX = TRUE,
                                      pageLength = 15,
                                      order = list(0, 'desc') # column indexing starts at 0
                       ),
                       colnames = c("School Year", "School Division", "Grade Level", "Total Students", "Pass Rate", "Subject")
    )
    
    table
  }
  
  # Reading Data table ----
  output$sol_table <- renderDT({
    
    dt_read <- table_func(dt_reading())
   
  })
  
  # Reading download button
  output$downloadBtn <- downloadHandler(
    filename = "sol-reading-pass-rates.csv",
    content = function(file) {
      write.csv(dt_reading(), file)
    }
  )
  
  
  # Math Data table ----
  output$sol_table_math <- renderDT({
    
    dt_math <- table_func(dt_math())
  
  })
  
  # Math download button
  output$downloadBtnMath <- downloadHandler(
    filename = "sol-math-pass-rates.csv",
    content = function(file) {
      write.csv(dt_math(), file)
    }
  )
  

}

# Run the application 
shinyApp(ui = ui, server = server)
