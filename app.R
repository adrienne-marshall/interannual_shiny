source("R/global.R")

# User Interface ------------------
ui <- fluidPage(
  # navbarPage("Snowpack variability", selected = "Map",
     #About page              
     # tabPanel("About", fluidRow(includeHTML("about.html"))), #end of first tabPanel
                 
  #Main page.
  # tabPanel("Map",
  
  # Title --------
  titlePanel(h1("Climate-driven changes in interannual variability of snowpack amount and timing")),
  
  # Explanatory text. 
  fluidRow(column(12, offset = 0, 
                  p("This tool is designed as an interactive method for exploring changes in 
                      snowpack in the western U.S., with a particular focus on how interannual variability
                      of snowpack amount, measured as annual maximum snow water equivalent (SWEmax), and 
                      date of SWEmax (DOMS) are changing."),
                  p(strong("Instructions:"), "Use the tabs at the bottom to select a time period,
                      snowpack variable, summary statistic, and GCM (including a 10-model mean). You can also
                    click on the map to display visualizations of the distribution of SWEmax and DOMS values 
                    over historic and future 30-year periods. To zoom in on a subset of the map, brush over the 
                    area you'd like to see and then double-click; to zoom back out, double-click again."), 
                  p("To learn more, contact Adrienne Marshall: adriennemarshall@uidaho.edu."))),
  
  
  # Sidebar layout ------------
  sidebarLayout(
    
    # Sidebar panel for inputs ------
    sidebarPanel(
      
      # Drop-down boxes ----------
      
      # Choose historic/future/delta. 
      radioButtons(inputId = "case",
                   label = "Choose time period",
                   choices = c("Historic (1970-1999)" = "historic",
                               "RCP 8.5 (2050-2079)" = "future",
                               "Change (RCP 8.5 - historic)" = "delta"),
                   selected = "delta"),
      
      # Choose SWE variable:
      radioButtons(inputId = "swe_variable", 
                  label = "Choose SWE variable",
                  choices = c("SWEmax" = "swe", "Date of SWEmax" = "doms"),
                  selected = "swe"),

      # Choose variability metric: 
      radioButtons(inputId = "variability_metric",
                  label = "Choose metric",
                  choices = list("25th percentile" = "25", 
                                 "75th percentile" = "75", 
                                 "Mean" = "means",
                                 "Interquartile range" = "iq_range", 
                                 "Standard deviation" = "sd",
                                 "Coefficient of variation" = "cv"),
                  selected = "iq_range"),
      
      # Choose GCM: 
      selectInput(inputId = "gcm",
                  label = "Choose GCM",
                  choices = c("10-model mean", gcms),
                  selected = "CNRM-CM5"),
      
      radioButtons(inputId = "significance_values",
                   label = "Show significance or numeric values?",
                   choices = list("Significance" = "significance", 
                                  "Numeric values" = "values"),
                   selected = "values")
      
    ), # end sidebar panel. 
    
    # Start main panel. --------------
    mainPanel(

        # Map
        column(width = 9,
               plotOutput("plot1", height = 700,
                          click = "plot1_click",
                          dblclick = "plot1_dblclick",
                          brush = brushOpts(
                            id = "plot1_brush",
                            resetOnNew = TRUE
                          ))),

        # PDF. 
        column(width = 12, plotOutput("plot2"))  
        


    )# end main panel.
    
  ) # end sidebar layout.


)# End UI -------------------------------

# Server -----------------------------
server <- function(input, output) {
  ranges <- reactiveValues(x = c(-124,-103.1562), y = c(32, 52))

  # Make a map. 
  output$plot1 <- renderPlot({
    make_map(dat,
             case = input$case,
             swe_variable = input$swe_variable,
             variability_metric = input$variability_metric,
             gcm = input$gcm,
             significance_values = input$significance_values) + 
      coord_quickmap(ylim = ranges$y, xlim = ranges$x) 
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- c(-124,-103.1562)
      ranges$y <- c(32, 52)
    }
  })
  

  # Make the PDF plot. 
  output$plot2 <- renderPlot({
     make_pdfs(click_info = input$plot1_click,
               annual_template = annual_template,
               gcm = input$gcm)
   })
} # End server. 

shinyApp(ui, server)
