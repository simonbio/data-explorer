library(shiny)


ui <- fluidPage(
  title = "DataExplorer",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.type == "view"',
        # Input: Select a file ----
        fileInput("file1", "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),

        # Horizontal line ----
        tags$hr(),

        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),

        # Input: Select separator ----
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","),

        # Input: Select quotes ----
        radioButtons("quote", "Quote",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),

        
        
        # # Horizontal line ----
        # tags$hr(),
        ## Uncomment below to allow the user to select between head or all display. Only useful when DT::datatable is not used to output table. 
        # # Input: Select number of rows to display ----
        # radioButtons("disp", "Display",
        #              choices = c(Head = "head",
        #                          All = "all"),
        #              selected = "head"),
        
        # Horizontal line ----
        tags$hr(),
        
        # placeholder for dynamic UI
        uiOutput("vars_controls")
        
      ),
      conditionalPanel(
        'input.type == "visualize"',
        helpText("Click the column header to sort a column.")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'type',
        tabPanel("view", DT::dataTableOutput("mytable1")),
        tabPanel("visualize", DT::dataTableOutput("mytable2"))
      )
    )
  )
)

server <- function(input, output) {
  
  data_in <- reactive({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)

    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    ## Uncomment below to allow the user to select between head or all display. Only useful when DT::datatable is not used to output table.
    # if(input$disp == "head") {
    #   return(head(df))
    # }
    # else {
    #   return(df)
    # }
  })
  
  # generate checkbox in the UI with variables for the chosen table
  output$vars_controls <- renderUI({
    vars <- names(data_in())
    checkboxGroupInput('show_vars', 'Columns in table:',
                       choices = vars, selected = vars)
  })

  output$mytable1 <- DT::renderDataTable({
    DT::datatable(data_in()[, input$show_vars, drop = FALSE])
  })
  

  
}

shinyApp(ui, server)