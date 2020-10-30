library(shiny)


ui <- fluidPage(
  title = "DataExplorer",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.type == "View"',
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
        'input.type == "Visualize"',
        
        selectInput("var_type", "Variable type", 
                    c("None",
                      Numerical = "numerical",
                      Categorical = "categorical"
                      )),
        
        # Numerical
        conditionalPanel(
          condition = "input.var_type == 'numerical'",
          uiOutput("ui_out1"),
          uiOutput("ui_out2"),
          actionButton("ui_out1_button", "Plot!")
        ),
        
        # Numerical
        conditionalPanel(
          condition = "input.var_type == 'categorical'",
          uiOutput("ui_out3"),
          uiOutput("ui_out4"),
          actionButton("ui_out3_button", "Plot!")
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        id = "type",
        tabPanel("View", DT::dataTableOutput("view")),
        tabPanel("Visualize", plotOutput("visualize"))
      )
    )
  )
)

server <- function(input, output) {
  require(ggplot2)
  require(ggthemes)
  
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
  
  
  #### VIEW ####
  
  
  # generate checkbox in the UI with variables for the chosen table
  output$vars_controls <- renderUI({
    vars <- names(data_in())
    checkboxGroupInput('show_vars', 'Columns in table:',
                       choices = vars, selected = vars)
  })

  output$view <- DT::renderDataTable({
    DT::datatable(data_in()[, input$show_vars, drop = FALSE])
  })
  
  
  #### VISUALIZE ####
  
  
  # generate input selector for numerical variable and outcome variable in the UI when numerical veriable type is chosen  
  observeEvent(input$var_type == "numerical",{
    
    output$ui_out1 <- renderUI({
      selectInput(
        inputId = "num_feature", label = "Select the feature to plot:", 
        choices = names(data_in()))
    })
    
    output$ui_out2 <- renderUI({
      selectInput(
        inputId = "outcome1", label = "Select the outcome variable to stratify by:", 
        choices = names(data_in()))
    })
  })
  
  
  # generate input selector for categorical variable and outcome variable in the UI when categorical veriable type is chosen  
  observeEvent(input$var_type == "categorical",{
    
    output$ui_out3 <- renderUI({
      selectInput(
        inputId = "cat_feature", label = "Select the feature to plot:", 
        choices = names(data_in()))
    })
    
    output$ui_out4 <- renderUI({
      selectInput(
        inputId = "outcome2", label = "Select the outcome variable to stratify by:", 
        choices = names(data_in()))
    })
  })
  
  
  
  
  # Respond to user inputs - action button "Plot"
  numerical_plot <- eventReactive(input$ui_out1_button, {
    
    p1 <- ggplot(data_in(), aes(x = data_in()[ ,input$num_feature], fill = factor(data_in()[ ,input$outcome1])))+geom_density(alpha = 0.5)+theme_fivethirtyeight()+
      scale_fill_manual(values = c("#999999", "#E69F00"))
    p1
  })
  
  # Respond to user inputs - action button "Plot"
  categorical_plot <- eventReactive(input$ui_out3_button, {
    
    p2 <- ggplot(data_in(), aes(x = data_in()[ ,input$cat_feature], fill = factor(data_in()[ ,input$outcome2])))+geom_bar(position = "fill")+theme_fivethirtyeight()+
      scale_fill_manual(values = c("#999999", "#E69F00")) + scale_x_discrete(labels  = c("Death Event:No","Death Event:Yes"))
    p2
  })
  
  
  
  
  # OUTPUT
  
  output$visualize <- renderPlot({
    numerical_plot()
  })
  
  output$visualize <- renderPlot({
    categorical_plot()
  })
}

shinyApp(ui, server)