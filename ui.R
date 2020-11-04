library(shiny)

ui <- fluidPage(
  title = "Data-Explo",
  sidebarLayout(
    sidebarPanel(
      
      # UI for view panel ----
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
      
      # UI for visualize panel ----
      conditionalPanel(
        'input.type == "Visualize"',
        
        uiOutput("variable"), 	# depends on dataset ( set by output$variable in  server.R)
        uiOutput("group"),  		# depends on dataset	( set by output$group in  server.R)
        selectInput("plot_type","Plot Type:", 
                    list(boxplot = "boxplot", histogram = "histogram", density = "density", bar = "bar")),
        checkboxInput("show_points", "show points", TRUE)
      )
    ),
    mainPanel(
      h3(textOutput("caption")),
      tabsetPanel(
        id = "type",
        tabPanel("View", DT::dataTableOutput("view")),
        tabPanel("Visualize", plotOutput("visualize"))
      )
    )
  )
)