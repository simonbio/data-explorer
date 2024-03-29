

ui <- fluidPage(theme = shinytheme("united"),
  
  title = "Data-Viewer",
  titlePanel("Data-Viewer"),
  tags$b("Author:"), (" Simon Kønig"),
  tags$hr(),
  
  sidebarLayout(
    sidebarPanel(
      helpText("This app is intented as a graphical user interface for non R users. It allows the user to carry out basic data
               manipulation as well as exploring the data visually with various plot types. The plot functionalities are setup
               to plot based on a predictor variable (feature) and a categorical outcome (label)"),
      
      # Horizontal line ----
      tags$hr(),
      
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
        
        selectInput("plot_type","Plot Type:", 
                    list(boxplot = "boxplot", histogram = "histogram", density = "density", bar = "bar")),
        uiOutput("variable"), 	# depends on dataset ( set by output$variable in  server.R)
        uiOutput("group"),  		# depends on dataset	( set by output$group in  server.R)
        checkboxInput("show_points", "show points", TRUE),
        # Horizontal line ----
        tags$hr(),
        
        actionButton("plot_button", "Plot!"),
        
        downloadButton("download_button", "Download"), 
        
        br(),
        br(),
        tags$b("Specify Width, Height and dpi of TIFF file"), ("(default are 17.8 x 17.8 cm - 300dpi):"),
        br(),
        br(),
        textInput("width", "Width of the TIFF file", ""),
        textInput("heigth", "Height of the TIFF file", ""),
        selectInput("unit", "Units", 
                    c("cm", "mm", "in")),
        selectInput("dpi", "Resolution", 
                    c("300", "600", "150"))
        
      )
      
    ),
    mainPanel(
      tabsetPanel(
        id = "type",
        tabPanel("View", fluidRow(DT::dataTableOutput("view")), 
                 tags$hr(),
                 fluidRow(tableOutput("introduce"))),
                 
    
        tabPanel("Visualize",
                 h3(textOutput("caption")),
                 plotOutput("visualize"))
      )
    )
  )
)