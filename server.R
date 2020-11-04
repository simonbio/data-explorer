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
  
  #update variable and group based on dataset
  output$variable <- renderUI({ 
    selectInput(
      inputId = "variable", label = "Select the variable to plot:", 
      choices = names(data_in())) # uddate UI 				 
  }) 
  
  
  
  output$group <- renderUI({ 
    selectInput(
      inputId = "group", label = "Select the variable to group by:", 
      choices = names(data_in())) # uddate UI 				 
  }) 
  
  output$caption<-renderText({
    switch(input$plot_type,
           "boxplot" 	= 	"Boxplot",
           "histogram" =	"Histogram",
           "density" 	=	"Density plot",
           "bar" 		=	"Bar graph")
  })
  
  
  #set caption
  output$caption<-renderText({
    switch(input$plot_type,
           "boxplot" 	= 	"Boxplot",
           "histogram" =	"Histogram",
           "density" 	=	"Density plot",
           "bar" 		=	"Bar graph")
  })
  
  
  #plotting function using ggplot2
  output$visualize <- renderPlot({
    
    #dynamic plotting options
    plot_type<-switch(input$plot_type,
                      "boxplot" 	= 	geom_boxplot(),
                      "histogram" =	geom_histogram(alpha=0.5,position="identity"),
                      "density" 	=	geom_density(alpha=.75),
                      "bar" 		=	geom_bar(position="dodge")
    )
    
    #plotting theme
    theme<- theme(
      axis.line = element_line(colour = 'gray', size = .75),
      panel.background = element_blank(),
      plot.background = element_blank()
    )
    if(input$plot_type=="boxplot")	{		#control for 1D or 2D graphs 
      p<-ggplot(data_in(), 
                aes(
                  x 		= data_in()[,input$group], 
                  y 		= data_in()[,input$variable],
                  fill 	= as.factor(data_in()[,input$group])
                )
      ) + plot_type
      
      if(input$show_points==TRUE)
      { 
        p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
      }
      
    } else {
      
      p<-ggplot(data_in(), 
                aes(
                  x 		= data_in()[,input$variable],
                  fill 	= as.factor(data_in()[,input$group]),
                  group 	= as.factor(data_in()[,input$group])
                  #color 	= as.factor(plot.obj$group)
                )
      ) + plot_type
    }
    
    p<-p+labs(
      fill 	= input$group,
      x 		= "",
      y 		= input$variable
    )  +
      theme
    print(p)
  })
  
}
