server <- function(input, output) {
  
  #### LOAD PACKAGES ####
  
  pkgs <- c('ggplot2', 'ggthemes', 'DataExplorer',
            'kableExtra', 'shiny', 'shinythemes')
  
  pkg_setup(pkgs)
  
  
  #### READ DATA ####
  
  data_in <- reactive({
    
    # input$file1 will be NULL initially.
    
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
  
  
  # OUTPUT
  
  
  output$view <- DT::renderDataTable({
    DT::datatable(data_in()[, input$show_vars, drop = FALSE],
                  rownames = FALSE,
                  filter = "top",
                  extensions = 'Buttons',
                  options = list(
                    dom = "tB",
                    buttons = c('csv', 'excel', 'pdf', 'print')
                  ))
  })
  
  output$introduce <- function(){
    tmp <- t(introduce(data_in()[, input$show_vars]))
  
    tmp %>% kbl(caption = 'Basic information for input data') %>%
      kable_styling()
  }
  
  
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
  
  #set caption
  output$caption<-renderText({
    switch(input$plot_type,
           "boxplot" 	= 	"Boxplot",
           "histogram" =	"Histogram",
           "density" 	=	"Density plot",
           "bar" 		=	"Bar graph")
  })
  
  
  plots <- eventReactive(input$plot_button, {
  
    #dynamic plotting options
    plot_type<-switch(input$plot_type,
                      "boxplot" 	= 	geom_boxplot(),
                      "histogram" =	geom_histogram(alpha=0.5,position="identity"),
                      "density" 	=	geom_density(alpha=.75),
                      "bar" 		=	geom_bar(position="fill")
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
      ) + plot_type+
        labs(
          fill 	= input$group,
          x 		= "",
          y 		= input$variable
        )
      
      if(input$show_points==TRUE)
      { 
        p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
      }
    }
      
    else if(input$plot_type=="bar") {
      
      p<-ggplot(data_in(), 
                aes(
                  x 		= data_in()[,input$variable], 
                  fill = as.factor(data_in()[,input$group])
                )
      ) + plot_type+
        labs(
        fill 	= input$group,
        x 		= input$variable,
        y 		= ""
      )
      
      
    } else {
      
      p<-ggplot(data_in(), 
                aes(
                  x 		= data_in()[,input$variable],
                  fill 	= as.factor(data_in()[,input$group]),
                  group 	= as.factor(data_in()[,input$group])
                  #color 	= as.factor(plot.obj$group)
                )
      ) + plot_type+
        labs(
          fill 	= input$group,
          x 		= input$variable,
          y 		= ""
        )
    }
    
    p<-p+theme
    print(p)
  })  
  
  
  # OUTPUT
  output$visualize <- renderPlot({
    plots()
  })
  
  
  #### DOWNLOAD ####
  
  
  output$download_button <- downloadHandler(
    filename = function() {
      paste0(input$plot_type, "_", input$variable, ".tiff")
    },
    content = function(file) {
      ggsave(filename = file, dpi = as.numeric(input$dpi), width=as.numeric(input$width), height=as.numeric(input$heigth), units=input$unit)
    }
  )
}
