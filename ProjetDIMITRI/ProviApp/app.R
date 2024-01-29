source("../procedures.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Provisionnement Non-Vie",titleWidth = 1000),
  
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(text="Data Base",tabName = "dataB",icon=icon("database")),
      menuItem(text="Evolution", tabName = "evol", icon = icon("chart-line")),
      menuItem(text="Chain-Ladder",tabName = "chainL", icon = icon("star")),
      menuItem(text = "Comparaison",tabName = "autreM", icon = icon("minimize"))
    )
    
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName="dataB",
              tabBox(id="t1", width = 12, 
                     tabPanel("Import", fileInput("file1", "Choose File",
                                                  accept = c("text/csv",
                                                             "text/comma-separated-values,text/plain",
                                                             ".csv",
                                                             ".xlsx")
                     ),icon=icon("file-import")),
                     tabPanel("Data", tableOutput("contents") , icon =icon("table") ,style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
                        )
                     
                    ),
              ),
      
      tabItem(tabName = "evol",
              tabBox(id = "t2", width = 12,
                     tabPanel("Graph",plotlyOutput(
                       outputId = "Id_plot1",
                       inline = FALSE,
                       reportTheme = TRUE)
                       )
                     )
              ),
      
      tabItem(tabName = "chainL",
              tabBox(id = "t3", width = 12,
                     tabPanel("facteur de Développement",plotlyOutput("factors")),
                     tabPanel("Hypothese1",tableOutput("Id_Hyp1"), style = "height:500px; overflow-y: scroll;overflow-x: scroll;"),
                     tabPanel("Hypothese2",plotlyOutput("Id_Hyp2"))
              )),
      
      tabItem(tabName = "autreM",
              tabBox(id = "t4", width = 12,
                     tabPanel("Mack",plotlyOutput("Id_plot2")),
                     tabPanel("Bootstrap",plotlyOutput("Id_plot3"))
              ))
        
      )
      )
    
  )
  

server <- function(input, output) {
 
  output$contents <- renderTable({
    file <- input$file1
    if (is.null(file)) {
      return(NULL)
    }
    import(file$datapath)
  })

  
  output$Id_plot1 <- renderPlotly(
    {
      file <- input$file1
      import(file$datapath) %>% evolutionGraph()
      
    }
  )
  
  output$Id_Hyp1 <- renderTable({
    file <- input$file1
    if (is.null(file)) {
      return(NULL)
    }
    import(file$datapath) %>%
      cummul() %>%
      independanceHyp()
  })
  
  output$factors <- renderPlotly({
    file <- input$file1
    if (is.null(file)) {
      return(NULL)
    }
    import(file$datapath) %>%
      fjGraph()
  })
  
  output$Id_Hyp2 <- renderPlotly(
    {
     file <- input$file1
     if (is.null(file)) {
       return(NULL)
     }
     df <- import(file$datapath)
     ccplot(df,sample(1:3,1))
     
    })
  
  output$Id_plot2 <- renderPlotly({
    file <- input$file1
    if (is.null(file)) {
      return(NULL)
    }
    df <- import(file$datapath)
    compare(df, "Mack")
  })
  
  output$Id_plot3 <- renderPlotly({
    file <- input$file1
    if (is.null(file)) {
      return(NULL)
    }
    df <- import(file$datapath)
    compare(df, "Boot")
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
