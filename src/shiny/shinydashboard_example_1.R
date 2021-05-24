library(shiny)
library(shinydashboard)
library(MASS)
library(platexpress)
source("../platexpress_module/platexpress_interactions.R")

#options(shiny.reactlog = TRUE)



#ui created with shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Platexpress Dashboard"),
  #sidebar element with tabs
  dashboardSidebar(
    sidebarMenu(
      menuItem("Plate Layout", tabName = "layout", icon = icon("file-upload")),
      menuItem("Plate Data", tabName = "data", icon = icon("filter")),
      menuItem("View Groups", tabName = "groups", icon = icon("vials")),
      menuItem("MatrixLayout", tabName = "matlay", icon = icon("outdent"))
    )
  ),
  #definitions of a tab content inside dashboardBody
  #ui's are reachable with tabItems(tabItem(tabName),tabItem(...))
  dashboardBody(
    tabItems(
      tabItem(
      #ui for readPlateMap 
        tabName="layout",
        fluidRow(
          box(
            textInput("blankId", "blank description", "blank"),
            selectInput("sep", "Separator:",
                        c("Tab" = "\t",
                          "Semicolon" = ";",
                          "Comma" = ",",
                          "Dot" = ".",
                          "Double-Dot"=":")),
		    #within-field separator
            selectInput("fsep", "Field Separator:",
                        c("Newline" = "\n",
                          "Tab" = "\t",
                          "Semicolon" = ";",
                          "Comma" = ",",
                          "Dot" = ".",
                          "Double-Dot"=":")),
            textInput("fields", "names for the field descriptor columns"),
            textInput("afields","field names which hold substance:amount pair info"),
            textInput("asep","substance:amount separator"),
            fileInput("file1", "Choose a plate layout CSV File", accept = ".csv"),
            #checkboxInput("header", "Header", TRUE),
		        actionButton("changeLayout","Change Layout", icon = icon("file-upload"),class="btn btn-secondary")
		        #dismiss all other submitButton's from the ui otherwise the actionButton's wan't
		        #work anymore...
            #submitButton("Submit")
          ),
          box(
            tableOutput("simplecsv"),
            tableOutput("plate")
          )
        )        
      ),
      tabItem(
      #ui for readPlateData 
        tabName = "data",
        fluidRow(
          column(
            12,
            plotOutput("data")
            )
        ),
        fluidRow(
          box(
            
            textInput("dataIds", "data ids for readPlateData", "Riboflavine, Biomass"),
            textInput("dec","Decimal separator","."),
            textInput("skipWells","Wells to skip"),
            selectInput("variable", "Plate type:",
                        c("BMG Optima and Mars v3.01 R" = "BMG",
                          "BMG Clariostar and Mars vXXX" = "BMG2",
                          "Biotek Synergy Mx" = "Synergy",
                          "BioLector" = "BioLector",
                          "BioLector Pro"="BioLectorPro")),
            fileInput("file2", "Choose a plate data CSV File", accept = ".csv"),
            actionButton("loadData","Load Data",class="btn btn-secondary")
            #submitButton("Submit")
          ),
          box(
            checkboxGroupInput("checkGroup", 
                               h3("Select rows to display"), 
                               choices = list("Row A" = "A", 
                                              "Row B" = "B", 
                                              "Row C" = "C",
                                              "Row D" = "D",
                                              "Row E" = "E",
                                              "Row F" = "F",
                                              "Row G" = "G",
                                              "Row H" = "H"),
                               selected = 1),
            
            numericInput("num", 
                         h3("Select the last column to display"), 
                         value = 12,
                         min = 1,
                         max = 12),
            box(textOutput("PlateDateErrorText")
            )
          )
        )

      ),
      tabItem(
      #ui for viewGroups
        tabName="groups",
        fluidRow(
          box(
            column(
              12,
              numericInput(
                "linewidth",
                h3("select the linewidth of the graph"),
                value = 0,
                min=0,
                max=10
              ),
              textInput(
                "groups",
                "groups to display"
              ),
              sliderInput(
                "nrows",
                "Number of rows to display",
                value=1,
                min = 1,
                max = 12
              ),
              actionButton("analyseGroups","Load Group Graph",class="btn btn-secondary")
              #submitButton("Submit")
            )
          )
        ),
        fluidRow(
          box(
            plotOutput("groupPlot")
          )
        )

        
      ),
        tabItem(
        #ui for Layoutcreation
        tabName = "matlay",
        fluidRow(
          box(
            column(
              12,
              matrixInput(
                "sample",
                value = m,
                rows = list(
                  extend = TRUE,editableNames = TRUE
                ),
                cols = list(
                  names = TRUE,editableNames = TRUE,extend = TRUE
                )
              ),
              actionButton("SaveMatrix","Create Layoutfile", icon = icon("download"),class="btn btn-secondary")
              
           )
          ),
          box(title = 'Output Preview',
              collapsible = TRUE,
              collapsed = T,
              status = 'primary',
            column(12,
            tableOutput("testmat")
            )
          ),
          box(
            title=' ShinyMatrix Help',
            collapsible = TRUE,
            collapsed=TRUE,
            status = 'primary',
            Helptext_matlay_col
          ),
        )
      )
    )#
  )
)

server <- function(input, output,session) {
  #reactive functions / does only reload if input changes
  #maybe not needed because of submit buttons
  getLayout <- reactive({
    readPlateLayoutFile(input)
  })
  getPlateData <- reactive({
     readDataFile(input)
  })
  
  output$simplecsv <- renderTable({
    req(input$file1)
    read.csv(input$file1$datapath)
  })
  #actual output
  #table output creation triggered by action button of layout tab
  plateLayout <- eventReactive(input$changeLayout,{
    getLayout()
  })
  
  output$plate <- renderTable({
    plateLayout()
  })
  
  #plot output creation triggered by action button of data tab
  plateData <- eventReactive(input$loadData,{
    plate <- getLayout()
    readPlateDataFile(input,plate)
  })
  output$data <- renderPlot({
    plateData()
  })
  
  #group plot output
  groupPlot <- eventReactive(input$analyseGroups,{
    if(input$groups == ""){
      readPlateDataFile(input,getLayout())
    } else {
      getGroupPlot(input,getLayout())
    }    
  })
  
  output$groupPlot <- renderPlot({
    groupPlot()
  })
  #####
  #shinyMatrix
  observeEvent(input$SaveMatrix ,{write.table(input$sample,col.names=NA,'../../Layout.csv')})
  
  #matcsv <- observeEvent(input$SaveMatrix ,{matcsv_helper(input,"../../Layout.csv")
   # print("Funktioniert das ?")})
  
  output$testmat <- renderTable(input$sample)
  #####
  #ErrorHandling
  
  output$PlateDateErrorText <- renderText({
    PlateDateError()
  })
  PlateDataError <- function (input,output){
    reactive(input$dataIds, {
      #print(paste0("You have chosen: ", input$checkGroups))
      #when(!is.null(input$dataIds),print(input$dataIds))
      print(input$dataIds)
    })
  }
  ####
  isthereaFile <- observeEvent (input$changeLayout, {
    validate(
      need(!is.null(input$file1),"Plea se upload a file!")
    )
  })
  
}

shinyApp(ui, server)

