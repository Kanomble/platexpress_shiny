#bioleqtor
library(shiny)
library(shinydashboard)
library(MASS)
library(platexpress)
library(rhandsontable)
library(dplyr)
library(tidyr)
getwd()
setwd

source("../platexpress_module/platexpress_interactions.R")

#options(shiny.reactlog = TRUE)



#ui created with shinydashboard
ui <- dashboardPage(
  dashboardHeader(title = "Platexpress Dashboard"),
  #sidebar element with tabs
  dashboardSidebar(
    sidebarMenu(
      menuItem("MatrixLayout", tabName = "matlay", icon = icon("outdent")),
      menuItem("Plate Layout", tabName = "layout", icon = icon("file-upload")),
      menuItem("Plate Data", tabName = "data", icon = icon("filter")),
      menuItem("View Groups", tabName = "groups", icon = icon("vials"))
      
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
                         max = 12)
            #box(textOutput("PlateDateErrorText")
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
          # box(
          column(
            12,
            
            rhandsontable::rHandsontableOutput('mat_sample'),
            br(),
            shiny::radioButtons('rad1','Platereader Type', choices = c('Synergy','Bioleq'), inline  = T),

            # matrixInput(
            #   value = m,
            #   rows = list(
            #     extend = TRUE,editableNames = TRUE
            #   ),
            #   cols = list(
            #     names = TRUE,editableNames = TRUE,extend = TRUE
            #   )
            # ),
            actionButton('ClearMatrix', 'Clear Values'),
            downloadButton('SaveMatrix'),
            hr()
            #actionButton("SaveMatrix","Create Layoutfile", icon = icon("download"),class="btn btn-secondary")
            
            # )
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
          )
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
  ##############################################################################
  #shinyMatrix
  #observeEvent(input$SaveMatrix ,{write.table(input$sample,col.names=NA,'../../Layout.csv')})
  
  data_hot <- reactiveVal(matrix('', 8, 12))
  
  observeEvent(input$ClearMatrix,{
    output$mat_sample <- renderRHandsontable({
      
      dff = matrix('', 8, 12)
      colnames(dff) = c(1:12)
      
      rhandsontable(dff, rowHeaders = T) %>% 
        hot_cols(colWidths = 120)
    })
    
  })
  
  output$mat_sample <- renderRHandsontable({
    
    dff = data_hot()
    colnames(dff) = c(1:12)
    
    rhandsontable(dff, rowHeaders = T) %>% 
      hot_cols(colWidths = 120)
  })
  
  
  output$SaveMatrix <- downloadHandler(
    filename = function() {
      paste0(input$rad1,'_layout.csv')
    },
    content = function(file) {
      v = hot_to_r(input$mat_sample)
      # vrm = which(v[,1] == '')
      # if(length(vrm)>0){
      #   v = v[-vrm,]
      # }

      
      if(input$rad1 == 'Synergy'){
        
        results = '\t'
        results =  paste0(results, paste(1:12, collapse =  '\t'))
        #results = paste0(results)
        for(i in 1:nrow(v)){
          
          row_i = LETTERS[i]
          print(v[i,])
          res_i = paste0(row_i, '\t')
          for(k in 1:ncol(v)){
            
            vik = v[i,k] %>% as.character()
            
            
            
            if(vik!=''){
              #print(vik)
              if(vik == 'blank'){
                res_i = paste0(res_i, 
                               vik,
                               '\t',
                               
                               '')
                
              }else{
                res_i = paste0(res_i, 
                               '\"',strsplit(vik,' ')[[1]][1],
                               '\n', 
                               strsplit(vik,' ')[[1]][2],
                               "\"",
                               '\t',
                               '')
              }
              
            }else {
              res_i = paste0(res_i, 
                             vik,
                             '\t',
                             '')
              #print(paste0("*",i,k))
              
              
            }
            
            
          }
          results = paste(results,'\n',res_i )
          #results = paste0 ('"', v, '"')
          #results = dQuote(v[2,2])
          results = gsub(" ", "", results, fixed = TRUE)
          
          
        }
      }else{
        ###
        results = ';'
        #print(paste0("1",results))
        results =  paste0(results, paste(1:8, collapse = ';' ))
        #print(paste0("2",results))
        results = paste0(results)
        #print(paste0("3",results))
        for(i in 1:nrow(v)){
          
          row_i = LETTERS[i]
          
          res_i = paste0(row_i,';')
          #print(paste0("1",res_i))
          for(k in 1:ncol(v)){
            
            vik = v[i,k] %>% as.character()
            
            if(vik!=''){
              #print(vik)
              if(vik == 'blank'){
                res_i = paste0(res_i, 
                               vik,
                               '')
                #print(paste0("2",res_i))
              }else{
                res_i = paste0(res_i, 
                               '\"',
                               vik,
                               "\"",
                               
                               '',';')
                #print(paste0("3",res_i))
              }
            
              
            }
            
            
          }
          
          if(v[i,1] !=''){
            results = paste(results,'\n',res_i )
          }
          
          #if (nchar(res_i != 2))
             
          res_i = substr(res_i,1,nchar(res_i)-2)
          print(res_i)
          
          #results = paste0 ('"', v, '"')
          #results = dQuote(v[2,2])
          results = gsub(" ", "", results, fixed = TRUE)
          
          
          
          
        }
        ###
      }
      ###

      
      #download part
      write.matrix(results, file)
      #write.table(results,file,quote = "[v]")
      
      #write.csv(results, file)
    },
    contentType = NULL
    
  )
  # observeEvent(input$SaveMatrix ,{
  #   
  #   v = hot_to_r(input$mat_sample)
  #     save(v, file = 'v.Rdata')
  #   print(hot_to_r(input$mat_sample))
  #   # browser()
  #   # write.table(input$sample,'../../Layout.csv')
  #   #                               print(input$SaveMatrix)
  #                                 
  #                                 })
  
  #loop for /t and /n cleaning
  
  
  
  observeEvent(input$sample, {
    print(input$sample)
    print(class(input$sample))
  })
  
  
  #matcsv <- observeEvent(input$SaveMatrix ,{matcsv_helper(input,"../../Layout.csv")
  # print("Funktioniert das ?")})
  
  output$testmat <- renderTable(input$sample)
  
  ##############################################################################
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
      need(!is.null(input$file1),"Please upload a file!")
    )
  })
  
}

shinyApp(ui, server)