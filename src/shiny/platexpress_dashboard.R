#required libraries
library(shiny)
library(shinydashboard)
library(platexpress)
library(rhandsontable)
library(dplyr)
library(dashboardthemes)
library(ggplot2)
library(shinycssloaders)
#R loads platexpress_interactions similar to a library
source("../platexpress_module/platexpress_interactions.R")
#enable the usage of reactlog which displays a reactivity graph
#to use reactlog press shift+F3 after the application is run
options(shiny.reactlog = TRUE)



#ui element creation with shinydashboard::dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "platexpress_shiny"),
  #creating a Sidebar and specifying the tabnames 
  #the order of the menu items corresponds to the order in the browser
  dashboardSidebar(
    sidebarMenu(
      menuItem("MatrixLayout", tabName = "matlay", icon = icon("outdent")),
      menuItem("Plate Layout", tabName = "layout", icon = icon("file-upload")),
      menuItem("Plate Data", tabName = "data", icon = icon("filter")),
      menuItem("View Groups", tabName = "groups", icon = icon("vials")),
      menuItem("Graphs", tabName = "grasta", icon = icon("chart-line"))
    )
  ),
  #definitions of a tab content inside dashboardBody
  #ui's are reachable with tabItems(tabItem(tabName),tabItem(...))
  
  #content of browserbody (shinydashboard::dashboardBody) is separated into multiple tabs
  #all tabs are inside shindashbaordy::tabItems
  #the generation of content inside each Tabs is specified in tabitem
  dashboardBody(
    tabItems(
      tabItem(
        #ui generation for platexpress::readPlateMap 
        tabName="layout",
        fluidRow(
          box(
            #input objects that allow to specify how a layout file is load in 
            textInput("blankId", "blank description", "blank"),
            #column seperator
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
            actionButton("changeLayout","Change Layout", icon = icon("file-upload"),class="btn btn-secondary")
          ),
          #output objects of the layout tab 
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
            actionButton("loadData","Load Data",class="btn btn-secondary",icon = icon("download"))
          
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
              actionButton("analyseGroups","Load Group Graph",class="btn btn-secondary",icon = icon("project-diagram"))
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
        #ui for layout file creation
        tabName = "matlay",
        fluidRow(
          # box(
          column(
            12,
            #generating interactive spreadsheet with rhandsontable::rHandsontableOutput 
            rhandsontable::rHandsontableOutput('mat_sample'),
            br(),
            #implementing shiny::radioButtons to specify which kind of layout file should be created
            shiny::radioButtons('rad1','Platereader Type', choices = c('Synergy','BiolecPro'), inline  = T),
            #if pressed clears all values from mat_sample
            actionButton('ClearMatrix', 'Clear Values',icon = icon("eraser")),
            #if pressed values inside rHandsontable getting downloaded as csv file 
            downloadButton('SaveMatrix',icon = icon("save")),
            #displays helptext for rHandsonTable
            actionButton('Help',tags$i('rhandsontable Help'),icon = icon("question")),
            hr()
          ),


        )
      ),
      tabItem(
        #Ui for graphs and stats
        tabName = "grasta",
        fluidRow(
          # box(
          column(
            12,
            #withSpinner displays a loading animation so its visible that the process is working
            plotOutput("grastaPlot") %>% withSpinner(),
            #allows the selction of the to plot dataID
            uiOutput('plot_id'),
            box(
              #allows the selection which ggplot plotdataids should plot
              selectInput("ggplot", "ggplot2:",
                          c("line graph" = "geom_line()",
                            "bar chart" = "geom_col()"))
              
              )
            )
            
            
            
          )
        )
      )
    )
  )



# server -----
server <- function(input, output,session) {
  #reactive function only reloads if inputs in 'layout' tab changes
  getLayout <- reactive({
    readPlateLayoutFile(input)
  })
  #reloads if input in 'data'tab changes
  getPlateData <- reactive({
    readDataFile(input)
  })
  #generate a static data table containing the data in file1
  output$simplecsv <- renderTable({
    req(input$file1)
    read.csv(input$file1$datapath)
  })
  #table output creation triggered by action button of layout tab
  plateLayout <- eventReactive(input$changeLayout,{
    getLayout()
  })
  #table with value of plateLayout is generated
  output$plate <- renderTable({
    plateLayout()
  })
  
  # reactive variable for plots inside 'grasta' tab
  output$plot_id <- renderUI({
    req(plotdataids())
    selectInput('plot_id','Select Plot', choices = names(plotdataids()))
  })
  
  #allows to plot ggplot objects 
  plotdataids <- reactive({
    #prevent scintific notation    
    options(scipen = 999)

    #assign data to dd1    
    dd1 = readDataFile( input)
    #dataIds to eliminate out of dataset    
    names_elim = c("Time", "xids", "wells","dataIDs")
    #assign plot_list with difference between names(dl) and names_elim
    plot_list <- setdiff(names(dd1),names_elim)
    
    print(plot_list)
    #generate empty list
    plots <- list()
    #p = element in plot_list 
    for(p in plot_list) {
      print(p)
      #only contents of p of dl are now transferred to v1 
      #v1 has four subdivisions time,temperatur,blank corrected data (data) and original data (org)
      v1 = dd1[[p]]
      #data is assigned to data1
      data1 = v1$data
      #dataframe with time and data 
      data1 = data.frame(Time = v1$time, data1)
      
      #pivots data1 in wide format in long format 
      data1 <- data1 %>% 
        pivot_longer(-Time)
      print(data1)
      #Depending on input$ggplot, the corresponding ggplot is output
      if(input$ggplot == 'geom_col()'){
        plot_p <-  data1 %>% 
          ggplot(aes(x = Time, y = value, fill = name)) +
          geom_col() +
          facet_wrap(~name) + 
          theme(legend.position = 'none') + 
          ylab('Measure') + 
          ggtitle(p)
      }else if(input$ggplot == 'geom_line()'){
        plot_p <-  data1 %>% 
          ggplot(aes(x = Time, y = value, color = name)) +
          geom_line() +
          facet_wrap(~name) + 
          theme(legend.position = 'none') + 
          ylab('Measure') + 
          ggtitle(p)
      }
      
      
      plots[[p]] = plot_p
      print(plots[[p]])
      
    }
    #plots wells of each data ID
    plots
  })
  
  
  #renderPLot assigns the ggplot of the in 'plot_id' selected data id
  output$grastaPlot <-renderPlot({
    req(input$plot_id)
    plotdataids()[[input$plot_id]]
  })
  # })
  
  
  
  #rhandsontable helptext 'mat_lay' tab
  observeEvent(input$Help, {
    showModal(modalDialog(
      title = tags$code("rhandsontable Helptext"),
      "> Enter your values into the table. You can use letters,
                      numbers and symbols as input. The cellsize adapt to the 
                      amount of values automatically. The script exspects an input
                      like 'EVC B1', separated by one space. If only one 
                      word/number/synbol is entered it throws an NA.",
      tags$p(),
      "> You can copy paste and mark cells by clicking on the 
                      cell/row/column you want to mark. By holding the bottom right
                      corner of a cell you can drag their content across the table
                      the content will be automatically copy and pasted.",
      tags$p(),
      ">With tab and the arrow keys you are able to move around the table.",
      tags$p(),
      ">The 'clear Values' button clears out the entire table and 
                      the Download' button creates a csv file with can be either 
                      opened or saved",
      easyClose = TRUE
    ))
  })
  
  
  #creation triggered by action button of 'data' tab
  plateData <- eventReactive(input$loadData,{
    plate <- getLayout()
    readPlateDataFile(input,plate)
  })
  #plot aboce object
  output$data <- renderPlot({
    plateData()
  })
  
  #group plot output if actionnButton in 'viewGroups' tab is pressed
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

#rHandsontablepart 'mat_lay' tab
  #create a 8x12 matrix to which other elements can take dependencies 
  data_hot <- reactiveVal(matrix('', 8, 12))
  

  #if actionButton 'ClearMatrix' is pressed all previous inputs 
  #are overwritten with a new mat_sample
  observeEvent(input$ClearMatrix,{
    output$mat_sample <- renderRHandsontable({
      
      dff = matrix('', 8, 12)
      colnames(dff) = c(1:12)
      
      rhandsontable(dff, rowHeaders = T) %>% 
        hot_cols(colWidths = 120)
    })
    
  })
  #generating of interactive spreadsheet
  output$mat_sample <- renderRHandsontable({
    #data_hot is the 8x12 matrix
    dff = data_hot()
    colnames(dff) = c(1:12)
    
    rhandsontable(dff, rowHeaders = T) %>% 
      hot_cols(colWidths = 120) 
  })
  #downloadhadler for downloading a platelayout csv file
  output$SaveMatrix <- downloadHandler(
    filename = function() {
      paste0(input$rad1,'_layout.csv')
    },
    content = function(file) {
      v = hot_to_r(input$mat_sample)
      #executed when radioButton 'Synergy'is selected
      if(input$rad1 == 'Synergy'){
        
        results = '\t'
        results =  paste0(results, paste(1:12, collapse =  '\t'))
        
        for(i in 1:nrow(v)){
          
          row_i = LETTERS[i]
          print(v[i,])
          res_i = paste0(row_i, '\t')
          for(k in 1:ncol(v)){
            
            vik = v[i,k] %>% as.character()
            
            
            
            if(vik!=''){
              
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
 
            }
            
            
          }
          results = paste(results,'\n',res_i )
          results = gsub(" ", "", results, fixed = TRUE)
          
          
        }
        #executed when RadioButton 'BiolecPro' is selected
      }else{
        
        results = ';'
        
        results =  paste0(results, paste(1:8, collapse = ';' ))
        results = paste0(results)
        for(i in 1:nrow(v)){
          
          row_i = LETTERS[i]
          
          res_i = paste0(row_i,';')
          for(k in 1:ncol(v)){
            
            vik = v[i,k] %>% as.character()
            
            if(vik!=''){
              if(vik == 'blank'){
                res_i = paste0(res_i, 
                               vik,
                               '')
              }else{
                res_i = paste0(res_i, 
                               '\"',
                               vik,
                               "\"",
                               
                               '',';')
              }
              
              
            }
            
            
          }
          
          if(v[i,1] !=''){
            results = paste(results,'\n',res_i )
          }
          
          
          res_i = substr(res_i,1,nchar(res_i)-2)
          print(res_i)
          
          results = gsub(" ", "", results, fixed = TRUE)
        }
        
      }
      
      
      
      #download part
      write.matrix(results, file)
    },
    contentType = NULL
    
  )


  
}

shinyApp(ui, server)


