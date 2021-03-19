#helper functions for shiny examples
library(platexpress)

#readPlateMap in tryCatch block
readPlateLayoutFile <- function(input) {
  out <- tryCatch(
    {
      layoutFile <- input$file1
      ext <- tools::file_ext(layoutFile$datapath)
      req(layoutFile)
      validate(need(ext == "csv", "Please upload a csv file"))
      layoutFields <- unlist(strsplit(input$fields,","))
      layoutFields <- trimws(layoutFields)
     
      if(input$afields != ""){
        
        layoutAfields <- unlist(strsplit(input$afields,","))
        layoutAfields <- trimws(layoutAfields)
        plate <- readPlateMap(file = layoutFile$datapath,sep = input$sep, blank.id = input$blankId,fsep = input$fsep, fields = layoutFields, afields = layoutAfields, asep = input$asep)
        return(plate)
      } else {
   
        plate <- readPlateMap(file = layoutFile$datapath,sep = input$sep, blank.id = input$blankId,fsep = input$fsep, fields = layoutFields)
        return(plate)
      }
    },
    error=function(cond) {
      print(cond)
      message(cond)
    },
    warning=function(cond) {
      #warning is always thrown in readPlateMap ...
      layoutFields <- unlist(strsplit(input$fields,","))
      layoutFields <- trimws(layoutFields)
      
      plate <- readPlateMap(file = layoutFile$datapath,sep = input$sep, blank.id = input$blankId,fsep = input$fsep, fields = layoutFields)
      return(plate)
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
      # If you want more than one expression to be executed, then you 
      # need to wrap them in curly brackets ({...}); otherwise you could
      # just have written 'finally=<expression>' 
    }
  )    
  return(out)
}

#readPlateData

#readPlateMap in tryCatch block
readPlateDataFile <- function(input,plate) {
  out <- tryCatch(
    {
      dataFile <- input$file2
      ext <- tools::file_ext(dataFile$datapath)
      req(dataFile)
      validate(need(ext == "csv", "Please upload a csv file"))
      
      columns <- unlist(strsplit(input$dataIds,","))
      columns <- trimws(columns)

      data.raw <- readPlateData(files=dataFile$datapath,data.ids = columns,type=input$variable)
      data <- correctBlanks(data=data.raw, plate=plate)
      
      #user response
      if(!is.null(input$checkGroup) && !is.null(input$num)){
        return(viewPlate(data,rows = input$checkGroup,cols=1:input$num))
      } else {
        return(viewPlate(data))
      }
    },
    error=function(cond) {
      message(cond)
    },
    warning=function(cond) {
      message(cond)
      data.raw <- readPlateData(files=dataFile$datapath,data.ids = columns,type=input$variable)
      viewPlate(data.raw)
    },
    finally={
      # NOTE:
      # Here goes everything that should be executed at the end,
      # regardless of success or error.
    }
  )    
  return(out)
}

readDataFile <- function(input){
  dataFile <- input$file2
  ext <- tools::file_ext(dataFile$datapath)
  req(dataFile)
  validate(need(ext == "csv", "Please upload a csv file"))
  columns <- unlist(strsplit(input$dataIds,","))
  columns <- trimws(columns)
  data.raw <- readPlateData(files=dataFile$datapath,data.ids = columns,type=input$variable)
  return(data.raw)
}

getGroupPlot <- function(input,plate) {
  out <- tryCatch(
    {
      data.raw <- readDataFile(input)
      dataGroup <- unlist(strsplit(input$groups,","))
      dataGroup <- trimws(dataGroup)
      dataGroup <- getGroups(plate,by=dataGroup)
      return(viewGroups(data.raw,groups = dataGroup,lwd.orig = input$linewidth,nrow=input$nrows))
    },
    error=function(cond) {
      message(cond)
    },
    warning=function(cond) {
      message(cond)
    },
    finally={
    }
  )    
  return(out)
}
