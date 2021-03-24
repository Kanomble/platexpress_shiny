#helper functions for shiny examples
library(platexpress)
#all functions that are executed in the server part of the shiny app are
#written inside an tryCatch block:

#result = tryCatch({
#expr
#}, warning = function(warning_condition) {
#  warning-handler-code
#}, error = function(error_condition) {
#  error-handler-code
#}, finally={
#  cleanup-code
#})
#try(func,error)


#readPlateMap in tryCatch block
#why are input$fields checked? --> because of warnings in the readPlateLayoutFile function
checkFieldInputAndReturnPlate <- function(input,layoutFile){
  #code if fields specifications are given
  if(is.null(input$fields) == FALSE){
    layoutFields <- unlist(strsplit(input$fields,","))
    layoutFields <- trimws(layoutFields)
    if(input$afields != ""){
      #afields are delivered as a string e.g. "sample, strain, Glc ..."
      layoutAfields <- unlist(strsplit(input$afields,","))
      layoutAfields <- trimws(layoutAfields)
      plate <- readPlateMap(file = layoutFile$datapath,sep = input$sep, blank.id = input$blankId,fsep = input$fsep, fields = layoutFields, afields = layoutAfields, asep = input$asep)
      return(plate)
    } else {
      #don't use afields and asep (inducer like Glc or Ace are not delivered as input)
      plate <- readPlateMap(file = layoutFile$datapath,sep = input$sep, blank.id = input$blankId,fsep = input$fsep, fields = layoutFields)
      return(plate)
    }
  }
}

readPlateLayoutFile <- function(input) {
  out <- tryCatch(
    {
      #if input$fields is null no output is created
      return(checkFieldInputAndReturnPlate(input,layoutFile))
    },
    error=function(cond) {
      #output error message
      message(cond)
    },
    #the platexpress readPlateMap function outputs warnings...
    warning=function(cond) {
      #warning is always thrown in readPlateMap ...
      return(checkFieldInputAndReturnPlate(input,layoutFile))
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

      data.raw <- readDataFile(input)

      
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
  data.raw <- readPlateData(files=dataFile$datapath,data.ids = columns,type=input$variable,dec=input$dec)
  if(input$skipWells != ""){
    wellsToSkip <- unlist(strsplit(input$skipWells,","))
    wellsToSkip <- trimws(wellsToSkip)
    data.raw <- skipWells(data.raw,skip=wellsToSkip)
  }
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
      data.raw <- readDataFile(input)
      dataGroup <- unlist(strsplit(input$groups,","))
      dataGroup <- trimws(dataGroup)
      dataGroup <- getGroups(plate,by=dataGroup)
      return(viewGroups(data.raw,groups = dataGroup,lwd.orig = input$linewidth,nrow=input$nrows))
      
    },
    finally={
    }
  )    
  return(out)
}
