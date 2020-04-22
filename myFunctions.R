library(readxl)
library(dplyr)
library(fmsb)

# Takes a list of data framees and rbinds them together whilst preserving the original rownames
rBindRetainRownames <- function(listToMerge){
  
  MergedDF <- do.call(rbind, unname(listToMerge))
  MergedDF
  
}

# User function to create a radar plot
MyPlot <- function(to_plot, title, include_legend=TRUE){
  
  to_plot=rbind(rep(4,nrow(to_plot)) , rep(0,nrow(to_plot)) , to_plot[,])
  
  # Radar plot
  colors_border=c( "blue","red1","green", "pink", "orange", "purple" ,"cyan","black", "violet" )
  radarchart(to_plot,plty=1, pcol=colors_border, plwd=3, title = title, caxislabels = c(0,1,2,3,4), axistype = 1)
  
  if (include_legend) {
    #legend(1.4,1,legend = rownames(to_plot[-c(1,2),]), pch=20, col=colors_border,cex=0.5, pt.cex=2)
    legend(1.4,0.25,legend = rownames(to_plot[-c(1,2),]), pch=20, col=colors_border,cex=0.5, pt.cex=2)
  }
}

ReadIndicatorData <- function(spreadsheetName, countryName){
  
  #countryName <- 'Spain'
  #spreadsheetName <- 'Table5A_withIndicators_2020.xlsx'
  
  my_data <- read_excel(spreadsheetName, sheet = countryName)
  
  # These are the columns we are interested in
  #positions <- c(1,20:25)
  positions <- c(1,6,20:32)
  
  #"Normal" column names - sometimes these are different for specific countries
  #UsualColNames <-colnames(my_data9)[positions]
  UsualColNames <- c("Table 5A: Quality assurance framework for biological data","...6","...20","...21","...22","...23","...24","...25","...26","...27","...28","...29","...30","...31","...32")   
  

  #if (  countryName == "Estonia"){
  if (grepl("Estonia", countryName, fixed=TRUE)){
    
    # Different column names used for Estonia - need to fix first before binding with the other data
    my_data <- select(my_data,positions)
    colnames(my_data)<-UsualColNames
  
    #} else if (countryName == "Finland"){
  } else if (grepl("Finland", countryName, fixed=TRUE)){
    
    # Different column positions used for Finland - need to fix first before binding with the other data
    #finData <- select(my_data11,c(1,25:30))
    my_data <- select(my_data,c(1,6,25:37))
    colnames(my_data)<-UsualColNames
    
  #} else if (countryName == "Lithuania"){
  } else if (grepl("Lithuania", countryName, fixed=TRUE)){
    
    # Different column names used for Lithuania - need to fix first before binding with the other data
    my_data <- select(my_data,positions)
    colnames(my_data)<-UsualColNames
    
  #} else if (countryName == "Ireland"){
  } else if (grepl("Ireland", countryName, fixed=TRUE)){
    
    # Different column names used for Ireland - need to fix first before binding with the other data
    my_data <- select(my_data,positions)
    colnames(my_data)<-UsualColNames
    
  #} else if (countryName == "Portugal"){
  } else if (grepl("Portugal", countryName, fixed=TRUE)){
    
    # Differet column names used for Portugal - need to fix first before binding with the other data
    my_data <- select(my_data,positions)
    colnames(my_data)<-UsualColNames
    
  #} else if (countryName == "Spain"){
  } else if (grepl("Spain", countryName, fixed=TRUE)){
    
    # Spain has some records with country as "ES" rather then "ESP" - fix this
    #espData <- my_data26
    my_data <- select(my_data,positions)
    
    my_data$`Table 5A: Quality assurance framework for biological data` <- as.character(my_data$`Table 5A: Quality assurance framework for biological data`)
    my_data$`Table 5A: Quality assurance framework for biological data`[my_data$`Table 5A: Quality assurance framework for biological data`=='ES']<-"ESP"

    
  } else {
    
    my_data <- select(my_data,positions)
    
  }
  
  my_data
  
}

readAndCleanData <- function(spreadsheetName, yearCalculated){
  
  #spreadsheetName <- 'Table5A_withIndicators_2020.xlsx'
  #yearCalculated <- 2020

  # Stick all the data frames from each country together
  total_data <- bind_rows(
    ReadIndicatorData(spreadsheetName,"Belgium"), 
    ReadIndicatorData(spreadsheetName,"Denmark"),
    ReadIndicatorData(spreadsheetName,"Estonia"), 
    ReadIndicatorData(spreadsheetName,"Finland"), 
    ReadIndicatorData(spreadsheetName,"France"), 
    ReadIndicatorData(spreadsheetName,"Germany"),
    ReadIndicatorData(spreadsheetName,"Ireland"), 
    ReadIndicatorData(spreadsheetName,"Latvia"),
    ReadIndicatorData(spreadsheetName,"Lithuania"), 
    ReadIndicatorData(spreadsheetName,"Netherlands"), 
    ReadIndicatorData(spreadsheetName,"Poland"),
    ReadIndicatorData(spreadsheetName,"Portugal"), 
    ReadIndicatorData(spreadsheetName,"Spain"), 
    ReadIndicatorData(spreadsheetName,"Sweden"), 
    ReadIndicatorData(spreadsheetName,"UK"), 
  )
  
  
  #View(total_data)

  colnames(total_data)=c("MS","SchemeName", "SamplingDesign","NonResponses","DataCapture","DataStorage","AccuracyBias","EditImpute","BS","NA","NSEA","LP","LDF","Rec.","Diad.")
  
  # Remove the rows we don't want e.g. the column headings and any extra blank rows
  final_data <- total_data[!is.na(total_data$MS),]
  final_data <- final_data [final_data$MS!="MS participating in sampling",]
  final_data <- final_data [final_data$MS!="MS",]
  
  # Add in the year we calcuated these indicators
  final_data$YearCalculated <- yearCalculated
  
  # get rid of any columns we don't want and put the year as the first column
  final_data <- final_data[,c("YearCalculated","MS","SchemeName", "SamplingDesign","NonResponses","DataCapture","DataStorage","AccuracyBias","EditImpute","BS","NA","NSEA","LP","LDF","Rec.","Diad.")]
  
  #View(final_data)
  
  # Convert factors to numbers
  # Introduces NAs for things that aren't numbers e.g. "?"
  final_data$SamplingDesign <- as.numeric(as.character(final_data$SamplingDesign))
  final_data$NonResponses <- as.numeric(as.character(final_data$NonResponses))
  final_data$DataCapture <- as.numeric(as.character(final_data$DataCapture))
  final_data$DataStorage <- as.numeric(as.character(final_data$DataStorage))
  final_data$AccuracyBias <- as.numeric(as.character(final_data$AccuracyBias))
  final_data$EditImpute <- as.numeric(as.character(final_data$EditImpute))
  
  final_data
  
}

readBlindEvaluations <- function(spreadsheetName, yearCalculated){
  
  #spreadsheetName <- 'Table5A_withIndicators_2020.xlsx'
  #yearCalculated <- 2020
  
  sheetNames <- excel_sheets(spreadsheetName)
  
  for (mySheet in sheetNames){

    
    for (otherSheet in sheetNames){
      if (mySheet == paste(otherSheet, "_2", sep ="")){
        print(mySheet)
        print(otherSheet)
        
      }
      
    }
    
  }
}

# Read the indicator definitions from a spreadsheet
readIndicatorDefinitions <- function(spreadsheetName){
  
  my_data2 <- read_excel(spreadsheetName, sheet = 'IndicatorDefinition') # Definition
  #View(my_data2)
  ind_define<-my_data2
  colnames(ind_define)=c("Col1","Indicator","Level 1","Level 2","Level 3","Level 4","NAs Allowed?")
  ind_define<-ind_define[!is.na(ind_define$Indicator),c("Indicator","Level 1","Level 2","Level 3","Level 4","NAs Allowed?")]
  
  ind_define
  
}

# Calculate the overall mean for all rows
calculateMeanForAllMS <- function(dataToUse){
  
  #dataToUse <- final_data
  
  MeanTotal <- data.frame(SamplingDesign=mean(dataToUse$SamplingDesign,na.rm = TRUE), NonResponse=mean(dataToUse$NonResponses,na.rm = TRUE), DataCapture=mean(dataToUse$DataCapture,na.rm = TRUE), DataStorage=mean(dataToUse$DataStorage,na.rm = TRUE), AccuracyBias=mean(dataToUse$AccuracyBias,na.rm = TRUE), EditImpute=mean(dataToUse$EditImpute,na.rm = TRUE))
  #View(MeanTotal)
  #dataToUse$YearCalculated[1]
  
  #rownames(MeanTotal)<-c("All MS")
  rownames(MeanTotal)<- dataToUse$YearCalculated[1]
  
  MeanTotal
  
}

# Calculate the mean for each indicator by MS
calculateMeanByMS <- function(dataToUse){
  
  MeanByMS <- data.frame(YearCalculated = numeric(0), MS = character(0), NumberOfRows = numeric(0), SamplingDesign = numeric(0), NonResponse = numeric(0), DataCapture = numeric(0), DataStorage= numeric(0), AccuracyBias= numeric(0), EditImpute= numeric(0))
  
  op <- par(mar=c(1, 2, 2, 1),mfrow=c(2, 2))
  
  for (myMS in unique(dataToUse$MS)  ){
    
    # Get the rows for the current MS
    msData <- dataToUse[dataToUse$MS==myMS,]
    
    # Get the mean values for the current MS
    msMean <- data.frame(YearCalculated = msData$YearCalculated[1], MS=myMS,  NumberOfRows=nrow(msData), SamplingDesign=mean(msData$SamplingDesign,na.rm = TRUE), NonResponse=mean(msData$NonResponses,na.rm = TRUE), DataCapture=mean(msData$DataCapture,na.rm = TRUE), DataStorage=mean(msData$DataStorage,na.rm = TRUE), AccuracyBias=mean(msData$AccuracyBias,na.rm = TRUE), EditImpute=mean(msData$EditImpute,na.rm = TRUE))
    
    rownames(msMean)<-  paste(myMS,msData$YearCalculated[1],sep="-")
    
    # Need to combine the data frames in this way to preserve the row names (these specify the country)
    MeanByMS <- rBindRetainRownames(list(a=MeanByMS, b=msMean))
    
  }
  
  MeanByMS <- MeanByMS[order(rownames(MeanByMS)),] 
  
  MeanByMS
  
}

# Calculate the mean for each indicator by Regio
calculateMeanByRegion <- function(dataToUse){
  
  #dataToUse <- final_data_2019
  
  regionalSummary <- data.frame(YearCalculated = numeric(0), Region = character(0), NumberOfRows = numeric(0), SamplingDesign = numeric(0), NonResponse = numeric(0), DataCapture = numeric(0), DataStorage= numeric(0), AccuracyBias= numeric(0), EditImpute= numeric(0))
  

  for (myColNumber in c(10,11,12,13,14,15,16)){
    
    regionName <- colnames(dataToUse)[myColNumber]
    #plotTitle <- colnames(dataToUse)[myColNumber]
    
    # Remove the rows which don't apply to the current region
    regionalData <- subset(dataToUse, !is.na(dataToUse[myColNumber]))
    regionalData <- regionalData[regionalData[myColNumber]=="X",]
    
    # Get the mean values for the current region
    regionalMean <- data.frame(YearCalculated = regionalData$YearCalculated[1], Region=regionName, NumberOfRows=nrow(regionalData), SamplingDesign=mean(regionalData$SamplingDesign,na.rm = TRUE), NonResponse=mean(regionalData$NonResponses,na.rm = TRUE), DataCapture=mean(regionalData$DataCapture,na.rm = TRUE), DataStorage=mean(regionalData$DataStorage,na.rm = TRUE), AccuracyBias=mean(regionalData$AccuracyBias,na.rm = TRUE), EditImpute=mean(regionalData$EditImpute,na.rm = TRUE))
    
    #rownames(regionalMean)<-c(plotTitle)
    rownames(regionalMean)<-  paste(regionName,regionalData$YearCalculated[1],sep="-")
    
    #regionalSummary <- bind_rows(regionalSummary, regionalMean )
    
    # Need to combine the data frames in this way to preserve the row names (these specify the region and year)
    regionalSummary <- rBindRetainRownames(list(a=regionalSummary, b=regionalMean))
    
    # Plot the data
    # Used this order for columns so that the indicators will read in a clockwise direction
    #data_to_plot<-regionalMean[,c(2,7:3)]
    

  }
  
  regionalSummary
  
}

#Find entries which do not have a value of 1,2,3, or 4 for the indicators
check1234<-function(dataToCheck){
  
  dataToReturn <- dataToCheck[!dataToCheck$SamplingDesign %in% c(1,2,3,4)  | !dataToCheck$NonResponses %in% c(1,2,3,4) | !dataToCheck$DataCapture %in% c(1,2,3,4) | !dataToCheck$DataStorage %in% c(1,2,3,4) | !dataToCheck$AccuracyBias %in% c(1,2,3,4) | !dataToCheck$EditImpute %in% c(1,2,3,4),c(1,2,3,4,5,6,7,8,9)]
  
}

#  Check whether rows with LDF, LP, Rec. or Diad. regions also have other regions  marked
checkRegions <- function(dataToCheck){
  
  #dataToCheck <- final_data_2019
  
  dataToCheck$NumberOfRegions <- ifelse(!is.na(dataToCheck[["BS"]]),1,0) + ifelse(!is.na(dataToCheck[["NA"]]),1,0)  + ifelse(!is.na(dataToCheck[["NSEA"]]),1,0)  + ifelse(!is.na(dataToCheck[["LP"]]),1,0)  + ifelse(!is.na(dataToCheck[["LDF"]]),1,0)  + ifelse(!is.na(dataToCheck[["Rec."]]),1,0)  + ifelse(!is.na(dataToCheck[["Diad."]]),1,0)

  # Check whether LDF, LP, Rec. or Diad. have other regions also marked
  errorsLDF <- dataToCheck[!is.na(dataToCheck$LDF) & dataToCheck$NumberOfRegions>1,]
  errorsLP <- dataToCheck[!is.na(dataToCheck$LP) & dataToCheck$NumberOfRegions>1,]
  errorsRec <- dataToCheck[!is.na(dataToCheck$Rec.) & dataToCheck$NumberOfRegions>1,]
  errorsDiad <-dataToCheck[!is.na(dataToCheck$Diad.) & dataToCheck$NumberOfRegions>1,]

  errorsToReturn <- rBindRetainRownames(list(a=errorsLDF, b=errorsLP, c=errorsRec, d=errorsDiad ))
  
  errorsToReturn
  
}

