library(tidyverse)

Data <- readRDS("")

regress <-
  function(data,
           y,
           x,
           reg_type,
           controls = as.character(),
           family = NA) {
    x_express <- paste0(x, collapse = " + ")
    vars_express <- paste0(y, " ~ ", x_express)
    controls_express <- paste0(controls, collapse = " + ")
    varsWcont_express <- if (length(controls) == 0) {
      vars_express
    }
    else {
      paste0(vars_express, " + ", controls_express)
    }
    express <-
      paste0(reg_type, "(", varsWcont_express, ", data = ", data)
    express <- if (is.na(family)) {
      paste0(express, ")")
    }
    else {
      paste0(express, ", family = ", family, ")")
    }
    model <- eval(parse(text = express))
    return(model)
  }

predict_data <- function(data,
                         y,
                         axisX,
                         x,
                         reg_type,
                         controls = as.character(),
                         family = NA,
                         group=NA,
                         xSep=10){
  if (is.na(group)){
    x <- c(axisX,x)
    model <- regress(data=data,y=y,x=x,controls=controls,reg_type = reg_type,family=family)
    
    DataObj <- eval(parse(text=data))
    
    numX <- which(names(DataObj)==axisX)
    minX <- min(DataObj[,numX],na.rm=T)
    maxX <- max(DataObj[,numX],na.rm=T)
    
    expressVars <- c(x,controls)
    expressMean <- paste0(expressVars," = mean(DataObj$",expressVars,", na.rm = T)")
    expressMean[1] <- paste0(axisX," = seq(minX,maxX, (maxX-minX)/xSep)")
    expressMean <- paste0(expressMean,collapse = ", ")
    expressMean <- paste0("data.frame(",expressMean,")")
    PredData <- eval(parse(text=expressMean))
    predictModel <- predict(model, PredData, se.fit=T, type="response")
    
    #erreur <- tryCatch({
    #              predictModel <- predict(model, PredData, se.fit=T, type="response")
    #            },
    #              error = function(cond){
    #                message(paste0("Warning: really weak predictive model for ", y, " and ", axisX))
    #                TRUE
    #              }
    #              )
    #
    #if (erreur == TRUE) {
    #  GraphDataOut <- data.frame(y = y, axisX = axisX, "se" = NA, "ciLow" = NA, "ciHigh" = NA)
    #  return(GraphDataOut)
    #} else {
    #  predictModel <- predict(model, PredData, se.fit=T, type="response")
    #}
    
    predY <- predictModel$fit
    predX <- seq(minX,maxX, (maxX-minX)/xSep)
    se <- predictModel$se.fit
    
    GraphData <- data.frame(predY,predX,se)
    GraphData$ciLow68<-GraphData$predY - (1.96*GraphData$se)
    GraphData$ciHi68 <-GraphData$predY + (1.96*GraphData$se)
    names(GraphData) <- c(y,axisX,"se","ciLow","ciHigh")
    GraphDataOut <- GraphData
  }
  else {
    if (group %in% x){
      x <- c(axisX,x)
      model <<- regress(data=data,y=y,x=x,controls=controls,reg_type = reg_type,family=family)
      
      DataObj <- eval(parse(text=data))
      
      numX <- which(names(DataObj)==axisX)
      minX <- min(DataObj[,numX],na.rm=T)
      maxX <- max(DataObj[,numX],na.rm=T)
      x <- x[-which(x==group)]
      groupCat <- as.numeric(names(table(DataObj[which(names(DataObj)==group)])))
      
      for (i in groupCat){
        groupX <- paste0(group,"=",i)
        
        expressVars <- c(x,controls)
        expressMean <- paste0(expressVars," = mean(DataObj$",expressVars,", na.rm = T)")
        expressMean <- c(groupX,expressMean)
        expressMean <- c(paste0(axisX," = seq(minX,maxX, (maxX-minX)/xSep)"),expressMean)
        expressMean <- paste0(expressMean,collapse = ", ")
        expressMean <- paste0("data.frame(",expressMean,")")
        
        PredData <- eval(parse(text=expressMean))
        predictModel <- predict(model,PredData,se.fit=T,type="response")
        
        predY <- predictModel$fit
        predX <- seq(minX,maxX, (maxX-minX)/xSep)
        se <- predictModel$se.fit
        
        GraphData <- data.frame(predY,predX,se)
        GraphData$group <- i
        GraphData$ciLow68<-GraphData$predY - (1.96*GraphData$se)
        GraphData$ciHi68 <-GraphData$predY + (1.96*GraphData$se)
        GraphData$pValAxisX <- summary(model)$coefficients[2,4]  
        names(GraphData) <- c(y,axisX,"se", "group", "ciLow","ciHigh","pValAxisX")
        if (i==groupCat[1]){
          GraphDataOut <- GraphData
        }
        else {
          GraphDataOut <- rbind(GraphDataOut,GraphData)
        }
      }
    }
    else if (group %in% axisX){
      stop(paste0("Argument 'group=",group,"' can't be the same as axisX!"))
    }
    else {
      stop(paste0("Argument 'group=",group,"' must be present in x!"))
    }
  }
  return(GraphDataOut)
}


predict_data("Dt",
             "op_turnout2019",
             "cons_whiteWineDrink",
             x = c("female", "male"),
             reg_type = "glm",
             family = "binomial",
             group = "male")

predict_data("Dt",
             "op_turnout2019",
             "cons_whiteWineDrink",
             x = c("female", "male"),
             reg_type = "glm",
             family = "binomial",
             group = "female")
