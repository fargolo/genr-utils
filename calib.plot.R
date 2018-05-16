#Calibration plot with:
#Bars: Absolute observed vs. predicted values
#Point distance to line: Deviation from observed
#"scale" defaulted to 100. This makes distances to points 
#represented in percentages on the y axis

calib.plot <- function(outcomes,probs,g = 10,graph.tit="Calibration"){
  require(ggplot2)
  require(ResourceSelection)
  require(tidyr)
  hosm.data <- hoslem.test(x = outcomes, y= probs,g=g) #Performs and prints Hosm-Lem test
  print(hosm.data)
  freq.data <- data.frame(cbind(hosm.data$expected[,2],hosm.data$observed[,2])) #Extract observed and predicted values
  colnames(freq.data) <- c("expected","observed") #var names 
  freq.data$dist <- numeric(length=nrow(freq.data)) #distance variable (calibration)
  plot.scale <- 50
  while (plot.scale <= freq.data$expected[g] | plot.scale <= freq.data$expected[g]){
    plot.scale <- plot.scale+50
  }
  freq.data$dist <- freq.data$expected/(freq.data$observed)*(plot.scale/2) #calculates distance
  freq.data$decile <- rownames(freq.data) #rownames to 'deciles' var
  freq.data$decile <- relevel(as.factor(freq.data$decile),ref = rownames(freq.data)[1]) #Relevel
  rownames(freq.data) <- NULL #clear varnames
  print(freq.data) #bug check
  freq.data.gat <- gather(freq.data,key = "key",value="value",c(1:3)) #gather data to plot
  print(freq.data.gat) #bug check
  #Plot
  cal.plot <- ggplot()+
    geom_bar(data=freq.data.gat[1:(g*2),],aes(y = value,x=decile,fill=key),
             stat="identity",position="dodge")+
    scale_fill_discrete(name="Risk",labels=c("Observed","Predicted"))+
    ylab("Outcomes") + xlab("Decile group")+ggtitle(graph.tit)+
    ylim(0,plot.scale)+ scale_x_discrete(labels=as.character(1:(g)))
  cal.plot2 <- cal.plot+
    geom_point(data=freq.data.gat[((g*2)+1):(g*3),],aes(y=value,x=decile))+
    geom_abline(intercept=plot.scale/2,slope=0,linetype="dashed")
  
  return(list(cal.plot,cal.plot2,freq.data.gat))
  }

