##########################################################################
# title      : Automate bivariate tables
# description: I lost count on how many bivariate tables I've run.
#              Sometimes, people want to discuss them. Sometimes, 
#              hypothesis testing. Avoid tedious work when comparing
#              groups. 
# usage      :       
# inputs     : (1) dataframe, (2) list of numeric variables in dataframe
#              (3) list of categhorical variables in dataframe
#              (4) value for groups, (5) decimal plates
#              (6) decimal plates for p value
# 
#              Decide if data has normal distribution.
#              2.1 - Case yes: mean +- std
#              2.2 - Case no : median (tertiles)
# returns    : table with summary of variables and p values
# warning    : this code was written when I was just started
#              learning R revisiting programming. 
#              It means cryptic code and 'suboptimal' style :)

bivtab <- function (data,var.num,var.cat,var.group, dec.plat= 1,dec.p = 3){
  print ("Variaveis categoricas e de grupo devem ser fatores")
  require(plyr)
  require(e1071)
  length.cat.f <- function(){
    leng.sub <- 0
    for (i in 1:length(var.cat)){
      cur.var.len <- length(levels(data[[var.cat[i]]]))
      leng.sub <- leng.sub + cur.var.len 
    }
    return(leng.sub)
  }
  leng.sub <- length.cat.f()
  #Defines cathegorical variables col length: 1 line for each cathegory level + 1 line for each cat name
  len.cat <- leng.sub + length(var.cat)
  #Defines row length: 1 for each numerical var + catheg var row len 
  row.len <- length(var.num) + len.cat 
  #Defines col lenght: one for each group + 1 for total + 1 for p values
  col.len <- length(levels(data[[var.group]])) + 2
  #Defines lenght of final table
  final.tab <- matrix (nrow = row.len, ncol = col.len)

  #Col names for groups, starts from col 3 and loops for every var
  cur.lev <- 1
  vec.group <- character(length=length(levels(data[[var.group]])))
  for(i in 1:length(levels(data[[var.group]]))){
    vec.group[i] <- paste(levels(data[[var.group]])[cur.lev],"(n =",table(data[var.group])[[cur.lev]],")")
    cur.lev <- cur.lev + 1
  }
  
  #Heading names
  colnames(final.tab) <- c(paste("Total (n =",nrow(data),")"), vec.group, "p value")
  
  #Defines var names for num variables
  cur.lin <- 1
  vec.num = character(length=length(var.num))
  for (i in 1:length(var.num)){
    vec.num[i] <- var.num[i]
  }
  #Defines var names for cat variables
  cur.i <- 1
 
  vec.cat <- character (length=leng.sub + length(var.cat))
  for (i in 1:length(var.cat)){
    vec.cat[cur.i] <- var.cat[i]
    print(var.cat[i])
    cur.i <- cur.i + 1
    for (n in 1:length(levels(data[[var.cat[i]]]))){
      vec.cat[cur.i] <- levels(data[[var.cat[i]]])[n]
      cur.i <- cur.i + 1
    }
    
  }
  rownames(final.tab) <- c(vec.num,vec.cat)
  #Goes trought each numerical var, plots histogram, tests normality  and asks for classification
  #saving as logical values in var.num.norm vector, with same length as var.num
  var.num.norm <- logical(length = length(var.num))
  for (i in 1:length(var.num)){
    qqnorm(data[var.num][[i]])
    qqline(data[var.num][[i]])
    boxplot(data[var.num][[i]],xlab=var.num[i], main = var.num[i])
    hist(data[var.num][[i]],xlab=var.num[i], main = var.num[i])
    if(nrow(data) <5000){ print(shapiro.test(data[var.num][[i]]))}
    cur.skewness <- skewness(data[var.num][[i]],na.rm=TRUE)
    print(paste("Skewness:",cur.skewness))
    cur.kurtosis <- kurtosis(data[var.num][[i]],na.rm=TRUE)
    print(paste("Kurtosis:",cur.kurtosis))
    message <- sprintf("Is %s normal?",var.num[i])
    print(message)
    cur.log.val <- readline(prompt = "Y/N:")
    if (cur.log.val == "Y")
      var.num.norm[i] <- TRUE
    else if (cur.log.val == "N")
      var.num.norm[i] <- FALSE
    else {
      print("Wrong input")
    }
  }
  #Goes throught each numerical var, putting in the final.tab 1st col, mean+-sd or media(iq) depending on normality
  for (i in 1:length(var.num)){
  cur.var.mean <- mean(data[var.num][[i]],na.rm=TRUE)
  cur.var.sd <- sd(data[var.num][[i]],na.rm=TRUE)
  cur.var.med <- median(data[var.num][[i]], na.rm=TRUE)
  cur.var.q25 <- quantile(data[var.num][[i]],na.rm=TRUE)[[2]] 
  cur.var.q75 <- quantile(data[var.num][[i]],na.rm=TRUE)[[4]]
  if (var.num.norm[i] == TRUE){
    final.tab[i,1] <- paste(format(round(cur.var.mean,digits=dec.plat),nsmall=dec.plat),
                            "\U00b1",
                            format(round(cur.var.sd,digits=dec.plat),nsmall=dec.plat),
                            sep = "")
  }
  else if(var.num.norm[i] == FALSE){
    final.tab[i,1] <- paste(format(round(cur.var.med,digits=dec.plat),nsmall=dec.plat),
                            "(",
                            format(round(cur.var.q25,digits=dec.plat),nsmall=dec.plat),
                            "-",
                            format(round(cur.var.q75,digits=dec.plat),nsmall=dec.plat),
                            ")",
                            sep = "")
  }  
  }
  #Goes throught each line after num vars, pasting freq(percent) for cat vars
  cur.lin <- length(var.num)
  for (i in 1:length(var.cat)){
    cur.lin <- cur.lin + 1
    final.tab[cur.lin,1] <- NA
    var.count <- table(data[[var.cat[i]]][!is.na(data[[var.cat[i]]] == TRUE)])
    for (n in 1:length(levels(data[[var.cat[i]]]))){
      cur.lin <- cur.lin + 1
      cur.percent <- (var.count[[n]]/length(data[[var.cat[i]]]))*(100)
      final.tab[cur.lin,1] <- paste(var.count[[n]],
                                    " (",
                                    format(round(cur.percent,digits=dec.plat),nsmall=dec.plat),
                                    ")",
                                    sep = "")
    }
  }
  #sets row 1 and col 2
  cur.col <- 2
  cur.lin <- 1
  #loops for col, subseting data according to group 
  for (i in 1:length(levels(data[[var.group]]))){
    cur.group <- levels(data[[var.group]])[[i]]
    cur.data <- data[(data[[var.group]] == cur.group),]
    #loops for vars/rows, calculating statistics for each, using subset data
    for (n in 1:length(var.num)){
      cur.var <- var.num[n]
      cur.var.mean <- mean(cur.data[[cur.var]],na.rm=TRUE)
      cur.var.sd <- sd(cur.data[[cur.var]],na.rm=TRUE)
      cur.var.med <- median(cur.data[[cur.var]],na.rm=TRUE)
      cur.var.q25 <- quantile(cur.data[[cur.var]],na.rm=TRUE)[[2]] 
      cur.var.q75 <- quantile(cur.data[[cur.var]],na.rm=TRUE)[[4]]
      if (var.num.norm[n] == TRUE){
        final.tab[n,cur.col] <- paste(format(round(cur.var.mean,digits=dec.plat),nsmall=dec.plat),
                                "\U00b1",
                                format(round(cur.var.sd,digits=dec.plat),nsmall=dec.plat),
                                sep = " ")
      }
      else if(var.num.norm[n] == FALSE){
        final.tab[n,cur.col] <- paste(format(round(cur.var.med,digits=dec.plat),nsmall=dec.plat),
                                "(",
                                format(round(cur.var.q25,digits=dec.plat),nsmall=dec.plat),
                                "-",
                                format(round(cur.var.q75,digits=dec.plat),nsmall=dec.plat),
                                ")",
                                sep = "")
      }  
    
    }
    cur.col <- cur.col + 1
  }
  cur.col <- 2
  for (i in 1:length(levels(data[[var.group]]))){
    cur.lin <- length(var.num)
    cur.group <- levels(data[[var.group]])[[i]]
    cur.data <- data[(data[[var.group]] == cur.group),]
    for (n in 1:length(var.cat)){
      cur.lin <- cur.lin + 1
      final.tab[cur.lin,cur.col] <- NA
      var.count <- table(cur.data[var.cat[n]])
      for (o in 1:length(levels(cur.data[[var.cat[n]]]))){
        cur.lin <- cur.lin + 1
        cur.percent <- (var.count[[o]]/length(cur.data[[var.cat[n]]]))*(100)
        final.tab[cur.lin,cur.col] <- paste(var.count[[o]],
                                            "(",
                                            format(round(cur.percent,digits=dec.plat),nsmall=dec.plat),
                                            ")",
                                            sep = "")
      }
      
    }
    cur.col <- cur.col + 1
  }
  cur.lin <- 0
  for (i in 1:length(var.num)){
    cur.lin <- cur.lin + 1
    if (var.num.norm[i] == TRUE){
      cur.lin.mod <- lm (data[[var.num[i]]] ~ data[[var.group]])
      num.p.val <- anova (cur.lin.mod)[[5]][1]
      num.p.val <- format(round(num.p.val,digits=dec.p),nsmall=dec.p)
      final.tab[cur.lin,cur.col] <- num.p.val
    }
    else {
      kruskal.data <- kruskal.test(x = data[,var.num[i]], g = data[[var.group]])
      num.p.val <- kruskal.data$p.value
      num.p.val <- format(round(num.p.val,digits=dec.p),nsmall=dec.p)
      final.tab[cur.lin,cur.col] <- num.p.val
    }
  }
  cur.lin <- length(var.num) + 1
  for (i in 1:length(var.cat)){
    print(var.cat[i])
    cur.result <- tryCatch(chisq.test(data[[var.cat[i]]],data[[var.group]])$p.value,
                                     warning = function(w) 
                                       chisq.test(data[[var.cat[i]]],data[[var.group]],simulate.p.value=TRUE)$p.value)
    cur.cat.p.val <- format(round(cur.result,digits=dec.p),nsmall=dec.p)
    final.tab[cur.lin,cur.col] <- cur.cat.p.val
    cur.lin <- cur.lin + length(levels(data[[var.cat[i]]])) + 1
  }
  print(var.num.norm)
  return (final.tab)
}