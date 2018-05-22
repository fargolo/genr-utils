#######################################################
# title      : lrm.summ
# description: Sequentially extracts some measures from a 
#              glm object fitted for logistic regression.  
#              Returns list with:
#              (1) AUROC plot with point estimate and bootstrapped CI.
#              (2) Matrix with: coeff,std err,OR and 95% CI
#              (3) Pseudo R2s, LR, Loglikelihood, Dev,AIC,BIC,N
#              (4) Model fitted
lrm.summ <- function(fit,roc.color="#0000AF",lang="pt", plot.title = "",
                  sdigits=3,geom="line"){
  require(ggplot2)
  require(pROC)
  require(memisc)
  require(ResourceSelection)
  conf.int <- function (x){
    round(cbind(Est=fit$coefficients,SdEr= sqrt(diag(vcov(fit))),exp(cbind(OR = coef(x),confint(x)))),digits=sdigits)
  }
  #Fit ROC
  fit.roc <- roc(predictor = fit$fitted.values,
                    response= fit$y,
                    ci=T,ci.method="bootstrap")
  #Fit sens and spec
  ds.fit <- as.data.frame(cbind(fit.roc$sensitivities,fit.roc$specificities))
  #Plot
  plot.labs <- character(length = 3)
  labs.pt <- c("Sensibilidade","1 - Especificidade",plot.title)
  labs.en <- c("Sensitivity","1 - Specificity",plot.title)
  if (lang=="pt"){
    plot.labs <- labs.pt
  }
  else (plot.labs <- labs.en)
  AUC.list <- as.character(lapply(c(fit.roc$auc[1],fit.roc$ci[1],fit.roc$ci[3]),FUN = function(x){format(round(x,sdigits),nsmall=sdigits)}))
  if (geom == "smooth"){
    fit.plot <- ggplot(ds.fit,aes(x=(1-V2),y=V1)) +
      geom_smooth(size=1.5,alpha=0.7,color=roc.color,se = F)+
      geom_abline(slope = 1,intercept = 0,size=2,alpha=0.2)+
      ylab(plot.labs[1])+ xlab(plot.labs[2])+
      scale_y_continuous(limits=c(0,1))+
      geom_text(aes(x=0.7,y=0,
                    label=paste("AUROC = ",AUC.list[[1]],
                                " (",
                                AUC.list[[2]],
                                " - ",
                                AUC.list[[3]],
                                ")")))+  
      ggtitle(plot.labs[3])}
  else if(geom == "line"){
    fit.plot <- ggplot(ds.fit,aes(x=(1-V2),y=V1)) + 
      geom_line(size=1.5,alpha=0.7,color=roc.color)+
      geom_abline(slope = 1,intercept = 0,size=2,alpha=0.2)+
      ylab(plot.labs[1])+ xlab(plot.labs[2])+
      scale_y_continuous(limits=c(0,1))+
      geom_text(aes(x=0.7,y=0,
                    label=paste("AUROC = ",AUC.list[[1]],
                                " (",
                                AUC.list[[2]],
                                " - ",
                                AUC.list[[3]],
                                ")")))+  
      ggtitle(plot.labs[3])
  }
  fit.summary <- summary(fit)
  conf.fit <- cbind(conf.int(fit),p_value = fit.summary$coefficients[,4])
  fit.hoslem <- hoslem.test(x = fit$y,y = fitted(fit))
  fit.hoslem.val <- c(fit.hoslem$statistic,fit.hoslem$parameter,fit.hoslem$p.value)
  fit.hoslem.val <- round(fit.hoslem.val,digits=sdigits)
  fit.hoslem.mat <- as.matrix(fit.hoslem.val)
  rownames(fit.hoslem.mat) <- c("Hoslem_X2","df","p_value")
  fit.stats <- mtable(fit,sdigits=sdigits)
  fit.stats <- rbind(fit.stats$summaries,fit.hoslem.mat)
  return(list(fit.plot,conf.fit,fit.stats,fit))
}