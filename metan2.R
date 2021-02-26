library(metafor)
set.seed(2600)

# Parameters
studies_n <- 30
samples_n <- 2

mean_interv <- 1
sd_interv <- 1
mean_contr <- 0
sd_contr <- 1

interv <- replicate(studies_n,rnorm(samples_n,mean_interv,sd_interv))
contr <- replicate(studies_n,rnorm(samples_n,mean_contr,sd_contr))

# Mean and var 
interv_m <- apply(interv,2,mean)
interv_sd <- apply(interv,2,sd)
contr_m <- apply(contr,2,mean)
contr_sd <- apply(contr,2,sd)

met_dat <- cbind(interv_m,interv_sd,contr_m,contr_sd)

# Meta analysis
meta_res <- escalc(measure = "SMD",
       m1i = interv_m,m2i = contr_m, 
       sd1i = interv_sd, sd2i = contr_sd,
       n1i = rep(samples_n,studies_n),
       n2i = rep(samples_n,studies_n),
       slab = paste("fulano et al",1:length(interv_m)),
       data = met_dat)

meta_rma <- rma(yi,vi,data=meta_res) 

plot(meta_rma)
forest(meta_rma)
