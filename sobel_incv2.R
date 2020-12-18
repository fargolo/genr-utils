require(medmod)

sobel_inc <- function(med_data,dep_var){
  clean_names <- names(med_data[,-which(names(med_data) %in% dep_var)])
  mat_size <- length(clean_names)
  G_mat <- matrix(0L, nrow = mat_size,ncol = mat_size)
  rownames(G_mat) <- clean_names ; colnames(G_mat) <- clean_names
  E_mat <- G_mat;p_mat <- G_mat
  for (i in 1:nrow(G_mat)){
    for (j in 1:ncol(G_mat)){
      effect_p <- NA
      effect_estimate <- NA
      
      if(i == j){ #Diagonal
        sing_form <- paste(dep_var,"~",clean_names[i])
        single_reg <- glm(as.formula(sing_form),data=med_data,family=gaussian)
        single_reg_s <- summary(single_reg) 
        effect_estimate <- single_reg_s$coefficients[2,1] 
        effect_p <- single_reg_s$coefficients[2,4]
      } else{ #Sobel tests
        med_obj <- medmod::med(data = med_data,dep = dep_var,med = clean_names[j],
                               pred = clean_names[i])
        effect_estimate <- med_obj$med$asDF$est[1]
        effect_p <- med_obj$med$asDF$p[1]
      }
      # For debug: print(paste("Row is",i)) /print(paste("Col is",j))
      #print(paste("p is",effect_p)) / print(paste("effect is",effect_estimate))
      E_mat[i,j] <- effect_estimate ; p_mat[i,j] <- effect_p
      
      if(is.na(effect_p) | is.na(effect_estimate)) {G_mat[i,j] <- NA 
      # Criteria with Bonferroni correction
      } else if(effect_p < 0.05/mat_size & effect_estimate > 0) {G_mat[i,j] <- 1
      } else if(effect_p < 0.05/mat_size & effect_estimate < 0) {G_mat[i,j] <- -1}}}
  list_mat <- list(inc_mat = G_mat,effects_mat = E_mat,p_mat = p_mat)
  print(G_mat)
  return(list_mat)}

