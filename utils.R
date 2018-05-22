################################################################
# title      : General Utilities
# description: (1) as.fumeric is a shortcut to directly
#              morph factors no numeric.
#              (2) recode.na was an alias to recode variables using
#              9999 and similar as NA codes.
#              (3) Alias to density plot using ggplot2

#General utilities

#Converts factor variable to numeric
as.fumeric <- function(x){
  as.numeric(as.character(x))
}

#Closure to recode NA values
recode.na <- function(x){
  require(car)
  car::recode(x,"999=NA;9999=NA;99999=NA")
}

density.plot <- function(dataset,statds){
  require(ggplot2)
  ggplot(data=dataset,aes(x=statds))+ geom_density()}