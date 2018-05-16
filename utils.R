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