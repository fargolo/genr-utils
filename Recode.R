library(car)

#copia nomes das variaveis do dataset
varlist <- colnames(dataset)
#Seleciona as variaveis que tenham a string STRING para uma lista
STRINGlist <- grep("STRING",varlist,value=T)
#Cria uma lista com os nomes salvos sguido da string "Rec"
STRINGlistcat <- paste(STRINGlist,"Rec",sep="")
#Cria dataset apenas com variaveis selecionadas
mycatvars <- dataset[,STRINGlist]
#Renomeia essas variaveis com o "Rec" no fim
names(mycatvars) <- STRINGlistcat
# Cria função para recodificar variáveis usando recode do pacote car.
# O código deve ser inserido totalmente dentro das aspas. É permitido strings entre 'apostrofo' e intervalos
myRecorder <- function (x) {recode(x,"-5:0='Hipo';1:4='Hiper';9=NA")}
# Aplica função
mycatvars[,STRINGlistcat] <- lapply(mycatvars[,STRINGlistcat],myRecorder)
#Junta datasets
dataset <- cbind (dataset,mycatvars)