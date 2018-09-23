# Script em R do exercício de KNN
# Aluna: Maira Tayana Menegas - PPG-CC - Aluna especial
# Aula 05

# Import data from CSV file
cliente <- read.csv(file="Conj_data.csv", header=TRUE, sep=",")
View(cliente)

# a. Retire o atributo ID, que não contribui para a predição
clientePart = cliente[-c(1)]
#clientePart <- subset(cliente, select = Rendimento:Idade:Tamanho_família:Comprou_antes:Comprou_anunciado)
View(clientePart)


# b. Converta os atributos qualitativos em quantitativos
clientePart$Tamanho_família = gsub("Grande", "1", clientePart$Tamanho_família)
clientePart$Tamanho_família = gsub("Pequena", "0", clientePart$Tamanho_família)
clientePart$Comprou_antes = gsub("Sim", "1", clientePart$Comprou_antes)
clientePart$Comprou_antes = gsub("Não", "0", clientePart$Comprou_antes)
clientePart$Comprou_anunciado = gsub("Sim", "1", clientePart$Comprou_anunciado)
clientePart$Comprou_anunciado = gsub("Não", "0", clientePart$Comprou_anunciado)
View(clientePart)

# c. Normalize os atributos quantitativos  
#    (os que forem cadeias de bits não precisam ser
#    normalizados, só os que não estiverem entre 0 e 1).
doNorm <- function(x) {(x - min(x))/(max(x)-min(x))}
clientePart$Idade <- as.numeric(clientePart$Idade)
clientePart$Rendimento <- as.numeric(clientePart$Rendimento)
clientePart$Tamanho_família <- as.numeric(clientePart$Tamanho_família)
clientePart$Comprou_antes <- as.numeric(clientePart$Comprou_antes)
clientePart$Comprou_anunciado <- as.numeric(clientePart$Comprou_anunciado)
clientePartNorm <- as.data.frame(lapply(clientePart, doNorm))
View(clientePartNorm)

library(class)
knn <- function(dataset,query,k=1){
  
  idClass <- ncol(dataset)
  
  Eucl_dist <- apply(dataset,1,function(row){
    sqrt(sum((query-as.numeric(row[1:idClass-1]))^2))
  })
  ids <- sort.list(Eucl_dist,dec=F)[1:k]
  labels <- dataset[ids,idClass]
  
  ret <- list()
  ret$nearest <- ids
  
  if(!is.numeric(dataset[,idClass])){
    # classification problem
    U <- unique(labels)
    R <- rep(0,length(U))
    for (i in 1:length(U)){
      R[i] <- sum(U[i] == labels)
    }
    idx <- which.max(R)
    
    ret$voted <- U
    ret$Nvotes <- R
    ret$pred <- U[idx]
  }
  else{
    ret$pred <- mean(labels)
  }
  
  return(ret)
}
# d. Usando esse conjunto de dados e a medida de distância Euclideana,
#    classifique os seguintes potenciais novos clientes, usando um vizinho mais próximo
#    d.1. Maria tem 55 anos, um rendimento de 9500 reais e uma família
#         pequena. Além disso, já comprou outros produtos da empresa anteriormente
knn(clientePart,c(9500,25,0,1),k=1)
# Resposta: $nearest 3; $pred 1

#    d.2. João é um jovem de 23 anos com rendimento de 900 reais e 
#         família pequena. Ele já comprou produtos da empresa.
knn(clientePart,c(900,23,0,1),k=1)
# Resposta: $nearest 9; $pred 1

# e. Classifique os clientes anteriores usando três vizinhos mais próximos
knn(clientePart,c(9500,25,0,1),k=3) # Resposta: $nearest 3 13 7; $pred 1
knn(clientePart,c(900,23,0,1),k=3) # Resposta: $nearest 9 1 8; $pred 0

