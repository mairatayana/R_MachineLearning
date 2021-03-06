# Script em R do exerc�cio de KNN
# Aluna: Maira Tayana Menegas - PPG-CC - Aluna especial
# Aula 05

# Import data from CSV file
cliente <- read.csv(file="Conj_data.csv", header=TRUE, sep=",")
View(cliente)

# a. Retire o atributo ID, que n�o contribui para a predi��o
clientePart = cliente[-c(1)]
#clientePart <- subset(cliente, select = Rendimento:Idade:Tamanho_fam�lia:Comprou_antes:Comprou_anunciado)
View(clientePart)


# b. Converta os atributos qualitativos em quantitativos
clientePart$Tamanho_fam�lia = gsub("Grande", "1", clientePart$Tamanho_fam�lia)
clientePart$Tamanho_fam�lia = gsub("Pequena", "0", clientePart$Tamanho_fam�lia)
clientePart$Comprou_antes = gsub("Sim", "1", clientePart$Comprou_antes)
clientePart$Comprou_antes = gsub("N�o", "0", clientePart$Comprou_antes)
clientePart$Comprou_anunciado = gsub("Sim", "1", clientePart$Comprou_anunciado)
clientePart$Comprou_anunciado = gsub("N�o", "0", clientePart$Comprou_anunciado)
View(clientePart)

# c. Normalize os atributos quantitativos  
#    (os que forem cadeias de bits n�o precisam ser
#    normalizados, s� os que n�o estiverem entre 0 e 1).
doNorm <- function(x) {(x - min(x))/(max(x)-min(x))}
clientePart$Idade <- as.numeric(clientePart$Idade)
clientePart$Rendimento <- as.numeric(clientePart$Rendimento)
clientePart$Tamanho_fam�lia <- as.numeric(clientePart$Tamanho_fam�lia)
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
# d. Usando esse conjunto de dados e a medida de dist�ncia Euclideana,
#    classifique os seguintes potenciais novos clientes, usando um vizinho mais pr�ximo
#    d.1. Maria tem 55 anos, um rendimento de 9500 reais e uma fam�lia
#         pequena. Al�m disso, j� comprou outros produtos da empresa anteriormente
knn(clientePart,c(9500,25,0,1),k=1)
# Resposta: $nearest 3; $pred 1

#    d.2. Jo�o � um jovem de 23 anos com rendimento de 900 reais e 
#         fam�lia pequena. Ele j� comprou produtos da empresa.
knn(clientePart,c(900,23,0,1),k=1)
# Resposta: $nearest 9; $pred 1

# e. Classifique os clientes anteriores usando tr�s vizinhos mais pr�ximos
knn(clientePart,c(9500,25,0,1),k=3) # Resposta: $nearest 3 13 7; $pred 1
knn(clientePart,c(900,23,0,1),k=3) # Resposta: $nearest 9 1 8; $pred 0

