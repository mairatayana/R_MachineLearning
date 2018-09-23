# Script em R do exercicio Naive Bayes
# Aluna: Maira Tayana Menegas - PPG-CC - Aluna especial
# Aula 09
##################################################################################

# Preparacao dos dados
# Importando datos de arquivo CSV
#cliente <- read.csv(file="Conj_data.csv", header=TRUE, sep=",")
cliente <- read.csv(file="//home-local//aluno//Downloads//Conj_dados_exercicio.csv", header=TRUE, sep=",")
View(cliente)

# Retirando o atributo ID
clientePart = cliente[-c(1)]
View(clientePart)

# Convertendo os atributos qualitativos em quantitativos
clientePart$Tamanho_familia = gsub("Grande", "1", clientePart$Tamanho_familia)
clientePart$Tamanho_familia = gsub("Pequena", "0", clientePart$Tamanho_familia)
clientePart$Comprou_antes = gsub("Sim", "1", clientePart$Comprou_antes)
clientePart$Comprou_antes = gsub("Nao", "0", clientePart$Comprou_antes)
clientePart$Idade <- as.numeric(clientePart$Idade)
clientePart$Rendimento <- as.numeric(clientePart$Rendimento)
clientePart$Tamanho_familia <- as.numeric(clientePart$Tamanho_familia)
clientePart$Comprou_antes <- as.numeric(clientePart$Comprou_antes)

# a. Discretize os atributos rendimento e idade da seguinte maneira:
#    Idade: menor que 30, entre 30 e 40, maior que 40
#    Rendimento: baixo (<= 2000), m?dio (entre 2000 e 7000), alto (>= 7000)
clientePart$Idade[clientePart$Idade <= 30] <- 0
clientePart$Idade[clientePart$Idade>30 & clientePart$Idade<40] <- 1
clientePart$Idade[clientePart$Idade >= 40] <- 2
clientePart$Rendimento[clientePart$Rendimento <= 2000] <- 0
clientePart$Rendimento[clientePart$Rendimento>2000 & clientePart$Rendimento<7000] <- 1
clientePart$Rendimento[clientePart$Rendimento >= 7000] <- 2
clientePart$Idade <- as.numeric(clientePart$Idade)
clientePart$Rendimento <- as.numeric(clientePart$Rendimento)
View(clientePart)

# b.  Monte um modelo Naive Bayes para classificar os clientes, a partir dessa base de
#     experiência (lembre-se de desconsiderar o atributo ID).

# Separando grupo de treino e de teste
trainingRowIndex <- sample(1:nrow(clientePart), 0.7*nrow(clientePart))
cliente.train <- clientePart[trainingRowIndex, ]
cliente.test <- clientePart[-trainingRowIndex, ]

#NB
library(e1071)

model <- naiveBayes(Comprou_anunciado ~ ., data=cliente.train)
model

NB_Predictions=predict(model,cliente.test)
#Confusion matrix to check accuracy
table(cliente.test$Comprou_anunciado,NB_Predictions)

# outputing the probabilities
NB_probs=predict(model,cliente.test,type="raw")
NB_probs

# c. Em seguida, classifique os seguintes potenciais novos clientes para receber a
#    propaganda:
#  c.1) Maria tem 55 anos, um alto rendimento e uma fam?lia pequena. Al?m
#       disso, j? comprou outros produtos da empresa anteriormente.
maria <- data.frame(Rendimento=c(2),Idade=(2),Tamanho_familia=(0),Comprou_antes=(1))
prediction <- predict(model, maria)
prediction
# Prediction -> Sim

#  c.2) Jo?o ? um adolescente com rendimento baixo e fam?lia pequena. Ele j?
#       comprou produtos da empresa.
joao <- data.frame(Rendimento=c(0),Idade=(0),Tamanho_familia=(0),Comprou_antes=(1))
prediction <- predict(model, joao)
prediction
# Prediction -> Sim
##################################################################################

#Refazendo o exercicio sem considerar um conjunto de separado para teste e treino
model2 <- naiveBayes(Comprou_anunciado ~ ., data=clientePart)
prediction <- predict(model2, maria)
prediction
# Prediction -> Sim
prediction <- predict(model2, joao)
prediction
# Prediction -> Sim
