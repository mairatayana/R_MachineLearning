# Script em R do exerc�cio de Decision trees
# Aluna: Maira Tayana Menegas - PPG-CC - Aluna especial
# Aula 06
##################################################################################

# Preparacao dos dados
# Importando datos de arquivo CSV
cliente <- read.csv(file="Conj_data.csv", header=TRUE, sep=",")
View(cliente)

# Retirando o atributo ID
clientePart = cliente[-c(1)]
View(clientePart)

# Convertendo os atributos qualitativos em quantitativos
clientePart$Tamanho_fam�lia = gsub("Grande", "1", clientePart$Tamanho_fam�lia)
clientePart$Tamanho_fam�lia = gsub("Pequena", "0", clientePart$Tamanho_fam�lia)
clientePart$Comprou_antes = gsub("Sim", "1", clientePart$Comprou_antes)
clientePart$Comprou_antes = gsub("N�o", "0", clientePart$Comprou_antes)
clientePart$Idade <- as.numeric(clientePart$Idade)
clientePart$Rendimento <- as.numeric(clientePart$Rendimento)
clientePart$Tamanho_fam�lia <- as.numeric(clientePart$Tamanho_fam�lia)
clientePart$Comprou_antes <- as.numeric(clientePart$Comprou_antes)

# a. Discretize os atributos rendimento e idade da seguinte maneira:
#    Idade: menor que 30, entre 30 e 40, maior que 40
#    Rendimento: baixo (<= 2000), m�dio (entre 2000 e 7000), alto (>= 7000)
clientePart$Idade[clientePart$Idade <= 30] <- 0
clientePart$Idade[clientePart$Idade>30 & clientePart$Idade<40] <- 1
clientePart$Idade[clientePart$Idade >= 40] <- 2
clientePart$Rendimento[clientePart$Rendimento <= 2000] <- 0
clientePart$Rendimento[clientePart$Rendimento>2000 & clientePart$Rendimento<7000] <- 1
clientePart$Rendimento[clientePart$Rendimento >= 7000] <- 2
clientePart$Idade <- as.numeric(clientePart$Idade)
clientePart$Rendimento <- as.numeric(clientePart$Rendimento)
View(clientePart)

# b. Monte a �rvore de decis�o para classificar os clientes, a partir dessa base de
#    experi�ncia (lembre-se de desconsiderar o atributo ID). 
#    Plote a �rvore e verifique que atributos foram efetivamente usados.

# Separando grupo de treino e de teste
trainingRowIndex <- sample(1:nrow(clientePart), 0.7*nrow(clientePart))
cliente.train <- clientePart[trainingRowIndex, ]
cliente.test <- clientePart[-trainingRowIndex, ]

# Montando e plotando a arvore de decisao
library(rpart)
fit <- rpart(Comprou_anunciado ~ ., data=cliente.train, method="class", control = rpart.control(minbucket = 1,cp=0))
par(xpd = TRUE)
plot(fit,compress=TRUE,uniform=TRUE, main="Classification Tree for Cliente")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#Resposta: Os atributos que foram efetivamente utilizados foram:
#  * Tamanho_familia
#  * Rendimento
#  * Comprou_antes
# Linhas usadas no treinamento: 1, 2, 3, 4, 7, 9, 11, 13, 14

# Verificando conjunto de teste
prediction <- predict(fit, cliente.test, type = "class")
table(cliente.test$Comprou_anunciado,prediction)
#   prediction (0->nao, 1->sim)
#       N�o Sim
#   N�o  1  1
#   Sim  1  2

# c. Em seguida, classifique os seguintes potenciais novos clientes para receber a
#    propaganda e explique o racioc�nio empregado pela �rvore nessas classifica��es:
#  c.1) Maria tem 55 anos, um alto rendimento e uma fam�lia pequena. Al�m
#       disso, j� comprou outros produtos da empresa anteriormente.
maria <- data.frame(Rendimento=c(2),Idade=(2),Tamanho_fam�lia=(0),Comprou_antes=(1))
prediction <- predict(fit, maria, type = "class")
prediction
# Prediction -> Sim

#  c.2) Jo�o � um adolescente com rendimento baixo e fam�lia pequena. Ele j�
#       comprou produtos da empresa.
joao <- data.frame(Rendimento=c(0),Idade=(0),Tamanho_fam�lia=(0),Comprou_antes=(1))
prediction <- predict(fit, newdata=joao, type = "class")
prediction
# Prediction -> Sim
##################################################################################

#Refazendo o exercicio sem considerar um conjunto de separado para teste e treino
fit2 <- rpart(Comprou_anunciado ~ ., data=clientePart, method="class", control = rpart.control(minbucket = 1,cp=0))
par(xpd = TRUE)
plot(fit2,compress=TRUE,uniform=TRUE, main="Classification Tree for Cliente")
text(fit2, use.n=TRUE, all=TRUE, cex=.8)
prediction <- predict(fit2, maria, type = "class")
prediction
# Prediction -> Sim
prediction <- predict(fit2, newdata=joao, type = "class")
prediction
# Prediction -> Sim
