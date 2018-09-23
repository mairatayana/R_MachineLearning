# Script em R do exerc??cio de SVM
# Aluna: Maira Tayana Menegas - PPG-CC - Aluna especial
##################################################################################

# Preparacao dos dados
# Importando datos de arquivo CSV
cliente <- read.csv(file="Conj_data.csv", header=TRUE, sep=",")
View(cliente)

# a) Remova o atributo de identifica????o
clientePart = cliente[-c(1)]
View(clientePart)

# b) Codifique os atributos qualitativos como quantitativos
clientePart$Tamanho_fam??lia = gsub("Grande", "1", clientePart$Tamanho_fam??lia)
clientePart$Tamanho_fam??lia = gsub("Pequena", "0", clientePart$Tamanho_fam??lia)
clientePart$Comprou_antes = gsub("Sim", "1", clientePart$Comprou_antes)
clientePart$Comprou_antes = gsub("N??o", "0", clientePart$Comprou_antes)

# Normalize os atributos num??ricos (bin??rios n??o precisam ser normalizados)
#doNorm <- function(x) {(x - min(x))/(max(x)-min(x))}
clientePart$Idade <- as.numeric(clientePart$Idade)
clientePart$Rendimento <- as.numeric(clientePart$Rendimento)
clientePart$Tamanho_fam??lia <- as.numeric(clientePart$Tamanho_fam??lia)
clientePart$Comprou_antes <- as.numeric(clientePart$Comprou_antes)
clientePart$Comprou_anunciado <- as.numeric(clientePart$Comprou_anunciado)
#clientePartNorm <- as.data.frame(lapply(clientePart, doNorm))
#View(clientePartNorm)

# c) Treine uma SVM, com par??metros default 
#    (se o c??digo usado n??o tiver a normaliza????o, ela deve ser feita antes) 
install.packages("e1071")
library("e1071") #j?? normaliza

model <- svm(Comprou_anunciado ~ ., data = clientePart)
summary(model)
#Parameters:
#SVM-Type:  eps-regression 
#SVM-Kernel:  radial 
#cost:  1 
#gamma:  0.25 
#epsilon:  0.1 
#Number of Support Vectors:  13


# d) Em seguida, usando a SVM obtida, classifique os seguintes potenciais 
#    novos clientes com essa SVM: 
#    d.1) Maria tem 55 anos, um rendimento de 9500 reais e uma fam??lia pequena. 
#         Al??m disso, j?? comprou outros produtos da empresa anteriormente.
maria <- data.frame(Rendimento=(9500),Idade=(55),Tamanho_fam??lia=(0),Comprou_antes=(1))
pred <- predict(model, maria)
pred
# Resposta -> sim

#    d.2) Jo??o ?? um jovem de 23 anos com rendimento de 900 reais e fam??lia 
#         pequena. Ele j?? comprou produtos da empresa.   
joao <- data.frame(Rendimento=(900),Idade=(23),Tamanho_fam??lia=(0),Comprou_antes=(1))
pred <- predict(model, joao)
pred
# Resposta -> sim

# e) Altere o kernel para linear e refa??a o exerc??cio. 
modelLinear <- svm(Comprou_anunciado ~ ., data = clientePart,kernel="linear")
print(modelLinear)
summary(modelLinear)

pred <- predict(modelLinear, maria)
pred
# Resposta -> sim

pred <- predict(modelLinear, joao)
pred
# Resposta -> sim
