# Script em R do exercício de Rede Neural
# Aluna: Maira Tayana Menegas - PPG-CC - Aluna especial
##################################################################################

# Preparacao dos dados
# Importando datos de arquivo CSV
cliente <- read.csv(file="Conj_data.csv", header=TRUE, sep=",")
View(cliente)

# a) Remova o atributo de identificação
clientePart = cliente[-c(1)]
View(clientePart)

# a) Codifique os atributos qualitativos como quantitativos
clientePart$Tamanho_família = gsub("Grande", "1", clientePart$Tamanho_família)
clientePart$Tamanho_família = gsub("Pequena", "0", clientePart$Tamanho_família)
clientePart$Comprou_antes = gsub("Sim", "1", clientePart$Comprou_antes)
clientePart$Comprou_antes = gsub("Não", "0", clientePart$Comprou_antes)

# b) Normalize os atributos numéricos (binários não precisam ser normalizados)
doNorm <- function(x) {(x - min(x))/(max(x)-min(x))}
clientePart$Idade <- as.numeric(clientePart$Idade)
clientePart$Rendimento <- as.numeric(clientePart$Rendimento)
clientePart$Tamanho_família <- as.numeric(clientePart$Tamanho_família)
clientePart$Comprou_antes <- as.numeric(clientePart$Comprou_antes)
clientePart$Comprou_anunciado <- as.numeric(clientePart$Comprou_anunciado)
clientePartNorm <- as.data.frame(lapply(clientePart, doNorm))
View(clientePartNorm)

# b) Retirando classe:
clientePartNormNoClass = clientePartNorm[-c(5)]
View(clientePartNormNoClass)

# c)  Use o algoritmo k-médias para agrupar os dados, selecionando aleatoriamente 
#     dois pontos do conjunto como centros
res <- kmeans(clientePartNormNoClass,centers =2,nstart = 20) # nstart tries different initializations and finds the one with lowest within variance
res

plot(clientePartNormNoClass[,1:2],col=res$cluster)
points(res$centers[,3:4],col="blue", pch=8, cex=2)


# e) Em seguida, usando os pesos obtidos, classifique os seguintes potenciais 
#    novos clientes com essa rede neural: 
#    e.1) Maria tem 55 anos, um rendimento de 9500 reais e uma família pequena. 
#         Além disso, já comprou outros produtos da empresa anteriormente.
mariaRendNorm = (9500 - min(cliente[,2]))/(max(cliente[,2]) - min(cliente[,2]))
mariaIdadeNorm = (55 - min(cliente[,3]))/(max(cliente[,3]) - min(cliente[,3]))
maria <- data.frame(Rendimento=(mariaRendNorm),Idade=(mariaIdadeNorm),Tamanho_família=(0),Comprou_antes=(1))
fitted.values(maria, res)
# Resposta -> sim


#    e.2) João é um jovem de 23 anos com rendimento de 900 reais e família 
#         pequena. Ele já comprou produtos da empresa.   
joaoRendNorm = (900 - min(cliente[,2]))/(max(cliente[,2]) - min(cliente[,2]))
joaoIdadeNorm = (23 - min(cliente[,3]))/(max(cliente[,3]) - min(cliente[,3]))
joao <- data.frame(Rendimento=(joaoRendNorm),Idade=(joaoIdadeNorm),Tamanho_família=(0),Comprou_antes=(1))
fitted.values(joao, res)
# Resposta -> sim

