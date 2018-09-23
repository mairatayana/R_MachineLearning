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

# b) Codifique os atributos qualitativos como quantitativos
clientePart$Tamanho_família = gsub("Grande", "1", clientePart$Tamanho_família)
clientePart$Tamanho_família = gsub("Pequena", "0", clientePart$Tamanho_família)
clientePart$Comprou_antes = gsub("Sim", "1", clientePart$Comprou_antes)
clientePart$Comprou_antes = gsub("Não", "0", clientePart$Comprou_antes)

# c) Normalize os atributos numéricos (binários não precisam ser normalizados)
doNorm <- function(x) {(x - min(x))/(max(x)-min(x))}
clientePart$Idade <- as.numeric(clientePart$Idade)
clientePart$Rendimento <- as.numeric(clientePart$Rendimento)
clientePart$Tamanho_família <- as.numeric(clientePart$Tamanho_família)
clientePart$Comprou_antes <- as.numeric(clientePart$Comprou_antes)
clientePart$Comprou_anunciado <- as.numeric(clientePart$Comprou_anunciado)
clientePartNorm <- as.data.frame(lapply(clientePart, doNorm))
View(clientePartNorm)

# d) Treine uma rede Perceptron com ?? = 0,3 
# threshold activation function
f_threshold <- function(u) {
  if (u >= 0)
    return (1)
  return (0)
}

# Training a perceptron
perceptron.train <- function(dataset, eta=0.1, epsilon=1e-2) {
  
  classId = ncol(dataset)
  X = dataset[,1:classId-1]
  Y = dataset[,classId]
  
  # initializing weights
  weights = runif(min=-0.5, max=0.5, n=ncol(dataset))
  
  ncycle = 0
  squaredError = 2*epsilon
  
  while (squaredError > epsilon) {
    squaredError = 0
    
    for (i in 1:nrow(X)) {
      example = c(1,as.numeric(X[i,]))
      class = Y[i]
      
      cat("example = ", as.numeric(X[i,]), "\n")
      cat("class = ", class, "\n")
      
      # computing predicted y
      u = example %*% weights
      
      cat("u = ", u, "\n")
      
      y = f_threshold(u)
      
      cat("predicted = ", y, "\n")
      
      # Error
      Error = class - y
      squaredError = squaredError + Error^2
      
      if(abs(Error) > 0){
        # update weights
        cat("updating weights...\n")
        delta = eta * Error * example        
        weights = weights + delta
      }
      
    }
    
    squaredError = squaredError / nrow(X)
    cat("Squared error = ", squaredError, "\n")
    ncycle = ncycle + 1
  }
  
  ret = list()
  ret$weights = weights
  ret$ncycle = ncycle
  cat("Final weights = ", weights, "\n")
  cat("Number of cycles = ", ncycle, "\n")
  
  return (ret)
}

# predicting with a perceptron
perceptron.run <- function(X, model) {
  
  cat("#example\tprediction\n")
  for (i in 1:nrow(X)) {
    example = c(1,as.numeric(X[i,]))
    
    # u
    u = example %*% model$weights
    y = f_threshold(u)
    
    cat(as.numeric(X[i,]), "\t", y, "\n")
  }
}

# training
cat("\n\n>> Training:\n")
model = perceptron.train(clientePartNorm, 0.3)
model

#Resultado
#predicted =  0 
#Squared error =  0 
#Final weights =  -0.6836698 2.002674 1.04126 -0.8598017 0.6839734 
#Number of cycles =  30 

# testing
cat("\n\n>> Testing:\n")
perceptron.run(clientePartNorm[,1:ncol(clientePartNorm)-1], model)



# e) Em seguida, usando os pesos obtidos, classifique os seguintes potenciais 
#    novos clientes com essa rede neural: 
#    e.1) Maria tem 55 anos, um rendimento de 9500 reais e uma família pequena. 
#         Além disso, já comprou outros produtos da empresa anteriormente.
mariaRendNorm = (9500 - min(cliente[,2]))/(max(cliente[,2]) - min(cliente[,2]))
mariaIdadeNorm = (55 - min(cliente[,3]))/(max(cliente[,3]) - min(cliente[,3]))
maria <- data.frame(Rendimento=(mariaRendNorm),Idade=(mariaIdadeNorm),Tamanho_família=(0),Comprou_antes=(1))
perceptron.run(maria, model)
# Resposta -> sim
#example	prediction
#0.9450549 1 0 1 	 1 

#    e.2) João é um jovem de 23 anos com rendimento de 900 reais e família 
#         pequena. Ele já comprou produtos da empresa.   
joaoRendNorm = (900 - min(cliente[,2]))/(max(cliente[,2]) - min(cliente[,2]))
joaoIdadeNorm = (23 - min(cliente[,3]))/(max(cliente[,3]) - min(cliente[,3]))
joao <- data.frame(Rendimento=(joaoRendNorm),Idade=(joaoIdadeNorm),Tamanho_família=(0),Comprou_antes=(1))
perceptron.run(joao, model)
# Resposta -> sim
#example	prediction
#0 0.03030303 0 1 	 1 

#f) Refaça o exercício com uma MLP, variando o número de neurônios na camada 
#   intermediária com os valores 2 e 5.
require(nnet)
require(tseriesChaos)

f <- function(net) {
  return ( 1/(1+exp(-net)) )
}

df_dnet <- function(net) {
  return ( f(net) * (1 - f(net)) )
}

fhard <- function(net) {
  r = net
  r[net > 0] = 1
  r[net <= 0] = 0
  return (r)
}

mlp.architecture <- function(input.length = 2,
                             hidden.length = 2,
                             output.length = 1,
                             my.f = f,
                             my.df_dnet = df_dnet) {
  
  layers = list()
  layers$hidden = matrix(runif(min=-0.5, max=0.5, 
                               n=hidden.length*(input.length+1)), 
                         nrow=hidden.length, ncol=input.length+1)
  
  layers$output = matrix(runif(min=-0.5, max=0.5, 
                               n=output.length*(hidden.length+1)), 
                         nrow=output.length, ncol=hidden.length+1)
  
  model = list()
  model$layers = layers
  model$f = my.f
  model$df_dnet = my.df_dnet
  
  return (model)
}

mlp.forward <- function(model, x_p) {
  # x = c(1, 0)
  
  f_h_net_h_pj = rep(0, nrow(model$layers$hidden))
  df_h_dnet_h_pj = rep(0, nrow(model$layers$hidden))
  for (j in 1:nrow(model$layers$hidden)) {
    net_h_pj = c(x_p, 1) %*% model$layers$hidden[j,]
    f_h_net_h_pj[j] = model$f(net_h_pj)
    df_h_dnet_h_pj[j] = model$df_dnet(net_h_pj)
  }
  
  f_o_net_o_pk = rep(0, nrow(model$layers$output))
  df_o_dnet_o_pk = rep(0, nrow(model$layers$output))
  for (k in 1:nrow(model$layers$output)) {
    net_o_pk = c(f_h_net_h_pj, 1) %*% model$layers$output[k,]
    f_o_net_o_pk[k] = model$f(net_o_pk)
    df_o_dnet_o_pk[k] = model$df_dnet(net_o_pk)
  }
  
  fwd = list()
  fwd$f_h_net_h_pj = f_h_net_h_pj
  fwd$f_o_net_o_pk = f_o_net_o_pk
  fwd$df_h_dnet_h_pj = df_h_dnet_h_pj
  fwd$df_o_dnet_o_pk = df_o_dnet_o_pk
  
  return (fwd)
}

mlp.backpropagation <- function(X, Y, model, eta=0.1, threshold=1e-2,msg = TRUE,ncycle=1000) {
  
  sqerror = 2 * threshold
  n = 0
  while ((sqerror > threshold)&&(n<ncycle)) {
    sqerror = 0
    
    # Treinando com cada exemplo de meu conjunto X dadas as classes em Y
    for (p in 1:nrow(X)) {
      x_p = X[p,]
      y_p = Y[p,]
      
      fwd = mlp.forward(model, x_p)
      o_p = fwd$f_o_net_o_pk
      
      delta_p = y_p - o_p
      
      # Calculando erro quadrático
      sqerror = sqerror + sum(delta_p^2)
      
      # Calculando delta da camada de saída para um padrão
      delta_o_p = delta_p * fwd$df_o_dnet_o_pk
      
      # Calculando delta da camada escondida para um padrão
      w.length = ncol(model$layers$output)-1
      delta_h_p = fwd$df_h_dnet_h_pj * 
        (delta_o_p %*% model$layers$output[,1:w.length])
      
      # Atualizando a camada de saída
      model$layers$output = model$layers$output + 
        eta * (as.vector(delta_o_p) %*% t(c(as.vector(fwd$f_h_net_h_pj), 1)))
      
      # Atualizando a camada escondida
      model$layers$hidden = model$layers$hidden +
        eta * (as.vector(delta_h_p) %*% t(c(x_p, 1)))
      
    }
    
    sqerror = sqerror / nrow(X)
    if(msg){
      cat("Average squared error: ", sqerror, "\n")
    }
    n = n+1
  }
  
  return (model)
}

client.test <- function(hidden.length = 2, eta=0.1, 
                      train.size=0.75, threshold=1e-2) {
  
  dataset = clientePartNorm
  features = dataset[,1:4]
  
  classes = class.ind(dataset[,5])# one-to-n encoding of the class
  dataset = cbind(features, classes)
  
  X = matrix(ts(dataset[,1:4]), ncol=4)
  Y = matrix(ts(dataset[,5:6]), ncol=2)

  model = mlp.architecture(4, hidden.length, 2)
  trained.model = mlp.backpropagation(X, Y, model, eta, threshold,msg = TRUE)

  res = NULL
  datasetMaria = maria
  X.testMaria = matrix(ts(datasetMaria[,1:4]), ncol=4)
  datasetJoao = joao
  X.testJoao = matrix(ts(datasetJoao[,1:4]), ncol=4)
  x_p_maria = X.testMaria[1,]
  fwd_maria = mlp.forward(trained.model, x_p_maria)
  res = rbind(res, c(fwd_maria$f_o_net_o_pk))
  x_p_joao = X.testJoao[1,]
  fwd_joao = mlp.forward(trained.model, x_p_joao)
  res = rbind(res, c(fwd_joao$f_o_net_o_pk))
  
  colnames(res) = c("O1", "O2")  

  
  return (res)
}


res2 <- client.test(hidden.length = 2)
res2
#Resposta para 2 camadas:
#        O1        O2
#[1,] 0.02744331 0.9709200 -> maria sim
#[2,] 0.18067932 0.8210844 -> joao  sim


res5 <- client.test(hidden.length = 5)
res5
#resposta para 5 camadas
#         O1        O2
#[1,] 0.006104979 0.9932054 -> maria sim
#[2,] 0.194616103 0.7989700 -> joao  sim
