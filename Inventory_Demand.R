setwd("~/Documentos/DSA/FCD/BigDataRAzure/Cap20/Projetos-1-2/Projeto-2")

# Bibliotecas
library(data.table)
library(dplyr)
library(ggplot2)
library(plyr)
library(caTools)
library(caret)
library(rmarkdown)

# Importando os arquivos
train <- fread('train.csv')
product <- fread('producto_tabla.csv')
city_state <- fread('town_state.csv')
client <- fread('cliente_tabla.csv')


# PRÉ-PROCESSAMENTO DOS DADOS

# Criando um subset dos dados pois dataset train contém muitas observações
train_index <- sample.split(train$Semana, SplitRatio = 0.06)
df_train <- subset(train, train_index == TRUE)

# Excluindo da memória do R
rm(train_index)
rm(train)

# Tabela Cliente
head(client)
str(client)

# Podemos observar que existem códigos de identificação de clientes
# duplicados então iremos excluí-los.
# Os nomes continuarão duplicados para que não ocorra inconsistência nos dados 
# quando ocorrer o join
df_client <- client %>% group_by(Cliente_ID) %>% filter (! duplicated(Cliente_ID))
#df_client <- as.data.table(df_client)

head(df_client)

rm(client)

# Agrupando os datasets para formar o dataset treino para análise exploratóra dos 
# dados
train_complete <- join_all(list(df_train, product, city_state, df_client))
str(train_complete)

# Verificar dados missing - nenhum NA identificado
sapply(train_complete, function(x) sum(is.na(x)/length(x))*100)


########################### ANÁLISE EXPLORATÓRIA ###########################


# Analisando as principais variáveis podemos observar a existencia de outliers.
# Como estes outliers podem afetar o calculo da média vamos exclui-los da base
# de dados antes de realizar os cálculos.

# A Distribuição dos dados referentes à Demanda_uni_equil se distribui de maneira
# assimétrica. Apesar dos métodos baseados em árvore que serão usados no modelo
# se tratarem de técnicas não paramétricas a transformação dos dados será feita 
# pois um modelo de regressão linear tamém será utilizado

summary(train_complete$Demanda_uni_equil)

ggplot(train_complete) +
  geom_histogram(aes(x=Demanda_uni_equil), bins = 100) +
  ggtitle(label = 'Demanda') +
  labs(x = 'Unidades', y = 'Observações') +
  theme_minimal()


# A target tem distribuição assimétrica (positiva). 
# Para facilitar as análises do modelo de regressão linear, vamos trabalhar com o
# log da target como variável dependente.

# criando o log do target
train_complete = train_complete %>%
  mutate(log_demanda = log(Demanda_uni_equil))

# visualização do novo target
ggplot(train_complete) +
  geom_histogram(aes(x=log_demanda), bins = 100) +
  ggtitle(label = 'Log da Demanda') +
  labs(x = 'Log(Unidades)', y = 'Observações') +
  theme_minimal()


# Venta_uni_hoy

summary(train_complete$Venta_uni_hoy)

# Vendo a distribuição da variável
ggplot(train_complete) +
  geom_histogram(aes(x=Venta_uni_hoy), bins = 100) +
  ggtitle(label = 'Venda unitaria') +
  labs(x = 'Unidades', y = 'Observações') +
  theme_minimal()


# Calculando o log para variável Venta_uni_hoy para melhorar a distribuição dos dados
train_complete = train_complete %>%
  mutate(log_venta_uni = log(Venta_uni_hoy))

# visualização do novo target
ggplot(train_complete) +
  geom_histogram(aes(x=log_venta_uni), bins = 100) +
  ggtitle(label = 'Log das Unidades Vendidas') +
  labs(x = 'Log(Unidades)', y = 'Observações') +
  theme_minimal()


# Venta_hoy
# A distribuição dos dados no boxplot também mostra outliers
summary(train_complete$Venta_hoy)

# Analisando a distribuição da variável
ggplot(train_complete) +
  geom_histogram(aes(x=Venta_hoy), bins =50) +
  ggtitle(label = 'Venda ($)') +
  labs(x = 'Pesos ($)', y = 'Observações') +
  theme_minimal()


# Calculando o log para variável Venta_uni_hoy para melhorar a distribuição dos dados
train_complete = train_complete %>%
  mutate(log_venta = log(Venta_hoy))

# visualização do novo target
ggplot(train_complete) +
  geom_histogram(aes(x=log_venta), bins = 100) +
  ggtitle(label = 'Log das Unidades Vendidas ($)') +
  labs(x = 'Log (Unidades)', y = 'Observações') +
  theme_minimal()


# Dev_uni_proxima
summary(train_complete$Dev_uni_proxima)

# Analisando a distribuição da variável
ggplot(train_complete) +
  geom_histogram(aes(x=Dev_uni_proxima), bins =50) +
  ggtitle(label = 'Unidades Devolvidas') +
  labs(x = 'Unidades', y = 'Observações') +
  theme_minimal()


# Calculando o log para variável Venta_uni_hoy para melhorar a distribuição dos dados
train_complete = train_complete %>%
  mutate(log_dev_uni = log(Dev_uni_proxima))

# visualização do novo target
ggplot(train_complete) +
  geom_histogram(aes(x=log_dev_uni), bins = 100) +
  ggtitle(label = 'Log das Unidades Devolvidas') +
  labs(x = 'Log (Unidades)', y = 'Observações') +
  theme_minimal()


# Dev_proxima
summary(train_complete$Dev_proxima)

# Analisando a distribuição da variável
ggplot(train_complete) +
  geom_histogram(aes(x=Dev_proxima), bins =50) +
  ggtitle(label = 'Unidades Devolvidas ($)') +
  labs(x = 'Pesos ($)', y = 'Observações') +
  theme_minimal()


# Calculando o log para variável Venta_uni_hoy para melhorar a distribuição dos dados
train_complete = train_complete %>%
  mutate(log_dev = log(Dev_proxima))

# visualização da nova variável
ggplot(train_complete) +
  geom_histogram(aes(x=log_dev), bins = 100) +
  ggtitle(label = 'Log das Devoluções ($)') +
  labs(x = 'Log (Devoluções ($))', y = 'Observações') +
  theme_minimal()

## Produtos

# Produtos mais vendidos e mais devolvidos (em unidades)

v1 <- train_complete %>% 
  arrange(desc(Venta_uni_hoy)) %>% 
  head() %>%
  ggplot(aes(x = reorder(NombreProducto, Venta_uni_hoy),Venta_uni_hoy))+
  geom_bar(stat = "identity", fill = "dodgerblue")+
  coord_flip()+
  theme_minimal()+
  labs(title = "Produtos mais vendidos", 
       y = "UNIDADE",
       x = "PRODUTO")

d2 <- train_complete %>% 
  arrange(desc(Dev_proxima)) %>% 
  head() %>% 
  ggplot(aes(x = reorder(NombreProducto, Dev_uni_proxima),Dev_uni_proxima))+
  geom_bar(stat = "identity", fill = "orange")+
  coord_flip()+
  theme_minimal()+
  labs(title = "Produtos mais devolvidos",
       y = "UNIDADE",
       x = "PRODUTO")

gridExtra::grid.arrange(v1,d2,nrow=2)

# Produtos mais vendidos e mais devolvidos (em pesos)
# Podemos observar que os produtos mais devolvidos não estão entre os produtos 
# mais vendidos

p1 <- train_complete %>% 
  arrange(desc(Venta_hoy)) %>% 
  head() %>%
  ggplot(aes(x = reorder(NombreProducto, Venta_hoy),Venta_hoy))+
  geom_bar(stat = "identity", fill = "dodgerblue")+
  coord_flip()+
  theme_minimal()+
  labs(title = "Produtos mais vendidos", 
       y = "PESOS",
       x = "PRODUTO")

p2 <- train_complete %>% 
  arrange(desc(Dev_proxima)) %>% 
  head() %>% 
  ggplot(aes(x = reorder(NombreProducto, Dev_proxima),Dev_proxima))+
  geom_bar(stat = "identity", fill = "orange")+
  coord_flip()+
  theme_minimal()+
  labs(title = "Produtos mais devolvidos",
       y = "PESOS",
       x = "PRODUTO")

gridExtra::grid.arrange(p1,p2,nrow=2)

# Vamos ver se os produtos mais devolvidos estão entre os menos vendidos
p1 <- train_complete %>% 
  arrange(desc(Venta_uni_hoy)) %>% 
  tail() %>%
  ggplot(aes(x = reorder(NombreProducto, Venta_uni_hoy),Venta_uni_hoy))+
  geom_bar(stat = "identity", fill = "dodgerblue")+
  coord_flip()+
  theme_minimal()+
  labs(title = "Produtos menos vendidos", 
       y = "PESOS",
       x = "PRODUTO")

p2 <- train_complete %>% 
  arrange(desc(Dev_uni_proxima)) %>% 
  tail() %>% 
  ggplot(aes(x = reorder(NombreProducto, Dev_uni_proxima),Dev_uni_proxima))+
  geom_bar(stat = "identity", fill = "orange")+
  coord_flip()+
  theme_minimal()+
  labs(title = "Produtos menos devolvidos",
       y = "PESOS",
       x = "PRODUTO")

gridExtra::grid.arrange(p1,p2,nrow=2)

## Estados (testar o gráfico)

train_complete %>% 
  #filter(continent == "Americas") %>% 
  ggplot(aes(x = log_demanda, y = reorder(Estado, log_demanda))) +
  geom_point(size = 3, color = "dodgerblue") 


## Cidade
# Maiores demandas identificadas por Estado
train_complete %>% 
  top_n(10, log_demanda)%>% 
  ggplot(aes(x = log_demanda, y = reorder(State, log_demanda))) +
  geom_point(size = 3, color = "dodgerblue")+
  labs(title = "Estados com maior demanda", 
     y = "ESTADO",
     x = "DEMANDA")


## Cliente
train_complete %>% 
  arrange(desc(Venta_hoy)) %>% 
  head() %>%
  ggplot(aes(x = reorder(NombreCliente, Venta_hoy),Venta_hoy))+
  geom_bar(stat = "identity", fill = "dodgerblue")+
  coord_flip()+
  theme_minimal()+
  labs(title = "Clientes que mais vendem (em pesos)", 
       y = "PESOS ($)",
       x = "CLIENTE")

# Criando o dataset para o divisão em treino e validação
df_model <- train_complete %>% select(Semana, Agencia_ID, Canal_ID, Ruta_SAK, Cliente_ID, Producto_ID, 
                                      log_demanda)


# Verificar e excluir dados infinito que podem ter surgido apís o log
#sapply(train_complete, function(x) sum(is.infinite(x)/length(x))*100)

# Excluindo valores infinitos criados no log
df_model <- df_model[is.finite(rowSums(df_model)),]

sapply(df_model, function(x) sum(is.infinite(x)/length(x))*100)



################### DIVIDIR O DATASET EM TREINO E VALIDAÇÃO ##################

# divisao dos dados
df_index <- sample.split(df_model$Semana, SplitRatio = 0.70)

# Criando dados de treino e de validação
trainset <- subset(df_model, df_index == TRUE)
validset <- subset(df_model, df_index == FALSE)

str(trainset)

rm(df_index)
#rm(train_complete)

########################## RANDOM FOREST PARA SELECIONAR VARIÁVEL ##############################
# Utilizando Random Forest para selecionar as variáveis (trainset)

control <- trainControl(method = "oob",verboseIter = F)

model_rf <- train(log_demanda ~. ,
                  data=trainset,
                  method="rf",
                  metric = "Rsquared",
                  trControl = control,
                  preProcess = c("knnImpute")
)

varImpPlot(model_rf$finalModel)


model_rf$finalModel$importance %>% 
  as.data.frame %>%
  mutate(row = rownames(.)) %>% 
  arrange(desc(IncNodePurity)) %>% 
  as_tibble()

library(randomForest)
model_rf <- randomForest(log_demanda ~ .,
                         data = trainset, 
                         ntree = 100, nodesize = 10, importance = T)

varImpPlot(model_rf)



# Seleção das variáveis mais importantes para o modelo

df_model %>% select(
  SalePrice  , Neighborhood, OverallQual , GrLivArea   , YearBuilt   ,  KitchenQual, 
  GarageCars ,  GarageArea , `1stFlrSF`  , ExterQual   , BsmtFinSF1  , FireplaceQu, 
  BsmtQual   , `2ndFlrSF`  , CentralAir  , GarageFinish, YearRemodAdd, FullBath, 
)


# Definindo novamente o conjunto de treino e validação
trainset <- subset(df_model, df_index == TRUE)
validset <- subset(df_model, df_index == FALSE)

#################### CORRELAÇÃO ################################

# Definindo as colunas (numéricas) para a análise de correlação 
# As variáveis Venta_uni_hoy e Dev_uni_proxima não entrarão pois elas compõem 
# a variável target.

str(df_model)  # alterar df nas correlação
cols_num <- c("Semana","Canal_ID","Ruta_SAK","Cliente_ID","Producto_ID", 
              "log_demanda","Agencia_ID")

# Vetor com os métodos de correlação
metodos <- c("pearson", "spearman")

# Aplicando os métodos de correlação com a função cor()
df_model <- as.data.frame(df_model)
cors <- lapply(metodos, function(method) 
  (cor(df_model[, cols_num], method = method)))

head(cors)

# Preparando o plot
plot.cors <- function(x, labs){
  diag(x) <- 0.0 
  plot( levelplot(x, 
                  main = paste("Plot de Correlação", labs),
                  scales = list(x = list(rot = 90), cex = 1.0)) )
}

# Mapa de Correlação mostra correlação alta entre a target e Venta_hoy e Venta_uni_hoy
Map(plot.cors, cors, metodos)

#################### AJUSTANDO O MODELO ARVORE DE DECISÃO ########################
# Rsquared = 0.996

control <- trainControl(method = "cv", number = 5,verboseIter = F)

tunegrid <- expand.grid(cp=seq(0.001, 0.01, 0.001))

model_tree <- 
  train(y=trainset$log_demanda, x=trainset[,-1],
        method="rpart",
        trControl=control,
        tuneGrid=tunegrid,
        metric = "Rsquared"
  )
model_tree

# Plot do resultado
library(rpart.plot)
rpart.plot(model_tree$finalModel, cex = 0.5)


# Salvando o resultado
validset$Cliente_ID %>% cbind(predict(model_tree, validset) %>% exp) %>% 
  `colnames<-`(c("Cliente_Id", "Demanda")) %>%
  write.csv("model_tree.csv",row.names = F)


#############################  BAGGING #################################### 
# Rsquared = 0.975

control <- trainControl(method = "cv", number = 5,verboseIter = F)

model_bag <- train(y=trainset$log_demanda, 
                    x=trainset[,-1], 
                    method = "treebag",
                    metric = "Rsquared",
                    trControl=control
)
model_bag

## Salvando o resultado em disco
validset$Cliente_ID %>% cbind(predict(model_bag, validset)%>% exp) %>% 
  `colnames<-`(c("Cliente_Id", "Demanda")) %>%
  write.csv("Model_bag.csv",row.names = F)

###################### RANDOM FOREST - não funcionou ############################### 

tunegrid <- expand.grid(mtry = seq(4, ncol(trainset) * 0.8, 2))

control <- trainControl(method = "cv", number = 5,verboseIter = F)

model_rf2 <- train(log_demanda ~. ,
               data=trainset,
               method="rf",
               metric = "Rsquared",
               tuneGrid=tunegrid,
               trControl=control
)
model_rf2

validset$Cliente_ID %>% cbind(predict(model_rf2, validset) %>% exp) %>% 
  `colnames<-`(c("Cliente_Id", "Demanda")) %>%
  write.csv("model_rf2.csv",row.names = F) 

################################# GBM - muito lento ######################################

control <- trainControl(method = "cv", number = 5,verboseIter = F)

model_gbm <- train(log_demanda~.,data=trainset,
                method = "gbm",
                trControl=control,
                tuneLength=5,
                metric = "Rsquared",
                verbose = FALSE
)
model_gbm


validset$Cliente_ID %>% cbind(predict(model_gbm, validset) %>% exp) %>%
  `colnames<-`(c("Cliente_Id", "Demanda")) %>%
  write.csv("Model_gbm.csv", row.names = F)

################### REGRESSÃO LINEAR #################################
#Rsquared: 0.04

control <- trainControl(method = "cv", number = 5,verboseIter = F)

model_lm <- train(log_demanda~.,data=trainset,
               method = "lmStepAIC",
               trControl=control,
               metric = "Rsquared",trace=F
)
model_lm

# Visualizando o modelo
library(GGally)

ggcoef(
  model_lm$finalModel,                      #O modelo a ser conferido
  vline_color = "red",          #Reta em zero  
  errorbar_color = "blue",      #Cor da barra de erros
  errorbar_height = .25,
  shape = 18,                   #Altera o formato dos pontos centrais
  size=2,                      #Altera o tamanho do ponto
  color="black",
  exclude_intercept = TRUE,                #Altera a cor do ponto
  mapping = aes(x = estimate, y = term, size = p.value))+
  scale_size_continuous(trans = "reverse")+ #Essa linha faz com que inverta o tamanho
  theme_bw()

# Análise dos ruídos
library(ggfortify)
model_lm$finalModel %>% 
  autoplot(which = 1:2) + 
  theme_bw()

# Salvando o modelo em disco
validset$Cliente_ID %>% cbind(predict(model_lm, validset) %>% exp ) %>% 
  `colnames<-`(c("Cliente_Id", "Demanda")) %>%
  write.csv("Model_lm.csv",row.names = F)

####################### COMPARANDO OS MODELOS ##########################3
install.packages('gdata')
library (gdata)
library(tidyr)

resamps <- resamples(list(rpart = model_tree,
                          treebag = model_bag, lm = model_lm))
                          #rf = model_rf2,
                          #gbm = model_gbm,
 #                         lmStepAIC = model_lm 

summary(resamps)
dotplot(resamps)


######
library(tidyr)

resamps$values %>%
  select(1, ends_with("RMSE")) %>%
  gather(model, RMSE, -1) %>%
  mutate(model = sub("~RMSE", "", model)) %>%
  {bwplot(model ~ RMSE, data = .)}

###################### OUTRO MODELO UTILIZANDO GRADIENT BOOSTING ###################### 

#library(doParallel)
library(gbm)

model_gbm <- gbm(log_demanda ~ . , 
                 data = trainset,
                 distribution = "gaussian", 
                 n.trees = 100, shrinkage = 0.1,             
                 interaction.depth = 3,
                 cv.folds = 5, 
                 verbose = FALSE, 
                 n.cores = NULL)  


summary(model_gbm)

## Avaliando o resultado do modelo criado.

pred_model_gbm <- data.frame(actual = validset$log_demanda,predict = round(predict(model_gbm, validset)))
pred_model_gbm <- as.vector(pred_model_gbm)
pred_model_gbm[,"FakeRow"] <- seq(1,nrow(pred_model_gbm))

ggplot() +
  geom_line(data = pred_model_gbm, aes(x = FakeRow, y = actual)) +
  geom_line(data = pred_model_gbm, aes(x = FakeRow, y = predict), color = "orange") +
  ylab("Demanda Estoque")

# 84% dos dados de validação tiveram uma previsão do valor correto!
pred_model_gbm <- mutate(pred_model_gbm, resids = predict - actual)
pred_model_set <- pred_model_gbm %>% mutate(classf = ifelse(resids == 0, 0, -1))

prop.table(table(pred_model_set$classf))


######################### DADOS TESTE ######################################

# Segundo as informações passadas do dataset Test, este não possui algumas variáveis. 

# importar dataset
test <- fread('test.csv')

# O dataset test não possui todas as variáveis do dataset train
str(test)
head(test)

test$id <- NULL

## PRÉ-PROCESSAMENTO

ggplot(test) +
  geom_bar(aes(x=Agencia_ID)) +
  ggtitle(label = 'Andar do imóvel') +
  labs(x = '# andar', y = 'Observações') +
  theme_minimal()


# Ajustar o dataset teste inserindo as colunas que faltam: demanda media de vendas e devoluções por cliente do dados treino
str(trainset)

# alterando a ordem das colunas
trainset <- trainset %>% select (log_demanda, Semana, Cliente_ID,Agencia_ID, Canal_ID, Ruta_SAK,
                                 Producto_ID)

# criando a variável com a média de compra por cliente
df_mean_cliente <- aggregate(trainset[, 1], list(trainset$Cliente_ID), mean)

# Renomear colunas
names(df_mean_cliente) <- c("Cliente_ID", "Media_Demanda_Cli")

# criando a variável com a demanda média por produto (Producto_ID é uma das variáveis que mais impactamo modelo)
df_mean_prod <- aggregate(trainset[, 1], list(trainset$Producto_ID), mean)

# Renomear colunas
names(df_mean_prod) <- c("Producto_ID", "Media_Demanda_Prod")

# unir os df com a média com o teste (apenas as linhas que possuem Cliente_ID
# correspondente)
testset <- left_join(test, df_mean_cliente, by = "Cliente_ID")

testset <- left_join(testset, df_mean_prod, by = "Producto_ID")

head(testset)

# Imputando dados de média de Demanda de acordo com produto em dados NA
i <- is.na(testset$Media_Demanda_Cli)
testset$Media_Demanda_Cli[i] <- testset$Media_Demanda_Prod[i]

# Excluindo coluna Media Demanda_Prod
testset$Media_Demanda_Prod <- NULL



### Inspecionando os dados (colunas, linhas e contagem de NA's)
nrow(df) 
length(which(is.na(testset$Media_Demanda_Cli)))

### Mapeia os valores NA's
na <- which(is.na(testset$Media_Demanda_Cli), arr.ind = TRUE, useNames = TRUE) 
head(na)


head(testset)
str(testset)




# Verificar os NAs
sapply(testset, function(x) sum(is.na(x)/length(x))*100)

# Substituir os NAs restantes pela média da coluna
# A estratégia escolhida para imputar os poucos dados NAs restante foi tomada de
# maneira arbitrária. Os valores faltantes serão preenchidos com a média da
# variável Média_Demanda_Cli
summary(testset$Media_Demanda_Cli)
testset$Media_Demanda_Cli[which(is.na(testset$Media_Demanda_Cli))] <- mean(testset$Media_Demanda_Cli)


#################### TESTAR O MODELO  #######################3

# APLICANDO O MODELO CRIADO O DATASET TEST CUJO O QUAL NÃO TEMOS COMO AVALIAR A ASSERTIVIDADE, OU SEJA, ESTAMOS APLICANDO ML
# PARA PREVER OS VALORES FUTUROS

pred_test <- round(predict(model_gbm, newdata = testset))

df_test <- data.frame(testset, pred_test)
head(df_test)

## Avaliando o resultado do modelo criado.

pred_test_gbm <- data.frame(actual = testset$Media_Demanda_Cli,predict = round(predict(model_gbm, testset)))
pred_test_gbm <- as.vector(pred_test_gbm)
pred_test_gbm[,"FakeRow"] <- seq(1,nrow(pred_test_gbm))

ggplot() +
  geom_line(data = pred_test_gbm, aes(x = FakeRow, y = actual)) +
  geom_line(data = pred_test_gbm, aes(x = FakeRow, y = predict), color = "orange") +
  ylab("Demanda Prevista")

# 84% dos dados de validação tiveram uma previsão do valor correto!
pred_test_gbm <- mutate(pred_model_gbm, resids = predict - actual)
pred_test_set <- pred_model_gbm %>% mutate(classf = ifelse(resids == 0, 0, -1))

prop.table(table(pred_test_set$classf))


#Arvore
testset$Cliente_ID %>% cbind(predict(model_tree, testset) %>% exp) %>% 
  `colnames<-`(c("Cliente_Id", "Demanda")) %>%
  write.csv("model_tree.csv",row.names = F)




