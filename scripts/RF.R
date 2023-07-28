install.packages(c("randomForest", "xlsx", "doParallel", "dplyr", "tidyverse", "caret", "fastDummies", "parallel"))

library(randomForest)
library(dplyr)
library(tidyverse)
library(caret)
library(fastDummies)
library(parallel)
library(doParallel)
library(xlsx)

# Aqui carregamos os dados

cars_train <- read.csv(file = "cars_train.csv",sep = "\t", fileEncoding = "UTF-16le")
cars_test <- read.csv(file = "cars_test.csv", sep = "\t", fileEncoding = "UTF-16le")

# Reduzimos o numero de variaveis de forma a reduzir o numero de vars dummies no modelo
dados_treino = cars_train %>% select(-id, -num_fotos, -num_portas, -cidade_vendedor, -versao, -cor, -entrega_delivery, -elegivel_revisao, -dono_aceita_troca, -garantia_de_fábrica)


# Aqui pegamos as vars numericas que estao como NA e substituimos elas pela mediana
# para nao termos de descartar essas vars
numeric_vars <- sapply(dados_treino, is.numeric)
dados_treino[numeric_vars] <- lapply(dados_treino[numeric_vars], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

# Aqui substituimos as vars categoricas que estao como NA pela moda, de forma a 
# evitar descartar essas vars
categorical_vars <- sapply(dados_treino, is.character)
dados_treino[categorical_vars] <- lapply(dados_treino[categorical_vars], function(x) ifelse(is.na(x), names(which.max(table(x))), x))


# Criamos uma base com todas as colunas transformadas em dummies atraves de hot encoding

df_dummies <- fastDummies::dummy_cols(dados_treino)

# Descartamos as vars nao numericas (ou seja, as categoricas nao dummies)
df_numeric <- df_dummies %>% select(where(is.numeric))

# Colocamos uma seed para replicar e separamos a base de treito e base de teste do modelo
set.seed(2023) 
train_indices <- sample(1:nrow(df_numeric), nrow(df_numeric) * 0.7)
train_set <- df_numeric[train_indices, ]
test_set <- df_numeric[-train_indices, ]

# Identificamos o numero de cores do processador
no_cores <- detectCores() - 1


# Aqui usamos o ntree de 500 arvores, que é o padrão, junto de um kfold de 3, 
# com 3 repetições e um tunegrid de 3, com mtrys deliberados de forma a evitar 
#um valor alto que gere overfitting, esses valores foram escolhidos de forma
# a reduzir o tempo para treinamento do modelo
tuning_grid <- expand.grid(.mtry = c(10, 33, 120))

# Train the random forest model with the specified mtry values
model_random_forests <- caret::train(
  x = train_set %>% select(-preco),
  y = train_set$preco,
  method = "rf",
  tuneGrid = tuning_grid,   # Use the custom tuning grid
  metric = "RMSE",
  trControl = trainControl(
    allowParallel = FALSE,   # Change to TRUE if you want to allow parallel processing
    number = 3,
    repeats = 3,
    method = "repeatedcv",
    verboseIter = TRUE
  )
)


stopCluster(cl)



# Aqui preparamos a base que queremos prever de forma a ficar igual a na qual 
# o modelo foi treinado

test_dummies <- fastDummies::dummy_cols(cars_test)

# Aqui pegamos as colunas dummy ausentes na base de teste, e transformamos em 0
# uma vez que sua ausencia significa nenhuma observacao com aquela caracteristica

missing_cols = setdiff(names(df_dummies), names(test_dummies))
test_dummies[missing_cols] <- 0


# Aqui retiramos as vars categoricas nao numericas, tal qual fizemos antes

teste_numeric <- test_dummies %>% select(where(is.numeric))
teste_numeric <- replace(teste_numeric, is.na(teste_numeric), 0)

# Aqui fazemos a previsao com o modelo
prediction = predict(model_random_forests, teste_numeric)

id = cars_test$id

precos_preditos = as.data.frame(cbind(id, prediction))

colnames(precos_preditos) = c("id", "price")

write.xlsx(precos_preditos, "precos_preditos.xlsx")

# Aqui fazemos plot com a selecao do mtry e salvamos

ggplot(model_random_forests) +
  geom_line(size = 1.3) +
  geom_point(size = 5) +
  geom_vline(xintercept = model_random_forests$bestTune$mtry, linetype = "dashed", alpha = 0.5, size = 1) +
  theme_bw(base_size = 22) +
  theme(aspect.ratio = 1)

ggsave(
  filename = "cross_validation_result.pdf",
  device = "pdf",
  width = 9,
  height = 7)

