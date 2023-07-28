
###INSTALANDO/CARREGANDO PACOTES
install.packages("dlookr") #pacote para consgeguir algumas medidas automatizadas em formato PDF
install.packages("tidyverse")#para rodas gráficos (pois contém ggplot2) efunções do dplyr
install.packages("corrplot") #pacote para consgeguir algumas medidas automatizadas em formato PDF
library(dlookr)
library(tidyverse)
library (corrplot)

## Carregando datasets
cars_train <- read.csv(file = "cars_train.csv",sep = "\t", fileEncoding = "UTF-16le")
cars_test <- read.csv(file = "cars_test.csv", sep = "\t", fileEncoding = "UTF-16le")

##Copiando bancos
d_treino <- cars_train
d_teste <- cars_test

#exportando para pdf - inserir no repositório
write.csv2(d_treino, file = "banco_tratado.csv", row.names = FALSE)
## Visualizando banco
View(d_treino)
glimpse(d_treino)
View(d_teste)
glimpse(d_teste)

## verificando observações duplicadas
duplicated(d_treino)
sum(duplicated(d_treino))

duplicated(d_teste)
sum(duplicated(d_teste)) 

## Verificando estatísticas do banco
summary(d_treino)
summary (d_teste)

# Retirando colunas desnecessárias para a análise 
# Justificativa: não tem nenhum dado ou constam variáveis categóricas e logicas que não tem dois níveis.

d_treino <- d_treino %>% 
  select(-id, -num_portas, -entrega_delivery, -garantia_de_fábrica,
         -revisoes_dentro_agenda, -veiculo_licenciado, -ipva_pago,
         -elegivel_revisao, -revisoes_concessionaria, -veiculo_único_dono,
         -dono_aceita_troca, -veiculo_alienado)

# Padronizando casas decimais na variável preco do banco treino
d_treino <- d_treino %>% 
  mutate(preco = round(preco, 2))
summary(d_treino$preco)

# Transformando espaços vazios em NA
d_treino [d_treino == ""] <- NA

###Tratando NAs
## Transformando Strings em Caracteres
d_treino$marca <- with(d_treino, as.factor(marca))
d_treino$modelo <- with(d_treino, as.factor(modelo))
d_treino$versao <- with(d_treino, as.factor(versao))
d_treino$cambio <- with(d_treino, as.factor(tipo))
d_treino$blindado <- with(d_treino, as.factor(blindado))
d_treino$tipo_vendedor <- with(d_treino, as.factor(tipo_vendedor))
d_treino$cidade_vendedor <- with(d_treino, as.factor(cidade_vendedor))
d_treino$estado_vendedor <- with(d_treino, as.factor(estado_vendedor))
d_treino$anunciante <- with(d_treino, as.factor(anunciante))
d_treino$troca <- with(d_treino, as.factor(troca))

## Aqui pegamos as vars numericas que estao como NA e substituimos elas pela mediana
# para nao termos de descartar essas vars
numeric_vars <- sapply(d_treino, is.numeric)
d_treino[numeric_vars] <- lapply(d_treino[numeric_vars], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))

# Aqui substituimos as vars categoricas que estao como NA pela moda, de forma a 
# evitar descartar essas vars
categorical_vars <- sapply(d_treino, is.character)
d_treino[categorical_vars] <- lapply(d_treino[categorical_vars], function(x) ifelse(is.na(x), names(which.max(table(x))), x))


### GRÁFICOS
## Distribuição fotos/anúncio
chart1 <- ggplot(d_treino) +
  aes(x = num_fotos) +
  geom_histogram(bins = 26L, fill = "#4682B4") +
  labs(
    x = "Número de fotos",
    y = "Contagem",
    title = "Distribuição do Número de Fotos por Anúncio"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14L),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.title.x = element_text(size = 12L,
                                face = "bold"))

ggsave("chart1.png", width = 4, height = 2.3, dpi = 200)

# Distribuição anos de fabrição
chart2 <- ggplot(d_treino) +
  aes(x = ano_de_fabricacao) +
  geom_histogram(bins = 26L, fill = "#4682B4") +
  labs(
    x = "Anos de Fabricação",
    y = "Contagem",
    title = "Distribuição dos Anos de Fabricação dos Veículos"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14L),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.title.x = element_text(size = 12L,
                                face = "bold"))

ggsave("chart2.png", width = 4, height = 2.3, dpi = 200)

# Distribuição anos dos modelo 
chart3 <- ggplot(d_treino) +
  aes(x = ano_modelo) +
  geom_histogram(bins = 26L, fill = "#4682B4") +
  labs(
    x = "Anos do Modeloo",
    y = "Contagem",
    title = "Distribuição dos Anos do Modelo dos Veículos"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14L),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.title.x = element_text(size = 12L,
                                face = "bold"))

ggsave("chart3.png", width = 4, height = 2.3, dpi = 200)

# Distribuição quilometragem
chart4 <- ggplot(d_treino) +
  aes(x = hodometro) +
  geom_histogram(bins = 26L, fill = "#4682B4") +
  labs(
    x = "Quilometragem dos Veículos",
    y = "Contagem",
    title = "Distribuição da Quilometragem dos Veículos"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14L),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.title.x = element_text(size = 12L,
                                face = "bold"))

ggsave("chart4.png", width = 4, height = 2.3, dpi = 200)

# Distribuição de nº portas
chart5 <- ggplot(d_treino) +
  aes(x = num_portas) +
  geom_bar(bins = 26L, fill = "#4682B4") +
  labs(
    x = "Número de Portas",
    y = "Contagem",
    title = "Distribuição do Número de Portas dos Veículos"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14L),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.title.x = element_text(size = 12L,
                                face = "bold"))

ggsave("chart5.png", width = 4, height = 2.3, dpi = 200)

# Variação preços veículos
chart6 <- ggplot(d_treino) +
  aes(x = preco) +
  geom_boxplot(bins = 26L, fill = "#4682B4") +
  labs(
    x = "Variação de Preços",
    y = "Contagem",
    title = "Variação de Preços dos Veículos Anunciados"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14L),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.title.x = element_text(size = 12L,
                                face = "bold"))
# Distribuição modelo
ggplot(d_teste) +
  aes(x = modelo) +
  geom_bar(fill = "#112446") +
  coord_flip() +
  theme_minimal()

# transformando banco em dataframe necessário para não conflitar com função do dlookr
df_treino <- as.data.frame(d_treino)

# gerando relatório automático do banco para usar template e algumas
# tabelas prontas, vou editar posteriorimente.

#================= Se você estiver usando o RStudio, vá para FERRAMENTAS >
#OPÇÕES GLOBAIS > SWEAVE e marque "Usar TinyTex ao compilar arquivos .tex".


eda_paged_report(d_treino,
                 output_format = "pdf",
                 output_file = "Banco_tratado.pdf",
                 title = "Desafio Lighthouse: EDA",
                 author = "Elig Cassiane Arse da Silva",
                 create_date = "",
                 theme = "blue")

### GRÁFICOS NOMINAIS
# Preço por ano de fabricação
chart7 <- ggplot(d_treino) +
  aes(x = preco, y = ano_de_fabricacao) +
  geom_boxplot(fill = "#4682B4") +
  labs(
    x = "Preço",
    y = "Ano de fabricação",
    title = "Preço por Ano de fabricação"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14L),
    axis.title.y = element_text(size = 12L,
                                face = "bold"),
    axis.title.x = element_text(size = 12L,
                                face = "bold")
  )
ggsave("chart7.png", width = 6, height = 6, dpi = 400)

# Anúncios por marca
chart.8 <- ggplot(d_treino) +
  aes(x = marca) +
  geom_bar(fill = "#4682B4") +
  labs(
    x = "Contagem",
    y = "Marcas",
    title = "Anúncios de Veículos por Marca"
  ) +
  coord_flip() +
  theme_bw() +
  theme(
    axis.title.y = element_text(size = 12L,
                                face = "bold"),
    axis.title.x = element_text(size = 12L,
                                face = "bold")
  )
ggsave("chart.8.png", width = 6, height = 6, dpi = 200)

#Carros por tipo
chart9 <- ggplot(d_treino) +
  aes(x = tipo) +
  geom_bar(fill = "#4682B4") +
  labs(
    x = "Tipos",
    y = "Contagem",
    title = "Distribuição de Carros por Tipo de Veículo"
  ) +
  theme_bw() +
  theme(
    axis.title.y = element_text(size = 12L,
                                face = "bold"),
    axis.title.x = element_text(size = 12L,
                                face = "bold")
  )
ggsave("chart9.png", width = 4, height = 2.3, dpi = 200)

# Carros por cor
chart10 <- ggplot(d_treino) +
  aes(x = cor) +
  geom_bar(fill = "#4682B4") +
  labs(
    x = "Cor",
    y = "Contagem",
    title = "Distribuição de Carros por cor"
  ) +
  theme_bw() +
  theme(
    axis.title.y = element_text(size = 12L,
                                face = "bold"),
    axis.title.x = element_text(size = 12L,
                                face = "bold")
  )
ggsave("chart10.png", width = 4, height = 2.3, dpi = 200)

# Tipo de vendedor por estado
chart11 <- ggplot(d_treino) +
  aes(x = estado_vendedor, fill = tipo_vendedor) +
  geom_bar() +
  scale_fill_manual(
    values = c(PF = "#5B5F60",
               PJ = "#4682B4")
  ) +
  labs(
    x = "Estado",
    y = "Contagem",
    title = "Tipo de Vendedor por Estado"
  ) +
  coord_flip() +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.title.y = element_text(size = 12L,
                                face = "bold"),
    axis.title.x = element_text(size = 12L,
                                face = "bold")
  )
ggsave("chart11.png", width = 6, height = 4, dpi = 200)

# Cor e adesão à troca
chart12 <- ggplot(d_treino) +
  aes(x = cor, fill = dono_aceita_troca) +
  geom_bar() +
  scale_fill_manual(values = c(`Aceita troca` = "#4682B4")) +
  labs(
    x = "Cor",
    y = "Count",
    title = "Relação de cor com adesão à troca"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14L),
    axis.title.y = element_text(size = 12L,
                                face = "bold"),
    axis.title.x = element_text(size = 12L,
                                face = "bold")
  )
ggsave("chart12.png", width = 6, height = 4, dpi = 200)

# Preço por marca
charta <- ggplot(d_treino) +
  aes(x = preco, y = marca) +
  geom_boxplot(fill = "#4682B4") +
  labs(x = "Preço", y = "Marca", title = "Preço por marca") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14L),
    axis.title.y = element_text(size = 12L,
                                face = "bold"),
    axis.title.x = element_text(size = 12L,
                                face = "bold")
  )
ggsave("charta.png", width = 6, height = 4, dpi = 200)

#Preço por ano fab
chartb <- ggplot(d_treino) +
  aes(x = preco, y = ano_fabricacao) +
  geom_boxplot(fill = "#4682B4") +
  labs(x = "Preço", y = "Ano fabricação", title = "Preço por ano de fabricação") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14L),
    axis.title.y = element_text(size = 12L,
                                face = "bold"),
    axis.title.x = element_text(size = 12L,
                                face = "bold")
  )
ggsave("chartb.png", width = 6, height = 4, dpi = 200)



# Preço por câmbio
chart13 <- ggplot(d_treino) +
  aes(x = preco, y = cambio) +
  geom_boxplot(fill = "#4682B4") +
  labs(x = "Preço", y = "Câmbio", title = "Preço por câmbio") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14L),
    axis.title.y = element_text(size = 12L,
                                face = "bold"),
    axis.title.x = element_text(size = 12L,
                                face = "bold")
  )
ggsave("chart13.png", width = 6, height = 4, dpi = 200)



# Preço por tipo
chart14 <- ggplot(d_treino) +
  aes(x = preco, y = tipo) +
  geom_boxplot(fill = "#4682B4") +
  labs(x = "Preço", y = "Tipo", title = "Preço por Tipo") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14L),
    axis.title.y = element_text(size = 12L,
                                face = "bold"),
    axis.title.x = element_text(size = 12L,
                                face = "bold")
  )
ggsave("chart14.png", width = 5, height = 4, dpi = 200)

# Preço por ano de fabricação
chart15 <- ggplot(d_treino) +
  aes(x = preco, y = ano_fabricacao) +
  geom_boxplot(fill = "#4682B4") +
  labs(x = "Preço", y = "Ano Fabricação", title = "Preço por Ano de Fabricação") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14L),
    axis.title.y = element_text(size = 12L,
                                face = "bold"),
    axis.title.x = element_text(size = 12L,
                                face = "bold")
  )
ggsave("chart15.png", width = 5, height = 4, dpi = 200)

### RESPONDENDO PERGUNTAS DE NEGÓCIO.
# A) Qual o melhor estado cadastrado na base de dados para se vender um carro de
# marca popular e por quê?

# Filtrando os dados para incluir apenas carros de marcas populares

car_pop <- d_treino %>%
  filter(marca %in% c("VOLKSWAGEN", "CHEVROLET", "FIAT"))

mean(x = car_pop$preco)
summary(car_pop)

estatisticas_preco_populares <- car_pop %>%
  summarize(
    Desvio_Padrao = sd(preco),
    Primeiro_Quartil = quantile(preco, 0.25),
    Mediana = median(preco),
    Media = mean(preco),
    Terceiro_Quartil = quantile(preco, 0.75),
    Min = min(preco),
    Max = max(preco)
  )
print(estatisticas_preco_populares)

#Variação de preço nos Estados
cart19 <- ggplot(car_pop) +
  aes(x = estado_vendedor, y = preco) +
  geom_boxplot(fill = "#4682B4") +
  labs(
    x = "Preço",
    y = "Estados",
    title = "Variação de preço por Estados"
  ) +
  coord_flip() +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 12L,
                                face = "bold")
  ) +
  facet_wrap(vars(marca))
ggsave("chart19.png", width = 5, height = 4, dpi = 200)

# Calculando o preço médio dos carros de marca popular para cada estado
preco_med_estado <- car_pop %>%
  group_by(estado_vendedor)%>%
  summarize(media_preco = mean(preco))

#Identificando o estado com o preço médio mais alto
estado_melhor_preco <- preco_med_estado %>%
  filter(media_preco == max(media_preco))

print(estado_melhor_preco)
#estado_vendedor media_preco
#<chr>                 <dbl>
#1 Piauí (PI)          181713.

mean(x = d_treino$preco)

# Verificando a variabilidade dos preços em cada estado
desv_pad_estado <- car_pop %>%
  group_by(estado_vendedor) %>%
  summarize(desvio_padrao_preco = sd(preco))

#Preparação para visualização gráfica dos resultados
carros_populares <- d_treino %>%
  filter(marca %in% c("VOLKSWAGEN", "CHEVROLET", "FIAT")) %>%
  group_by(estado_vendedor) %>%
  summarize(desvio_padrao_preco = sd(preco))

# Gráfico de barras mostrando o desvio padrão de cada estado
chart18 <- ggplot(carros_populares, aes(x = estado_vendedor, y = desvio_padrao_preco)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Desvio Padrão do Preço por Estado",
       x = "Estado Vendedor",
       y = "Desvio Padrão do Preço") +
  coord_flip() +
  theme_bw()
ggsave("chart18.png", width = 5, height = 4, dpi = 200)


### RESPONDENDO PERGUNTAS DE NEGÓCIO.
# B) Qual o melhor estado cadastrado na base de dados para se vender um carro de
# marca popular e por quê?
#Variância do preço de picapes
chart20 <- d_treino %>%
  filter(cambio %in% "Automática") %>%
  filter(tipo %in% "Picape") %>%
  filter(!is.na(dono_aceita_troca)) %>%
  filter(!is.na(veiculo_único_dono)) %>%
  filter(!is.na(revisoes_concessionaria)) %>%
  filter(!is.na(ipva_pago)) %>%
  filter(!is.na(veiculo_licenciado)) %>%
  filter(!is.na(garantia_de_fábrica)) %>%
  filter(!is.na(revisoes_dentro_agenda)) %>%
  ggplot() +
  aes(x = preco, y = estado_vendedor) +
  geom_boxplot(fill = "#4682B4") +
  labs(
    x = "Preço",
    y = "Estado",
    title = "Variação do preço de picapes"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 12L,
                                face = "bold"),
    axis.title.x = element_text(size = 12L,
                                face = "bold")
  )
ggsave("chart20.png", width = 4, height = 2, dpi = 200)

# Filtrando os dados para incluir apenas picapes com transmissão automática
picapes_automaticas <- d_treino %>%
  filter(tipo == "Picape" & cambio == "Automática")

# Calculando as estatísticas desejadas
estatisticas_preco_picapes_automaticas <- picapes_automaticas %>%
  summarize(
    Desvio_Padrao = sd(preco),
    Primeiro_Quartil = quantile(preco, 0.25),
    Mediana = median(preco),
    Media = mean(preco),
    Terceiro_Quartil = quantile(preco, 0.75)
  )

# Exibindo a tabela com as estatísticas
print(estatisticas_preco_picapes_automaticas)

#Gerando box-plot (menor)
picapes_automaticas <- d_treino %>%
  filter(cambio == "Automática" & tipo == "Picape")

chart21 <- ggplot(picapes_automaticas) +
  aes(x = estado_vendedor, y = preco, aes(x = estado_vendedor, y = preco)) +
  geom_boxplot(fill = "#4682B4") +
  labs(x = "Estado Vendedor", y = "Preço") +
  coord_flip() +
  theme_bw()

ggsave("chart21.png", width = 4, height = 2, dpi = 200)

### RESPONDENDO PERGUNTAS DE NEGÓCIO.
# C) Qual o melhor estado para se comprar carros que ainda estejam
#dentro da garantia de fábrica e por quê?
#Variância do preço de de garantia

# Filtrando os carros que ainda estão dentro da garantia de fábrica
# Filtrar os carros com garantia de fábrica
carros_garantia_fabrica <- d_treino %>%
  filter(garantia_de_fábrica == "Garantia de fábrica")

# Calculando estatísticas por estado
estatisticas_por_estado <- carros_garantia %>%
  group_by(estado_vendedor) %>%
  summarize(media_preco = mean(preco),
            mediana_preco = median(preco),
            desvio_padrao_preco = sd(preco),
            q1_preco = quantile(preco, 0.25),
            q3_preco = quantile(preco, 0.75))

## Calcular somente média dos preços por estado
media_preco_por_estado <- carros_garantia_fabrica %>%
  group_by(estado_vendedor) %>%
  summarize(media_preco = mean(preco))

#Escolhendo o estado com a menor média de preços
melhor_estado <- media_preco_por_estado %>%
  filter(media_preco == min(media_preco))

## Criando o gráfico
chart23<-ggplot(media_preco_por_estado, aes(x = estado_vendedor, y = media_preco)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Média de Preços por Estado para Carros com Garantia de Fábrica",
       x = "Estado Vendedor",
       y = "Média de Preço") +
  coord_flip()+
  theme_minimal()
ggsave("chart23.png", width = 4, height = 2, dpi = 200)
# Visualizando a tabela com as estatísticas
print(estatisticas_por_estado)

## Removendo estados sem desvio padrão (sem variação de preço)
estatisticas_por_estado <- estatisticas_por_estado %>%
  filter(!is.na(desvio_padrao_preco))
# repetir código do gráfico
chart23<-ggplot(media_preco_por_estado, aes(x = estado_vendedor, y = media_preco)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Média de Preços por Estado para Carros com Garantia de Fábrica",
       x = "Estado Vendedor",
       y = "Média de Preço") +
  coord_flip()+
  theme_minimal()
ggsave("chart23.png", width = 4, height = 2, dpi = 200)

