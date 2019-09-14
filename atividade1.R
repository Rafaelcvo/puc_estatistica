install.packages("corrplot")
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(corrplot)
library(carData)

#importando os dataset
df <- Prestige
View(df)

# Descrição do Dataset
# O dataset Prestige contém 102 observações com seis variáveis. 
# Education: Número médio de anos de estudo para a formação na ocupação.
# Income: Rendimento médio na ocupação, em dólares.
# Women: Percentual de mulheres na ocupação.
# Prestige: Taxa média de prestígio na ocupação.
# Census: Código da ocupação usado na pesquisa.
# Type: Tipo de ocupação.

# 1. Filtrando as colunas para remover Census e Type.
#df$census <- NULL
#df$type <- NULL
df <- df[,c(1,2,3,4)]

# 2. Matriz de correlação.
# Correlaçao de Pearson.
cor(df, df$prestige, method = "pearson")

# Correlação de Kendall
cor(df, df$prestige, method = "kendall")

# Correlação de Spearman
cor(df, df$prestige, method = "spearman")

#plot do em grafico com a matriz de correlação usando numeros.
m <- cor(df)
corrplot(m, method = 'number')

# Matriz de correlação usando PerformanceAnalytics.
chart.Correlation(df, histogram = TRUE)

# 3. Modelo de regressão linear composta.
rl <- lm(prestige ~ education + income + women, data = df)
summary(rl)
plot(rl, which = 5)












