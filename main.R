# Importando pacotes
if(!require("pacman")) install.packages("pacman")
pacman::p_load(latex2exp, moments, tidyverse)


#Lendo os dados
dados <- readxl::read_xlsx("Amostra_g04_Andre_Camila_Rita.xlsx")
dados$LOCAL <- factor(dados$LOCAL,
                      levels = c(1, 2), 
                      labels = c("Urbana", "Rural"))
dados$ADM<- factor(dados$ADM, 
                   levels = c(1, 2, 3), 
                   labels = c("Federal", "Estadual", "Municipal"))
dados$TAM_MUN<- factor(dados$TAM_MUN, 
                       levels = c(1, 2, 3, 4, 5), 
                       labels = c("<20000 hab", 
                                  "20000 a 49999 hab", 
                                  "50000 a 99999 hab", 
                                  "100000 a 999999 hab", 
                                  "1000000 ou mais"))
dados$TAM_ESCOLA<- factor(dados$TAM_ESCOLA,
                          levels = c(1, 2, 3, 4), 
                          labels = c("<25", 
                                     "25 a 49",
                                     "50 a 99",
                                     "100 ou mais"))

set.seed(20242)
amostra <- dados %>% 
  sample_n(size = 50)

#**OBJETIVO 1**
#**Descrever as características das escolas e o desempenho de seus estudantes na Prova**


table(dados$REG)
dados %>%
  ggplot() +
  geom_bar(aes(x = REG))

table(amostra$REG)
amostra %>%
  ggplot() +
  geom_bar(aes(x = REG))


table(dados$LOCAL)
dados %>%
  ggplot() +
  geom_bar(aes(x = LOCAL))

table(amostra$LOCAL)
amostra %>%
  ggplot() +
  geom_bar(aes(x = LOCAL))


table(dados$TAM_MUN)
dados %>%
  ggplot() +
  geom_bar(aes(x = TAM_MUN))

table(amostra$TAM_MUN)
amostra %>%
  ggplot() +
  geom_bar(aes(x = TAM_MUN))


table(dados$ADM)
dados %>%
  ggplot() +
  geom_bar(aes(x = ADM))

table(amostra$ADM)
amostra %>%
  ggplot() +
  geom_bar(aes(x = ADM))



table(dados$TAM_ESCOLA)
dados %>%
  ggplot() +
  geom_bar(aes(x = TAM_ESCOLA))
dados$TAM_ESCOLA %>% levels()

table(amostra$TAM_ESCOLA)
amostra %>%
  ggplot() +
  geom_bar(aes(x = TAM_ESCOLA))
amostra$TAM_ESCOLA %>% levels()


#**Matriculados**
stem(dados$MATRICULADOS)

stem(amostra$MATRICULADOS)


dados %>%
  mutate(mat = cut(MATRICULADOS, breaks = c(0, 25, 50, 100, Inf), 
                   labels = c("<25", "25 a 49","50 a 99","100 ou mais"))) %>%
  ggplot() +
  geom_bar(aes(x = mat))

amostra %>%
  mutate(mat = cut(MATRICULADOS, breaks = c(0, 25, 50, 100, Inf), 
                   labels = c("<25", "25 a 49","50 a 99","100 ou mais"))) %>%
  ggplot() +
  geom_bar(aes(x = mat))



dados %>%
  ggplot() +
  geom_histogram(aes(x = MATRICULADOS,
                     y = after_stat(density)), 
                 bins =  10)
amostra %>%
  ggplot() +
  geom_histogram(aes(x = MATRICULADOS,
                     y = after_stat(density)), 
                 bins =  10)


dados %>%
  ggplot() +
  geom_boxplot(aes(y = PARTICIPACAO))

amostra %>%
  ggplot() +
  geom_boxplot(aes(y = PARTICIPACAO))


#**Participacao**
stem(dados$PARTICIPACAO)

stem(amostra$PARTICIPACAO)


dados %>%
  mutate(part = cut(PARTICIPACAO, breaks = seq(50, 100, length.out = 3), 
                    labels = c("< 75 %", "75% ou mais"))) %>%
  ggplot() +
  geom_bar(aes(x = part))

amostra %>%
  mutate(part = cut(PARTICIPACAO, breaks = seq(50, 100, length.out = 3), 
                    labels = c("< 75 %", "75% ou mais"))) %>%
  ggplot() +
  geom_bar(aes(x = part))


dados %>%
  ggplot() +
  geom_histogram(aes(x = PARTICIPACAO,
                     y = after_stat(density)), 
                 bins =  10)
amostra %>%
  ggplot() +
  geom_histogram(aes(x = PARTICIPACAO,
                     y = after_stat(density)), 
                 bins =  10)


dados %>%
  ggplot() +
  geom_boxplot(aes(y = PARTICIPACAO))

amostra %>%
  ggplot() +
  geom_boxplot(aes(y = PARTICIPACAO))


#**MATRICULADOS X PARTICIPACAO**

dados %>%
  mutate(mat = cut(MATRICULADOS, breaks = c(0, 25, 50, 100, Inf), 
                   labels = c("<25", "25 a 49","50 a 99","100 ou mais"))) %>%
  select(mat, TAM_ESCOLA) %>%
  pivot_longer(everything()) %>%
  ggplot() +
  geom_bar(aes(x = value, fill = name), 
           position = position_dodge())


#**Nota Lingua Portuguesa**
stem(dados$NOTA_LP)

dados %>%
  mutate(nota = cut(NOTA_LP, breaks = seq(100, 250, 50))) %>%
  ggplot() +
  geom_bar(aes(x = nota))

dados %>%
  ggplot() +
  geom_histogram(aes(x = NOTA_LP,
                     y = after_stat(density)), 
                 fill = "red", 
                 alpha = 0.7, 
                 bins = 10) +
  geom_function(fun = \(x)dnorm(x, mean = mean(dados$NOTA_LP), sd = sd(dados$NOTA_LP)), 
                linewidth = 1)
dados %>%
  ggplot() +
  geom_boxplot(aes(y = NOTA_LP))

#**Nota Matematica**
stem(dados$NOTA_MT)
dados$NOTA_MT %>% min()
dados$NOTA_MT %>% max()

dados %>%
  mutate(nota = cut(NOTA_MT, breaks = seq(150, 300, 50))) %>%
  ggplot() +
  geom_bar(aes(x = nota))

media = mean(dados$NOTA_MT)
desvio  = sd(dados$NOTA_MT)
dados %>%
  ggplot() +
  geom_histogram(aes(x = NOTA_MT,
                     y = after_stat(density)),
                 fill = "red",
                 alpha = 0.7,
                 bins = 10) +
  geom_function(fun = \(x)dnorm(x, mean = mean(dados$NOTA_MT), sd = sd(dados$NOTA_MT)), 
                linewidth = 1) 

dados %>%
  ggplot() +
  geom_boxplot(aes(y = NOTA_MT))

#**Notas MT X LP**
dados %>%
  select(NOTA_LP, NOTA_MT) %>%
  pivot_longer(everything(), names_to = "materia", values_to = "nota") %>%
  mutate(nota_cat = cut(nota, breaks = seq(0, 500, 50))) %>%
  ggplot() +
  geom_bar(aes(x = nota_cat, fill = materia), position = position_dodge())

dados %>%
  select(NOTA_LP, NOTA_MT) %>%
  pivot_longer(everything(), names_to = "materia", values_to = "nota") %>%
  ggplot() +
  geom_boxplot(aes(y = nota, x = materia))


#**Medidas de: MATRICULADOS, PARTICIPACAO, NOTA_LP e NOTA_MT**

aux <- dados$NOTA_MT
sqrt((N-1)/N) * sd(aux)
N = length(aux)
sqrt((N-1)/N) * sqrt((sum((aux - mean(aux))^2))/ (N - 1))
sqrt((sum((aux - mean(aux))^2))/ (N - 1))
sqrt((mean((aux - mean(aux))^2)))

# Funcoes auxiliares
curtose <- function(values) (quantile(values, 0.75) - quantile(values, 0.25))/(2*(quantile(values, 0.9) - quantile(values, 0.1))) # calculo q achamos num PDF de uma aula na internet

curtose_definicao <- function(values)
{
  n <- length(values)
  m <- mean(values)
  s <- sqrt((n-1) / n) * sd(values) # precisa ser o desvio populacional, por algum motivo
  
  ((sum(((values - m)^4)/n)) / (s^4))
  # ref: https://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm#:~:text=Skewness%20is%20a%20measure%20of,relative%20to%20a%20normal%20distribution.
}

excesso_curtose <- function(values)
{
  n <- length(values)
  m <- mean(values)
  s <- sqrt((n-1) / n) * sd(values) # precisa ser o desvio populacional, por algum motivo
  
  ((sum(((values - m)^4)/n)) / (s^4)) - 3 # a curtose de uma normal padrao e igual a 3
  # ref: https://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm#:~:text=Skewness%20is%20a%20measure%20of,relative%20to%20a%20normal%20distribution.
}

assimetria_corrigida_pearson <- \(values) # Ainda e uma funcao so q com uma notacao diferente, function() == \()
{
  n = length(values) # usando igual so por noia, mas e a msma coisa de usar o <-
  m = mean(values)
  s = sd(values)
  
  (sqrt((n * (n - 1)) / (n - 2))) * (sum(((values - m)^3)/n) / s^3)
  # quanto mais perto de zero mais simetrico, 
  # quanto mais negativo mais assimetrico pra esquerda
  # quanto mais positivo mais assimetrico pra direita
  # ref: https://www.itl.nist.gov/div898/handbook/eda/section3/eda35b.htm#:~:text=Skewness%20is%20a%20measure%20of,relative%20to%20a%20normal%20distribution.
}

###############################################
?pivot_longer()#TODO: OLHAR ESSE HELP
?pivot_longer()#**OLHA ESSE HELP !!!!!!!!**#
###############################################

medidas_resumo_quantitativas <- dados %>%
  select(MATRICULADOS, PARTICIPACAO, NOTA_LP, NOTA_MT) %>%
  pivot_longer(everything()) %>% 
  group_by(name) %>%
  summarize(media = mean(value),
            variancia = var(value),
            desvio_padrao = sd(value),
            q1 = quantile(value, 0.25),
            mediana = quantile(value, 0.5),
            q3 = quantile(value, 0.75),
            curt_def = curtose_definicao(value),
            curt_excesso = excesso_curtose(value),
            assimetria = assimetria_corrigida_pearson(value))

View(medidas_resumo_quantitativas)

################################################################################
################################################################################
################################################################################

#**OBJETIVO 2**
#**Estimar a proporção de escolas que menos de 75% de seus estudantes participaram da Prova**

alpha <- 0.05

dados <- dados %>%
  mutate(part75 = PARTICIPACAO < 75)
p_sum <- sum(dados$part75)
p_test <- prop.test(p_sum, n = N, conf.level=1-alpha)

p_test$conf.int
p_test$estimate

################################################################################
################################################################################
################################################################################
#**OBJETIVO 4**
#**Verificar se houve melhora do resultado da Prova Brasil de 2009 para 2011.** 
#**Na Prova realizada em 2009 a proficiência em LP foi 184,3 e em MT 204,3.**

# h0: nota LP de 2011 e menor ou igual a nota LP de 2009
# h1: nota LP de 2011 e maior que a nota LP de 2009
t.test(dados$NOTA_LP, mu=184.3, alternative = "greater", conf.level = 1-alpha)
# resultado: como p-valor = 0.4545 > 0.05, não rejeitamos h0
# nao encontramos evidencias para acreditar que houve melhora na nota de LP entre  2009 e 2011

# h0: nota MT de 2011 e menor ou igual a nota MT de 2009
# h1: nota MT de 2011 e maior que a nota MT de 2009
t.test(dados$NOTA_MT, mu=204.3, alternative = "greater", conf.level = 1-alpha)
# resultado: como p-valor = 0.6328 > 0.05, não rejeitamos h0
# nao encontramos evidencias para acreditar que houve melhora na nota de MT entre  2009 e 2011


################################################################################
################################################################################
################################################################################

#**OBJETIVO 8**
#**Comparar a proporção de escolas que menos de 75% de seus estudantes participaram da Prova**
#**segundo: Local da escola; Região de localização da escola.**

dados %>%
  filter(part75) %>%
  select(LOCAL, REG) %>%
  group_by(LOCAL, REG) %>%
  summarise(count = n())

local <- dados %>% 
  # filter(part75) %>%
  pull(LOCAL)

regiao <- dados %>% 
  # filter(part75) %>%
  pull(REG)

part75 <- dados %>%
  pull(part75)

tabela1 <- table(part75, local)
tabela1

addmargins(tabela1) # marginais

# Frequencias relativas 
prop.table(tabela1) # em relacao ao total
prop.table(tabela1, 1) # em relacao as linhas
prop.table(tabela1, 2) # em relacao as colunas

# h0: as proporcoes entre escolas que tiveram menos de 75% de alunos inscritos e mais é igual entre os locais
# h1: as proporcoes entre escolas que tiveram menos de 75% de alunos inscritos e mais é diferente entre os locais
chisq.test(tabela1)
# conclusao: não encontramos evidencias para acreditar que a proporcao de esculas nos locais muda entre as escolas com mais e menos de 75% de inscritos


tabela2 <- table(part75, regiao)
tabela2

addmargins(tabela2) # marginais

# Frequencias relativas 
prop.table(tabela2) # em relacao ao total
prop.table(tabela2, 1) # em relacao as linhas
prop.table(tabela2, 2) # em relacao as colunas

chisq.test(tabela2)
# conclusao: nao rejeitamos H0, acreditamos que as proporcoes sejam iguais


################################################################################
################################################################################
################################################################################
#**OBJETIVO 10**
#**Verificar se a nota em LP é um bom indicador para predizer a nota existe em MT, ou**
#**seja se estão associadas**

dados %>%
  ggplot() +
  geom_point(aes(x = NOTA_LP, y = NOTA_MT)) +
  geom_abline(slope = fit$coefficients[2], intercept = fit$coefficients[1], color = "red")

fit <- lm(NOTA_MT ~ NOTA_LP,  data = dados)
summary(fit)

cor.test(dados$NOTA_MT, dados$NOTA_LP, alternative="two.sided", method="pearson", conf.level=1-alpha)
# conclusao: Rejeitamos H0, ou seja, temos fortes indicios de que a correlação é diferente de zero
