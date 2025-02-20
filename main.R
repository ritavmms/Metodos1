# Importando pacotes
if(!require("pacman")) install.packages("pacman")
pacman::p_load(latex2exp, knitr, tidyverse)


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

#Separando nossa amostra de 50
#A seed é para garantir que será a mesma toda vez que rodarem o código
set.seed(20242)
amostra <- dados %>% 
  sample_n(size = 50)

#Análise das variáveis apresentadas
#**Matriculados**
#Ramo-e-folhas

cat(Filter(function(s) grepl("decimal|\\|.*[0-9]", s),
           capture.output(stem(dados$MATRICULADOS, scale = 2))),
    sep="\n")

cat(Filter(function(s) grepl("decimal|\\|.*[0-9]", s),
           capture.output(stem(amostra$MATRICULADOS, scale = 3))),
    sep="\n")

#Distribuição de frequências com intervalos de classe
dados %>%
  mutate(mat = cut(MATRICULADOS, breaks = c(0, 25, 50, 100, Inf), 
                   labels = c("<25", "25 a 49","50 a 99","100 ou mais"))) %>%
  ggplot() +
  geom_bar(aes(x = mat), fill= "#663399")+
  labs(title = "", y= "Quantidade", x= "Matriculados")

amostra %>%
  mutate(mat = cut(MATRICULADOS, breaks = c(0, 25, 50, 100, Inf), 
                   labels = c("<25", "25 a 49","50 a 99","100 ou mais"))) %>%
  ggplot() +
  geom_bar(aes(x = mat), fill= "#663399") +
  labs(title = "", y= "Quantidade", x= "Matriculados")


#Histograma
dados %>%
  ggplot() +
  geom_histogram(aes(x = MATRICULADOS,
                     y = after_stat(density)), 
                 bins =  10,
                 fill= "#663399")+
  labs(title = "", y= "Densidade", x= "Matriculados")

amostra %>%
  ggplot() +
  geom_histogram(aes(x = MATRICULADOS,
                     y = after_stat(density)), 
                 bins =  10,
                 fill= "#663399")+
  labs(title = "", y= "Densidade", x= "Matriculados")

#Boxplot
dados %>%
  ggplot() +
  geom_boxplot(aes(y = MATRICULADOS),
               fill= "#663399")+
  labs(title = "", y= "Quantidade de Matriculados", x= "")

amostra %>%
  ggplot() +
  geom_boxplot(aes(y = MATRICULADOS),
               fill= "#663399")+
  labs(title = "", y= "Quantidade de Matriculados", x= "")


#**Participacao**
#Ramo-e-folhas

cat(Filter(function(s) grepl("decimal|\\|.*[0-9]", s),
           capture.output(stem(dados$PARTICIPACAO, scale= 2))),
    sep="\n")


cat(Filter(function(s) grepl("decimal|\\|.*[0-9]", s),
           capture.output(stem(amostra$PARTICIPACAO, scale = 0.5))),
    sep="\n")

#Distribuição de frequências com intervalos de classe
dados %>%
  mutate(part = cut(PARTICIPACAO, breaks = seq(50, 100, length.out = 3), 
                    labels = c("< 75 %", "75% ou mais"))) %>%
  ggplot() +
  geom_bar(aes(x = part), fill= "#663399")+
  labs(title = "", y= "Quantidade", x= "Matriculados")

amostra %>%
  mutate(part = cut(PARTICIPACAO, breaks = seq(50, 100, length.out = 3), 
                    labels = c("< 75 %", "75% ou mais"))) %>%
  ggplot() +
  geom_bar(aes(x = part), fill= "#663399")+
  labs(title = "", y= "Quantidade", x= "Matriculados")


#Histograma
dados %>%
  ggplot() +
  geom_histogram(aes(x = PARTICIPACAO,
                     y = after_stat(density)), 
                 bins =  10,
                 fill= "#663399")+
  labs(title = "", y= "Densidade", x= "Participacao")

amostra %>%
  ggplot() +
  geom_histogram(aes(x = PARTICIPACAO,
                     y = after_stat(density)), 
                 bins =  10,
                 fill= "#663399")+
  labs(title = "", y= "Densidade", x= "Participacao")

#Boxplot
dados %>%
  ggplot() +
  geom_boxplot(aes(y = PARTICIPACAO), fill= "#663399")+
  labs(title = "", y= "% de Participacao", x= "")

amostra %>%
  ggplot() +
  geom_boxplot(aes(y = PARTICIPACAO), fill= "#663399")+
  labs(title = "", y= "% de Participacao", x= "")


#**MATRICULADOS X PARTICIPACAO**

dados %>%
  mutate(mat = cut(MATRICULADOS, breaks = c(0, 25, 50, 100, Inf), 
                   labels = c("<25", "25 a 49","50 a 99","100 ou mais"))) %>%
  select(mat, TAM_ESCOLA) %>%
  pivot_longer(everything()) %>%
  ggplot() +
  geom_bar(aes(x = value, fill = name), 
           position = position_dodge())

amostra %>%
  mutate(mat = cut(MATRICULADOS, breaks = c(0, 25, 50, 100, Inf), 
                   labels = c("<25", "25 a 49","50 a 99","100 ou mais"))) %>%
  select(mat, TAM_ESCOLA) %>%
  pivot_longer(everything()) %>%
  ggplot() +
  geom_bar(aes(x = value, fill = name), 
           position = position_dodge())

#**Nota Lingua Portuguesa**
#Ramo-e-folhas


cat(Filter(function(s) grepl("decimal|\\|.*[0-9]", s),
           capture.output(stem(dados$NOTA_LP))),
    sep="\n")

cat(Filter(function(s) grepl("decimal|\\|.*[0-9]", s),
           capture.output(stem(amostra$NOTA_LP))),
    sep="\n")

#Distribuição de frequências com intervalos de classe
dados %>%
  mutate(nota = cut(NOTA_LP, breaks = seq(100, 250, 50))) %>%
  ggplot() +
  geom_bar(aes(x = nota), fill= "#663399")+
  labs(title = "", y= "Quantidade", x= "Nota Lingua Portuguesa") 

amostra %>%
  mutate(nota = cut(NOTA_LP, breaks = seq(100, 250, 50))) %>%
  ggplot() +
  geom_bar(aes(x = nota), fill= "#663399")+
  labs(title = "", y= "Quantidade", x= "Nota Lingua Portuguesa")

#Histograma
dados %>%
  ggplot() +
  geom_histogram(aes(x = NOTA_LP,
                     y = after_stat(density)), 
                 fill = "#663399", 
                 alpha = 0.9, 
                 bins = 10) +
  geom_function(fun = \(x)dnorm(x, mean = mean(dados$NOTA_LP), sd = sd(dados$NOTA_LP)), 
                linewidth = 1) +
  labs(title = "", y= "Densidade", x= "Nota Lingua Portugesa")

amostra %>%
  ggplot() +
  geom_histogram(aes(x = NOTA_LP,
                     y = after_stat(density)), 
                 fill = "#663399", 
                 alpha = 0.9, 
                 bins = 10) +
  geom_function(fun = \(x)dnorm(x, mean = mean(amostra$NOTA_LP), sd = sd(amostra$NOTA_LP)), 
                linewidth = 1) +
  labs(title = "", y= "Densidade", x= "Nota Lingua Portuguesa")

#Boxplot
dados %>%
  ggplot() +
  geom_boxplot(aes(y = NOTA_LP), fill= "#663399")+
  labs(title = "", y= "Nota de Lingua Portuguesa", x= "")


amostra %>%
  ggplot() +
  geom_boxplot(aes(y = NOTA_LP), fill= "#663399")+
  labs(title = "", y= "Nota de Lingua Portuguesa", x= "")


#**Nota Matematica**
#Ramo-e-folhas


cat(Filter(function(s) grepl("decimal|\\|.*[0-9]", s),
           capture.output(stem(dados$NOTA_MT))),
    sep="\n")

cat(Filter(function(s) grepl("decimal|\\|.*[0-9]", s),
           capture.output(stem(amostra$NOTA_MT))),
    sep="\n")


#Distribuição de frequências com intervalos de classe
dados %>%
  mutate(nota = cut(NOTA_MT, breaks = seq(150, 300, 50))) %>%
  ggplot() +
  geom_bar(aes(x = nota), fill= "#663399")+
  labs(title = "", y= "Quantidade", x= "Nota Matemática")

amostra %>%
  mutate(nota = cut(NOTA_MT, breaks = seq(150, 300, 50))) %>%
  ggplot() +
  geom_bar(aes(x = nota), fill= "#663399") +
  labs(title = "", y= "Quantidade", x= "Nota Matemática")

#Histograma
media = mean(dados$NOTA_MT)
desvio  = sd(dados$NOTA_MT)
dados %>%
  ggplot() +
  geom_histogram(aes(x = NOTA_MT,
                     y = after_stat(density)),
                 fill = "#663399",
                 alpha = 0.9,
                 bins = 10) +
  geom_function(fun = \(x)dnorm(x, mean = mean(dados$NOTA_MT), sd = sd(dados$NOTA_MT)), 
                linewidth = 1)+
  labs(title = "", y= "Densidade", x= "Nota Matematica")

media = mean(amostra$NOTA_MT)
desvio  = sd(amostra$NOTA_MT)
amostra %>%
  ggplot() +
  geom_histogram(aes(x = NOTA_MT,
                     y = after_stat(density)),
                 fill = "#663399",
                 alpha = 0.9,
                 bins = 10) +
  geom_function(fun = \(x)dnorm(x, mean = mean(amostra$NOTA_MT), sd = sd(amostra$NOTA_MT)), 
                linewidth = 1)+
  labs(title = "", y= "Densidade", x= "Nota Matematica")

#Boxplot
dados %>%
  ggplot() +
  geom_boxplot(aes(y = NOTA_MT), fill= "#663399")+
  labs(title = "", y= "Nota de Lingua Portuguesa", x= "")

amostra %>%
  ggplot() +
  geom_boxplot(aes(y = NOTA_MT), fill= "#663399")+
  labs(title = "", y= "Nota de Lingua Portuguesa", x= "")


#**Notas MT X LP**
dados %>%
  select(NOTA_LP, NOTA_MT) %>%
  pivot_longer(everything(), names_to = "materia", values_to = "nota") %>%
  mutate(nota_cat = cut(nota, breaks = seq(0, 500, 50))) %>%
  ggplot() +
  geom_bar(aes(x = nota_cat, fill = materia), position = position_dodge())

amostra %>%
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

amostra %>%
  select(NOTA_LP, NOTA_MT) %>%
  pivot_longer(everything(), names_to = "materia", values_to = "nota") %>%
  ggplot() +
  geom_boxplot(aes(y = nota, x = materia))


#**Medidas de: MATRICULADOS, PARTICIPACAO, NOTA_LP e NOTA_MT**
# Funcoes auxiliares
curtose <- function(values) (quantile(values, 0.75) - quantile(values, 0.25))/(2*(quantile(values, 0.9) - quantile(values, 0.1))) # calculo q achamos num PDF de uma aula na internet

curtose_definicao <- function(values)
{
  n <- length(values)
  m <- mean(values)
  s <- sqrt((n-1) / n) * sd(values) # precisa ser o desvio populacional, por algum motivo
  
  ((sum(((values - m)^4)/n)) / (s^4))
}

excesso_curtose <- function(values)
{
  n <- length(values)
  m <- mean(values)
  s <- sqrt((n-1) / n) * sd(values) # precisa ser o desvio populacional, por algum motivo
  
  ((sum(((values - m)^4)/n)) / (s^4)) - 3 # a curtose de uma normal padrao e igual a 3
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
}

medidas_resumo_quantitativasD <- dados %>%
  select(MATRICULADOS, PARTICIPACAO, NOTA_LP, NOTA_MT) %>%
  pivot_longer(everything()) %>% 
  group_by(name) %>%
  summarize(media = mean(value),
            # variancia = var(value),
            desvio_padrao = sd(value),
            q1 = quantile(value, 0.25),
            mediana = quantile(value, 0.5),
            q3 = quantile(value, 0.75),
            # curt_def = curtose_definicao(value),
            curt_excesso = excesso_curtose(value),
            assimetria = assimetria_corrigida_pearson(value))

<<<<<<< HEAD
kable(medidas_resumo_quantitativasD, 
      digits = 2,
      caption = "Medidas resumo do \textnumero de matriculados, porcentagem de participação, notas de Lingua Portuguesa e Matemática, para a amostra de 200")
=======
# View(medidas_resumo_quantitativasD)
medidas_resumo_quantitativasD
medidas_resumo_quantitativasD 
>>>>>>> f771cc5 (Ajusted table quantitative measures)

kable(medidas_resumo_quantitativasD, 
      digits = 2,
      caption = "Medidas resumo do N\\textordmasculine{} de matriculados, porcentagem de participação, notas de Lingua Portuguesa e Matemática, para a amostra de 200")
?kable

dados %>%
  select(-Tipo) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarise(`Média` = mean(value),
            `Desvio Padrão` = sd(value),
            `Minimo` = min(value),
            `Máximo` = max(value),
            `1\\textordmasculine Quartil` = quantile(value, 0.25),
            `Mediana` = median(value),
            `3\\textordmasculine Quartil` = quantile(value, 0.75)) %>%
  column_to_rownames(var = "name") %>%
  as.matrix() %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "Medida")

medidas_resumo_quantitativasD <- dados %>%
  select(MATRICULADOS, PARTICIPACAO, NOTA_LP, NOTA_MT) %>%
  pivot_longer(everything()) %>% 
  group_by(name) %>%
  summarise(`Média` = mean(value),
            `Desvio Padrão` = sd(value),
            `Minimo` = min(value),
            `1\\textordmasculine{} Quartil` = quantile(value, 0.25),
            `Mediana` = median(value),
            `3\\textordmasculine{} Quartil` = quantile(value, 0.75), 
            `Máximo` = max(value),
            `Excesso de Curtose` = excesso_curtose(value), 
            `Assimetria` = assimetria_corrigida_pearson(value)) %>%
  column_to_rownames(var = "name") %>%
  as.matrix() %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "Medida") %>% 
  rename(`N\\textordmasculine{} de matriculados` = MATRICULADOS,
         `Participação` = PARTICIPACAO,
         `Nota de Lingua Portuguesa` = NOTA_LP,
         `Nota de Matemática` = NOTA_MT)

medidas_resumo_quantitativasD

summarize(media = mean(value),
          # variancia = var(value),
          desvio_padrao = sd(value),
          q1 = quantile(value, 0.25),
          mediana = quantile(value, 0.5),
          q3 = quantile(value, 0.75),
          # curt_def = curtose_definicao(value),
          curt_excesso = excesso_curtose(value),
          assimetria = assimetria_corrigida_pearson(value))

medidas_resumo_quantitativasA <- amostra %>%
  select(MATRICULADOS, PARTICIPACAO, NOTA_LP, NOTA_MT) %>%
  pivot_longer(everything()) %>% 
  group_by(name) %>%
  summarize(media = mean(value),
            # variancia = var(value),
            desvio_padrao = sd(value),
            q1 = quantile(value, 0.25),
            mediana = quantile(value, 0.5),
            q3 = quantile(value, 0.75),
            # curt_def = curtose_definicao(value),
            curt_excesso = excesso_curtose(value),
            assimetria = assimetria_corrigida_pearson(value))

kable(medidas_resumo_quantitativasA,
      digits = 2,
      caption = "Medidas resumo do \textnumero de matriculados, porcentagem de participação, notas de Lingua Portuguesa e Matemática, para a amostra de 50")



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
  geom_bar(aes(x = LOCAL), fill= "#663399")

table(amostra$LOCAL)
amostra %>%
  ggplot() +
  geom_bar(aes(x = LOCAL), fill= "#663399")


table(dados$TAM_MUN)
dados %>%
  ggplot() +
  geom_bar(aes(x = TAM_MUN), fill= "#663399")

table(amostra$TAM_MUN)
amostra %>%
  ggplot() +
  geom_bar(aes(x = TAM_MUN), fill= "#663399")


table(dados$ADM)
dados %>%
  ggplot() +
  geom_bar(aes(x = ADM), fill= "#663399")

table(amostra$ADM)
amostra %>%
  ggplot() +
  geom_bar(aes(x = ADM), fill= "#663399")



table(dados$TAM_ESCOLA)
dados %>%
  ggplot() +
  geom_bar(aes(x = TAM_ESCOLA), fill= "#663399")
dados$TAM_ESCOLA %>% levels()

table(amostra$TAM_ESCOLA)
amostra %>%
  ggplot() +
  geom_bar(aes(x = TAM_ESCOLA),)
amostra$TAM_ESCOLA %>% levels()

################################################################################
################################################################################
################################################################################

#**OBJETIVO 2**
#**Estimar a proporção de escolas que menos de 75% de seus estudantes participaram da Prova**
#Risco máximo de 5%
alpha <- 0.05
N <- nrow(dados)

dados <- dados %>%
  mutate(part75 = PARTICIPACAO < 75)
p_sum <- sum(dados$part75)
p_test <- prop.test(p_sum, n = N, conf.level=1-alpha)

amostra <- amostra %>%
  mutate(part75 = PARTICIPACAO < 75)
p_sum <- sum(amostra$part75)
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
t.test(amostra$NOTA_LP, mu=184.3, alternative = "greater", conf.level = 1-alpha)
# resultado: como p-valor = 0.4545 > 0.05, não rejeitamos h0
# nao encontramos evidencias para acreditar que houve melhora na nota de LP entre  2009 e 2011

# h0: nota MT de 2011 e menor ou igual a nota MT de 2009
# h1: nota MT de 2011 e maior que a nota MT de 2009
t.test(dados$NOTA_MT, mu=204.3, alternative = "greater", conf.level = 1-alpha)
t.test(amostra$NOTA_MT, mu=204.3, alternative = "greater", conf.level = 1-alpha)
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

amostra %>%
  filter(part75) %>%
  select(LOCAL, REG) %>%
  group_by(LOCAL, REG) %>%
  summarise(count = n())


Dlocal <- dados %>%
  pull(LOCAL)

Alocal <- amostra %>%
  pull(LOCAL)


Dregiao <- dados %>% 
  pull(REG)

Aregiao <- amostra %>% 
  pull(REG)


part75D <- dados %>%
  pull(part75)

part75A <- amostra %>%
  pull(part75)

tabela1D <- table(part75D, Dlocal)
tabela1D

tabela1A <- table(part75A, Alocal)
tabela1A

# h0: as proporcoes entre escolas que tiveram menos de 75% de alunos inscritos e mais é igual entre os locais
# h1: as proporcoes entre escolas que tiveram menos de 75% de alunos inscritos e mais é diferente entre os locais
chisq.test(tabela1D)
chisq.test(tabela1A)
# Ele vai smp dar a mensagem de aviso
# conclusao: não encontramos evidencias para acreditar que a proporcao de esculas nos locais muda entre as escolas com mais e menos de 75% de inscritos

tabela2D <- table(part75D, Dregiao)
tabela2D

tabela2A <- table(part75A, Aregiao)
tabela2A

chisq.test(tabela2D)
chisq.test(tabela2A)
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

amostra %>%
  ggplot() +
  geom_point(aes(x = NOTA_LP, y = NOTA_MT)) +
  geom_abline(slope = fit$coefficients[2], intercept = fit$coefficients[1], color = "red")


fit <- lm(NOTA_MT ~ NOTA_LP,  data = dados)
summary(fit)

fit <- lm(NOTA_MT ~ NOTA_LP,  data = amostra)
summary(fit)


cor.test(dados$NOTA_MT, dados$NOTA_LP, alternative="two.sided", method="pearson", conf.level=1-alpha)
cor.test(amostra$NOTA_MT, amostra$NOTA_LP, alternative="two.sided", method="pearson", conf.level=1-alpha)
# conclusao: Rejeitamos H0, ou seja, temos fortes indicios de que a correlação é diferente de zero
