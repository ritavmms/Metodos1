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
                       labels = c("<20 mil hab", 
                                  "20 a 49 mil hab", 
                                  "50 a 99 mil hab", 
                                  "100 a 999 mil hab", 
                                  "1 milhao de hab >"))
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
  labs(title = "", y= " ", x= "Matriculados")

amostra %>%
  mutate(mat = cut(MATRICULADOS, breaks = c(0, 25, 50, 100, Inf), 
                   labels = c("<25", "25 a 49","50 a 99","100 ou mais"))) %>%
  ggplot() +
  geom_bar(aes(x = mat), fill= "#663399") +
  labs(title = "", y= " ", x= "Matriculados")


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
  labs(title = "", y= " ", x= "Matriculados")

amostra %>%
  mutate(part = cut(PARTICIPACAO, breaks = seq(50, 100, length.out = 3), 
                    labels = c("< 75 %", "75% ou mais"))) %>%
  ggplot() +
  geom_bar(aes(x = part), fill= "#663399")+
  labs(title = "", y= " ", x= "Matriculados")


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
  labs(title = "", y= " ", x= "Nota Lingua Portuguesa") 

amostra %>%
  mutate(nota = cut(NOTA_LP, breaks = seq(100, 250, 50))) %>%
  ggplot() +
  geom_bar(aes(x = nota), fill= "#663399")+
  labs(title = "", y= " ", x= "Nota Lingua Portuguesa")

#Histograma
dados %>%
  ggplot() +
  geom_histogram(aes(x = NOTA_LP,
                     y = after_stat(density)), 
                 fill = "#663399",
                 bins = 10) +
  labs(title = "", y= "Densidade", x= "Nota Lingua Portugesa")

amostra %>%
  ggplot() +
  geom_histogram(aes(x = NOTA_LP,
                     y = after_stat(density)), 
                 fill = "#663399", 
                 bins = 10) +
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
  labs(title = "", y= " ", x= "Nota Matematica")

amostra %>%
  mutate(nota = cut(NOTA_MT, breaks = seq(150, 300, 50))) %>%
  ggplot() +
  geom_bar(aes(x = nota), fill= "#663399") +
  labs(title = "", y= " ", x= "Nota Matematica")

#Histograma

dados %>%
  ggplot() +
  geom_histogram(aes(x = NOTA_MT,
                     y = after_stat(density)),
                 fill = "#663399",
                 bins = 10) +
  labs(title = "", y= "Densidade", x= "Nota Matematica")


amostra %>%
  ggplot() +
  geom_histogram(aes(x = NOTA_MT,
                     y = after_stat(density)),
                 fill = "#663399",
                 bins = 10) +
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

# o número que sair é o excesso de curtose
# o quanto os dados são mais ou menos curticos do que uma normal padrão
# sendo a normal padrão 0 nessa escala

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
  summarise(`Média` = mean(value),
            `Desvio Padrão` = sd(value),
            `Minimo` = min(value),
            `Q1` = quantile(value, 0.25),
            `Mediana` = median(value),
            `Q3` = quantile(value, 0.75), 
            `Máximo` = max(value),
            `Excesso de Curtose` = excesso_curtose(value), 
            `Assimetria` = assimetria_corrigida_pearson(value)) %>%
  column_to_rownames(var = "name") %>%
  as.matrix() %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "Medida") %>% 
  rename(`N de matriculados` = MATRICULADOS,
         `Participação` = PARTICIPACAO,
         `Nota de Lingua Portuguesa` = NOTA_LP,
         `Nota de Matemática` = NOTA_MT)

kable(medidas_resumo_quantitativasD, 
      digits = 2,
      caption = "Medidas resumo do N de matriculados, porcentagem de participação, notas de Lingua Portuguesa e Matemática, para a amostra de 200")

medidas_resumo_quantitativasA <- amostra %>%
  select(MATRICULADOS, PARTICIPACAO, NOTA_LP, NOTA_MT) %>%
  pivot_longer(everything()) %>% 
  group_by(name) %>%
  summarise(`Média` = mean(value),
            `Desvio Padrão` = sd(value),
            `Minimo` = min(value),
            `Q1` = quantile(value, 0.25),
            `Mediana` = median(value),
            `Q3` = quantile(value, 0.75), 
            `Máximo` = max(value),
            `Excesso de Curtose` = excesso_curtose(value), 
            `Assimetria` = assimetria_corrigida_pearson(value)) %>%
  column_to_rownames(var = "name") %>%
  as.matrix() %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "Medida") %>% 
  rename(`N de matriculados` = MATRICULADOS,
         `Participação` = PARTICIPACAO,
         `Nota de Lingua Portuguesa` = NOTA_LP,
         `Nota de Matemática` = NOTA_MT)

kable(medidas_resumo_quantitativasA,
      digits = 2,
      caption = "Medidas resumo do N de matriculados, porcentagem de participação, notas de Lingua Portuguesa e Matemática, para a amostra de 50")


#**OBJETIVO 1**
#**Descrever as características das escolas e o desempenho de seus estudantes na Prova**

kable(table(dados$REG))
dados %>%
  ggplot() +
  geom_bar(aes(x = REG), fill="#663399") +
  labs(y= " ", x= "Regiao")

table(amostra$REG)
amostra %>%
  ggplot() +
  geom_bar(aes(x = REG), fill="#663399")+
  labs(y= " ", x= "Regiao")


table(dados$LOCAL)
dados %>%
  ggplot() +
  geom_bar(aes(x = LOCAL), fill= "#663399")+
  labs(y= " ", x= "Local")

table(amostra$LOCAL)
amostra %>%
  ggplot() +
  geom_bar(aes(x = LOCAL), fill= "#663399")+
  labs(y= " ", x= "Local")


table(dados$TAM_MUN)
dados %>%
  ggplot() +
  geom_bar(aes(x = TAM_MUN), fill= "#663399")+
  labs(y= " ", x= "Tamanho do Municipio")

table(amostra$TAM_MUN)
amostra %>%
  ggplot() +
  geom_bar(aes(x = TAM_MUN), fill= "#663399")+
  labs(y= " ", x= "Tamanho do muncipio")


table(dados$ADM)
dados %>%
  ggplot() +
  geom_bar(aes(x = ADM), fill= "#663399")+
  labs(y= "", x= "Administracao")

table(amostra$ADM)
amostra %>%
  ggplot() +
  geom_bar(aes(x = ADM), fill= "#663399")+
  labs(y= "", x= "Administracao")

table(dados$TAM_ESCOLA)
dados %>%
  ggplot() +
  geom_bar(aes(x = TAM_ESCOLA), fill= "#663399")+
  labs(y= "", x= "Tamanho da Escola")

table(amostra$TAM_ESCOLA)
amostra %>%
  ggplot() +
  geom_bar(aes(x = TAM_ESCOLA), fill= "#663399")+
  labs(y= "", x= "Tamanho da Escola")


#**MATRICULADOS X PARTICIPACAO**

dados %>%
  mutate(mat = cut(MATRICULADOS, breaks = c(0, 25, 50, 100, Inf), 
                   labels = c("<25", "25 a 49","50 a 99","100 ou mais"))) %>%
  select(mat, TAM_ESCOLA) %>%
  pivot_longer(everything()) %>%
  ggplot() +
  geom_bar(aes(x = value, fill = name), 
           position = position_dodge()) +
  labs(y= "", x= "Alunos Matriculados") + 
  scale_fill_manual(name = "Legenda", 
                    labels = c("Prova Brasil", "Escola"),
                    values = c("mat"= "#663399", "TAM_ESCOLA"= "#d4b4f6"))

amostra %>%
  mutate(mat = cut(MATRICULADOS, breaks = c(0, 25, 50, 100, Inf), 
                   labels = c("<25", "25 a 49","50 a 99","100 ou mais"))) %>%
  select(mat, TAM_ESCOLA) %>%
  pivot_longer(everything()) %>%
  ggplot() +
  geom_bar(aes(x = value, fill = name), 
           position = position_dodge())+
  labs(y= "", x= "Alunos Matriculados")+ 
  scale_fill_manual(name = "Legenda", 
                    labels = c("Prova Brasil", "Escola"),
                    values = c("mat"= "#663399", "TAM_ESCOLA"= "#d4b4f6"))

#**Notas MT X LP**
dados %>%
  select(NOTA_LP, NOTA_MT) %>%
  pivot_longer(everything(), names_to = "materia", values_to = "nota") %>%
  mutate(nota_cat = cut(nota, breaks = seq(0, 500, 50))) %>%
  ggplot() +
  geom_bar(aes(x = nota_cat, fill = materia), position = position_dodge()) +
  labs(y= "", x= "Notas")+ 
  scale_fill_manual(name = "Legenda", 
                    labels = c("Lingua Portuguesa", "Matematica"),
                    values = c("NOTA_MT"= "#663399", "NOTA_LP"= "#d4b4f6"))

amostra %>%
  select(NOTA_LP, NOTA_MT) %>%
  pivot_longer(everything(), names_to = "materia", values_to = "nota") %>%
  mutate(nota_cat = cut(nota, breaks = seq(0, 500, 50))) %>%
  ggplot() +
  geom_bar(aes(x = nota_cat, fill = materia), position = position_dodge())+
  labs(y= "", x= "Notas")+ 
  scale_fill_manual(name = "Legenda", 
                    labels = c("Lingua Portuguesa", "Matematica"),
                    values = c("NOTA_MT"= "#663399", "NOTA_LP"= "#d4b4f6"))



dados %>%
  select(NOTA_LP, NOTA_MT) %>%
  pivot_longer(everything(), names_to = "materia", values_to = "nota") %>%
  ggplot() +
  geom_boxplot(aes(y = nota, x = materia), fill= "#663399")+
  labs(y= "Notas", x= "Materia")

amostra %>%
  select(NOTA_LP, NOTA_MT) %>%
  pivot_longer(everything(), names_to = "materia", values_to = "nota") %>%
  ggplot() +
  geom_boxplot(aes(y = nota, x = materia), fill= "#663399")+
  labs(y= "Notas", x= "Materia")

################################################################################
################################################################################
################################################################################

#**OBJETIVO 2**
#**Estimar a proporção de escolas que menos de 75% de seus estudantes participaram da Prova**
#Risco máximo de 5%
alpha <- 0.05
DN <- nrow(dados)
AN <- nrow(amostra)

dados <- dados %>%
  mutate(part75 = PARTICIPACAO < 75)
p_sumD <- sum(dados$part75)
p_testD <- prop.test(p_sum, n = DN, conf.level=1-alpha)

amostra <- amostra %>%
  mutate(part75 = PARTICIPACAO < 75)
p_sumA <- sum(amostra$part75)
p_testA <- prop.test(p_sum, n = AN, conf.level=1-alpha)

p_testD$conf.int
p_testD$estimate

p_testA$conf.int
p_testA$estimate


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

# h0: as proporcoes entre escolas que tiveram menos de 75% de alunos inscritos é igual entre os locais
# h1: as proporcoes entre escolas que tiveram menos de 75% de alunos inscritos é diferente entre os locais
chisq.test(tabela1D)
chisq.test(tabela1A)
# conclusao: não encontramos evidencias para acreditar que a proporcao de escolas nos locais muda entre as escolas com mais e menos de 75% de inscritos

tabela2D <- table(part75D, Dregiao)
tabela2D

tabela2A <- table(part75A, Aregiao)
tabela2A

# h0: as proporcoes entre escolas que tiveram menos de 75% de alunos inscritos é igual entre as regiões
# h1: as proporcoes entre escolas que tiveram menos de 75% de alunos inscritos é diferente entre as regiões
chisq.test(tabela2D)
chisq.test(tabela2A)
# conclusao: nao rejeitamos H0, acreditamos que as proporcoes sejam iguais
?chisq.test

################################################################################
################################################################################
################################################################################
#**OBJETIVO 10**
#**Verificar se a nota em LP é um bom indicador para predizer a nota existe em MT, ou**
#**seja se estão associadas**
Dfit <- lm(NOTA_MT ~ NOTA_LP,  data = dados)

dados %>%
  ggplot() +
  geom_point(aes(x = NOTA_LP, y = NOTA_MT), color= "#663399") +
  geom_abline(slope = Dfit$coefficients[2], intercept = Dfit$coefficients[1],
              color = "black") +
  labs(y= "Nota de Lingua Portugesa", x= "Nota de Matematica")


Afit <- lm(NOTA_MT ~ NOTA_LP,  data = amostra)

amostra %>%
  ggplot() +
  geom_point(aes(x = NOTA_LP, y = NOTA_MT), color= "#663399") +
  geom_abline(slope = Afit$coefficients[2], intercept = Afit$coefficients[1], 
              color = "black") +
  labs(y= "Nota de Lingua Portugesa", x= "Nota de Matematica")



cor.test(dados$NOTA_MT, dados$NOTA_LP, alternative="two.sided", method="pearson", conf.level=1-alpha)
cor.test(amostra$NOTA_MT, amostra$NOTA_LP, alternative="two.sided", method="pearson", conf.level=1-alpha)
# conclusao: Rejeitamos H0, ou seja, temos fortes indicios de que a correlação é diferente de zero
