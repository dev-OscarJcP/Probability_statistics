#---------------------------------------------------------------------------------------------------------
# Universidade Estadual de Santa Cruz
# Disciplina: CET083 - Probabilidade e Estatística
# Tema: Variáveis Aleatórias Discretas - VAD
# Alunos: Oscar Euzebio Neto,
#         Thiago Souza Pereira
#---------------------------------------------------------------------------------------------------------

#. Criação de funções customizadas para demonstração

#.. Bernoulli
# x: número de sucessos
# p: probabilidade do sucesso

dbern_custom <- function(x, p)
  p^x * (1-p)^(1-x)

#.. Binomial
# x: número de sucessos
# n: número de ensaios
# p: probabilidade do sucesso

dbinom_custom <- function(x, n, p)
   (factorial(n)/(factorial(x) * factorial(n-x))) *
   p^x * (1-p)^(n-x)
 
#... Observar: Binomial generaliza a de Bernoulli!
# Em que situação?
# Quando (factorial(n)/(factorial(x)* factorial(n - x))) é igual a 1!
# Ou seja em apenas uma repetição do experimento!

# Verificação:
n <- 1  # número de tentativas
x <- 1  # número de sucessos

(factorial(n)/(factorial(x) * factorial(n-x)))

#... Exemplo 1 - Chute da questão - Distribuição de Bernoulli e Binomial
# Em uma questão com 5 alternativas, qual a probabilidade de acertar essa 
# questão no chute?
x <- 1
p <- 1/5

# Bernoulli
(res_bern <- dbern_custom(x, p))

# Binomial
(res_dbin <- dbinom_custom(x,
                          1,
                          p))

# A distribuição de Bernoulli é um caso especial da distribuição Binomial, com n = 1.
res_bern == res_dbin

# Verificação
# Sabemos que chutando uma alternativa entre 5 possíveis, a probabilidade de 
# acertar em uma única tentativa é: 1/5 = 0.2
res_bern == 1/5

#... Exemplo 2 - Lançamento da moeda - Distribuição Binomial
# Uma moeda é lançada 10 vezes. Qual a probabilidade de ocorrer pelo menos
# 1 coroa?
# Como só não pode ocorrer 0 coroas, temos:
x <- 0    # Número de sucessos
n <- 10   # Número de tentativas
p <- 1/2  # Probabilidade de sucesso

# Binomial customizada
(res_dbin <- dbinom_custom(x,
                          n,
                          p))

# A probabilidade de ocorrer pelo menos uma coroa, são todas as outras probabilidades, 
# menos a de 0 coroas, logo:
# P(S) - P(x=0)
1 - res_dbin

# dbinom: observar a implementação do função de probabilidade binomial no R
str(dbinom)
# 
dbinom(x=x,
       size=1,
       prob=p)

# Exemplo usando função nativa do R
# Utilizando ainda o exemplo anterior, podemos calcular agora a probalidade de 
# ocorrer 6 caras em 10 lançamentos de uma moeda:
x <- 6       # Número de sucessos 
size <- 10   # Número de tentativas
prob <- 1/2  # Probabilidade de sucesso

# Utilizando a binomial já implementada no R
(verify <- dbinom(x,
                  size,
                  prob))

# Verificação
# A probabilidade de ocorrer 6 caras em 10 lançamentos é de 210/1024 = 0.2050781, 
# ou seja, aproximadamente 21%.
round(verify, 7) == round(210/1024, 7)

# (210/1024)?
ncol(combn(10, 6))
2^10

# Esperança Matemática: E(X)
(E <- p)

# Variância: V(X)
(V <- p*(1-p))

#... Exemplo 3 - Moléculas raras - Distribuição Binomial
x <- 2    # Número de moléculas raras que quero achar (número de sucessos)
n <- 18   # Número de tentativas ou experimentos
p <- 0.1  # Probabilidade de se encontrar uma molécula rara em uma tentativa

# Probabilidade de se encontrar 2 moléculas em 18 tentativas
dbinom_custom(x,
              n,
              p)

dbinom(x,
       n,
       p)

# Vamos alterar o exemplo
x <- 5    # Desejamos encontrar 5 moléculas raras
n <- 10   # Alterar o némero de tentativas para 10
p <- 0.5  # Alterar a probabilidade para 50%

dbinom_custom(x,
              n,
              p) # Probabiliade de se encontrar 5 moléculas em 10 tentativas

dbinom(x,
       n,
       p)

# Vamos agora diminuir o número de sucessos que desejamos encontrar
x <- 2

# A probabilidade vai aumentar ou diminuir?
dbinom_custom(x,
              n,
              p)

dbinom(x,
       n,
       p)

# A probabilidade diminui!

# Esperança Matemática
(E <- n*p)

# Variância
(V <- n*p*(1-p))

#... Exemplo 4 - Controle de Qualidade - Distribuição Geométrica
# Uma linha de produção está sendo analisada para efeito de controle de qualidade 
# das peças produzidas. Tendo em vista o alto padrão requerido, a produção é interrompida 
# para regulagem toda vez que uma peça defeituosa é observada, onde a probabilidade da 
# peça ser defeituosa é de 0,01.

# Calcule a probabilidade da 1ª peça ser defeituosa.
x <- 1-1   # Peças defeituosas Tentativa k - 1.
p <- 0.01  # Probabilidade de ocorrer sucesso na k-ésima tentativa

#.. Geométrica (nativa do R)
(result <- dgeom(x,
                 p))

# Verificação
# Para ocorrer uma peça defeituosa logo na primeira peça fabricada, a possibilidade 
# de fracasso é 0. (Tentativas - 1). Portanto, há somente a possibilidade de sucesso, 
# que é 0.01, dada no exemplo.
result == 0.01

# Esperança Matemática
(E <- 1/p)

# Variância
(V <- (1-p)/p*p) 

#... Exemplo 5 - Lançamento de moedas - Distribuição Binomial Negativa
# Calcular o número de tentativas até que ocorram r sucessos. 
# Vamos lançar uma moeda até que saiam 3 caras.
x <- 3-3  # Número de fracassos dado pelo número de tentativas - número de sucessos.
r <- 3    # Número de sucessos
p <- 1/2  # Probabilidade de sucesso

#.. Binomial negativa (nativa do R)
(result<- dnbinom(x,
                  r,
                  p))

# Verificação
# Sabemos que para que saiam 3 caras em 3 tentativas, temos que obter três sucessos 
# consecutivos, portanto a probabilidade é p³, sendo 1/2 a probabilidade de cada 
# tentativa, temos: (1/2)³ = 0.125.
round(result, 3) == round(0.125, 3) # Aproximação utilizando 3 casas decimais.

# Esperança Matemática
(E <- r/p)

# Variância
(V <-  r*(1-p)/p*p)

#... Exemplo 6 - Caixas com bolas coloridas - Distribuição Hipergeométrica
# Em uma caixa temos 10 bolas, sendo 4 brancas e 6 vermelhas. Se três são retiradas 
# sem reposição, qual a probabilidade de sair duas bolas vermelhas?
k <- 6       # Número de elementos do tipo desejado
N <- 10 - 6  # Totalidade de elementos - número de elementos do tipo desejado.
n <- 3       # Número de elementos selecionados
x <- 2       # Número de sucessos desejados

#.. Hipergeométrica (nativa do R)
(result <- dhyper(x,
                  k,
                  N,
                  n))

# Verificação 
# A partir de contas feitas utilizando a função de probabilidade estudada anteriomente, 
# a probabilidade é igual a 1/2.
round(result, 2) == round(1/2, 2) # Aproximação utilizando 2 casas decimais.

(p <- k/N)

# Esperança Matemática
(E <- n*p)

# Variância
(V <- n*p*(1-p)*((N-n)/(N-1)))

#.. Poisson
# Função de probabilidade implementada
dpois_custom <- function(x, lambda)
  (exp(1)^-lambda)*(lambda^x) / factorial(x)

#... Exemplo 7 - Fio de cobre - Poisson
x <- 2
lambda <- 2.3

dpois_custom(x,
             lambda)

# Função nativa do R
dpois(x,
      lambda)

# Probabilidades de erros no intervalo de 0 a 10
x <- 0:10
lambda <- 2.3

(pois <- dpois(x,
               lambda))

x11()

plot(x,
     pois,
     type='b',
     pch=20,
     col='darkred',
     xlab= "Nº de erros por milímetro",
     ylab="Probabilidade de Poisson",
     main="Distribuição de Poisson")

# Esperança Matemática
(E <- lambda)

# Variância
(V <- lambda)

# Função customizada com a variável tempo
dpois_custom2 <- function(x, lambda, tempo)
  ((exp(1)^-(lambda*tempo))*((lambda*tempo)^x)) / factorial(x)

#... Exemplo 8  - Chamadas - Distribuição de Poisson
# Em média há 2 chamadas por hora em um certo telefone. Calcule a probabilidade 
# de receber no máximo 3 chamadas em 2 horas.
x <- 0:3
lambda <- 2
tempo <- 2
(result <- dpois_custom2(x, lambda, tempo))

# A probabilidade de se receber no máximo 3 chamadas em 2 horas, é uma soma das 
# probabilidades de receber: 0, 1, 2 e 3 chamadas.
