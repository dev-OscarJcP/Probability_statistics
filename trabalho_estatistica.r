## Função

gerar_dados <- function(m1=202121141,
                        m2=202121148,
                        m3=202121141,
                        n=2e3)
{
  stopifnot(is.numeric(m1) &
              is.numeric(m2) &
              is.numeric(m3))
  
  set.seed(m1 +
             m2 +
             m3)
  
  m_1 <- runif(1,
               min=20,
               max=40)
  
  m_2 <- runif(1,
               min=20,
               max=40)
  
  ## Categórica
  n_cat_1 <- n/10 * sample(4:8,
                           1)
  
  ## Matriz de variâncias e covariâncias
  sigma_1 <- matrix(c(m_1,
                      m_1 / 1.1,
                      m_1 / 1.1,
                      m_2),
                    ncol=2)
  
  sigma_2 <- matrix(c(m_1,
                      -1 * (m_2 / 1.2),
                      -1 * (m_2 / 1.2),
                      m_2),
                    ncol=2)
  
  require(Matrix) # S4
  near_1 <- nearPD(sigma_1)
  
  near_2 <- nearPD(sigma_2)
  
  sigma_1 <- matrix(near_1[['mat']]@x,
                    nc=ncol(sigma_1))
  
  sigma_2 <- matrix(near_2[['mat']]@x,
                    nc=ncol(sigma_2))
  
  ## Escala proporcional
  require(mvtnorm)
  v_pro_1 <- round(rmvnorm(n=n_cat_1,
                           mean=c(m_1,
                                  m_2),
                           sigma=sigma_1),
                   2)
  
  v_pro_2 <- round(rmvnorm(n=(n - n_cat_1),
                           mean=c(m_1,
                                  m_2),
                           sigma=sigma_2),
                   2)
  
  ## Escala categórica
  cat_1 <- rep('M',
               n_cat_1)
  
  cat_2 <- rep('F',
               n - n_cat_1)
  
  v_pro <- c('v_pro_1',
             'v_pro_2')
  
  v_cat <- c('cat_1',
             'cat_2')
  
  ord <- sample(1:2,
                2)
  
  sexo <- c(eval(parse(text=v_cat[ord[1]])),
            eval(parse(text=v_cat[ord[2]])))
  
  
  ## Frame de dados
  res <- as.data.frame(rbind(eval(parse(text=v_pro[ord[1]])),
                             eval(parse(text=v_pro[ord[2]]))))
  
  res <- cbind(res,
               sexo)
  
  colnames(res) <- c('Y1',
                     'Y2',
                     'Sexo')
  
  ## Outlier v_pro_1
  n_out_v1 <- sample(10:20,
                     1)
  
  out_v1 <- sample(1:length(res[, 1]),
                   n_out_v1)
  
  res[, 1][out_v1] <- sample(730:999,
                             n_out_v1)
  
  ## Outlier v_pro_2
  n_out_v2 <- sample(10:30,
                     1)
  
  out_v2 <- sample(1:length(res[, 2]),
                   n_out_v2)
  
  res[, 2][out_v2] <- sample(200:300,
                             n_out_v2)
  
  ## NAs
  res[, 1][sample(1:n, 
                  sample(10:20, 
                         1))] <- NA
  
  res[, 2][sample(1:n, 
                  sample(10:20, 
                         1))] <- NA
  
  res[, 3][sample(1:n, 
                  sample(10:20, 
                         1))] <- NA
  
  ## Negativos
  res[, 1][sample(1:n, 
                  sample(10:20, 
                         1))] <- -999
  
  res[, 2][sample(1:n, 
                  sample(10:20, 
                         1))] <- -999
  
  invisible(res)
}

gerar_dados_rl <- function(m1=202121143,
                           m2=202121120,
                           m3=202121120,
                           n=10)
{
  stopifnot(is.numeric(m1) &
              is.numeric(m2) &
              is.numeric(m3))
  
  set.seed(sum(m1,
               m2,
               m3))
  
  X <- seq(0, 10, length=n)
  Y <- 1 + 2*X + -.08*X^2 + rnorm(n)
  
  res <- data.frame(X,
                    Y)
  
  invisible(res)
}

## Função
gerar_tdf <- function(m1=202121143,
                      m2=202121120,
                      m3=202121120)
{
  stopifnot(is.numeric(m1) &
              is.numeric(m2) &
              is.numeric(m3))
  
  set.seed(sum(m1,
               m2,
               m3))
  
  classes <- c("[10, 020)",
               "[20, 030)",
               "[30, 040)",
               "[40, 050)",
               "[50, 060)",
               "[60, 070)",
               "[70, 080)",
               "[80, 090)",
               "[90, 100)")
  
  X <- c(seq(f=10, 
             t=50, 
             b=10), 
         seq(f=40, 
             t=10, 
             b=-10))
  
  Y <- sample(1:3,
              length(X),
              rep=T)
  
  f <- (X - Y)
  
  
  rfp <- round(100*f/sum(f),
               2)
  
  cfp <- round(100*cumsum(f/sum(f)),
               2)
  
  res <- data.frame(classes,
                    f,
                    rfp,
                    cfp)
  
  names(res) <- c('Classes',
                  'f',
                  'rf(%)',
                  'cf(%)')
  
  invisible(res)
}

library('fdth')

## gerar_dados 
dad = gerar_dados(m1=202121143, m2=202121120, m3=202121120)

## gerar_dados_rl
dad_rl = gerar_dados_rl(m1=202121143, m2=202121120, m3=202121120)

## gerar_tdf
tb = gerar_tdf(m1=202121143, m2=202121120, m3=202121120)

#1
#1.1
#a
par (mfrow=c(1,2))
boxplot(dad$Y1, dad$Y2, main = "Antes", names = c("Y1", "Y2"), col = "gray")
boxplot(dad$Y1, dad$Y2, main = "Após", names = c("Y1", "Y2"), outline=FALSE, col = "gray")

#Tratativa de dados
summary(dad)
outliers = boxplot(dad$Y1, plot=FALSE)$out
dad[which(dad$Y1 %in% outliers), ]$Y1 = median(dad$Y1, na.rm = T)
outliers = boxplot(dad$Y2, plot=FALSE)$out
dad[which(dad$Y2 %in% outliers), ]$Y2 = median(dad$Y2, na.rm = T)
dad[is.na(dad$Y1), ]$Y1 = median(dad$Y1, na.rm = T)
dad[is.na(dad$Y2), ]$Y2 = median(dad$Y2, na.rm = T)
dad[is.na(dad$Sexo), ]$Sexo = "M"

dad_m = dad[which (dad$Sexo == "M"), ]
dad_f = dad[which (dad$Sexo == "F"), ]

#b
par (mfrow=c(1,2))
boxplot(dad_m$Y1, dad_m$Y2, main = "Masculino", names = c("Y1", "Y2"), outline=FALSE, col = "gray")
boxplot(dad_f$Y1, dad_f$Y2, main = "Feminino", names = c("Y1", "Y2"), outline=FALSE, col = "gray")


#1.2
#a
tabela_m = fdt (dad_m$Y1)
tabela_f = fdt (dad_f$Y1)
tabela_m
tabela_f

#b
plot(tabela_m, type='cfph', x.round=2, xlab='Y1', ylab='Frequência acumulada')
plot(tabela_m, type='cfpp', x.round=2, ylab='Frequência acumulada', xlab='Y1')

plot(tabela_f, type='cfph', x.round=2, xlab='Y1', ylab='Frequência acumulada')
plot(tabela_f, type='cfpp', x.round=2, ylab='Frequência acumulada', xlab='Y1')

#2
#2.1.a
moday1 = max(table(dad_m$Y1))
moday2 = max(table(dad_m$Y2))
n  = c(mean(dad_m$Y1), mean(dad_m$Y2))
m = c(median(dad_m$Y1), median(dad_m$Y2))
md = c(moday1, moday2)
tabela_m = cbind (n, m, md)
rownames(tabela_m) = c("Y1", "Ỳ2")

moday1 = max(table(dad_f$Y1))
moday2 = max(table(dad_f$Y2))
n  = c(mean(dad_f$Y1), mean(dad_f$Y2))
m = c(median(dad_f$Y1), median(dad_f$Y2))
md = c(moday1, moday2)
tabela_f = cbind (n, m, md)
rownames(tabela_f) = c("Y1", "Ỳ2")

tabela_m
tabela_f

#2.1.b
#quartis
a = c(quantile(dad_m$Y1, 0.25), quantile(dad_m$Y2, 0.25))
b = c(quantile(dad_m$Y1, 0.5), quantile(dad_m$Y2, 0.5))
c = c(quantile(dad_m$Y1, 0.75), quantile(dad_m$Y2, 0.75))
tabela_m = cbind (a, b, c)
rownames(tabela_m) = c("Y1", "Ỳ2")
colnames(tabela_m) = c("25%", "50%", "75%")

a = c(quantile(dad_f$Y1, 0.25), quantile(dad_f$Y2, 0.25))
b = c(quantile(dad_f$Y1, 0.5), quantile(dad_f$Y2, 0.5))
c = c(quantile(dad_f$Y1, 0.75), quantile(dad_f$Y2, 0.75))
tabela_f = cbind (a, b, c)
rownames(tabela_f) = c("Y1", "Ỳ2")
colnames(tabela_f) = c("25%", "50%", "75%")

tabela_m
tabela_f

#decis
a = c(quantile(dad_m$Y1, 0.1), quantile(dad_m$Y2, 0.1))
b = c(quantile(dad_m$Y1, 0.2), quantile(dad_m$Y2, 0.2)) 
c = c(quantile(dad_m$Y1, 0.3), quantile(dad_m$Y2, 0.3))
d = c(quantile(dad_m$Y1, 0.4), quantile(dad_m$Y2, 0.4))
e = c(quantile(dad_m$Y1, 0.5), quantile(dad_m$Y2, 0.5))
f = c(quantile(dad_m$Y1, 0.6), quantile(dad_m$Y2, 0.6))
g = c(quantile(dad_m$Y1, 0.7), quantile(dad_m$Y2, 0.7))
h = c(quantile(dad_m$Y1, 0.8), quantile(dad_m$Y2, 0.8))
i = c(quantile(dad_m$Y1, 0.9), quantile(dad_m$Y2, 0.9))
tabela_m = cbind (a, b, c, d, e, f, g, h, i)
rownames(tabela_m) = c("Y1", "Ỳ2")
colnames(tabela_m) = c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%")

a = c(quantile(dad_f$Y1, 0.1), quantile(dad_f$Y2, 0.1))
b = c(quantile(dad_f$Y1, 0.2), quantile(dad_f$Y2, 0.2)) 
c = c(quantile(dad_f$Y1, 0.3), quantile(dad_f$Y2, 0.3))
d = c(quantile(dad_f$Y1, 0.4), quantile(dad_f$Y2, 0.4))
e = c(quantile(dad_f$Y1, 0.5), quantile(dad_f$Y2, 0.5))
f = c(quantile(dad_f$Y1, 0.6), quantile(dad_f$Y2, 0.6))
g = c(quantile(dad_f$Y1, 0.7), quantile(dad_f$Y2, 0.7))
h = c(quantile(dad_f$Y1, 0.8), quantile(dad_f$Y2, 0.8))
i = c(quantile(dad_f$Y1, 0.9), quantile(dad_f$Y2, 0.9))
tabela_f = cbind (a, b, c, d, e, f, g, h, i)
rownames(tabela_f) = c("Y1", "Ỳ2")
colnames(tabela_f) = c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%")

tabela_m
tabela_f

#2.1.c
tabela_m = cbind ( c(diff(range(dad_m$Y1)), diff(range(dad_m$Y2))), c(var(dad_m$Y1), var(dad_m$Y2)), c(sd(dad_m$Y1), sd(dad_m$Y2)), c(sd(dad_m$Y1)/mean(dad_m$Y1)*100, sd(dad_m$Y2)/mean(dad_m$Y2)*100) )
rownames(tabela_m) = c("Y1", "Ỳ2")
colnames(tabela_m) = c("a.t", "variância", "d.padrão", "c.v")

tabela_f = cbind ( c(diff(range(dad_f$Y1)), diff(range(dad_f$Y2))), c(var(dad_f$Y1), var(dad_f$Y2)), c(sd(dad_f$Y1), sd(dad_f$Y2)), c(sd(dad_f$Y1)/mean(dad_f$Y1)*100, sd(dad_f$Y2)/mean(dad_f$Y2)*100) )
rownames(tabela_f) = c("Y1", "Ỳ2")
colnames(tabela_f) = c("a.t", "variância", "d.padrão", "c.v")

tabela_m
tabela_f

#2.2.a
tb
rf = prop.table(tb$f)
rf
cf = cumsum(tb$f)
cf

tabela = cbind (tb$f, rf, tb$`rf(%)`, cf, tb$`cf(%)`)
colnames(tabela) = c("f", "rf", "rf(%)", "cf", "cf(%)")
tabela

#media
intervalos_medio = c(15, 25, 35, 45, 55, 65, 75, 85, 95)
aux = intervalos_medio * tb$f
aux = sum(aux)
medida = aux/sum(tb$f)

#mediana
intervalos = cbind(c(10, 20, 30, 40, 50, 60, 70, 80, 90), c(20, 30, 40, 50, 60, 70, 80, 90, 100))
f_a = cumsum(tb$f)
n = sum(tb$f)*0.5
i=1
aux = 0
while (n > aux){
    aux = intervalos[i, 1] + aux
    if (n > aux){
      i = i+1  
    }
    
}
md = intervalos[i][1] + ( (n-f_a[i-1])/tb$f[i] )* 10

#moda
i=1
while (tb$f[i] != max(tb$f)){
  i = i+1  
}
moda = (intervalos[i, 2] + intervalos[i, 1])/2

#tabela
tabela = cbind (medida, md, moda)
rownames(tabela) = c("medida")
colnames(tabela) = c("m", "md", "mo")
tabela

#2.2.b
#Quartis
#Q1
n = sum(tb$f)*0.25
i=1
aux = 0
while (n > aux){
  aux = intervalos[i, 1] + aux
  if (n > aux){
    i = i+1  
  }
  
}
q1 = intervalos[i][1] + ( (n-f_a[i-1])/tb$f[i] )* 10

#Q3
n = sum(tb$f)*0.75
i=1
aux = 0
while (n > aux){
  aux = intervalos[i, 1] + aux
  if (n > aux){
    i = i+1  
  }
  
}
q3 = intervalos[i][1] + ( (n-f_a[i-1])/tb$f[i] )* 10

#tabela
tabela = cbind (q1, md, q3)
rownames(tabela) = c("quartil")
colnames(tabela) = c("25%", "50%", "75%")
tabela


#Decis
n = sum(tb$f)*0.1
i=1
aux = 0
while (n > aux){
  aux = intervalos[i, 1] + aux
  if (n > aux){
    i = i+1  
  }
  
}
d1 = intervalos[i][1] + ( (n-f_a[i-1])/tb$f[i] )* 10
d1

n = sum(tb$f)*0.2
i=1
aux = 0
while (n > aux){
  aux = intervalos[i, 1] + aux
  if (n > aux){
    i = i+1  
  }
  
}
d2 = intervalos[i][1] + ( (n-f_a[i-1])/tb$f[i] )* 10

n = sum(tb$f)*0.3
i=1
aux = 0
while (n > aux){
  aux = intervalos[i, 1] + aux
  if (n > aux){
    i = i+1  
  }
  
}
d3 = intervalos[i][1] + ( (n-f_a[i-1])/tb$f[i] )* 10

n = sum(tb$f)*0.4
i=1
aux = 0
while (n > aux){
  aux = intervalos[i, 1] + aux
  if (n > aux){
    i = i+1  
  }
  
}
d4 = intervalos[i][1] + ( (n-f_a[i-1])/tb$f[i] )* 10

d5 = md

n = sum(tb$f)*0.6
i=1
aux = 0
while (n > aux){
  aux = intervalos[i, 1] + aux
  if (n > aux){
    i = i+1  
  }
  
}
d6 = intervalos[i][1] + ( (n-f_a[i-1])/tb$f[i] )* 10

n = sum(tb$f)*0.7
i=1
aux = 0
while (n > aux){
  aux = intervalos[i, 1] + aux
  if (n > aux){
    i = i+1  
  }
  
}
d7 = intervalos[i][1] + ( (n-f_a[i-1])/tb$f[i] )* 10

n = sum(tb$f)*0.8
i=1
aux = 0
while (n > aux){
  aux = intervalos[i, 1] + aux
  if (n > aux){
    i = i+1  
  }
  
}
d8 = intervalos[i][1] + ( (n-f_a[i-1])/tb$f[i] )* 10

n = sum(tb$f)*0.9
i=1
aux = 0
while (n > aux){
  aux = intervalos[i, 1] + aux
  if (n > aux){
    i = i+1  
  }
  
}
d9 = intervalos[i][1] + ( (n-f_a[i-1])/tb$f[i] )* 10

#tabela
tabela = cbind (d1, d2, d3, d4, d5, d6, d7, d8, d9)
rownames(tabela) = c("decil")
colnames(tabela) = c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%")
tabela


#2.2.c
#amplitude
am = 80

#dp
fi = tb$f
xi = intervalos_medio
fi_xi = fi*intervalos_medio
fi_xi_2 = fi*intervalos_medio*intervalos_medio
n = sum(fi)
a = sum(fi_xi_2)
b = sum(fi_xi)
dp = sqrt(a/n - (b/n)**2)

#variancia
var = dp**2
#coeficiente de variacao
cv = (dp/medida)*100

#tabela
tabela = cbind (am, var, dp, cv)
rownames(tabela) = c("medida")
colnames(tabela) = c("amplitude", "variância", "d.padrão", "c.v")
tabela

#3.1.a
#Covariancia

tabela_m = cbind (c(cov(dad_m$Y1, dad_m$Y1), cov(dad_m$Y1, dad_m$Y2)), c(cov(dad_m$Y2, dad_m$Y1), cov(dad_m$Y2, dad_m$Y2)))
rownames(tabela_m) = c("Y1", "Y2")
colnames(tabela_m) = c("Y1", "Y2")
tabela_m

tabela_f = cbind (c(cov(dad_f$Y1, dad_f$Y1), cov(dad_f$Y1, dad_f$Y2)), c(cov(dad_f$Y2, dad_f$Y1), cov(dad_f$Y2, dad_f$Y2)))
rownames(tabela_f) = c("Y1", "Y2")
colnames(tabela_f) = c("Y1", "Y2")
tabela_f

#correlação linear
tabela_m = cbind (c(cor(dad_m$Y1, dad_m$Y1), cor(dad_m$Y1, dad_m$Y2)), c(cor(dad_m$Y2, dad_m$Y1), cor(dad_m$Y2, dad_m$Y2)))
rownames(tabela_m) = c("Y1", "Y2")
colnames(tabela_m) = c("Y1", "Y2")
tabela_m

tabela_f = cbind (c(cor(dad_f$Y1, dad_f$Y1), cor(dad_f$Y1, dad_f$Y2)), c(cor(dad_f$Y2, dad_f$Y1), cor(dad_f$Y2, dad_f$Y2)))
rownames(tabela_f) = c("Y1", "Y2")
colnames(tabela_f) = c("Y1", "Y2")
tabela_f


#3.1.b
par(mfrow=c(1, 2))
plot(dad_m$Y1, dad_m$Y2, main = "Masculino", xlab = "Y1", ylab  = "Y2")
plot(dad_f$Y1, dad_f$Y2, main = "Feminino", xlab = "Y1", ylab  = "Y2")


#3.2.a
modelo_grau1 = lm(data = dad_rl, formula = Y ~ X)
modelo_grau2 = lm(data = dad_rl, formula = Y ~ X + I(X^2))
summary(modelo_grau1)
summary(modelo_grau2)

x = dad_rl$X
y = dad_rl$Y

#3.2.b

(X <- cbind(1, x))        # Modelo linear de 1 grau: com intercepto

(Y <- cbind(y))

(XlX     <- t(X) %*% X)      # X'X

#     -1
(XlX_inv <- solve(XlX))      # (X'X) 

(XlY     <- t(X) %*% Y)      #  X'Y

#      -1     
(b_est   <- XlX_inv %*% XlY) # (X'X) . X'Y

(Y_est   <- X %*% b_est)     # Ys estimados pelo modelo ajustado

# Visualizando os dados


#Modelo linear de 1 grau: com intercepto
par(mfrow=c(1, 2))
plot(y ~ x, main="A: Polinomio I grau", xlim=c(0, 10), ylim=c(0, 15), pch=19, col='blue')

lines(spline(Y_est ~ x), col='red', lw=2)     

#Modelo linear de 2 grau: com intercepto
plot(y ~ x, main="B: Polinomio II grau", xlim=c(0, 10), ylim=c(0, 15), pch=19, col='blue')

lines(spline(fitted(lm(y ~ x + I(x^2))) ~ x, n=1e2), col='red', lw=2)    
