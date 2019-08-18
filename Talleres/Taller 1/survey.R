##################################
#### ESTIMADORES DE REGRESIÓN ####
##################################

# R code from vignette source 'Survey.Rnw'
# Encoding: ISO8859-1

library(survey)
MU281 <- read.table("MU281.txt", header=TRUE)
MU281[1:5,]
dim(MU281)
N <- nrow(MU281)
k <- seq(1, N)
fpc <- rep(N, N)
U <- data.frame(k, MU281$RMT85, MU281$CS82, MU281$SS82, fpc)
nombres <- c("k", "y", "x.1", "x.2", "N")
colnames(U) <- nombres
U[1:5,]
t.y.U <- sum(U$y)
t.x1.U <- sum(U$x.1)
t.x2.U <- sum(U$x.2)
t.x.U <- matrix(c(N, t.x1.U, t.x2.U))
t.y.U
t.x.U
n <- 100
pw <- rep(n/N, N)
set.seed(48182)
s <- sample(seq(1,N), n, replace = FALSE, prob = pw)
datos <- U[s,]
ps <- svydesign(id=~1, data=datos, fpc=~N)
summary(ps)
svytotal(~y, ps)

# \hat{t_{y_{pi}}}
r <- ftable(svytotal(~y, ps))
est <- r[1]
des <- r[2]
ra1 <- svyratio(~y, ~x.1, ps)
pop <- data.frame(x.1=t.x1.U) 
predict(ra1, pop$x.1)

# \hat{t_{y_{ra}_{x_1}}}

# Responde al modelo: y_k = beta*x_k1 + epsilon_k 
# con V(epsilon_k) = sigma^2 * x_k1

ra2 <- svyratio(~y, ~x.2, ps)
pop <- data.frame(x.2=t.x2.U) 
predict(ra2, pop$x.2)
# \hat{t_{y_{ra}_{x_2}}}

# Responde al modelo: y_k = beta*x_k2 + epsilon_k 
# con V(epsilon_k) = sigma^2 * x_k2

reg1 <- svyglm(y ~ x.1, ps)
summary(reg1)
pop <- data.frame(x.1=t.x1.U) 
predict(reg1, newdata=pop, total=N)

summary(lm(y ~ x.1, data=datos))

# Las diferencias entre svylm y lm es peque?a debido al dise?o, pero lm no corrige por poblaci?n finita.
# Responde al modelo: y_k = alpha + beta*x_k1 + epsilon_k 
# con V(epsilon_k) = sigma^2

reg2 <- svyglm(y ~ x.2, ps)
summary(reg2)
pop <- data.frame(x.2=t.x2.U) 
predict(reg2, newdata=pop, total=N)

summary(lm(y ~ x.2, data=datos))

# Las diferencias entre svylm y lm es peque?a debido al dise?o, pero lm no corrige por poblaci?n finita.
# Responde al modelo: y_k = alpha + beta * x_k2 + epsilon_k 
# con V(epsilon_k) = sigma^2


# Ahora con x_2 pero sin constante
reg2 <- svyglm(y ~ x.2 - 1, ps)
summary(reg2)
pop <- data.frame(x.2=t.x2.U) 
predict(reg2, newdata=pop, total=N)

# Responde al modelo: y_k = beta * x_k2 + epsilon_k 
# con V(epsilon_k) = sigma^2


reg12 <- svyglm(y ~ x.1 + x.2, ps)
summary(reg12)
pop <- data.frame(x.1=t.x1.U, x.2=t.x2.U) 
predict(reg12, newdata=pop, total=N)

# t_y_reg_x1;x2

# Responde al modelo: y_k = alpha + beta_1 * x_k1 + beta_2 * x_k2 + epsilon_k 
# con V(epsilon_k) = sigma^2

y.s <- as.matrix(datos$y)

tpi <- (N/n)*sum(y.s)
tpi
vpi <- N^2*(1-n/N)*(1/n)*var(y.s)
sqrt(vpi)

x.s <- as.matrix(rbind(1, datos$x.1, datos$x.2))

T_hat <- (N/n)*x.s%*%t(x.s)
t_hat <- (N/n)*x.s%*%y.s
b_hat <- solve(T_hat)%*%t_hat

treg <- t(t.x.U) %*% b_hat
treg

e.k.s <- y.s - t(x.s)%*%b_hat

vreg1 <- (N^2/n)*(1 - n/N)*var(e.k.s)
sqrt(vreg1)

# Survey usa los g_k_s, por eso no dan iguales. En este caso estoy fijando las g_k_s en 1.

t.x.s <- as.matrix(apply((N/n)*x.s, 1, sum)) #Los gks
g.k.s <- t(1 + t((t.x.U - t.x.s)) %*% solve(T_hat) %*% x.s)

#Funcionan los gks?
(N/n)*sum(y.s*g.k.s)
(N/n)*apply(x.s%*%g.k.s, 1, sum)

#Var con los gks
vreg2 <- (N^2/n)*(1-n/N)*(n-1)^(-1)*sum(c((e.k.s)^2)*(g.k.s^2))
sqrt(vreg2)

R <- 5000
tpi <- rep(0, R)
vtpi <- rep(0, R)
treg <- rep(0, R)
vreg1 <- rep(0, R)
vreg2 <- rep(0, R)

set.seed(987654321)
for (i in 1:R){
  s <- sample(seq(1, N), n, replace=FALSE, prob=pw)
  y.s <- as.matrix(U[s,"y"]) 
  tpi[i] <- (N/n)*sum(y.s)
  vtpi[i] <- N^2*(1-n/N)*(1/n)*var(y.s)
  x.s <- as.matrix(rbind(1, U[s, "x.1"], U[s, "x.2"]))
	T_hat <- (N/n)*x.s%*%t(x.s)
  t_hat <- (N/n)*x.s%*%y.s
  b_hat <- solve(T_hat)%*%t_hat
  treg[i] <- t(t.x.U)%*%b_hat #(6.5.5)
  
  #Var sin gks
  e.k.s <- y.s - t(x.s)%*%b_hat
  vreg1[i] <- (N^2/n)*(1-n/N)*var(e.k.s)
	
  #Los gks
  t.x.s <- as.matrix(apply((N/n)*x.s, 1, sum))
  g.k.s <- t(1 + t((t.x.U - t.x.s))%*%solve(T_hat)%*%x.s)
  
  #Var con gks
  vreg2[i] <- (N^2/n)*(1-n/N)*(n-1)^(-1)*sum(c((e.k.s)^2)*(g.k.s^2))
}

a11 <- mean(tpi)*10^-4
a12 <- var(tpi)*10^-8
a14 <- mean(vtpi)*10^-8

a61 <- mean(treg)*10^-4
a62 <- var(treg)*10^-8
a63 <- mean(vreg2)*10^-8
a64 <- mean(vreg1)*10^-8

par(mfrow=c(1, 2))
hist(tpi, ylim=c(0, 0.00017), xlim=c(35000, 70000), main="", freq=F)
abline(v=t.y.U, col="red")
hist(treg, ylim=c(0, 0.00017), xlim=c(35000, 70000), main="", freq=F)
abline(v=t.y.U, col="red")

aux <- calibrate(design=ps, y ~ x.1 + x.2, pop=as.vector(c(N, as.matrix(pop))))
summary(aux)

svytotal(~y, aux)
plot(weights(ps), weights(aux), col="red", alpha=0.1, pch=".")

################################
#### FIN DE LA PROGRAMACI?N ####
################################