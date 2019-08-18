##########################
#### TALLER 7/11/2017 ####
##########################

library(survey)

mu284 <- read.table("Talleres/Taller 3/mu284.txt", header=TRUE)
N <- dim(mu284)[1]

# Muestra de primer fase
n_sa <- 150
set.seed(123321)
s1 <- sample(N, n_sa)
sa <- mu284[s1, ]

# Seleccón de muestra de segunda fase
Ens <- 10
pik_sa <- Ens * sa$P75 / sum(sa$P75)

set.seed(123321)
u <- runif(n_sa)
s2 <- (u < pik_sa)
n_s <- sum(s2)

sa$s <- s2
sa$pi_ak <- rep(n_sa/N, n_sa)
sa$pik_sa <- pik_sa
sa$fpc <- rep(N, n_sa)

p.2f <- twophase(id=list(~1, ~1), # 1 -> directo de elementos en ambas fases 
                 probs=list(~pi_ak, ~pik_sa), # pi de cada fase (diseño)
                 fpc=list(~fpc, NULL),
                 data=sa,
                 subset=~s) # variable que indica cuáles son elementos de s
summary(p.2f)

svytotal(~REV84, p.2f)
sum(mu284$REV84)

# total
t_g <- sum(sa$REV84)

###############################
#### FIN DE LA PROGRAMCI?N ####
###############################