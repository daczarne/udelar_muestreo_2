#############################
#### TALLER 4 - VARIANZA ####
#############################

# library(PracTools)

library(survey)

?as.svrepdesign

data(scd)
scd
# ambulance = PSU = 2. Hay 5 y eleige 2
# arrest: ataque cardíaco
# alive: los que llegan vivos

scddes <- svydesign(data=scd, prob=~1, id=~ambulance, strata=~ESA, 
                    nest=TRUE, fpc=rep(5,6)) # con fpc
scdnofpc <- svydesign(data=scd, prob=~1, id=~ambulance, strata=~ESA, 
                      nest=TRUE) # sin fpc

# convert to BRR replicate weights
scd2brr <- as.svrepdesign(scdnofpc, type="BRR") # pesos replicados con brr
weights(scd2brr)

scd2fay3 <- as.svrepdesign(scdnofpc, type="Fay",fay.rho=0.3)
weights(scd2fay3) # idem pero para el Fay

scd2fay9 <- as.svrepdesign(scdnofpc, type="Fay",fay.rho=0.9)
weights(scd2fay9)

# convert to JKn weights
scd2jkn <- as.svrepdesign(scdnofpc, type="JKn")
weights(scd2jkn)

# convert to JKn weights with finite population correction
scd2jknf <- as.svrepdesign(scddes, type="JKn")
weights(scd2jknf)

## with user-supplied hadamard matrix
scd2brr1 <- as.svrepdesign(scdnofpc, type="BRR", hadamard.matrix=hadamard(9))
weights(scd2brr1)


svyratio(~alive,~arrests, design=scddes)
svyratio(~alive,~arrests, design=scdnofpc)
svyratio(~alive,~arrests, design=scd2brr)
svyratio(~alive,~arrests, design=scd2brr1)
svyratio(~alive,~arrests, design=scd2fay3)
svyratio(~alive,~arrests, design=scd2fay9)
svyratio(~alive,~arrests, design=scd2jkn)
svyratio(~alive,~arrests, design=scd2jknf)


data(api)

# The Academic Performance Index (API) is computed for all California 
# schools based on standardised testing of students.
# The data sets contain information for all schools with at least 100
# students and for various probability samples of the data.
# The full population data in apipop are a data frame with 6194

# api00 API in 2000
# api99 API in 1999
# enroll number of students enrolled
# api.stu number of students tested.


## Stratified sample
dstrat <- svydesign(id=~1, strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
summary(dstrat)
## convert to JKn jackknife
rstrat <- as.svrepdesign(dstrat, type="JKn")
## convert to bootstrap
bstrat <- as.svrepdesign(dstrat, type="bootstrap", replicates=500)

svytotal(~enroll, dstrat)
svytotal(~enroll, rstrat)
svytotal(~enroll, bstrat)

svymean(~api00, dstrat)
svymean(~api00, rstrat)
svymean(~api00, bstrat)

svyratio(~api.stu, ~enroll, dstrat)
svyratio(~api.stu, ~enroll, rstrat)
svyratio(~api.stu, ~enroll, bstrat)

## one-stage cluster sample
dclus1 <- svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
## convert to JK1 jackknife
rclus1 <- as.svrepdesign(dclus1)
## convert to bootstrap
bclus1 <- as.svrepdesign(dclus1, type="bootstrap", replicates=100)

svytotal(~enroll, dclus1)
svytotal(~enroll, rclus1)
svytotal(~enroll, bclus1)

svymean(~api00, dclus1)
svymean(~api00, rclus1)
svymean(~api00, bclus1)

svyratio(~api.stu, ~enroll, dclus1)
svyratio(~api.stu, ~enroll, rclus1)
svyratio(~api.stu, ~enroll, bclus1)


## two-stage cluster sample
dclus2<-svydesign(id = ~dnum + snum, fpc = ~fpc1 + fpc2, data = apiclus2)
## convert to bootstrap
mrbclus2<-as.svrepdesign(dclus2, type="mrb",replicates=100)

svytotal(~enroll, dclus2, na.rm=TRUE)
svytotal(~enroll, mrbclus2, na.rm=TRUE)


svymean(~api00, dclus2)
svymean(~api00, mrbclus2)


svyratio(~api.stu, ~enroll, dclus2, na.rm=TRUE)
svyratio(~api.stu, ~enroll, mrbclus2, na.rm=TRUE)

################################
#### FIN DE LA PROGRAMACIÓN ####
################################