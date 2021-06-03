library(MASS)
set.seed(1236415)

cormat_alternative <- matrix(nrow = 11, byrow = T,
                          data = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                   .5, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                   .5, .5, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                                   .5, .5, .5, 1, 0, 0, 0, 0, 0, 0, 0,
                                   .5, .5, .5, .5, 1, 0, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, .5, 1, 0, 0, 0, 0,
                                   0, 0, 0, 0, 0, .5, .5, 1, 0, 0, 0,
                                   0, 0, 0, 0, 0, .5, .5, .5, 1, 0, 0,
                                   0, 0, 0, 0, 0, .5, .5, .5, .5, 1, 0,
                                   .3, .3, .3, .3, .3, 0, 0, 0, 0, 0, 1))
round(cor(mvrnorm(n = 100, mu = rep(0,11), Sigma = cormat_alternative, empirical = T)),2)

cormat_null <- cormat_alternative
cormat_null[11,1:10]<-0

runNullSim <- function(muvec = rep(0,11), SigmaMat){
    simMeanH0CorH0Data <- mvrnorm(n = 1e3, mu = muvec, Sigma = SigmaMat)
    braindata <- simMeanH0CorH0Data[, -11]
    behave <- simMeanH0CorH0Data[, 11]
    sigvoxels <- apply(braindata, 2, function(x) t.test(x)$p.value < .05)
    if(any(sigvoxels)) {
        if (sum(sigvoxels) == 1){
            meanROI <- braindata[,sigvoxels]
        } else {
            meanROI <- apply(braindata[,sigvoxels], 1, mean)
        }
        corp <- cor.test(meanROI, behave)$p.value
    } else {
        corp <- NA
    }
    return(corp)
}

nullSimsData <- replicate(5e3, runNullSim(muvec = rep(0, 11), SigmaMat = cormat_null))
sum(na.omit(nullSimsData) < .05 )/length(na.omit(nullSimsData))

hist(na.omit(nullSimsData),20)

sum(is.na(nullSimsData))/length(nullSimsData)

trueMeanSimsData <- replicate(5e3, runNullSim(muvec = c(rep(1, 5), rep(0, 6)), SigmaMat = cormat_null))
sum(na.omit(trueMeanSimsData) < .05 )/length(na.omit(trueMeanSimsData))

hist(na.omit(trueMeanSimsData),20)

sum(is.na(trueMeanSimsData))/length(trueMeanSimsData)

## ALT

altSimsData <- replicate(5e3, runNullSim(muvec = rep(0, 11), SigmaMat = cormat_alternative))
sum(na.omit(altSimsData) < .05 )/length(na.omit(altSimsData))

hist(na.omit(altSimsData),20)

sum(is.na(altSimsData))/length(altSimsData)

altTrueMeanSimsData <- replicate(5e3, runNullSim(muvec = c(rep(1, 5), rep(0, 6)), SigmaMat = cormat_alternative))
sum(na.omit(altTrueMeanSimsData) < .05 )/length(na.omit(altTrueMeanSimsData))

hist(na.omit(altTrueMeanSimsData),20)

sum(is.na(altTrueMeanSimsData))/length(altTrueMeanSimsData)
