library(tidyverse)
library(psych)

data("bfi")

adf <- read_csv('~/code_new/misc-r-projects/dimensions_ra/dimensions.csv')

sum(is.na(adf[,-1]))
acor <- cor(adf[,-1])
adf.n.obs <- dim(adf)[1]

VSS(acor, n = 20, fm = 'mle', n.obs = adf.n.obs, rotate = "oblimin")
VSS.scree(acor)
scree(acor)

apca <- fa(acor, nfactors = 3, fm = 'mle', rotate = "varimax", n.obs = adf.n.obs)
psych::print.psych(apca, sort = T)

names(bfi[,1:25])
sum(is.na(bfi[,1:25]))
cor(bfi[,1:25])
aBFIcor <- cor(bfi[,1:25], use = 'pairwise.complete.obs')
aBFI.n.obs <- dim(bfi[,1:25])[1]

VSS(aBFIcor, n = 20, fm = 'mle', n.obs = aBFI.n.obs, rotate = "varimax")
VSS.scree(aBFIcor)
scree(aBFIcor)

aBFIpca <- pca(aBFIcor, nfactors = 5, rotate = "varimax",  n.obs = aBFI.n.obs, fm = 'mle')
psych::print.psych(aBFIpca, sort = T)

data("bfi.dictionary")
print(bfi.dictionary[,c('Big6', 'Item')])
