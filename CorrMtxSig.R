#Example from:
## http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software

# install.packages("Hmisc")
library(Hmisc)
rcorr(as.matrix(mtcars[,1:7]))

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

res<-rcorr(as.matrix(mtcars[,1:7]))
flattenCorrMatrix(res$r, res$P)

# install.packages("corrplot")
library(corrplot)
corrplot(mcor, type="upper", order="hclust", tl.col="black", tl.srt=45)

# install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
mydata <- mtcars[, c(1,3,4,5,6,7)]
chart.Correlation(mydata, histogram=TRUE, pch=19)