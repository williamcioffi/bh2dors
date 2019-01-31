# check correlation between TL and different params

pred <- vector()
rsqr <- vector()

for(i in 1:ncol(bba)) {
	pred[i] <- colnames(bba)[i]
	rsqr[i] <- summary(lm(bba$TL ~ bba[, i]))$adj.r.squared
}

preddf <- data.frame(pred, rsqr)
preddf[order(preddf$rsqr, decreasing = TRUE), ]

