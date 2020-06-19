rt <- function(...) read.table(..., header = TRUE, sep = ',', stringsAsFactors = FALSE)
gma <- rt("globi_yanekura_1980.csv")
plot(gma$WDORS, gma$TL, col = as.numeric(as.factor(gma$sex)), las = 1, xlab = 'base of dorsal fin', ylab = 'total length')
legend('topleft', legend = unique(as.factor(gma$sex)), col = as.numeric(unique(as.factor(gma$sex))), pch = 1)

which(gma$WDORS > 60 & gma$TL < 300)