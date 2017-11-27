# quick plot of TL and BHDORS

ttu <- read.table("ttgraph.csv", header = TRUE, sep = ',')
gma <- read.table("gma_scirepwhalesresinstno32_1980.csv", header = TRUE, sep = ',')
cma <- read.table("ross_et_al_1975_caperea.csv", header = TRUE, sep = ',')
bph <- read.table("testbp.csv", header = TRUE, sep = ',')
bmu <- read.table("testbm.csv", header = TRUE, sep = ',')
bba <- read.table("bba_SC01089-132.csv", header = TRUE, sep = ',')

#ttu in cm
#gma in cm
#cma TL in cm everything else in percentages

TL 		<- vector()
BH2DORS <- vector()
sp 		<- vector()
sex 	<- vector()

#ttu
TL 		<- c(TL, ttu$TL)
BH2DORS <- c(BH2DORS, ttu$BH2DORS)
sp 		<- c(sp, rep("ttu", nrow(ttu)))
sex 	<- c(sex, rep(NA, nrow(ttu)))

#gma
TL 		<- c(TL, gma$TL)
gma$NOTCH2DORS <- sqrt(gma$NOTCH2APEXDORS^2 - gma$HDORS^2)
gma$BH2DORS <- gma$TL - gma$ROSTBH - gma$NOTCH2DORS
BH2DORS <- c(BH2DORS, gma$BH2DORS)
sp 		<- c(sp, rep("gma", nrow(gma)))
sex 	<- c(sex, rep(gma$sex, nrow(gma)))

#cma
cma[, 4:ncol(cma)] <- cma[, 4:ncol(cma)]*cma$TL / 100
TL 		<- c(TL, cma$TL)
cma$BH2DORS <- cma$TL - cma$ROST2BH - cma$NOTCH2DORS 
BH2DORS <- c(BH2DORS, cma$BH2DORS)
sp 		<- c(sp, rep("cma", nrow(cma)))
sex 	<- c(sex, rep(cma$sex, nrow(cma)))

#bph
TL 		<- c(TL, bph$TL*100)
bph$BH2DORS <- bph$TL - bph$ROSTBH - bph$NOTCHTOFIN 
BH2DORS <- c(BH2DORS, bph$BH2DORS*100)
sp 		<- c(sp, rep("bph", nrow(bph)))
sex 	<- c(sex, rep(bph$sex, nrow(bph)))


#bmu
TL 		<- c(TL, bmu$TL*100)
bmu$BH2DORS <- bmu$TL - bmu$ROSTBH - bmu$NOTCHTOFIN 
BH2DORS <- c(BH2DORS, bmu$BH2DORS*100)
sp 		<- c(sp, rep("bmu", nrow(bmu)))
sex 	<- c(sex, rep(bmu$sex, nrow(bmu)))

#bba
# remove fetus / very small
bba.backup <- bba
bba <- bba[-which(bba$TL < 400), ]

TL <- c(TL, bba$TL)
bba$BH2DORS <- bba$TL - bba$ROSTBH - bba$NOTCH2DORS
BH2DORS <- c(BH2DORS, bba$BH2DORS)
sp 		<- c(sp, rep("bba", nrow(bba)))
sex 	<- c(sex, rep(bba$sex, nrow(bba)))

plot(BH2DORS, TL, 
	pch = as.numeric(as.factor(sp)),
 	col = as.numeric(as.factor(sp)),
 	log = 'xy',
 	las = 1,
 	xlab = "blowhole to dorsal fin length (cm)",
 	ylab = "total length (cm)"
 )


lTL <- split(TL, sp)
lBH2DORS <- split(BH2DORS, sp)
cols <- sort(unique(as.numeric(as.factor(sp))))

for(i in 1:length(lTL)) {
	y <- log10(lTL[[i]])
	x <- log10(lBH2DORS[[i]])
	x.min <- min(x, na.rm = TRUE)
	x.max <- max(x, na.rm = TRUE)
	
	mod <- lm(y ~ x)
	int <- coef(mod)[1]
	bet <- coef(mod)[2]
	
	seq.x <- seq(x.min, x.max, length = 100)
	seq.y <- bet * seq.x + int
	
	lines(10^seq.x, 10^seq.y, lty = 2, col = cols[i])
}

legend("topleft", legend = sort(unique(sp)), col = cols, pch = sort(unique(as.numeric(as.factor(sp)))), bty = 'n')