# quick plot of TL and BHDORS
rt <- function(...) read.table(..., header = TRUE, sep = ',', stringsAsFactors = FALSE)

ttu <- rt("truncatus_cheney_et_al_2017.csv")
tad <- rt("aduncus_vanaswegen2017.csv")
gma <- rt("globi_yanekura_1980.csv")
cma <- rt("caperea_ross_et_al_1975.csv")
bph <- rt("phyalus_mackintosh_wheeler_discoveryreports_1929.csv")
bmu <- rt("musculus_mackintosh_wheeler_discoveryreports_1929.csv")
bba <- rt("bairdius_omura_et_al_SC01089-132.csv")

#ttu in cm
#gma in cm
#tad in cm
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

#tad
TL <- c(TL, tad$TL)
BH2DORS <- c(BH2DORS, tad$BH2DORS)
sp <- c(sp, rep("tad", nrow(tad)))
sex <- c(sex, tad$sex)

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

# paste them together
mdf <- data.frame(tl = TL, bh2dors = BH2DORS, sp = sp, stringsAsFactors = FALSE)

# make a list
mdfl <- split(mdf, mdf$sp)

# set up plot
plot(mdf$bh2dors, mdf$tl, 
 	log = 'xy',
 	las = 1,
 	type = 'n',
 	xlab = "blowhole to dorsal fin length (cm)",
 	ylab = "total length (cm)"
 )

library(colorspace)
cols <- rainbow_hcl(length(mdfl))
pchs <- 1:length(mdfl)

for(i in 1:length(mdfl)) {
	points(mdfl[[i]]$bh2dors, mdfl[[i]]$tl, col = cols[i], pch = pchs[i])
	
	y <- log10(mdfl[[i]]$tl)
	x <- log10(mdfl[[i]]$bh2dors)
	x.min <- min(x, na.rm = TRUE)
	x.max <- max(x, na.rm = TRUE)
	
	mod <- lm(y ~ x)
	int <- coef(mod)[1]
	bet <- coef(mod)[2]
	
	seq.x <- seq(x.min, x.max, length = 100)
	seq.y <- bet * seq.x + int
	
	lines(10^seq.x, 10^seq.y, lty = 2, col = cols[i])
}

legend("topleft", legend = names(mdfl), col = cols, pch = pchs, bty = 'n')