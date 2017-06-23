#Goal: Different Plots Per Country

#Sector Stability (Affects Volatility): 
re <- 1.5 #resources
se <- .7 #services
fd <- 1.2 #foodstuffs
#Sector Health (Affects Drift): 
reh <- 1.2 #resources
seh <- 1.5 #services
fdh <- 1 #foodstuffs
#Nations Stability (Affects Volatility):
gls <- runif(1,.5,1.5) #Global Market Stability
ni <- .9*gls #Nikea
i <- 1.2*gls #Irania
po <- 1*gls #Posedina
v <- 1.35*gls #Valkia
#Nations Growth (Affects Drift)
ng <- 1
irg <- 1.2
pog <- .7
vg <- 1.8
#Exogenous Details:
t <- 1
n <- 1000
mu <- .1 #basedrift
bs <-.5 #basevolatility

#NU10 (Nikean Union 10)
#Nikean Metals (NM) BLUE
#Scantatian Lumber (SNL) RED
#Nikean Air and Shipping (NAS) GOLD
#Nikean Oats (UHN) BLACK

NMG0 <- 80 #starting price
NMmu <- bmu*ng*reh #drift parameter
NMsig <- bs*ni*re #volatility
NMrv <- rnorm(n,0,sqrt(t/n))
NMbm <- c(0,cumsum(NMrv))
NMxproc <- mu*seq(0,t,t/n) + NMsig*NMbm
NMgbm <- (NMG0)*exp(NMxproc)
steps <- seq(0,t,length=n+1)
NMbm1 <- sum(NMrv)
NMxproc1 <- NMmu*(t) + NMsig*NMbm1
NMgbm1 <- (NMG0)*exp(NMxproc1) 
NMgbm1

SNLG0 <- 80 #starting price
SNLmu <- bmu*ng*reh #drift parameter
SNLsig <- bs*ni*re #volatility
SNLrv <- rnorm(n,0,sqrt(t/n))
SNLbm <- c(0,cumsum(SNLrv))
SNLxproc <- mu*seq(0,t,t/n) + SNLsig*SNLbm
SNLgbm <- (SNLG0)*exp(SNLxproc)
steps <- seq(0,t,length=n+1)
SNLbm1 <- sum(SNLrv)
SNLxproc1 <- SNLmu*(t) + SNLsig*SNLbm1
SNLgbm1 <- (SNLG0)*exp(SNLxproc1) 
SNLgbm1

NASG0 <- 80 #starting price
NASmu <- bmu*ng*seh #drift parameter
NASsig <- bs*ni*se #volatility
NASrv <- rnorm(n,0,sqrt(t/n))
NASbm <- c(0,cumsum(NASrv))
NASxproc <- mu*seq(0,t,t/n) + NASsig*NASbm
NASgbm <- (NASG0)*exp(NASxproc)
steps <- seq(0,t,length=n+1)
NASbm1 <- sum(NASrv)
NASxproc1 <- NASmu*(t) + NASsig*NASbm1
NASgbm1 <- (NASG0)*exp(NASxproc1) 
NASgbm1

UHNG0 <- 80 #starting price
UHNmu <- bmu*ng*fdh #drift parameter
UHNsig <- bs*ni*fd #volatility
UHNrv <- rnorm(n,0,sqrt(t/n))
UHNbm <- c(0,cumsum(UHNrv))
UHNxproc <- mu*seq(0,t,t/n) + UHNsig*UHNbm
UHNgbm <- (UHNG0)*exp(UHNxproc)
steps <- seq(0,t,length=n+1)
UHNbm1 <- sum(UHNrv)
UHNxproc1 <- UHNmu*(t) + UHNsig*UHNbm1
UHNgbm1 <- (UHNG0)*exp(UHNxproc1) 
UHNgbm1

mean(NMgbm1,SNLgbm1,NASgbm1,UHNgbm1) #average stock price


#IE (The Iranian Exchange)
#Iranian Gold (IG) BLUE
#GlassCo (GC) RED
#Iranian Construction Industries (IC) GOLD
#United Honeys (UHI) BLACK

IGG0 <- 80 #starting price
IGmu <- bmu*irg*reh #drift parameter
IGsig <- bs*i*re #volatility
IGrv <- rnorm(n,0,sqrt(t/n))
IGbm <- c(0,cumsum(IGrv))
IGxproc <- mu*seq(0,t,t/n) + IGsig*IGbm
IGgbm <- (IGG0)*exp(IGxproc)
steps <- seq(0,t,length=n+1)
IGbm1 <- sum(IGrv)
IGxproc1 <- IGmu*(t) + IGsig*IGbm1
IGgbm1 <- (IGG0)*exp(IGxproc1) 
IGgbm1

GCG0 <- 80 #starting price
GCmu <- bmu*irg*reh #drift parameter
GCsig <- bs*i*re #volatility
GCrv <- rnorm(n,0,sqrt(t/n))
GCbm <- c(0,cumsum(GCrv))
GCxproc <- mu*seq(0,t,t/n) + GCsig*GCbm
GCgbm <- (GCG0)*exp(GCxproc)
steps <- seq(0,t,length=n+1)
GCbm1 <- sum(GCrv)
GCxproc1 <- GCmu*(t) + GCsig*GCbm1
GCgbm1 <- (GCG0)*exp(GCxproc1) 
GCgbm1

ICG0 <- 80 #starting price
ICmu <- bmu*irg*seh #drift parameter
ICsig <- bs*i*se #volatility
ICrv <- rnorm(n,0,sqrt(t/n))
ICbm <- c(0,cumsum(ICrv))
ICxproc <- mu*seq(0,t,t/n) + ICsig*ICbm
ICgbm <- (ICG0)*exp(ICxproc)
steps <- seq(0,t,length=n+1)
ICbm1 <- sum(ICrv)
ICxproc1 <- ICmu*(t) + ICsig*ICbm1
ICgbm1 <- (ICG0)*exp(ICxproc1) 
ICgbm1

UHIG0 <- 80 #starting price
UHImu <- bmu*irg*fdh #drift parameter
UHIsig <- bs*i*fd #volatility
UHIrv <- rnorm(n,0,sqrt(t/n))
UHIbm <- c(0,cumsum(UHIrv))
UHIxproc <- mu*seq(0,t,t/n) + UHIsig*UHIbm
UHIgbm <- (UHIG0)*exp(UHIxproc)
steps <- seq(0,t,length=n+1)
UHIbm1 <- sum(UHIrv)
UHIxproc1 <- UHImu*(t) + UHIsig*UHIbm1
UHIgbm1 <- (UHIG0)*exp(UHIxproc1) 
UHIgbm1



#PSE (Posedinan Stock Exchange)
#Fishing International (FI) BLUE
#Sea and Shipping Services (SSS) RED
#Posedinan Medical Solutions (PM) GOLD

FIG0 <- 80 #starting price
FImu <- bmu*pog*reh #drift parameter
FIsig <- bs*po*re #volatility
FIrv <- rnorm(n,0,sqrt(t/n))
FIbm <- c(0,cumsum(FIrv))
FIxproc <- mu*seq(0,t,t/n) + FIsig*FIbm
FIgbm <- (FIG0)*exp(FIxproc)
steps <- seq(0,t,length=n+1)
FIbm1 <- sum(FIrv)
FIxproc1 <- FImu*(t) + FIsig*FIbm1
FIgbm1 <- (FIG0)*exp(FIxproc1) 
FIgbm1

SSSG0 <- 80 #starting price
SSSmu <- bmu*pog*fdh #drift parameter
SSSsig <- bs*po*fd #volatility
SSSrv <- rnorm(n,0,sqrt(t/n))
SSSbm <- c(0,cumsum(SSSrv))
SSSxproc <- mu*seq(0,t,t/n) + SSSsig*SSSbm
SSSgbm <- (SSSG0)*exp(SSSxproc)
steps <- seq(0,t,length=n+1)
SSSbm1 <- sum(SSSrv)
SSSxproc1 <- SSSmu*(t) + SSSsig*SSSbm1
SSSgbm1 <- (SSSG0)*exp(SSSxproc1) 
SSSgbm1

PMG0 <- 80 #starting price
PMmu <- bmu*pog*seh #drift parameter
PMsig <- bs*po*se #volatility
PMrv <- rnorm(n,0,sqrt(t/n))
PMbm <- c(0,cumsum(PMrv))
PMxproc <- mu*seq(0,t,t/n) + PMsig*PMbm
PMgbm <- (PMG0)*exp(PMxproc)
steps <- seq(0,t,length=n+1)
PMbm1 <- sum(PMrv)
PMxproc1 <- PMmu*(t) + PMsig*PMbm1
PMgbm1 <- (PMG0)*exp(PMxproc1) 
PMgbm1

#VPM (Valkian Public Metal Trade)
#Valkian Iron and Ores (VIO) BLUE
#Vulcan Jewels and Gemstones (VJG) RED
#Valkian Defense Contracting (VDC) GOLD

VIOG0 <- 80 #starting price
VIOmu <- bmu*vg*reh #drift parameter
VIOsig <- bs*v*re #volatility
VIOrv <- rnorm(n,0,sqrt(t/n))
VIObm <- c(0,cumsum(VIOrv))
VIOxproc <- mu*seq(0,t,t/n) + VIOsig*VIObm
VIOgbm <- (VIOG0)*exp(VIOxproc)
steps <- seq(0,t,length=n+1)
VIObm1 <- sum(VIOrv)
VIOxproc1 <- VIOmu*(t) + VIOsig*VIObm1
VIOgbm1 <- (VIOG0)*exp(VIOxproc1) 
VIOgbm1

VJGG0 <- 80 #starting price
VJGmu <- bmu*vg*reh #drift parameter
VJGsig <- bs*v*re #volatility
VJGrv <- rnorm(n,0,sqrt(t/n))
VJGbm <- c(0,cumsum(VJGrv))
VJGxproc <- mu*seq(0,t,t/n) + VJGsig*VJGbm
VJGgbm <- (VJGG0)*exp(VJGxproc)
steps <- seq(0,t,length=n+1)
VJGbm1 <- sum(VJGrv)
VJGxproc1 <- VJGmu*(t) + VJGsig*VJGbm1
VJGgbm1 <- (VJGG0)*exp(VJGxproc1) 
VJGgbm1

VDCG0 <- 80 #starting price
VDCmu <- bmu*pog*seh #drift parameter
VDCsig <- bs*po*se #volatility
VDCrv <- rnorm(n,0,sqrt(t/n))
VDCbm <- c(0,cumsum(VDCrv))
VDCxproc <- mu*seq(0,t,t/n) + VDCsig*VDCbm
VDCgbm <- (VDCG0)*exp(VDCxproc)
steps <- seq(0,t,length=n+1)
VDCbm1 <- sum(VDCrv)
VDCxproc1 <- VDCmu*(t) + VDCsig*VDCbm1
VDCgbm1 <- (VDCG0)*exp(VDCxproc1) 
VDCgbm1

#NU10 Plot
par(mfrow=c(2,2))
plot(steps,NMgbm,type="l", ylim=range(c(0,max(NMgbm,SNLgbm,NASgbm,UHNgbm))), col="blue", ylab="Price", xlab="Time", main="Nikean Union Stock Prices")
par(new=TRUE)
plot(steps,SNLgbm,type="l",ylim=range(c(0,max(NMgbm,SNLgbm,NASgbm,UHNgbm))),col="red",ann=FALSE, axes=FALSE)
par(new=TRUE)
plot(steps,NASgbm,type="l",ylim=range(c(0,max(NMgbm,SNLgbm,NASgbm,UHNgbm))),col="gold",ann=FALSE, axes=FALSE)
par(new=TRUE)
plot(steps,UHNgbm,type="l",ylim=range(c(0,max(NMgbm,SNLgbm,NASgbm,UHNgbm))),col="black",ann=FALSE, axes=FALSE)
legend('topleft', legend=c("Nikean Metals","Scantatian Lumber","Nikean Air and Shipping","Nikean Oats") , 
       lty=1, col=c('blue', 'red', 'gold',' black'), bty='n', cex=.75)
abline(h =NMgbm1,lty=2)
abline(h =SNLgbm1,lty=2)
abline(h =NASgbm1,lty=2)
abline(h =UHNgbm1,lty=2)

abline(h=mean(c(NMgbm1,SNLgbm1,NASgbm1,UHNgbm1)))

#IE Plot

plot(steps,IGgbm,type="l", ylim=range(c(0,max(IGgbm,GCgbm,ICgbm,UHIgbm))), col="blue", ylab="Price", xlab="Time", main="Iranian Exchange")
par(new=TRUE)
plot(steps,GCgbm,type="l",ylim=range(c(0,max(IGgbm,GCgbm,ICgbm,UHIgbm))),col="red",ann=FALSE, axes=FALSE)
par(new=TRUE)
plot(steps,ICgbm,type="l",ylim=range(c(0,max(IGgbm,GCgbm,ICgbm,UHIgbm))),col="gold",ann=FALSE, axes=FALSE)
par(new=TRUE)
plot(steps,UHIgbm,type="l",ylim=range(c(0,max(IGgbm,GCgbm,ICgbm,UHIgbm))),col="black",ann=FALSE, axes=FALSE)
legend('topleft', legend=c("Iranian Gold","GlassCo","Iranian Construction","United Honey") , 
       lty=1, col=c('blue', 'red', 'gold',' black'), bty='n', cex=.75)
abline(h =IGgbm1,lty=2)
abline(h =GCgbm1,lty=2)
abline(h =ICgbm1,lty=2)
abline(h =UHIgbm1,lty=2)

abline(h=mean(c(IGgbm1,GCgbm1,ICgbm1,UHIgbm1)))

#PSE Plot

plot(steps,FIgbm,type="l", ylim=range(c(0,max(FIgbm,SSSgbm,PMgbm))), col="blue", ylab="Price", xlab="Time", main="Posedinan Stock Exchange")
par(new=TRUE)
plot(steps,SSSgbm,type="l",ylim=range(c(0,max(FIgbm,SSSgbm,PMgbm))),col="red",ann=FALSE, axes=FALSE)
par(new=TRUE)
plot(steps,PMgbm,type="l",ylim=range(c(0,max(FIgbm,SSSgbm,PMgbm))),col="gold",ann=FALSE, axes=FALSE)
legend('topleft', legend=c("Fishing International","Sea and Shipping Services","Posedinan Medical Solutions") , 
       lty=1, col=c('blue', 'red', 'gold',' black'), bty='n', cex=.75)
abline(h =FIgbm1,lty=2)
abline(h =SSSgbm1,lty=2)
abline(h =PMgbm1,lty=2)

abline(h=mean(c(FIgbm,SSSgbm,PMgbm)))

#VPM Plot

plot(steps,VIOgbm,type="l", ylim=range(c(0,max(VIOgbm,VJGgbm,VDCgbm))), col="blue", ylab="Price", xlab="Time", main="Valkian Public Metal Trade")
par(new=TRUE)
plot(steps,VJGgbm,type="l",ylim=range(c(0,max(VIOgbm,VJGgbm,VDCgbm))),col="red",ann=FALSE, axes=FALSE)
par(new=TRUE)
plot(steps,VDCgbm,type="l",ylim=range(c(0,max(VIOgbm,VJGgbm,VDCgbm))),col="gold",ann=FALSE, axes=FALSE)
legend('topleft', legend=c("Valkian Iron and Ores","Vulcan Jewels and Gems","Valkian Defense Contracting") , 
       lty=1, col=c('blue', 'red', 'gold',' black'), bty='n', cex=.75)
abline(h =VIOgbm1,lty=2)
abline(h =VJGgbm1,lty=2)
abline(h =VDCgbm1,lty=2)

abline(h=mean(c(VIOgbm,VJGgbm,VDCgbm)))