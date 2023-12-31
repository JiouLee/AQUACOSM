require(mgcv)
#GAM function
#takes (x, y, color, linetype)
fgam<-function(x, y, Col, Lty){
  #Generalized Additive Model
  mod<-gam(y~s(x, k=2)) #s() defines smooths in GAM
  ndat<-data.frame(x=unique(x))
  pred<-predict(mod, se.fit=T, newdata=ndat)
  ndat$fit<-pred$fit
  dat$lo<-pred$fit-pred$se.fit
  dat$hi<-pred$fit+pred$se.fit
  lines(fit~x, data=ndat, col=Col, lty=Lty, lwd=2)
}

dat<-read.delim2(file="salinvade.txt")

dat$TP<-factor(dat$TP)
head(dat)

#Creating base jitter plot
rb<-fields::tim.colors(4) #color table
plot(Ft~jitter(Day, am=0.1), data=dat, pch=c(19, 1)[dat$TP], col=rb[factor(dat$Salinity)])

legend("topleft", leg=c(c("low TP", "high TP"), paste(levels(factor(dat$Salinity)), "ppt")), pch=c(1, rep(19, 5)), col=c(1, 1, rb))


#Creation evolution curve 
for(j in 1:2){
  if(j==1) dat1<-dat[dat$TP=="high", ] else dat1<-dat[dat$TP=="low",]
ix<-split(1:nrow(dat1), dat1$Salinity) #divide df according to salinity
for(i in 1:4) fgam(dat1$Day[ix[[i]]], dat1$Ft[ix[[i]]], rb[i], j)
}
#This is Tolga's branch. My property. Nı ha ha ha ha ha !
#My CODE. MINE!!!!

