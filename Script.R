dat<-read.delim2(file="salinvade.txt")
par(mfrow=c(1, 2))
# make productivity a factor, can be used like number for defining plotting symbols
dat$Productivity<-factor(dat$Productivity)

# setFt values <1 to 1 (allow log scale; neg values are nonsenical)
dat$Ft[dat$Ft<1]<-1

# change some names, easier to call the variables later for plotting
names(dat)[c(6, 10, 11)]<-c("Conductivity", "SRP", "TP")

# create a color gradient with 4 colors
rb<-rainbow(10)[c(7, 5, 3, 1)]

# plot Ft over time, using formula interface
plot(Ft~Day, data=dat)

plot(Ft~Day, data=dat, log="y")

# add color code for salinity
unique(dat$Salinity)
plot(Ft~Day, data=dat, log="y", col=rb[dat$Salinity])
# mhhh...

unique(factor(dat$Salinity))
plot(Ft~Day, data=dat, log="y", col=rb[factor(dat$Salinity)])
# ahh!

# ..and productivity
plot(Ft~Day, data=dat, log="y", col=rb[factor(dat$Salinity)], pch=c(19, 1)[Productivity])

# we can also add a little noise to reduce overlay of data
plot(Ft~jitter(Day), data=dat, log="y", col=rb[factor(dat$Salinity)], pch=c(19, 1)[Productivity])
