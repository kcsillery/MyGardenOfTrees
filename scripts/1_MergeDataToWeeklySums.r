library(lubridate)
library(plyr)

## import timeline of actions data
## #####################################
dat <- read.csv("recruitement_actions.csv", head=T, sep=",")
dat$date <- ISOdate(dat$Year, dat$Month, dat$Day)
dat$year <- year(dat$date)
dat$month <- month(dat$date)

dat$action.type <- unlist(lapply(strsplit(dat$Recruit_code, "_"), function(a) a[[1]]))
dat$action.sub.type <- unlist(lapply(strsplit(dat$Recruit_code, "_"), function(a) a[[2]]))

## number of people reached with actions
aggregate(dat$People_reached, list(dat$action.type), sum, na.rm=T)

## remove actions that targeted scientists
dat <- subset(dat, Target_public!="scientists")

## ## remove website and FB actions and replace by website statistics
dat$People_reached[grep("book", dat$Item_title)] <- NA  ## Facebook
dat$People_reached[dat$Item_type=="website"] <- NA
dat$People_reached[dat$Item_type=="video"] <- NA
aggregate(dat$People_reached, list(dat$action.type), sum, na.rm=T)

## expand timeline of actions data to all countries at all dates
## ###############################################################
partic <- read.csv("partic_anonym.csv")
partic$date.end <- as.Date(partic$date.end)
partic$date.start <- as.Date(partic$date.start)

country.list <- sort(c(unique(c(partic$country, dat$Target_country)), "Finland", "Sweden", "Greece", "Europe"))
start.date <- as.Date(min(c(partic$date.end, dat$date)))
end.date <- max(as.Date(partic$date.end))
all.dates <- seq(start.date, end.date, by = '1 day')

dat.base <- expand.grid(all.dates, country.list)
names(dat.base) <- c("date", "country")

names(dat)[2] <- "country"

all <- merge(dat.base, dat, by=c("date", "country"), all=T, sort=T)
all$country <- as.character(all$country)
all <- all[order(all$date), ]

all$year <- year(all$date)
all$week <- week(all$date)
all$week[all$year==2023] <- all$week[all$year==2023]+52
all$week[all$year==2024] <- all$week[all$year==2024]+104

## summarize data by week for all countries together
## ##################################################
datsum.all <- ddply(all, .(week), summarize,
                    ## social media
                    num.SM=sum(action.type=="SM", na.rm=T),
                    pr.SM=sum(People_reached[action.type=="SM"], na.rm=T),
                    ## outreach (mainly magazine article)
                    num.OR=sum(action.type=="OR", na.rm=T),
                    pr.OR=sum(People_reached[action.type=="OR"], na.rm=T),
                    ## direct contact
                    num.DC=sum(action.type=="DC", na.rm=T),
                    pr.DC=sum(People_reached[action.type=="DC"], na.rm=T),
                    ## place to go to
                    num.PL=sum(na.omit(action.type=="PL")),
                    pr.PL=sum(People_reached[action.type=="PL"], na.rm=T))

## check the data
par(mfrow=c(2,1), mar=c(4,4,1,1))
plot(smooth.spline(datsum.all$week, datsum.all$num.DC), col=2, type="l", ylab="number of actions", xlab="week")
lines(smooth.spline(datsum.all$week, datsum.all$num.PL), col=3)
lines(smooth.spline(datsum.all$week, datsum.all$num.OR), col=4)
lines(smooth.spline(datsum.all$week, datsum.all$num.SM), col=5)

plot(smooth.spline(datsum.all$week, datsum.all$pr.DC), col=2, type="l", ylim=c(0, 60), ylab="people reached", xlab="week")
lines(smooth.spline(datsum.all$week, datsum.all$pr.PL), col=3)
lines(smooth.spline(datsum.all$week, datsum.all$pr.OR), col=4)
lines(smooth.spline(datsum.all$week, datsum.all$pr.SM), col=5)
legend("topright", legend=c("Direct contact", "Places", "Outreach media", "Social media"), col=2:5, lty=1)

## import and merge social media statistics
## #########################################
sm <- read.csv("media_statistics.csv", head=T)

## add FB, Web, Video statistics to other SM and OR People_reached numbers 
sm$year <- year(sm$date)
sm$week <- week(sm$date)
sm$week[sm$year==2023] <- sm$week[sm$year==2023]+52
sm$week[sm$year==2024] <- sm$week[sm$year==2024]+104
smsum <- ddply(sm, .(week), summarize,
            website.clicks=sum(website.clicks),
            video.clicks=sum(video.clicks),
            fb.followers=sum(fb.followers),
            fb.visites=sum(fb.visites))

## sanity ckecks
plot(smsum$week, smsum$website.clicks)
plot(smsum$week, smsum$fb.visites)
plot(smsum$week, smsum$video.clicks)

## spread out outlier values:
## website click >100 = it's us developing the website
outl.web <- order(smsum$website.clicks>100, decreasing=T)[1:sum(smsum$website.clicks>100, na.rm=T)]
smsum$website.clicks[outl.web] <- round((smsum$website.clicks[outl.web-2]+smsum$website.clicks[outl.web+2])/2)
## fb visits more than 150 = it's us checking
outl.fb <- order(smsum$fb.visites>150, decreasing=T)[1:sum(smsum$fb.visites>150, na.rm=T)]
smsum$fb.visites[outl.fb] <- round((smsum$fb.visites[outl.fb-1]+smsum$fb.visites[outl.fb+1])/2)
## video clicks more than 80 = it's us checking
outl.video <- order(smsum$video.clicks>80, decreasing=T)[1:sum(smsum$video.clicks>80, na.rm=T)]
smsum$video.clicks[outl.video] <- round((smsum$video.clicks[outl.video-1]+smsum$video.clicks[outl.video+1])/2)

## merge with summary table including all actions
tmp <- merge(datsum.all, smsum, by=c("week"), all=T)
tmp$pr.SM <- apply(tmp[, c("pr.SM", "fb.followers", "fb.visites")], 1, sum, na.rm=T) 
tmp$pr.OR <- apply(tmp[, c("pr.OR", "website.clicks", "video.clicks")], 1, sum, na.rm=T)
datsum.all <- tmp[, c("week", "num.SM", "pr.SM", "num.OR", "pr.OR", "num.DC", "pr.DC", "num.PL", "pr.PL")]

## check the data
par(mfrow=c(2,1), mar=c(4,4,1,1))
plot(smooth.spline(datsum.all$week, datsum.all$num.DC), col=2, type="l", ylab="number of actions", xlab="week")
lines(smooth.spline(datsum.all$week, datsum.all$num.PL), col=3)
lines(smooth.spline(datsum.all$week, datsum.all$num.OR), col=4)
lines(smooth.spline(datsum.all$week, datsum.all$num.SM), col=5)

plot(smooth.spline(datsum.all$week, datsum.all$pr.DC), col=2, type="l", ylim=c(0, 120), ylab="how many people reached", xlab="week")
lines(smooth.spline(datsum.all$week, datsum.all$pr.PL), col=3)
lines(smooth.spline(datsum.all$week, datsum.all$pr.OR), col=4)
lines(smooth.spline(datsum.all$week, datsum.all$pr.SM), col=5)
legend("topleft", legend=c("Direct contact", "Places", "Outreach media", "Social media"), col=2:5, lty=1)

## import and merge social media statistics BY COUNTRY
## ######################################################
datsum.country <- ddply(all, .(week, country), summarize,
                    ## social media
                    num.SM=sum(action.type=="SM", na.rm=T),
                    pr.SM=sum(People_reached[action.type=="SM"], na.rm=T),
                    ## outreach (mainly magazine article)
                    num.OR=sum(action.type=="OR", na.rm=T),
                    pr.OR=sum(People_reached[action.type=="OR"], na.rm=T),
                    ## direct contact
                    num.DC=sum(action.type=="DC", na.rm=T),
                    pr.DC=sum(People_reached[action.type=="DC"], na.rm=T),
                    ## place to go to
                    num.PL=sum(na.omit(action.type=="PL")),
                    pr.PL=sum(People_reached[action.type=="PL"], na.rm=T))

sm.w <- read.csv("media_weights.csv", head=T)
sm.w$country[sm.w$country=="Czechia"] <- "Czech Republic"
sm.w$country[sm.w$country=="Luxembourg"] <- "Luxemburg"
sm.w[,-1] <- apply(sm.w[,-1], 2, function(a) a/(sum(a)))
## create one variable for FB because there is only 1 weight
smsum$fb.clicks <- smsum$fb.visites + smsum$fb.followers
## create base with all combinations of countries and weeks
sm.base <- expand.grid(sm.w$country, smsum$week)
names(sm.base) <- c("country", "week")
sm.base <- merge(sm.base, smsum, by=c("week"), all=T)
sm.base <- merge(sm.base, sm.w, by=c("country"), all=T)
## correct clicks by weight of the country
sm.base$website.clicks <- sm.base$website.clicks * sm.base$website.weights
sm.base$fb.clicks <- sm.base$fb.clicks * sm.base$fb.weights
## select out only the two corrected variables of interest
sm.base <- sm.base[, c("week", "country", "fb.clicks", "website.clicks")]
## combine with all the timeline of actions data
datsum.country <- merge(datsum.country, sm.base, by=c("week", "country"), all=T)
datsum.country$pr.SM <- apply(cbind(datsum.country$pr.SM, datsum.country$fb.clicks), 1, sum, rm.list=T)
datsum.country$pr.OR <- apply(cbind(datsum.country$pr.OR, datsum.country$website.clicks), 1, sum, rm.list=T)
datsum.country <- datsum.country[, 1:10]
## replace NAs with 0s because NA means 0 action and 0 persons reached
datsum.country[is.na(datsum.country)]  <- 0

## check data
par(mfrow=c(2,1), mar=c(4,4,1,1))
plot(smooth.spline(datsum.country$week, datsum.country$num.DC), col=2, type="l", ylab="number of actions", xlab="week")
lines(smooth.spline(datsum.country$week, datsum.country$num.PL), col=3)
lines(smooth.spline(datsum.country$week, datsum.country$num.OR), col=4)
lines(smooth.spline(datsum.country$week, datsum.country$num.SM), col=5)

plot(smooth.spline(datsum.country$week, datsum.country$pr.DC), col=2, type="l", ylim=c(0, 5), ylab="people reached", xlab="week")
lines(smooth.spline(datsum.country$week, datsum.country$pr.PL), col=3)
lines(smooth.spline(datsum.country$week, datsum.country$pr.OR), col=4)
lines(smooth.spline(datsum.country$week, datsum.country$pr.SM), col=5)
legend("topleft", legend=c("Direct contact", "Places", "Outreach media", "Social media"), col=2:5, lty=1)

## import and merge registration form data
## #########################################
partic$date <- partic$date.end
partic$month <- month(partic$date)
partic$year <- year(partic$date)
partic$week <- week(partic$date)
partic$week[partic$year==2023] <- partic$week[partic$year==2023]+52
partic$week[partic$year==2024] <- partic$week[partic$year==2024]+104
## remove species
partic <- partic[, -grep("sp.", names(partic))]
## remove when no country info is given
partic <- subset(partic, !is.na(partic$country))


## calculate
## number of completed registrations per country
## number of any started registrations per country
partsum.country <- ddply(partic, .(week, country), summarize,
                         numreg.ok=sum(complete==1),
                         numreg.all=length(complete))
partsum.all <- ddply(partic, .(week), summarize,
                     numreg.ok=sum(complete==1),
                     numreg.all=length(complete))

## combine timeline of action and registration data
## #################################################
dat <- merge(datsum.all, partsum.all, by=c("week"), all=T)
dat$numreg.ok[is.na(dat$numreg.ok)] <- 0
dat$numreg.all[is.na(dat$numreg.all)] <- 0
# adding a date column
dat$date <- dat$week
date1 <- as.Date(paste(2022, dat$week[1:47], 1, sep="-"), "%Y-%U-%u")
date2 <- as.Date(paste(2023, dat$week[48:(47+52)]-52, 1, sep="-"), "%Y-%U-%u")
date3 <- as.Date(paste(2024, dat$week[100:121]-104, 1, sep="-"), "%Y-%U-%u")
dat$date <- c(date1, date2, date3)

dat.country <- merge(datsum.country, partsum.country, by=c("week", "country"), all=T)
dat.country$numreg.ok[is.na(dat.country$numreg.ok)] <- 0
dat.country$numreg.all[is.na(dat.country$numreg.all)] <- 0

country.region <- read.csv("country_region.csv", h=T, sep=";")
dat.region.merge <- merge(dat.country, country.region, by="country", all.x=T)
dat.region <- ddply(dat.region.merge, .(week, region), summarise, num.SM=sum(num.SM), pr.SM=sum(pr.SM),
                                                   num.OR=sum(num.OR), pr.OR=sum(pr.OR),
                                                   num.DC=sum(num.DC), pr.DC=sum(pr.DC),
                                                   num.PL=sum(num.PL), pr.PL=sum(pr.PL),
                                                   numreg.ok=sum(numreg.ok), numreg.all=sum(numreg.all))
dat.region$region[is.na(dat.region$region)] = "Europe"

## sanity checks
## #################
par(mfcol=c(2,1))

plot(smooth.spline(dat$numreg.ok~dat$week), lwd=1.5, type="l", xlab="week", ylab="Num registrations / actions")
lines(smooth.spline(dat$week, dat$num.DC), col=2, lwd=1.5)
lines(smooth.spline(dat$week, dat$num.PL), col=3, lwd=1.5)
lines(smooth.spline(dat$week, dat$num.OR), col=4, lwd=1.5)
lines(smooth.spline(dat$week, dat$num.SM), col=5, lwd=1.5)
legend("topleft", legend=c("Registrations", "Direct contact", "Places", "Outreach media", "Social media"), col=1:5, lty=1)

plot(dat$numreg.ok~dat$week, main="same as above with raw data points", xlab="week", ylab="Num registrations / actions")
lines(smooth.spline(dat$numreg.ok~dat$week), lwd=1.5)
lines(smooth.spline(dat$week, dat$num.DC), col=2, lwd=1.5)
points(dat$week, dat$num.DC, col=2, pch=20)
lines(smooth.spline(dat$week, dat$num.PL), col=3, lwd=1.5)
points(dat$week, dat$num.PL, col=3, pch=20)
lines(smooth.spline(dat$week, dat$num.OR), col=4, lwd=1.5)
points(dat$week, dat$num.OR, col=4, pch=20)
lines(smooth.spline(dat$week, dat$num.SM), col=5, lwd=1.5)
points(dat$week, dat$num.SM, col=5, pch=20)


## save data
save(dat, file="dat.RData")
save(dat.country, file="dat.country.RData")
save(dat.region, file="dat.region.RData")







