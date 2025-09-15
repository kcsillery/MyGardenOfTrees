library(RColorBrewer)
library(PerformanceAnalytics)
library(minpack.lm)
source("adstock_funs.r")

load("dat.RData")
names(dat) = tolower(names(dat))

load("dat.region.RData")
dat.region <- na.omit(dat.region)
names(dat.region) <- tolower(names(dat.region))

load("dat.country.RData")
names(dat.country) <- tolower(names(dat.country))

## cut the end of the period - no more recruitement was acepted
filter <- dat$week<=112
dat <- dat[filter, ]
dat.region <- dat.region[filter, ]
dat.country <- dat.country[filter, ]

## Exploratory analysis
par(mfcol=c(1,2))

## number of actions
chart.Correlation(dat[,grep("num.", names(dat))], histogram = TRUE, pch=19)
## number of people reached
chart.Correlation(dat[,grep("pr.", names(dat))], histogram = TRUE, pch=19)

## number of actions
chart.Correlation(dat.region[,grep("num.", names(dat.region))], histogram = TRUE, pch=19)
## number of people reached
chart.Correlation(dat.region[,grep("pr.", names(dat.region))], histogram = TRUE, pch=19)

## number of actions
chart.Correlation(dat.country[,grep("num.", names(dat.country))], histogram = TRUE, pch=19)
## number of people reached
chart.Correlation(dat.country[,grep("pr.", names(dat.country))], histogram = TRUE, pch=19)

## OR and SM are correlated
## PL and DC are correlated

## advertising: A sequence.
## N: Length of the advertising sequence.
## lambda: Adstock decay rate.
## theta: The delay of the peak effect.
## L: The maximum duration of carryover effect.

## estimate all parameters 
## #######################
myN = nrow(dat)
nlc <- nls.control(maxiter = 1000)
mod0 = nlsLM(numreg.ok ~ alpha + beta.or * DelayedSimpleAdstock(pr.or, N=myN, lambda=myla.or, theta=myth, L=myL) +
                  beta.dc * GeometricSimpleAdstock(pr.dc, lambda=myla.dc) +
                  beta.pl * GeometricSimpleAdstock(pr.pl, lambda=myla.pl) +
                  beta.sm * GeometricSimpleAdstock(pr.sm, lambda=myla.sm),
              dat, start = list(alpha = 0, beta.or = 0, beta.dc = 0, beta.pl = 0, beta.sm = 0,
                                myla.or = 0.9, myla.dc=0.9, myla.pl=0.9, myla.sm = 0.9, myth=4, myL=6),
             control=nlc)
summary(mod0)
AIC(mod0) ## 503.7788
plot(dat$numreg.ok, predict(mod0))
abline(0,1)

## with geom decay also for OR
mod1 = nlsLM(numreg.ok ~ alpha + beta.or * GeometricSimpleAdstock(pr.or, lambda=myla.or) +
                  beta.dc * GeometricSimpleAdstock(pr.dc, lambda=myla.dc) +
                  beta.pl * GeometricSimpleAdstock(pr.pl, lambda=myla.pl) +
                  beta.sm * GeometricSimpleAdstock(pr.sm, lambda=myla.sm),
              dat, start = list(alpha = 0, beta.or = 0, beta.dc = 0, beta.pl = 0, beta.sm = 0,
                                myla.or = 0.9, myla.dc=0.9, myla.pl=0.9, myla.sm = 0.9),
             control=nlc)
summary(mod1)
AIC(mod1) ## 491.388
points(dat$numreg.ok, predict(mod1), col=2)
## alpha   -0.633148   0.369659  -1.713   0.0899 .  
## beta.or  0.004781   0.002089   2.288   0.0243 *  
## beta.dc  0.012684   0.002071   6.125 1.89e-08 ***
## beta.pl  0.016345   0.007198   2.271   0.0254 *  
## beta.sm -0.006433   0.006596  -0.975   0.3318    
## myla.or  0.752044   0.156935   4.792 5.88e-06 ***
## myla.dc  0.931682   0.024191  38.513  < 2e-16 ***
## myla.pl  0.046626   0.446279   0.104   0.9170    
## myla.sm  0.654463   0.460529   1.421   0.1585   

## without the intercept
mod2 = nlsLM(numreg.ok ~ 0 + beta.or * GeometricSimpleAdstock(pr.or, lambda=myla.or) +
                  beta.dc * GeometricSimpleAdstock(pr.dc, lambda=myla.dc) +
                  beta.pl * GeometricSimpleAdstock(pr.pl, lambda=myla.pl) +
                  beta.sm * GeometricSimpleAdstock(pr.sm, lambda=myla.sm),
              dat, start = list(beta.or = 0, beta.dc = 0, beta.pl = 0, beta.sm = 0,
                                myla.or = 0.9, myla.dc=0.9, myla.pl=0.9, myla.sm = 0.9),
             control=nlc)
summary(mod2)
AIC(mod2) ## 492.58
points(dat$numreg.ok, predict(mod2), col=3)
## beta.or  0.004633   0.002129   2.176   0.0319 *  
## beta.dc  0.012210   0.002131   5.730 1.08e-07 ***
## beta.pl  0.015151   0.007270   2.084   0.0397 *  
## beta.sm -0.006157   0.006425  -0.958   0.3402    
## myla.or  0.746264   0.169192   4.411 2.62e-05 ***
## myla.dc  0.923740   0.029080  31.766  < 2e-16 ***
## myla.pl  0.018868   0.486374   0.039   0.9691    
## myla.sm  0.681847   0.430566   1.584   0.1165  

## estimated lambdas
myla.or = 0.746264; myla.dc=0.923740; myla.pl=0.018868; myla.sm = 0.681847

## model with estimated adstock parameters for each media - used for prediction
dat$adstock.or = GeometricSimpleAdstock(dat$pr.or, lambda=myla.or)
dat$adstock.dc = GeometricSimpleAdstock(dat$pr.dc, lambda=myla.dc)
dat$adstock.pl = GeometricSimpleAdstock(dat$pr.pl, lambda=myla.pl)
dat$adstock.sm = GeometricSimpleAdstock(dat$pr.sm, lambda=myla.sm)

## predictions
## ##################
mod.pred = nlsLM(numreg.ok ~ 0 + beta.or * adstock.or +
                  beta.dc * adstock.dc + 
                  beta.pl * adstock.pl +
                  beta.sm * adstock.sm,
                 dat, start = list(beta.or = 0, beta.dc = 0, beta.pl = 0, beta.sm = 0), control=nlc)

## how many of each action we should have done to recruit as many as we did?
## 22 DC per week
dat.pred=data.frame(pr.or=rep(0,myN), pr.dc=rep(22,myN), pr.pl=rep(0,myN), pr.sm=rep(0,myN), numreg.ok=NA)
dat.pred$adstock.or = GeometricSimpleAdstock(dat.pred$pr.or, lambda=myla.or)
dat.pred$adstock.dc = GeometricSimpleAdstock(dat.pred$pr.dc, lambda=myla.dc)
dat.pred$adstock.pl = GeometricSimpleAdstock(dat.pred$pr.pl, lambda=myla.pl)
dat.pred$adstock.sm = GeometricSimpleAdstock(dat.pred$pr.sm, lambda=myla.sm)
mypred = predict(mod.pred, dat.pred)
plot(mypred)
sum(mypred)
## 313

## reach 200 people per week with an outreach (no direct contacts needed)
dat.pred=data.frame(pr.or=rep(200,myN), pr.dc=rep(0,myN), pr.pl=rep(0,myN), pr.sm=rep(0,myN), numreg.ok=NA)
## possible to publish one article every month reaching 800 
dat.pred=data.frame(pr.or=c(rep(c(0,0,0,800),myN/4), 0, 0, 0), pr.dc=rep(0,myN), pr.pl=rep(0,myN), pr.sm=rep(0,myN), numreg.ok=NA)

## reach 210 per week at events (not possible because there are no that many events)

## separate models by region, fix alpha to global values
## #####################################################

dat.sub <- subset(dat.region, region=="East")
myN <- nrow(dat.sub)
regE = nlsLM(numreg.ok ~ 0 + beta.or * GeometricSimpleAdstock(pr.or, lambda=0.746264) +
                  beta.dc * GeometricSimpleAdstock(pr.dc, lambda=0.92374) +
                  beta.pl * GeometricSimpleAdstock(pr.pl, lambda=0.018868) +
                  beta.sm * GeometricSimpleAdstock(pr.sm, lambda=0.681847),
              dat.sub, start = list(beta.or = 0, beta.dc = 0, beta.pl = 0, beta.sm = 0,
                              myla.or = 0.746264, myla.dc=0.92374, myla.pl=0.018868, myla.sm = 0.681847),
             control=nlc)
summary(regE)

dat.sub <- subset(dat.region, region=="West")
myN <- nrow(dat.sub)
regW = nlsLM(numreg.ok ~ 0 + beta.or * GeometricSimpleAdstock(pr.or, lambda=0.746264) +
                  beta.dc * GeometricSimpleAdstock(pr.dc, lambda=0.92374) +
                  beta.pl * GeometricSimpleAdstock(pr.pl, lambda=0.018868) +
                  beta.sm * GeometricSimpleAdstock(pr.sm, lambda=0.681847),
              dat.sub, start = list(beta.or = 0, beta.dc = 0, beta.pl = 0, beta.sm = 0,
                              myla.or = 0.746264, myla.dc=0.92374, myla.pl=0.018868, myla.sm = 0.681847),
             control=nlc)
summary(regW)

dat.sub <- subset(dat.region, region=="South")
myN <- nrow(dat.sub)
regS = nlsLM(numreg.ok ~ 0 + beta.or * GeometricSimpleAdstock(pr.or, lambda=0.746264) +
                  beta.dc * GeometricSimpleAdstock(pr.dc, lambda=0.92374) +
                  beta.pl * GeometricSimpleAdstock(pr.pl, lambda=0.018868) +
                  beta.sm * GeometricSimpleAdstock(pr.sm, lambda=0.681847),
              dat.sub, start = list(beta.or = 0, beta.dc = 0, beta.pl = 0, beta.sm = 0,
                             myla.or = 0.746264, myla.dc=0.92374, myla.pl=0.018868, myla.sm = 0.681847),
             control=nlc)
summary(regS)


## coefficients for plotting
res <- summary(mod2)$coefficients[4:1,]
resE <- summary(regE)$coefficients[4:1,]
resW <- summary(regW)$coefficients[4:1,]
resS <- summary(regS)$coefficients[4:1,]
res <- rbind(resS[1,], resW[1,], resE[1,], res[1,],
             resS[2,], resW[2,], resE[2,], res[2,],
             resS[3,], resW[3,], resE[3,], res[3,],
             resS[4,], resW[4,], resE[4,], res[4,])

## compare with data for markeitng attribution
## ###############################################
partic <- read.csv("partic_anonym.csv")
country.region <- read.csv("country_region.csv", h=T, sep=";")
partic.merge <- merge(partic, country.region, by="country", all.x=T)
partic <- partic.merge[, c("country", "region", "heard.MGOT.assigned")]
partic$region = factor(partic$region, levels=c("East", "West", "South"), ordered=T)
x <- c("Outreach article", "Circular, flyer, website", "MGOT contact", "Inside contact", "Outside contact", "Friend", "Forest event", "Social media, Radio", "Unknown" )
x = x[length(x):1]
partic$heard.MGOT.assigned <- factor(partic$heard.MGOT.assigned, levels=x, ordered=T)

reg.name = c("Southern Europe", "Western Europe", "Eastern Europe")

## load data for barriers
barr <- read.csv("Dataset_S3_MAXQDA_coded_sentences_barriers.csv")
barr.merge <- merge(barr, country.region, by="country", all.x=T)
barr.merge <- subset(barr.merge, !is.na(region))
barr.merge$region <- factor(barr.merge$region, levels=c("East", "West", "South"), ordered=T)
barr.fq <- apply(as.matrix(table(barr.merge$Code, barr.merge$region)), 2, function(a) a/sum(a))
barr.num <- as.matrix(table(barr.merge$Code, barr.merge$region))
barr.dat <- data.frame(region=rep(colnames(barr.fq), each=nrow(barr.fq)), Code=rep(rownames(barr.fq), 3),
                       prop=c(barr.fq), num=c(barr.num))
barr.dat$Code <- factor(barr.dat$Code, levels=names(sort(table(barr.merge$Code), T)), ordered=T)
barr.dat$region.ext = factor(rep(reg.name, each=7), levels=reg.name[3:1], ordered=T)

kruskal.test(barr.dat$num, barr.dat$region)
## Kruskal-Wallis chi-squared = 7.0777, df = 2, p-value = 0.02905
