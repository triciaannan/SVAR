rm(list = ls())
graphics.off()
library(vars)
library(patchwork)
library(ggplot2)
setwd("C:/Users/oktri/OneDrive/Desktop/FALL 2023/Econometrics III/Macroeconometrics")
dat <- read.csv(file = "Final_R.csv")

reer <- ts(log(dat$REER), start = c(1996, 1,1), freq = 4) 
yo <- ts(log(dat$yo), start = c(1996, 1,1), freq = 4) 
RPR <- ts(log(dat$Ind_input), start = c(1996, 1,1), freq = 4) 
RPO<- ts(log(dat$RPO), start = c(1996, 1,1), freq = 4)
RPM<- ts(log(dat$RPM), start = c(1996, 1,1), freq = 4) 
RPF<- ts(log(dat$RPF), start = c(1996, 1,1), freq = 4) 
ffshock <- ts((dat$Fed_Fund_Shock), start = c(1996, 1,1), freq = 4) 
ir <- ts((dat$X1.Year.Interest), start = c(1996, 1,1), freq = 4) 
news_shock <- ts((dat$New_Shock), start = c(1996, 1,1), freq = 4) 



lin.mod <- lm(RPF ~ time(RPF))
lin.trend <- lin.mod$fitted.values
linear <- ts(lin.trend, start = c(1996, 1,1), frequency = 4)
lin.cycle <-  RPF - linear

adf.lin <- ur.df(lin.cycle, type = "none", selectlags = c("AIC"))
summary(adf.lin)

#In this case the results suggest that we are able to reject the null of a unit root at the 5% level.
#As the test statistic -2.734 is smaller (i.e. more negative) than-1.95
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot.ts(lin.cycle)

dataRPF <- cbind(yo,ir,reer,RPO,RPF)
colnames(dataRPF) <- c("yo","ir","reer","RPO","RPF")
info.var <- VARselect(dataRPF, lag.max = 5, type = "both")
info.var$selection

dataRPR <- cbind(yo,ir,reer,RPO,RPR)
colnames(dataRPR) <- c("yo","ir","reer","RPO","RPR")
info.varR <- VARselect(dataRPR, lag.max = 5, type = "both")
info.varR$selection
dataRPM <- cbind(yo,ir, reer,RPO,RPM)
colnames(dataRPM) <- c("yo", "ir", "reer","RPO","RPM")
info.varM <- VARselect(dataRPM, lag.max = 5, type = "both")
info.varM$selection



dum96 <- rep(0, length(ir))
dum96[1] <- -0.0000000000000035527136788005
dum96[3] <- -0.007484

dum97 <- rep(0, length(ir))
dum97[5]<- 0.003188  
dum97[6]<- -0.032878788
dum97[7]<-   -0.007126437
dum97[8]<-  -0.016666667

dum98 <- rep(0, length(ir))
dum98[9]<- 0.00194444444444405 
dum98[10]<- -0.0086111111111201
dum98[11]<-   0.0217222222222238
dum98[12]<-  -0.0192307692307674

dum99 <- rep(0, length(ir))
dum99[13]<- -0.00333333333333267
dum99[14]<- -0.017307692
dum99[15]<-  0.013333333
dum99[16]<-  0.011089744

dum00 <- rep(0, length(ir))
dum00[17]<- -0.028234568
dum00[18]<- 0.003777778
dum00[19]<-  0.013333333
dum00[20]<- -0.005740741

dum01 <- rep(0, length(ir))
dum01[21]<- 0.032121212
dum01[22]<- 0.001041667
dum01[23]<- 0.005166667
dum01[24]<- -0.058804598

  
dum02 <- rep(0, length(ir))
dum02[25]<- -0.004611111
dum02[26]<- 0.003819444
dum02[27]<- 0.018148148
dum02[28]<- -0.06875

dum03 <- rep(0, length(ir))
dum03[29]<- 0.011282051
dum03[30]<-0.0578
dum03[31]<- 0.00085213

dum04 <- rep(0, length(ir))
dum04[33]<- 0.001666667
dum04[34]<- -0.007160494
dum04[35]<- 0.01047619
dum04[36]<- -0.005539216

dum05 <- rep(0, length(ir))
dum05[37]<- -0.005740741
dum05[38]<- 0.000178571
dum05[39]<- 0.012651515
dum05[40]<- -0.001724138

dum06 <- rep(0, length(ir))
dum06[41]<- 0.003333333
dum06[42]<- -0.005
dum06[43]<- -0.020724638
dum06[44]<- -0.001666667


dum07 <- rep(0, length(ir))
dum07[45]<- 0.005166667
dum07[47]<- -0.064513889
dum07[48]<- 0.003666667

dum08 <- rep(0, length(ir))
dum08[49]<- -0.006538462
dum08[50]<- -0.025
dum08[51]<- 0.031149267
dum08[52]<- -0.061611111

dum09<- rep(0, length(ir))
dum09[53]<- -0.000320513
dum09[55]<- -0.003333333
dum09[56]<- -0.007089744

dum10 <- rep(0, length(ir))
dum10[57]<- -0.005111111
dum10[59]<- 0.001230159
dum10[60]<- 0.004297386

dum11 <- rep(0, length(ir))
dum11[61]<- -0.001666667
dum11[63]<- 0.000429293
dum11[64]<- -0.000350529

dum12 <- rep(0, length(ir))
dum12[65]<- -0.000231481
dum12[66]<- 0.0025
dum12[67]<- 0.008107843
dum12[68]<-0.001359649

dum12 <- rep(0, length(ir))
dum12[69]<- 0.001666667

dum13 <- rep(0, length(ir))
dum13[75]<- -0.001666667
dum13[76]<- -0.001845238


dum14 <- rep(0, length(ir))
dum14[77]<- -0.001987179
dum14[78]<- -0.003589744
dum14[79]<- -0.022820513
dum14[80]<- 0.005222222

dum15 <- rep(0, length(ir))
dum15[81]<- -0.006833333
dum15[82]<- -0.001666667
dum15[83]<- -0.017222222
dum15[84]<- -0.003305322

dum16 <- rep(0, length(ir))
dum16[85]<- -0.001728395
dum16[86]<- 0.005327381
dum16[88]<- 0.000573116

dum17 <- rep(0, length(ir))
dum17[89]<- -0.001666667
dum17[90]<- -0.002361393
dum17[91]<- 0.002472222
dum17[92]<- 0.010883838

dum18 <- rep(0, length(ir))
dum18[94]<- 0.011843434
dum18[95]<-0.025
dum18[96]<- -0.004166667

dum19 <- rep(0, length(ir))
dum19[97]<- 0.003333333
dum19[98]<- -0.010833333
dum19[99]<- -0.000119048
dum19[100]<- -0.000722222

dum20 <- rep(0, length(ir))
dum20[101]<- -0.003690476
dum20[102]<- 0.003809524
dum20[103]<- 0.001666667
dum20[104]<- 0.000925926

dum96 <- ts(dum96,start = c(1996, 1,1), freq = 4)
dum97 <- ts(dum97,start = c(1996, 1,1), freq = 4)
dum98 <- ts(dum98,start = c(1996, 1,1), freq = 4)
dum99 <- ts(dum99,start = c(1996, 1,1), freq = 4)  
dum00 <- ts(dum00,start = c(1996, 1,1), freq = 4)
dum01 <- ts(dum01,start = c(1996, 1,1), freq = 4)
dum02 <- ts(dum02,start = c(1996, 1,1), freq = 4)
dum03 <- ts(dum03,start = c(1996, 1,1), freq = 4)
dum04 <- ts(dum04,start = c(1996, 1,1), freq = 4)
dum05 <- ts(dum05,start = c(1996, 1,1), freq = 4)
dum06 <- ts(dum06,start = c(1996, 1,1), freq = 4)
dum07 <- ts(dum07,start = c(1996, 1,1), freq = 4)
dum08 <- ts(dum08,start = c(1996, 1,1), freq = 4)
dum09 <- ts(dum09,start = c(1996, 1,1), freq = 4)
dum10 <- ts(dum10,start = c(1996, 1,1), freq = 4)
dum11 <- ts(dum11,start = c(1996, 1,1), freq = 4)
dum12 <- ts(dum12,start = c(1996, 1,1), freq = 4)
dum13 <- ts(dum13,start = c(1996, 1,1), freq = 4)
dum14 <- ts(dum14,start = c(1996, 1,1), freq = 4)
dum15 <- ts(dum15,start = c(1996, 1,1), freq = 4)
dum16 <- ts(dum16,start = c(1996, 1,1), freq = 4)
dum17 <- ts(dum17,start = c(1996, 1,1), freq = 4)
dum18 <- ts(dum18,start = c(1996, 1,1), freq = 4)
dum19 <- ts(dum19,start = c(1996, 1,1), freq = 4)
dum20 <- ts(dum20,start = c(1996, 1,1), freq = 4)

dum <- cbind(dum96, dum97,dum98,dum99,dum00,dum01,dum02,dum03,dum04,dum05,dum06,dum07,
             dum08,dum09,dum10,dum11,dum12,dum13,dum14,dum15,dum16,dum17,dum18,dum19,
             dum20)
colnames(dum) <- c( "dum96", "dum97","dum98","dum99","dum00","dum01","dum02","dum03",
                    "dum04","dum05","dum06","dum07","dum08","dum09","dum10","dum11",
                    "dum12","dum13","dum14","dum15","dum16","dum17","dum18","dum19",
                    "dum20" )

varV<- VAR(dataRPF, p = 1, type = "const", season = NULL)
var.est1 <- VAR(dataRPF, p = 1, type = "const", season = NULL, 
                exog =dum )
summary(var.est1)


a.mat <- diag(5)
diag(a.mat) <- NA
a.mat[2, 1] <- NA
a.mat[3, 1] <- NA
a.mat[3, 2] <- NA
a.mat[4, 1] <- NA
a.mat[4, 2] <- NA
a.mat[4, 3] <- NA
a.mat[5, 1] <- NA
a.mat[5, 2] <- NA
a.mat[5, 3] <- NA
a.mat[5, 4] <- NA
print(a.mat)



b.mat <- diag(5)
diag(b.mat) <- NA
print(b.mat)


svarV <- SVAR(varV, Amat = a.mat, Bmat = b.mat, max.iter = 10000, 
                 hessian = TRUE)
one.intV <- irf(svarV,  n.ahead = 50, ortho = TRUE, boot = TRUE,runs=1000)



svar.RPF <- SVAR(var.est1, Amat = a.mat, Bmat = b.mat, max.iter = 10000, 
                 hessian = TRUE)
one.int <- irf(svar.RPF,  n.ahead = 50, ortho = TRUE, boot = TRUE,runs=1000)


mycaption <- paste0("\ Redline is the response and the blue shade is the 95% confidence interval",
                    "\nResponses are based on four variables:log outputs from OECD,log of real price of oil, log REER,log real price of food,1-year interest rate without monetary policy shock",
                    "\nQuarterly data Jan 1996 to Dec 2021",
                    "\nSources: IMF, OECD, Federal Reserve",
                    "\nRetrieved from FRED, Federal Reserve Bank of St. Louis")





myf <- function(i = 1, j = 2) {
  irf_varV <- data.frame(
    period = 1:51,
    low = one.intV$Lower[[j]][, i],
    up = one.intV$Upper[[j]][, i],
    m = one.intV$irf[[j]][, i]
  )




  ggplot(data=irf_varV, aes(x=period,y=-m,ymin=-low,ymax=-up))+
    geom_ribbon(fill="dodgerblue",alpha=0.5)+
    geom_line(color="red")+
    geom_hline(yintercept=0,linetype=2)+
    theme(plot.caption=element_text(hjust=0))+
    scale_y_continuous(labels=scales::percent)+
    scale_x_continuous(breaks=seq(0,50,10))+
    theme_minimal()+
    theme(plot.caption=element_text(hjust=0))+
    labs(x="Quarters",
         y="Response",
         title=paste0(colnames(dataRPF)[i]," response to ",colnames(dataRPF)[j], " shock"),
         subtitle="95% confidence interval,1000 runs")

}
myf(5,1)+myf(5,2)+myf(5,3)+myf(5,4)+myf(5,5)+labs(subtitle="")
myf(1,1)+labs(subtitle="")+
myf(1,2)+labs(subtitle="")+
myf(1,3)+labs(subtitle="")+
myf(1,4)+labs(subtitle="")+
myf(1,5)+labs(subtitle="")+
myf(2,1)+labs(subtitle="")+
myf(2,2)+labs(subtitle="") + 
myf(2,3)+labs(subtitle="") +
myf(2,4)+labs(subtitle="") +
myf(2,5)+labs(subtitle="") +
myf(3,1)+labs(subtitle="") +
myf(3,2)+labs(subtitle="") +
  plot_annotation(caption=mycaption, theme = theme(plot.caption = element_text(hjust=0)))
myf(3,3)+labs(subtitle="") +
myf(3,4)+labs(subtitle="") +
myf(3,5)+labs(subtitle="") +
myf(4,1)+labs(subtitle="") +
myf(4,2)+labs(subtitle="") +
myf(4,3)+labs(subtitle="") +
myf(4,4)+labs(subtitle="") +
myf(4,5)+labs(subtitle="") +
myf(5,1)+labs(subtitle="") +
myf(5,2)+labs(subtitle="") +
myf(5,3)+labs(subtitle="") +
myf(5,4)+labs(subtitle="") +
myf(5,5)+labs(subtitle="") +
  plot_annotation(caption=mycaption, theme = theme(plot.caption = element_text(hjust=0)))

fevd_result1 <- fevd(svarV,n.ahead=50)

custom_colors <- c("aquamarine2", "cadetblue", "coral", "burlywood","azure4")

# Plot the FEVD results with custom colors
plot(fevd_result1, col = custom_colors)
#-------------------------------------------------------------------------------

lin.mod2 <- lm(RPM ~ time(RPM))
lin.trend2 <- lin.mod2$fitted.values
linear2 <- ts(lin.trend2, start = c(1996, 1,1), frequency = 4)
lin.cycle2 <-  RPM - linear2

adf.lin2 <- ur.df(lin.cycle2, type = "none", selectlags = c("AIC"))
summary(adf.lin2)

#In this case the results suggest that we are able to reject the null of a unit root at the 5% level.
#As the test statistic -2.0716 is smaller (i.e. more negative) than-1.95
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot.ts(lin.cycle2)

dataRPM <- cbind(yo,ir, reer,RPO,RPM)
colnames(dataRPM) <- c("yo", "ir", "reer","RPO","RPM")



var.est2 <- VAR(dataRPM, p = 2, type = "both", season = NULL, 
                exog =dum )
summary(var.est2)



svar.two <- SVAR(var.est2, Amat = a.mat, Bmat = b.mat, max.iter = 10000, 
                 hessian = TRUE)
svar.two

two.int <- irf(svar.two,  n.ahead = 50, ortho = TRUE, boot = TRUE,runs=1000)
mycaption1 <- paste0("\ Redline is the response and the blue shade is the 95% confidence interval",
                    "\nResponses are based on four variables:log outputs from OECD,log real price of oil,log REER,log real price of Metal,1-year interest rate and monetary policy shock",
                    "\nQuarterly data Jan 1996 to Dec 2021",
                    "\nSources: IMF, OECD, Federal Reserve",
                    "\nRetrieved from FRED, Federal Reserve Bank of St. Louis")





myfm <- function(i = 1, j = 2) {
  irf_var2 <- data.frame(
    period = 1:51,
    low = two.int$Lower[[j]][, i],
    up = two.int$Upper[[j]][, i],
    m =two.int$irf[[j]][, i]
  )
  
  
  
  
  ggplot(data=irf_var2, aes(x=period,y=-m,ymin=-low,ymax=-up))+
    geom_ribbon(fill="dodgerblue",alpha=0.5)+
    geom_line(color="red")+
    geom_hline(yintercept=0,linetype=2)+
    theme(plot.caption=element_text(hjust=0))+
    scale_y_continuous(labels=scales::percent)+
    scale_x_continuous(breaks=seq(0,50,10))+
    theme_minimal()+
    theme(plot.caption=element_text(hjust=0))+
    labs(x="Quarters",
         y="Response",
         title=paste0(colnames(dataRPM)[i]," response to ",colnames(dataRPM)[j], " shock"),
         subtitle="95% confidence interval,1000 runs")
  
}

myfm(1,1)+labs(subtitle="")+
  myfm(1,2)+labs(subtitle="")+
  myfm(1,3)+labs(subtitle="")+
  myfm(1,4)+labs(subtitle="")+
  myfm(1,5)+labs(subtitle="")+
  myfm(2,1)+labs(subtitle="")+
  myfm(2,2)+labs(subtitle="") + 
  myfm(2,3)+labs(subtitle="") +
  myfm(2,4)+labs(subtitle="") +
  myfm(2,5)+labs(subtitle="") +
  myfm(3,1)+labs(subtitle="") +
  myfm(3,2)+labs(subtitle="") +
  plot_annotation(caption=mycaption1, theme = theme(plot.caption = element_text(hjust=0)))
  myfm(3,3)+labs(subtitle="") +
  myfm(3,4)+labs(subtitle="") +
  myfm(3,5)+labs(subtitle="") +
  myfm(4,1)+labs(subtitle="") +
  myfm(4,2)+labs(subtitle="") +
  myfm(4,3)+labs(subtitle="") +
  myfm(4,4)+labs(subtitle="") +
  myfm(4,5)+labs(subtitle="") +
  myfm(5,1)+labs(subtitle="") +
  myfm(5,2)+labs(subtitle="") +
  myfm(5,3)+labs(subtitle="") +
  myfm(5,4)+labs(subtitle="") +
  myfm(5,5)+labs(subtitle="") +
  plot_annotation(caption=mycaption1, theme = theme(plot.caption = element_text(hjust=0)))

fevd_result2 <- fevd(svar.two,n.ahead=50)

custom_colors <- c("aquamarine2", "cadetblue", "coral", "burlywood","azure4")

# Plot the FEVD results with custom colors
plot(fevd_result2, col = custom_colors)

#-------------------------------------------------------------------------------
lin.mod3<- lm(RPR ~ time(RPR))
lin.trend3 <- lin.mod3$fitted.values
linear3 <- ts(lin.trend3, start = c(1996, 1,1), frequency = 4)
lin.cycle3 <-  RPR - linear3

adf.lin3 <- ur.df(lin.cycle3, type = "none", selectlags = c("AIC"))
summary(adf.lin3)

var.est3 <- VAR(dataRPR, p = 2, type = "const", season = NULL, 
                exog =dum )
summary(var.est3)

svar.rpr <- SVAR(var.est3, Amat = a.mat, Bmat = b.mat, max.iter = 10000, 
                 hessian = TRUE)
svar.rpr


rpr.int <- irf(svar.rpr, n.ahead = 50, ortho = TRUE, boot = TRUE,runs=1000)


mycaption2<- paste0("\ Redline is the response and the blue shade is the 95% confidence interval",
                     "\nResponses are based on four variables:log outputs from OECD,log real price of oil,log REER,log real price of industrial inputs,1-year interest rate and monetary policy shock",
                     "\nQuarterly data Jan 1996 to Dec 2021",
                     "\nSources: IMF, OECD, Federal Reserve",
                     "\nRetrieved from FRED, Federal Reserve Bank of St. Louis")





myfr <- function(i = 1, j = 2) {
  irf_var3 <- data.frame(
    period = 1:51,
    low = rpr.int$Lower[[j]][, i],
    up = rpr.int$Upper[[j]][, i],
    m = rpr.int$irf[[j]][, i]
  )
  
  
  
  
  ggplot(data=irf_var3, aes(x=period,y=-m,ymin=-low,ymax=-up))+
    geom_ribbon(fill="dodgerblue",alpha=0.5)+
    geom_line(color="red")+
    geom_hline(yintercept=0,linetype=2)+
    theme(plot.caption=element_text(hjust=0))+
    scale_y_continuous(labels=scales::percent)+
    scale_x_continuous(breaks=seq(0,50,10))+
    theme_minimal()+
    theme(plot.caption=element_text(hjust=0))+
    labs(x="Quarters",
         y="Response",
         title=paste0(colnames(dataRPR)[i]," response to ",colnames(dataRPR)[j], " shock"),
         subtitle="95% confidence interval,1000 runs")
  
}

myfr(1,1)+labs(subtitle="")+
  myfr(1,2)+labs(subtitle="")+
  myfr(1,3)+labs(subtitle="")+
  myfr(1,4)+labs(subtitle="")+
  myfr(1,5)+labs(subtitle="")+
  myfr(2,1)+labs(subtitle="")+
  myfr(2,2)+labs(subtitle="") + 
  myfr(2,3)+labs(subtitle="") +
  myfr(2,4)+labs(subtitle="") +
  myfr(2,5)+labs(subtitle="") +
  myfr(3,1)+labs(subtitle="") +
  myfr(3,2)+labs(subtitle="") +
  plot_annotation(caption=mycaption2, theme = theme(plot.caption = element_text(hjust=0)))
  myfr(3,3)+labs(subtitle="") +
  myfr(3,4)+labs(subtitle="") +
  myfr(3,5)+labs(subtitle="") +
  myfr(4,1)+labs(subtitle="") +
  myfr(4,2)+labs(subtitle="") +
  myfr(4,3)+labs(subtitle="") +
  myfr(4,4)+labs(subtitle="") +
  myfr(4,4)+labs(subtitle="") +
  myfr(4,5)+labs(subtitle="") +
  myfr(5,1)+labs(subtitle="") +
  myfr(5,2)+labs(subtitle="") +
  myfr(5,3)+labs(subtitle="") +
  myfr(5,4)+labs(subtitle="") +
  myfr(5,4)+labs(subtitle="") +
  myfr(5,5)+labs(subtitle="") +
  plot_annotation(caption=mycaption2, theme = theme(plot.caption = element_text(hjust=0)))

fevd_result3 <- fevd(svar.rpr,n.ahead=50)

custom_colors <- c("aquamarine2", "cadetblue", "coral", "burlywood","azure4")

# Plot the FEVD results with custom colors

plot(fevd_result3, col = custom_colors)



