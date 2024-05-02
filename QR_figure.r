
#Figure 1: Plot of adhesive bond B data.
dat <- read.csv('bond/data.csv')[,-1]
png('bond/bdata.png', width=600, height=600, pointsize=20)
type <- ifelse(dat$temp==50, 2, ifelse(dat$temp==60,3, 4))
plot(dat$time^2, dat$response, col=type, pch=type,
     xlim=c(0, max(dat$time)^2), ylim=c(log(exp(2.5)),log(exp(5))),
     xlab='Time in Weeks', ylab='Log of Strength (Newtons)')

legend('topright',border='white',
       pch=2:4, col=2:4, legend=c('50??C','60??C','70??C'))
dev.off()



#Figure 2: Fitted QR ADDT models at accelerated temperature and normal use temperature, 
#          along with the mean ADDT modeling fits, for adhesive bond B data.
temp <- split(dat, dat$temp)
par.mean <- c(4.4713, -8.6384*10^8, 0.6364, 0.1609)
par.qr <- read.csv('gem_op2_par.csv')[c(2,4,6),-1]

deg <- function(par, time, temp){
  par <- unlist(par)
  par[1] +par[2]*exp( par[3]*(-11605/(temp+273.15)) ) * sqrt(time)
}

for (i in 1:length(temp)){
  temper <- as.numeric(names(temp)[i])
  png(paste('bond/',temper,'plot.png',sep=''), 
      width=600, height=600, pointsize=20)
  
  plot(temp[[i]]$time^2, temp[[i]]$response, col=i+1, pch=i+1,
       xlim=c(0, max(dat$time)^2), ylim=c(log(exp(2.5)),log(exp(5))),
       xlab='Time in Weeks', ylab='Log of Strength (Newtons)')
  
  curve(deg(par.mean, x, temper), add=T,
        col=i+1, lwd=2, lty=3)
  
  ltys <- c(2,1,4)
  for (j in 1:nrow(par.qr)){
    curve(deg(as.numeric(par.qr[j,]), x, temper), add=T, 
          col=i+1, lwd=2,lty=ltys[j])
  }
  legend(ifelse(i%in% c(1,4),'bottomright','topright'), legend=c('mean','0.1Q','0.5Q','0.9Q'),
         lty=c(3:1,4), col=i+1, lwd=2, cex=1.2)
  dev.off()
}
png('bond/25plot.png', width=600, height=600, pointsize=20)

plot(NULL,
     xlim=c(0, max(dat$time)^2), ylim=c(log(exp(2.5)),log(exp(5))),
     xlab='Time in Weeks', ylab='Log of Strength (Newtons)')

curve(deg(par.mean, x, 25), add=T, lwd=2, lty=3)

ltys <- c(2,1,4)
for (j in 1:nrow(par.qr)){
  curve(deg(as.numeric(par.qr[j,]), x, 25), add=T, lwd=2,lty=ltys[j])
}
legend('bottomright', legend=c('mean','0.1Q','0.5Q','0.9Q'),
       lty=c(3:1,4), cex=1.2, lwd=2)
dev.off()

par.qr <- read.csv('bond/basic_par.csv')[c(2,4,6),-1]
for (i in 1:length(temp)){
  temper <- as.numeric(names(temp)[i])
  png(paste('bond/',temper,'plot_basic.png',sep=''), 
      width=600, height=600, pointsize=20)
  
  plot(temp[[i]]$time^2, temp[[i]]$response, col=i+1, pch=i+1,
       xlim=c(0, max(dat$time)^2), ylim=c(log(exp(2.5)),log(exp(5))),
       xlab='Time in Weeks', ylab='Log of Strength (Newtons)')
  
  curve(deg(par.mean, x, temper), add=T,
        col=i+1, lwd=2, lty=3)
  
  ltys <- c(2,1,4)
  for (j in 1:nrow(par.qr)){
    curve(deg(as.numeric(par.qr[j,]), x, temper), add=T, 
          col=i+1, lwd=2,lty=ltys[j])
  }
  legend('topright', legend=c('mean','0.1Q','0.5Q','0.9Q'),
         lty=c(3:1,4), cex=0.5, col=i+1, lwd=2)
  dev.off()
}

png('bond/25plot_basic.png', width=600, height=600, pointsize=20)

plot(NULL, col='grey',
     xlim=c(0, max(dat$time)^2), ylim=c(log(exp(2.5)),log(exp(5))),
     xlab='Time in Weeks', ylab='Log of Strength (Newtons)')

curve(deg(par.mean, x, 25), add=T,
      col='grey', lwd=2, lty=3)

ltys <- c(2,1,4)
for (j in 1:nrow(par.qr)){
  curve(deg(as.numeric(par.qr[j,]), x, 25), add=T, 
        col='grey', lwd=2,lty=ltys[j])
}
legend('topright', legend=c('mean','0.1Q','0.5Q','0.9Q'),
       lty=c(3:1,4), cex=0.5, col='grey', lwd=2)
dev.off()



#Figure 3: Plot of bi-functional return-spring ADDT data.
dat <- read.csv('bifunction/data.csv')

png('bifunction/bifunction.png', width=600, height=600, pointsize=20)
type <- ifelse(dat$temp==300, 2, 3)
plot(dat$time, dat$variation, col=type, pch=type,
     xlab='Time in Hours', ylab='Actuating Force (Ratio)')

legend('topright',border='white',
       pch=2:3, col=2:3, legend=c('300??C','325??C'))
dev.off()



#Figure 4: Fitted QR ADDT models at accelerated temperature and normal use temperature,
#          along with the mean ADDT modeling fits, for return-spring data.
temp <- split(dat, dat$temp)
par.mean <- c(-0.0114, 0.2571)
par.qr <- read.csv('bifunction/gem_op2_par.csv')[c(2,4,6),-1]

deg <- function(par, time, temp){
  par <- unlist(par)
  (time + 1)^( par[1]*exp( par[2]*11605*(1/(240 + 273.15) - 1/(temp + 273.15))))
}

for (i in 1:length(temp)){
  temper <- as.numeric(names(temp)[i])
  png(paste('bifunction/',temper,'plot.png',sep=''), 
      width=600, height=600, pointsize=20)
  
  plot(temp[[i]]$time, temp[[i]]$variation, col=i+1, pch=i+1,
       xlim=c(0, max(dat$time)), ylim=c(0.75,1),
       xlab='Time in Hours', ylab='Actuating Force (Ratio)')
  
  curve(deg(par.mean, x, temper), add=T,
        col=i+1, lwd=2, lty=3)
  
  ltys <- c(2,1,4)
  for (j in 1:nrow(par.qr)){
    curve(deg(as.numeric(par.qr[j,]), x, temper), add=T, 
          col=i+1, lwd=2,lty=ltys[j])
  }
  legend('topright', legend=c('mean','0.1Q','0.5Q','0.9Q'),
         lty=c(3:1,4), cex=1.2, col=i+1, lwd=2)
  dev.off()
}

png('bifunction/230plot.png', width=600, height=600, pointsize=20)
plot(NULL, 
     xlim=c(0, max(dat$time)), ylim=c(0.75,1),
     xlab='Time in Hours', ylab='Actuating Force (Ratio)')

curve(deg(par.mean, x, 230), add=T,lwd=2, lty=3)

ltys <- c(2,1,4)
for (j in 1:nrow(par.qr)){
  curve(deg(as.numeric(par.qr[j,]), x, 230), add=T, 
        lwd=2,lty=ltys[j])
}
legend('bottomright', legend=c('mean','0.1Q','0.5Q','0.9Q'),
       lty=c(3:1,4), cex=1.2, lwd=2)
dev.off()



par.qr <- read.csv('bifunction/basic_par.csv')[c(2,4,6),-1]
for (i in 1:length(temp)){
  temper <- as.numeric(names(temp)[i])
  png(paste(temper,'plot_basic.png',sep=''), 
      width=600, height=600, pointsize=20)
  
  plot(temp[[i]]$time, temp[[i]]$variation, col=i+1, pch=i+1,
       xlim=c(0, max(dat$time)), ylim=c(0.75,1),
       xlab='Time in Hours', ylab='Actuating Force (Ratio)')
  
  curve(deg(par.mean, x, temper), add=T,
        col=i+1, lwd=2, lty=3)
  
  ltys <- c(2,1,4)
  for (j in 1:nrow(par.qr)){
    curve(deg(as.numeric(par.qr[j,]), x, temper), add=T, 
          col=i+1, lwd=2,lty=ltys[j])
  }
  legend('topright', legend=c('mean','0.1Q','0.5Q','0.9Q'),
         lty=c(3:1,4), cex=0.5, col=i+1, lwd=2)
  dev.off()
}

png('bifunction/230plot_basic.png', width=600, height=600, pointsize=20)

plot(NULL, col='grey',
     xlim=c(0, max(dat$time)), ylim=c(0.75,1),
     xlab='Time in Hours', ylab='Actuating Force (Ratio)')

curve(deg(par.mean, x, 230), add=T,
      col='grey', lwd=2, lty=3)

ltys <- c(2,1,4)
for (j in 1:nrow(par.qr)){
  curve(deg(as.numeric(par.qr[j,]), x, 230), add=T, 
        col='grey', lwd=2,lty=ltys[j])
}
legend('topright', legend=c('mean','0.1Q','0.5Q','0.9Q'),
       lty=c(3:1,4), cex=0.5, col='grey', lwd=2)
dev.off()

temp <- split(dat, dat$temp)
model <- c('exp','log','logit','')

deg <- function(par, time, temp, model){
  par <- unlist(par)
  
  if (model=='exp'){
    (-exp(par[1])*exp(par[2]*(11605/(240+273.15)-11605/(temp+273.15)))*time)
  } else if (model=='log'){
    par[1]*exp(par[2]*(11605/(240+273.15) - 11605/(temp+273.15)))*log(time+1)+1
  } else if (model=='logit'){
    1/(1+exp(par[1])*exp(par[2]*(11605/(240+273.15)-11605/(temp+273.15)))*time)
  } else {
    (time + 1)^( par[1]*exp( par[2]*11605*(1/(240 + 273.15) - 1/(temp + 273.15))))
  }
}

for (j in 1:3){
  par.qr <- read.csv(paste('bifunction/gem_op2_par_',model[j],'.csv',sep=''))[,-1]
  
  png(paste('gof_',model[j],'_plot.png',sep=''), 
      width=600, height=600, pointsize=20)
  #300, 325 Plot
  for (i in 1:length(temp)){
    temper <- as.numeric(names(temp)[i])
    
    if (i==1){
      plot(temp[[i]]$time, temp[[i]]$variation, col=i+1, pch=i+1,
           xlim=c(0, max(dat$time)), ylim=c(0.75,1),
           xlab='Time in Hours', ylab='Actuating Force (Ratio)')
    } else {
      points(temp[[i]]$time, temp[[i]]$variation, col=i+1, pch=i+1)
    }
    ltys <- c(2,1,4)
    for (k in 1:nrow(par.qr)){
      curve(deg(as.numeric(par.qr[k,-3]), x, temper, model[j]), add=T, 
            col=i+1, lwd=2, lty=ltys[i])
    }
  }
  legend('topright', legend=c('300??C-0.1Q','300??C-0.5Q','300??C-0.9Q','325??C-0.1Q','325??C-0.5Q','325??C-0.9Q'),
         lty=c(2,1,4,2,1,4), cex=0.5, col=rep(c(2,3),each=3), lwd=2)
  dev.off()
}
deg(as.numeric(par.qr[k,-3]), 1:1000, temper, model[j])
