

ux<-unique(dat$PERMNO)

alpha<-data.frame(matrix(nrow=length(ux),ncol=4))
alpha[,1]<-ux
colnames(alpha)<-c("Fund","alpha","se","t_stat")


dat$RET_RF<-dat$RET-(dat$rf/100)

for(i in 1:ux)
{
  dat_n<-dat[dat$PERMNO==ux[i],]
  reg<-lm(dat$RET_RF~dat$mktrf)
  
  alpha[,2]<-summary(reg)$coefficients[1,1]
  alpha[,3]<-summary(reg)$coefficients[1,2]
  alpha[,4]<-summary(reg)$coefficients[1,3]
}

write_dta(alpha,"C:/Users/rd1150/OneDrive - USNH/My Research_NEW/Skewness/STOCKS_ALPHA/alpha.dta")