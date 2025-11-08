#calculate t_alpha using null=neg alpha

dat<-read_dta("D:/PhD/Research MG/FF Bootstrap/BS_FAMA_NET/ACT_1F_BM_FULL.dta")

#skewness parameter
alpha_1f=(0.3707067)/100
#alpha_4f=(-0.1802801)/100

dat$t_alpha_sk<-(dat$alpha-alpha_1f)/dat$se

write_dta(dat,"D:/PhD/Research MG/Skewness/FAMA_BS_NET/FF/ACT_1F_BM_FULL.dta")

#calculate the level of sig
dat<-read_dta("D:/PhD/Research MG/Skewness/FAMA_BS_NET/FF/BS_1F_BM_FULL.dta")
dat<-dat[is.na(dat$t_alpha_sk)==FALSE,]
dat<-dat[dat$R_sqr<1,]
  
dat_pct<-dat %>%
  group_by(Fund) %>%
  summarise(quantile(t_alpha_sk,0.95),quantile(t_alpha_sk,0.05),quantile(t_alpha,0.95))

dat_pct<-rename(dat_pct,"pct_95"="quantile(t_alpha_sk, 0.95)")
dat_pct<-rename(dat_pct,"pct_5"="quantile(t_alpha_sk, 0.05)")  
dat_pct<-rename(dat_pct,"pct_95_act"="quantile(t_alpha, 0.95)") 
write_dta(dat_pct,"D:/PhD/Research MG/Skewness/FAMA_BS_NET/PCT TABLE/1F_BM_FULL.dta")


#draw the cdfs
dat_act<-read_dta("D:/PhD/Research MG/Skewness/FAMA_BS_NET/FF/ACT_1F_BM_FULL.dta")
dat_pct<-read_dta("D:/PhD/Research MG/Skewness/FAMA_BS_NET/PCT TABLE/1F_BM_FULL.dta")


plot(ecdf(dat_act$t_alpha_sk),lwd=2,xlim=c(-6,6),main="",xlab="",ylab="") 
lines(ecdf(dat_pct$pct_95),lty=3,lwd=2,col="red")

plot(ecdf(dat_act$t_alpha),lwd=2,xlim=c(-4,6),main="",xlab="",ylab="")
lines(ecdf(dat_pct$pct_95_act),lty=3,lwd=2,col="blue")
