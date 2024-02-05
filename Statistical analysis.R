library(cowplot)
library(ggplot2)
library(raincloudplots)
library(readxl)

########################## Variables ##########################

s1_ohdg_vol # time-series of volume-corrected urinary 8-OHdG levels for subject 1
s1_ohdg_crea # time-series of creatinine-corrected urinary 8-OHdG levels for subject 1
s2_ohdg_vol # time-series of volume-corrected urinary 8-OHdG levels for subject 2
s2_ohdg_crea # time-series of creatinine-corrected urinary 8-OHdG levels for subject 2

s1_ohdg_vol <- Export_S1_S2_OHdG$S2_OHdG_Volumen_Correction[1:55]/12/1000
s1_ohdg_crea <- Export_S1_S2_OHdG$S2_OHdG[1:55]
s2_ohdg_vol <- Export_S1_S2_OHdG$S1_OHdG_Volumen_Correction[1:63]/12/1000
s2_ohdg_crea <- Export_S1_S2_OHdG$S1_OHdG[1:63]


################## Coefficients of variation ##################

# 12h-interval day
sd(s1_ohdg_vol[seq(2,55,2)])/mean(s1_ohdg_vol[seq(2,55,2)]) # subject 1 and volume-correction
sd(s1_ohdg_crea[seq(2,55,2)])/mean(s1_ohdg_crea[seq(2,55,2)]) # subject 1 and creatinine-correction
sd(s2_ohdg_vol[seq(2,63,2)])/mean(s2_ohdg_vol[seq(2,63,2)]) # subject 2 and volume-correction
sd(s2_ohdg_crea[seq(2,63,2)])/mean(s2_ohdg_crea[seq(2,63,2)]) # subject 2 and creatinine-correction

# 12h-interval night
sd(s1_ohdg_vol[seq(1,55,2)])/mean(s1_ohdg_vol[seq(1,55,2)]) # subject 1 and volume-correction
sd(s1_ohdg_crea[seq(1,55,2)])/mean(s1_ohdg_crea[seq(1,55,2)]) # subject 1 and creatinine-correction
sd(s2_ohdg_vol[seq(1,63,2)])/mean(s2_ohdg_vol[seq(1,63,2)]) # subject 2 and volume-correction
sd(s2_ohdg_crea[seq(1,63,2)])/mean(s2_ohdg_crea[seq(1,63,2)]) # subject 2 and creatinine-correction 

# 24h-interval
mean(c(sd(s1_ohdg_vol[seq(1,54,2)] + s1_ohdg_vol[seq(2,54,2)])/mean(s1_ohdg_vol[seq(1,54,2)] + s1_ohdg_vol[seq(2,54,2)]), 
       sd(s1_ohdg_vol[seq(2,55,2)] + s1_ohdg_vol[seq(3,55,2)])/mean(s1_ohdg_vol[seq(2,55,2)] + s1_ohdg_vol[seq(3,55,2)]))) # subject 1 and volume-correction

mean(c(sd(s1_ohdg_crea[seq(1,54,2)] + s1_ohdg_crea[seq(2,54,2)])/mean(s1_ohdg_crea[seq(1,54,2)] + s1_ohdg_crea[seq(2,54,2)]), 
       sd(s1_ohdg_crea[seq(2,55,2)] + s1_ohdg_crea[seq(3,55,2)])/mean(s1_ohdg_crea[seq(2,55,2)] + s1_ohdg_crea[seq(3,55,2)]))) # subject 1 and creatinine-correction

mean(c(sd(s2_ohdg_vol[seq(1,62,2)] + s2_ohdg_vol[seq(2,62,2)])/mean(s2_ohdg_vol[seq(1,62,2)] + s2_ohdg_vol[seq(2,62,2)]), 
       sd(s2_ohdg_vol[seq(2,63,2)] + s2_ohdg_vol[seq(3,63,2)])/mean(s2_ohdg_vol[seq(2,63,2)] + s2_ohdg_vol[seq(3,63,2)]))) # subject 2 and volume-correction

mean(c(sd(s2_ohdg_crea[seq(1,62,2)] + s2_ohdg_crea[seq(2,62,2)])/mean(s2_ohdg_crea[seq(1,62,2)] + s2_ohdg_crea[seq(2,62,2)]), 
       sd(s2_ohdg_crea[seq(2,63,2)] + s2_ohdg_crea[seq(3,63,2)])/mean(s2_ohdg_crea[seq(2,63,2)] + s2_ohdg_crea[seq(3,63,2)]))) # subject 2 and creatinine-correction 

# 48h-interval
mean(c(sd((s1_ohdg_vol[seq(1,54,2)] + s1_ohdg_vol[seq(2,54,2)])[seq(1,26,2)] + (s1_ohdg_vol[seq(1,54,2)] + s1_ohdg_vol[seq(2,54,2)])[seq(2,26,2)])/mean((s1_ohdg_vol[seq(1,54,2)] + s1_ohdg_vol[seq(2,54,2)])[seq(1,26,2)] + (s1_ohdg_vol[seq(1,54,2)] + s1_ohdg_vol[seq(2,54,2)])[seq(2,26,2)]), 
       sd((s1_ohdg_vol[seq(1,54,2)] + s1_ohdg_vol[seq(2,54,2)])[seq(2,27,2)] + (s1_ohdg_vol[seq(1,54,2)] + s1_ohdg_vol[seq(2,54,2)])[seq(3,27,2)])/mean((s1_ohdg_vol[seq(1,54,2)] + s1_ohdg_vol[seq(2,54,2)])[seq(2,27,2)] + (s1_ohdg_vol[seq(1,54,2)] + s1_ohdg_vol[seq(2,54,2)])[seq(3,27,2)]), 
       sd((s1_ohdg_vol[seq(2,55,2)] + s1_ohdg_vol[seq(3,55,2)])[seq(1,26,2)] + (s1_ohdg_vol[seq(2,55,2)] + s1_ohdg_vol[seq(3,55,2)])[seq(2,26,2)])/mean((s1_ohdg_vol[seq(2,55,2)] + s1_ohdg_vol[seq(3,55,2)])[seq(1,26,2)] + (s1_ohdg_vol[seq(2,55,2)] + s1_ohdg_vol[seq(3,55,2)])[seq(2,26,2)]), 
       sd((s1_ohdg_vol[seq(2,55,2)] + s1_ohdg_vol[seq(3,55,2)])[seq(2,27,2)] + (s1_ohdg_vol[seq(2,55,2)] + s1_ohdg_vol[seq(3,55,2)])[seq(3,27,2)])/mean((s1_ohdg_vol[seq(2,55,2)] + s1_ohdg_vol[seq(3,55,2)])[seq(2,27,2)] + (s1_ohdg_vol[seq(2,55,2)] + s1_ohdg_vol[seq(3,55,2)])[seq(3,27,2)]))) # subject 1 and volume-correction

mean(c(sd((s1_ohdg_crea[seq(1,54,2)] + s1_ohdg_crea[seq(2,54,2)])[seq(1,26,2)] + (s1_ohdg_crea[seq(1,54,2)] + s1_ohdg_crea[seq(2,54,2)])[seq(2,26,2)])/mean((s1_ohdg_crea[seq(1,54,2)] + s1_ohdg_crea[seq(2,54,2)])[seq(1,26,2)] + (s1_ohdg_crea[seq(1,54,2)] + s1_ohdg_crea[seq(2,54,2)])[seq(2,26,2)]), 
       sd((s1_ohdg_crea[seq(1,54,2)] + s1_ohdg_crea[seq(2,54,2)])[seq(2,27,2)] + (s1_ohdg_crea[seq(1,54,2)] + s1_ohdg_crea[seq(2,54,2)])[seq(3,27,2)])/mean((s1_ohdg_crea[seq(1,54,2)] + s1_ohdg_crea[seq(2,54,2)])[seq(2,27,2)] + (s1_ohdg_crea[seq(1,54,2)] + s1_ohdg_crea[seq(2,54,2)])[seq(3,27,2)]), 
       sd((s1_ohdg_crea[seq(2,55,2)] + s1_ohdg_crea[seq(3,55,2)])[seq(1,26,2)] + (s1_ohdg_crea[seq(2,55,2)] + s1_ohdg_crea[seq(3,55,2)])[seq(2,26,2)])/mean((s1_ohdg_crea[seq(2,55,2)] + s1_ohdg_crea[seq(3,55,2)])[seq(1,26,2)] + (s1_ohdg_crea[seq(2,55,2)] + s1_ohdg_crea[seq(3,55,2)])[seq(2,26,2)]), 
       sd((s1_ohdg_crea[seq(2,55,2)] + s1_ohdg_crea[seq(3,55,2)])[seq(2,27,2)] + (s1_ohdg_crea[seq(2,55,2)] + s1_ohdg_crea[seq(3,55,2)])[seq(3,27,2)])/mean((s1_ohdg_crea[seq(2,55,2)] + s1_ohdg_crea[seq(3,55,2)])[seq(2,27,2)] + (s1_ohdg_crea[seq(2,55,2)] + s1_ohdg_crea[seq(3,55,2)])[seq(3,27,2)]))) # subject 1 and creatinine-correction

mean(c(sd((s2_ohdg_vol[seq(1,62,2)] + s2_ohdg_vol[seq(2,62,2)])[seq(1,30,2)] + (s2_ohdg_vol[seq(1,62,2)] + s2_ohdg_vol[seq(2,62,2)])[seq(2,30,2)])/mean((s2_ohdg_vol[seq(1,62,2)] + s2_ohdg_vol[seq(2,62,2)])[seq(1,30,2)] + (s2_ohdg_vol[seq(1,62,2)] + s2_ohdg_vol[seq(2,62,2)])[seq(2,30,2)]), 
       sd((s2_ohdg_vol[seq(1,62,2)] + s2_ohdg_vol[seq(2,62,2)])[seq(2,31,2)] + (s2_ohdg_vol[seq(1,62,2)] + s2_ohdg_vol[seq(2,62,2)])[seq(3,31,2)])/mean((s2_ohdg_vol[seq(1,62,2)] + s2_ohdg_vol[seq(2,62,2)])[seq(2,31,2)] + (s2_ohdg_vol[seq(1,62,2)] + s2_ohdg_vol[seq(2,62,2)])[seq(3,31,2)]), 
       sd((s2_ohdg_vol[seq(2,63,2)] + s2_ohdg_vol[seq(3,63,2)])[seq(1,30,2)] + (s2_ohdg_vol[seq(2,63,2)] + s2_ohdg_vol[seq(3,63,2)])[seq(2,30,2)])/mean((s2_ohdg_vol[seq(2,63,2)] + s2_ohdg_vol[seq(3,63,2)])[seq(1,30,2)] + (s2_ohdg_vol[seq(2,63,2)] + s2_ohdg_vol[seq(3,63,2)])[seq(2,30,2)]), 
       sd((s2_ohdg_vol[seq(2,63,2)] + s2_ohdg_vol[seq(3,63,2)])[seq(2,31,2)] + (s2_ohdg_vol[seq(2,63,2)] + s2_ohdg_vol[seq(3,63,2)])[seq(3,31,2)])/mean((s2_ohdg_vol[seq(2,63,2)] + s2_ohdg_vol[seq(3,63,2)])[seq(2,31,2)] + (s2_ohdg_vol[seq(2,63,2)] + s2_ohdg_vol[seq(3,63,2)])[seq(3,31,2)]))) # subject 2 and volume-correction

mean(c(sd((s2_ohdg_crea[seq(1,62,2)] + s2_ohdg_crea[seq(2,62,2)])[seq(1,30,2)] + (s2_ohdg_crea[seq(1,62,2)] + s2_ohdg_crea[seq(2,62,2)])[seq(2,30,2)])/mean((s2_ohdg_crea[seq(1,62,2)] + s2_ohdg_crea[seq(2,62,2)])[seq(1,30,2)] + (s2_ohdg_crea[seq(1,62,2)] + s2_ohdg_crea[seq(2,62,2)])[seq(2,30,2)]), 
       sd((s2_ohdg_crea[seq(1,62,2)] + s2_ohdg_crea[seq(2,62,2)])[seq(2,31,2)] + (s2_ohdg_crea[seq(1,62,2)] + s2_ohdg_crea[seq(2,62,2)])[seq(3,31,2)])/mean((s2_ohdg_crea[seq(1,62,2)] + s2_ohdg_crea[seq(2,62,2)])[seq(2,31,2)] + (s2_ohdg_crea[seq(1,62,2)] + s2_ohdg_crea[seq(2,62,2)])[seq(3,31,2)]), 
       sd((s2_ohdg_crea[seq(2,63,2)] + s2_ohdg_crea[seq(3,63,2)])[seq(1,30,2)] + (s2_ohdg_crea[seq(2,63,2)] + s2_ohdg_crea[seq(3,63,2)])[seq(2,30,2)])/mean((s2_ohdg_crea[seq(2,63,2)] + s2_ohdg_crea[seq(3,63,2)])[seq(1,30,2)] + (s2_ohdg_crea[seq(2,63,2)] + s2_ohdg_crea[seq(3,63,2)])[seq(2,30,2)]), 
       sd((s2_ohdg_crea[seq(2,63,2)] + s2_ohdg_crea[seq(3,63,2)])[seq(2,31,2)] + (s2_ohdg_crea[seq(2,63,2)] + s2_ohdg_crea[seq(3,63,2)])[seq(3,31,2)])/mean((s2_ohdg_crea[seq(2,63,2)] + s2_ohdg_crea[seq(3,63,2)])[seq(2,31,2)] + (s2_ohdg_crea[seq(2,63,2)] + s2_ohdg_crea[seq(3,63,2)])[seq(3,31,2)]))) # subject 2 and creatinine-correction

####################### Diurnal rhythm #######################

# t-test between day and night levels of urinary 8-OHdG
t.test(s1_ohdg_vol[seq(1,54,2)], s1_ohdg_vol[seq(2,54,2)], paired = TRUE)
t.test(s1_ohdg_crea[seq(1,54,2)], s1_ohdg_crea[seq(2,54,2)], paired = TRUE)
t.test(s2_ohdg_vol[seq(1,62,2)], s2_ohdg_vol[seq(2,62,2)], paired = TRUE)
t.test(s2_ohdg_crea[seq(1,62,2)], s2_ohdg_crea[seq(2,62,2)], paired = TRUE)

# check for trend in the time-series of urinary 8-OHdG
summary(lm(s1_ohdg_vol ~ c(1:55)))   
fit<-summary(lm(s1_ohdg_crea ~ c(1:55)))   
summary(lm(s2_ohdg_vol ~ c(1:63)))   
summary(lm(s2_ohdg_crea ~ c(1:63)))   

# autocorrelation functions for the time-series of urinary 8-OHdG
cor.test(s1_ohdg_vol[1:54], s1_ohdg_vol[2:55])
cor.test(fit$residuals[1:54], fit$residuals[2:55])
cor.test(s2_ohdg_vol[1:62], s2_ohdg_vol[2:63])
cor.test(s2_ohdg_crea[1:62], s2_ohdg_crea[2:63])


############################ Plots ############################

#series
s1_cr_l<-data.frame(c(1:63),s1_cr)
s1_exr_l<-data.frame(c(1:63),s1_exr)
s2_cr_l<-data.frame(c(1:55),s2_cr)
s2_exr_l<-data.frame(c(1:55),s2_exr)

l1<-ggplot(s1_cr_l,aes(c(1:63),s1_cr))+geom_line()+theme_classic()+theme(axis.text = element_text(size = 16),axis.title.x=element_text(size=18),axis.title.y = element_text(size=18),legend.text=element_text(size=14),plot.title = element_text(face = "bold", size=24, hjust = 0.5))+ylab("8-OHdG (ng/mg creatinine)")+xlab("Time (12h-intervals)")+scale_x_continuous(breaks = seq(1,126,by = 20))+ggtitle("Subject 1")
l2<-ggplot(s1_exr_l,aes(c(1:63),s1_exr))+geom_line()+theme_classic()+theme(axis.text = element_text(size = 16),axis.title.x=element_text(size=18),axis.title.y = element_text(size=18),legend.text=element_text(size=14))+ylab("8-OHdG (µg/h)")+xlab("Time (12h-intervals)")+scale_x_continuous(breaks = seq(1,112,by = 20))
l3<-ggplot(s2_cr_l,aes(c(1:55),s2_cr))+geom_line()+theme_classic()+theme(axis.text = element_text(size = 16),axis.title.x=element_text(size=18),axis.title.y = element_text(size=18),legend.text=element_text(size=14),plot.title = element_text(face = "bold", size=24, hjust = 0.5))+ylab("8-OHdG (ng/mg creatinine)")+xlab("Time (12h-intervals)")+scale_x_continuous(breaks = seq(1,55,by = 10))+ggtitle("Subject 2")
l4<-ggplot(s2_exr_l,aes(c(1:55),s2_exr))+geom_line()+theme_classic()+theme(axis.text = element_text(size = 16),axis.title.x=element_text(size=18),axis.title.y = element_text(size=18),legend.text=element_text(size=14))+ylab("8-OHdG (µg/h)")+xlab("Time (12h-intervals)")+scale_x_continuous(breaks = seq(1,63,by = 10))

series<-plot_grid(l1,l3,l2,l4,ncol=2,align="hv")
ggsave("series.jpg", series, dpi = 150, height = 10, width = 15)

#acf
acf_s2_cr<-acf(s2_cr_fit$residuals,lag.max = 6)
a <- data.frame(acf_s2_cr$acf[2:7],acf_s2_cr$lag[2:7],acf_s2_cr$acf[2:7])    
acf1 <- ggplot(a,aes(acf_s2_cr$lag[2:7],acf_s2_cr$acf[2:7]))+geom_bar(stat="identity",fill="grey",color="black",alpha=10,size=.8)+geom_hline(yintercept = c(0.19,-0.19),linetype="dashed",color="black",alpha=.8)+theme(axis.title.y = element_text(size=18, face="bold"), axis.title.x = element_text(size=18, face="bold",vjust = 2))+xlab("Lag")+ylab("ACF 8-OHdG (ng/mg creatinine)")+theme_classic()+theme(axis.text = element_text(size = 14),axis.title.x=element_text(size=18),axis.title.y = element_text(size=18),plot.title = element_text(face = "bold", size=24, hjust = 0.5))+ggtitle("Subject 2")+scale_x_continuous(breaks=c(1:6))

acf_s2_exr<-acf(s2_exr,lag.max = 6)
b <- data.frame(acf_s2_exr$acf[2:7],acf_s2_exr$lag[2:7],acf_s2_exr$acf[2:7])    
acf2 <- ggplot(b,aes(acf_s2_exr$lag[2:7],acf_s2_exr$acf[2:7]))+geom_bar(stat="identity",fill="grey",color="black",alpha=10,size=.8)+geom_hline(yintercept = c(0.19,-0.19),linetype="dashed",color="black",alpha=.8)+theme(axis.title.y = element_text(size=18, face="bold"), axis.title.x = element_text(size=18, face="bold",vjust = 2))+xlab("Lag")+ylab("ACF 8-OHdG (µg/h)")+theme_classic()+theme(axis.text = element_text(size = 14),axis.title.x=element_text(size=18),axis.title.y = element_text(size=18))+scale_x_continuous(breaks=c(1:6))

acf_s1_cr<-acf(s1_cr,lag.max = 6)
c <- data.frame(acf_s1_cr$acf[2:7],acf_s1_cr$lag[2:7],acf_s1_cr$acf[2:7])    
acf3 <- ggplot(c,aes(acf_s1_cr$lag[2:7],acf_s1_cr$acf[2:7]))+geom_bar(stat="identity",fill="grey",color="black",alpha=10,size=.8)+geom_hline(yintercept = c(0.19,-0.19),linetype="dashed",color="black",alpha=.8)+theme(axis.title.y = element_text(size=18, face="bold"), axis.title.x = element_text(size=18, face="bold",vjust = 2))+xlab("Lag")+ylab("ACF 8-OHdG (ng/mg creatinine)")+theme_classic()+theme(axis.text = element_text(size = 14),axis.title.x=element_text(size=18),axis.title.y = element_text(size=18),plot.title = element_text(face = "bold", size=24, hjust = 0.5))+ggtitle("Subject 1")+scale_x_continuous(breaks=c(1:6))

acf_s1_exr<-acf(s1_exr,lag.max = 6)
d <- data.frame(acf_s1_exr$acf[2:7],acf_s1_exr$lag[2:7],acf_s1_exr$acf[2:7])    
acf4 <- ggplot(d,aes(acf_s1_exr$lag[2:7],acf_s1_exr$acf[2:7]))+geom_bar(stat="identity",fill="grey",color="black",alpha=10,size=.8)+geom_hline(yintercept = c(0.19,-0.19),linetype="dashed",color="black",alpha=.8)+theme(axis.title.y = element_text(size=18, face="bold"), axis.title.x = element_text(size=18, face="bold",vjust = 2))+xlab("Lag")+ylab("ACF 8-OHdG (µg/h)")+theme_classic()+theme(axis.text = element_text(size = 14),axis.title.x=element_text(size=18),axis.title.y = element_text(size=18))+scale_x_continuous(breaks=c(1:6))

acfs <- plot_grid(acf3,NULL,acf1,acf4,NULL,acf2,ncol=3, rel_widths = c(1, 0.05, 1))
ggsave("acfs.jpg", acfs, dpi = 150, height = 10, width = 13)

#rain
s1.1 <- data_1x1( 
  array_1 = s2_ohdg_crea[c(seq(1,63,2))],
  array_2 = s1_cr[c(seq(2,63,2))],
  jit_distance = .09,
  jit_seed = 321)

s1.2 <- data_1x1( 
  array_1 = s1_exr[c(seq(1,63,2))],
  array_2 = s1_exr[c(seq(2,63,2))],
  jit_distance = .09,
  jit_seed = 321)

s2.1 <- data_1x1( 
  array_1 = s2_cr[c(seq(1,55,2))],
  array_2 = s2_cr[c(seq(2,55,2))],
  jit_distance = .09,
  jit_seed = 321)

s2.2 <- data_1x1( 
  array_1 = s2_exr[c(seq(1,55,2))],
  array_2 = s2_exr[c(seq(2,55,2))],
  jit_distance = .09,
  jit_seed = 321)

a<-raincloud_1x1_repmes(
  data = s1.1,
  line_color = 'gray',
  col = (c("blue", "red")),
  fill = (c("blue", "red")),
  line_alpha = .3,
  size = 1.5,
  alpha = .5,
  align_clouds = FALSE) +
  scale_x_continuous(breaks=c(1,2), labels=c("Night", "Day"), limits=c(0, 3)) +
  ylab("8-OHdG (ng/mg creatinine)") +
  xlab("") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 14), plot.title = element_text(size=18, face = "bold", hjust = 0.5))+
  labs(title = "Subject 1")

b<-raincloud_1x1_repmes(
  data = s1.2,
  line_color = 'gray',
  col = (c("blue", "red")),
  fill = (c("blue", "red")),
  line_alpha = .3,
  size = 1.5,
  alpha = .5,
  align_clouds = FALSE) +
  scale_x_continuous(breaks=c(1,2), labels=c("Night", "Day"), limits=c(0, 3)) +
  ylab("8-OHdG (µg/h)") +
  xlab("") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 14)) 

c<-raincloud_1x1_repmes(
  data = s2.1,
  line_color = 'gray',
  col = (c("blue", "red")),
  fill = (c("blue", "red")),
  line_alpha = .3,
  size = 1.5,
  alpha = .5,
  align_clouds = FALSE) +
  scale_x_continuous(breaks=c(1,2), labels=c("Night", "Day"), limits=c(0, 3)) +
  ylab("8-OHdG (ng/mg creatinine)") +
  xlab("") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 12), plot.title = element_text(size=18, face = "bold", hjust = 0.5), axis.title.y = element_text(size = 14))+
  labs(title = "Subject 2")

d<-raincloud_1x1_repmes(
  data = s2.2,
  line_color = 'gray',
  col = (c("blue", "red")),
  fill = (c("blue", "red")),
  line_alpha = .3,
  size = 1.5,
  alpha = .5,
  align_clouds = FALSE) +
  scale_x_continuous(breaks=c(1,2), labels=c("Night", "Day"), limits=c(0, 3)) +
  ylab("8-OHdG (µg/h)") +
  xlab("") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 14))

rain<-plot_grid(a,c,b,d,ncol=2,align="hv")
ggsave("rain.jpg", rain, dpi = 150, height = 7, width = 9)


#acf
acf_s1_vol <- data.frame(a = s1_ohdg_vol[1:54], b = s1_ohdg_vol[2:55])
acf1 <- ggplot(acf_s1_vol, aes(b, a))+geom_point()+theme_bw()+geom_smooth(method = "lm")+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

acf_s1_crea <- data.frame(a = s1_ohdg_crea[1:54], b = s1_ohdg_crea[2:55])
acf2 <- ggplot(acf_s1_crea, aes(b, a))+geom_point()+theme_bw()+geom_smooth(method = "lm")+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

acf_s2_vol <- data.frame(a = s2_ohdg_vol[1:62], b = s2_ohdg_vol[2:63])
acf3 <- ggplot(acf_s2_vol, aes(b, a))+geom_point()+theme_bw()+geom_smooth(method = "lm")+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

acf_s2_crea <- data.frame(a = s2_ohdg_crea[1:54], b = s2_ohdg_crea[2:55])
acf4 <- ggplot(acf_s2_crea, aes(b, a))+geom_point()+theme_bw()+geom_smooth(method = "lm")+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

acfs <- plot_grid(acf4,acf2,acf3,acf1,ncol=2,align="hv",labels = "AUTO", hjust = -1.5, vjust =23)

ggsave("acf.jpg", acfs, dpi = 150, height = 7, width = 7)

