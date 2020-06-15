# Learning, I really need to learn 

#María Fernanda Meneses González 
#Assignment 4
#Topic: Regression Discontinuity Design

#Clear everything
rm(list = ls()) 

#Required packages
library(data.table)
library(foreign)
library(haven)
library(lubridate)
library(rdd)
library(dplyr)
library(ggplot2)
library(stargazer)
library(gridExtra)

# Set directories 
Input  = "C:/Users/Mafito/Documents/Mafe/Research Design/Tareas/4. Assignment 4/Mafe/Input"
Output =  "C:/Users/Mafito/Documents/Mafe/Research Design/Tareas/4. Assignment 4/Mafe/Output"
setwd(Input)

# We upload the information 
Data = read_stata("hansen_dwi.dta")
Data = as.data.table(Data)

#3. Dummy creation
Data[,Dummy:=0] Data[bac1>=0.08,Dummy:=1]
num.Dummy = sum(Data$Dummy)

#4.a McCrary test 

#Density graph
Test      = rdd::DCdensity(Data$bac1,cutpoint = 0.08,ext.out = T, htest = T, plot = T)
Data_test = Test$data 
Data_test = Data_test %>% mutate(cellval=ifelse(cellval==0,NA,cellval))

ggplot(Data_test, mapping = aes(x = cellmp, y = cellval))+ #Todo para el scatterplot 
geom_point(shape=15, color="purple")+ geom_col()+ labs(title = "Punishment and deterrence", subtitle ="McCrary Density test",x = "BAC", y = "Density")+
geom_smooth(se = F, size = .4, color="black")+ geom_vline(xintercept = 0.08, linetype = "dashed")+ theme_gray()
ggsave("McCrary1.pdf", height = 5, width = 9)

#Test - Regression of McCray 
Test2 = rdd::DCdensity(runvar = Data$bac1,cutpoint = 0.08, ext.out = T, htest = T, plot = T,bin = 0.001) 
Variables <- c("white","male","aged","acc")
BW <- 0.051

results <- list()
Data[,M:=bac1*Dummy]

m1 = lm(formula = Data$white ~ Data$Dummy + Data$bac1 + Data$M)
m2 = lm(formula = Data$male ~ Data$Dummy + Data$bac1 + Data$M)
m3 = lm(formula = Data$aged ~ Data$Dummy + Data$bac1 + Data$M)
m4 = lm(formula = Data$acc ~ Data$Dummy + Data$bac1 + Data$M)

dm1 = coeftest(m1, vcov = vcovHC(m1,"HC1"))
dm2 = coeftest(m1, vcov = vcovHC(m2,"HC1"))
dm3 = coeftest(m1, vcov = vcovHC(m3,"HC1"))
dm4 = coeftest(m1, vcov = vcovHC(m4,"HC1"))
stargazer(m1,m2,m3,m4,out = "D.tex",  title = "My iris models")

#Balance Graphs
bin <- cut(Data$bac1, seq(min(Data$bac1),max(Data$bac1),0.0021)) #bins = 0.002 as in the paper
data_bin <- aggregate(Data,list(bin),function(x) { return(c(mean(x),length(x)))})
#Accident
ggacc <- ggplot() + 
  geom_point(data = data_bin, aes(x=bac1[,1],y=acc[,1], alpha=.5)) + 
  stat_smooth(data = Data, aes(x = bac1, y = acc, color = factor(Dummy), group = factor(Dummy)), size = 0.5, method = lm)+
  geom_vline(xintercept = 0.08, linetype = "longdash")+
  scale_x_continuous(name = "BAC", limits = c(0,0.2))+
  scale_y_continuous(name = "")+
  coord_cartesian(ylim=c(0.05,0.25)) +
  scale_alpha_continuous(name = "BAC", breaks = "0.5", labels = "")+
  scale_color_manual(values = c("royalblue4","dodgerblue"), name = "", breaks = c("0","1"), labels = c("Control Fit","Treatment Fit"))+
  labs(title = "Panel A. Accident at scene")+
  theme_classic()+
  theme(legend.position = "bottom")
#Male
ggmale <- ggplot() + 
  geom_point(data = data_bin, aes(x=bac1[,1],y=male[,1], alpha=.5)) + 
  stat_smooth(data = Data, aes(x = bac1, y = male, color = factor(Dummy), group = factor(Dumyy)), size = 0.5, method = lm)+
  geom_vline(xintercept = 0.08, linetype = "longdash")+
  scale_x_continuous(name = "BAC", limits = c(0,0.2))+
  scale_y_continuous(name = "")+
  coord_cartesian(ylim=c(0.74,0.82)) +
  scale_alpha_continuous(name = "BAC", breaks = "0.5", labels = "")+
  scale_color_manual(values = c("royalblue4","dodgerblue"), name = "", breaks = c("0","1"), labels = c("Control Fit","Treatment Fit"))+
  labs(title = "Panel B. Male")+
  theme_classic()+
  theme(legend.position = "bottom")
#Age
ggage <- ggplot() + 
  geom_point(data = data_bin, aes(x=bac1[,1],y=aged[,1], alpha=.5)) + 
  stat_smooth(data = Data, aes(x = bac1, y = aged, color = factor(Dummy), group = factor(Dummy)), size = 0.5, method = lm)+
  geom_vline(xintercept = 0.08, linetype = "longdash")+
  scale_x_continuous(name = "BAC", limits = c(0,0.2))+
  scale_y_continuous(name = "")+
  coord_cartesian(ylim=c(33,39)) +
  scale_alpha_continuous(name = "BAC", breaks = "0.5", labels = "")+
  scale_color_manual(values = c("royalblue4","dodgerblue"), name = "", breaks = c("0","1"), labels = c("Control Fit","Treatment Fit"))+
  labs(title = "Panel C. Age")+
  theme_classic()+
  theme(legend.position = "bottom")
#White
ggwhite <- ggplot() + 
  geom_point(data = data_bin, aes(x=bac1[,1],y=white[,1], alpha=.5)) + 
  stat_smooth(data = Data, aes(x = bac1, y = white, color = factor(Dummy), group = factor(Dummy)), size = 0.5, method = lm)+
  geom_vline(xintercept = 0.08, linetype = "longdash")+
  scale_x_continuous(name = "BAC", limits = c(0,0.2))+
  scale_y_continuous(name = "")+
  coord_cartesian(ylim=c(0.8,0.9)) +
  scale_alpha_continuous(name = "BAC", breaks = "0.5", labels = "")+
  scale_color_manual(values = c("royalblue4","dodgerblue"), name = "", breaks = c("0","1"), labels = c("Control Fit","Treatment Fit"))+
  labs(title = "Panel D. White")+
  theme_classic()+
  theme(legend.position = "bottom")
#Linear fit group
ggarrange(ggacc,ggmale,ggage,ggwhite,ncol = 2, nrow = 2)
ggsave("OMG.pdf", height = 6, width = 9)

#Accident
ggacc <- ggplot() + 
  geom_point(data = data_bin, aes(x=bac1[,1],y=acc[,1], alpha=.5)) + 
  stat_smooth(data = dt, aes(x = bac1, y = acc, color = factor(DUI), group = factor(DUI)), size = 0.5, method = lm, formula = y ~ poly(x,2))+
  geom_vline(xintercept = 0.08, linetype = "longdash")+
  scale_x_continuous(name = "BAC", limits = c(0,0.2))+
  scale_y_continuous(name = "")+
  coord_cartesian(ylim=c(0.05,0.25)) +
  scale_alpha_continuous(name = "BAC", breaks = "0.5", labels = "")+
  scale_color_manual(values = c("royalblue4","dodgerblue"), name = "", breaks = c("0","1"), labels = c("Control QFit","Treatment QFit"))+
  labs(title = "Panel A. Accident at scene")+
  theme_classic()+
  theme(legend.position = "bottom")
#Male
ggmale <- ggplot() + 
  geom_point(data = data_bin, aes(x=bac1[,1],y=male[,1], alpha=.5)) + 
  stat_smooth(data = dt, aes(x = bac1, y = male, color = factor(DUI), group = factor(DUI)), size = 0.5, method = lm, formula = y ~ poly(x,2))+
  geom_vline(xintercept = 0.08, linetype = "longdash")+
  scale_x_continuous(name = "BAC", limits = c(0,0.2))+
  scale_y_continuous(name = "")+
  coord_cartesian(ylim=c(0.74,0.82)) +
  scale_alpha_continuous(name = "BAC", breaks = "0.5", labels = "")+
  scale_color_manual(values = c("royalblue4","dodgerblue"), name = "", breaks = c("0","1"), labels = c("Control QFit","Treatment QFit"))+
  labs(title = "Panel B. Male")+
  theme_classic()+
  theme(legend.position = "bottom")
#Age
ggage <- ggplot() + 
  geom_point(data = data_bin, aes(x=bac1[,1],y=aged[,1], alpha=.5)) + 
  stat_smooth(data = dt, aes(x = bac1, y = aged, color = factor(DUI), group = factor(DUI)), size = 0.5, method = lm, formula = y ~ poly(x,2))+
  geom_vline(xintercept = 0.08, linetype = "longdash")+
  scale_x_continuous(name = "BAC", limits = c(0,0.2))+
  scale_y_continuous(name = "")+
  coord_cartesian(ylim=c(33,39)) +
  scale_alpha_continuous(name = "BAC", breaks = "0.5", labels = "")+
  scale_color_manual(values = c("royalblue4","dodgerblue"), name = "", breaks = c("0","1"), labels = c("Control QFit","Treatment QFit"))+
  labs(title = "Panel C. Age")+
  theme_classic()+
  theme(legend.position = "bottom")
#White
ggwhite <- ggplot() + 
  geom_point(data = data_bin, aes(x=bac1[,1],y=white[,1], alpha=.5)) + 
  stat_smooth(data = dt, aes(x = bac1, y = white, color = factor(DUI), group = factor(DUI)), size = 0.5, method = lm, formula = y ~ poly(x,2))+
  geom_vline(xintercept = 0.08, linetype = "longdash")+
  scale_x_continuous(name = "BAC", limits = c(0,0.2))+
  scale_y_continuous(name = "")+
  coord_cartesian(ylim=c(0.8,0.9)) +
  scale_alpha_continuous(name = "BAC", breaks = "0.5", labels = "")+
  scale_color_manual(values = c("royalblue4","dodgerblue"), name = "", breaks = c("0","1"), labels = c("Control QFit","Treatment QFit"))+
  labs(title = "Panel D. White")+
  theme_classic()+
  theme(legend.position = "bottom")
#Linear fit group
ggarrange(ggacc,ggmale,ggage,ggwhite,ncol = 2, nrow = 2)
ggsave("Figures/CovBalanceQuad.pdf", height = 6, width = 9)


