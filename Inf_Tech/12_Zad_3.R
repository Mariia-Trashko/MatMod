dat12=read.csv("3_data_var12.csv",header = TRUE,sep = ";",dec = ",")
length(dat12$Hum)
# 36
length(dat12$PhOx)
# 36
mode(dat12$Hum)
# "numeric"
mode(dat12$PhOx)
# "numeric"
min(dat12$Hum)
# 1.52
min(dat12$PhOx)
# 1
varH= var(dat12$Hum); varH
# 0.05512857
varP= var(dat12$PhOx); varP
# 39.43492
mH = mean(dat12$Hum); mH
# 2.008333
mP = mean(dat12$PhOx); mP
# 14.77778
sum(dat12$Hum)/length(dat12$Hum)
# 2.008333
sum(dat12$PhOx)/length(dat12$PhOx)
# 14.77778
sH=sd(dat12$Hum); sH
# 0.2347947
sP=sd(dat12$PhOx); sP
# 6.279723
sqrt(varH)
# 0.2347947
sqrt(varP)
# 6.279723
vH=100*sH/mH; vH
# 11.69102
vP=100*sP/mP; vP
# 42.49437
summary(dat12)
# Hum             PhOx      
# Min.   :1.520   Min.   : 1.00  
# 1st Qu.:1.880   1st Qu.:11.75  
# Median :2.030   Median :14.50  
# Mean   :2.008   Mean   :14.78  
# 3rd Qu.:2.163   3rd Qu.:19.50  
# Max.   :2.520   Max.   :26.00  
quantile(dat12$Hum,.50)
# 50% 
# 2.03
quantile(dat12$PhOx,.50)
# 50% 
# 14.5 
quantile(dat12$Hum, c(.32, .57, .98))
# 32%    57%    98% 
# 1.9160 2.0395 2.4360 
quantile(dat12$PhOx, c(.32, .57, .98))
# 32%  57%  98% 
# 12.2 15.0 25.3 
kvart=c(0,0.1,0.25,0.5,0.75,0.9,1)
resH=quantile(dat12$Hum, kvart)
plot(resH, kvart, type= "b", main = "График квантилей", xlab ="Содержание гумуса", ylab ="квантили", col ="red")
resP=quantile(dat12$PhOx, kvart)
plot(resP, kvart, type= "b", main = "График квантилей", xlab ="Содержание оксида фосфора", ylab ="квантили", col ="red")
Y=1: length(dat12$Hum); Y
Y=Y/length(dat12$Hum); Y
XH = sort(dat12$Hum); XH
plot(XH, Y, type= "b", main = "Огива", xlab ="Содержание гумуса", ylab ="Частоты", col ="yellow")
XP = sort(dat12$PhOx); XP
plot(XP, Y, type= "b", main = "Огива", xlab ="Содержание оксида фосфора", ylab ="Частоты", col ="yellow")
boxplot(dat12$Hum, main = "Диаграмма размаха Гумус", col="yellow",ylab="Содержание гумуса")
boxplot(dat12$PhOx, main = "Диаграмма размаха Оксид фосфора", col="blue",ylab="Содержание оксида фосфора")
