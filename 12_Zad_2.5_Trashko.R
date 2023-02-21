# Задание 2.5

W12=read.table("Tras12.csv",header = TRUE, sep = ";",dec = ",")

dim(W12)
summary(W12)
names(W12)

#Mw-вес влаги
Mw=W12$Mbsw-W12$Mbs
#Ms-вес сухой почвы
Ms=W12$Mbs-W12$Mb
# Moisture-влажность
Moisture=100*Mw/Ms
Moisture
write.table (Moisture, "Moist12.csv", sep = ";", dec = ",", quote=FALSE)