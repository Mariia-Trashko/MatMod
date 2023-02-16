# Задание 2.4

novob12=read.table("Trashko.csv",header = TRUE, sep = ";",dec = ".")
dim(novob12)
summary(novob12)
novob12$Pole
novob12$Humus
novob12$K2O
novob12$A1
names(novob12)
plot(novob12$Humus,novob12$K2O)
write.table (novob12, "novob.csv", sep = ";", dec = ",", quote=FALSE)
