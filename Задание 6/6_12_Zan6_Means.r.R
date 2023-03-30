dat12=read.csv("6data_var12.csv", header=TRUE, sep=";", dec=",")
dat12
dat12= dat12[,1:6]
dat12
# проверим размерность данных
dim(dat12)
# посмотрим имена
names(dat12)
# проверим, что данные имеют правильный тип
summary(dat12)
#Построение коробочки с усиками для 3 полей
boxplot(dat12[,1:3], col =c("aquamarine4","azure4","bisque3"), outline = TRUE)
# Построение графиков на нормальной вероятносной бумаге
par(mfrow=c(1,3))
qqnorm(dat12$Hum12, main = "Гумус 12", datax=TRUE)
qqline(dat12$Hum12, datax=TRUE)
qqnorm(dat12$Hum17, main = "Гумус 17", datax=TRUE)
qqline(dat12$Hum17, datax=TRUE)
qqnorm(dat12$Hum22, main = "Гумус 22", datax=TRUE)
qqline(dat12$Hum22, datax=TRUE)
par(mfrow=c(1,1))
# Тест на нормальность распределения
shapiro.test(dat12$Hum12)
shapiro.test(dat12$Hum17)
shapiro.test(dat12$Hum22)
#Сравнение средних по t-критерию
t.test(dat12$Hum12, dat12$Hum17)
t.test(dat12$Hum12, dat12$Hum22)
t.test(dat12$Hum22, dat12$Hum17)
# построение коробочки с усиками для 2 подвыборок 1 переменной
# с выбросами
boxplot(Weeds12 ~ Teacher, data = dat12, outline = TRUE)
# без выбросов
boxplot(Weeds12 ~ Teacher, data = dat12, outline = FALSE)
# c 95 % доверительными интервалами для медианы
boxplot(Weeds12 ~ Teacher, data = dat12, notch = TRUE)
# Выберем данные, где студенты работали с учителем
weeds1= dat12$Weeds12[dat12$Teacher== 1]
#Выберем данные, где студенты работали БЕЗ учителя
weeds2= dat12$Weeds12[dat12$Teacher== 2]
# Тест Шапиро-Уилка
shapiro.test(weeds1)
shapiro.test(weeds2)
#использование непараметрического теста
wilcox.test(Weeds12 ~ Teacher, data = dat12)
# Получение статистических данных с помощью функции
tapply(dat12$Weeds12,dat12$Teacher,length)
tapply(dat12$Weeds12,dat12$Teacher,mean)
tapply(dat12$Weeds12,dat12$Teacher,median)

