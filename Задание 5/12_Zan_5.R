#Отчет студента Трашко М., вариант 12
# задание 5
#очистим полностью память
rm(list=ls())

# считаем файл и распечатаем его содержимое
dat12=read.csv("5_data_var12.csv", header=TRUE, sep=";", dec=",")
dat12

# проверим размерность данных
dim(dat12)

# посмотрим имена
names(dat12)

# #####################################################
# #######Содержание гумуса
# среднее арифметическое сохраним в переменную mhum
mhum=mean(dat12$Hum12); mhum

# стандартное отклонение сохраним в переменную shum
shum=sd(dat12$ Hum12); shum

# построение коробочки с усиками
boxplot(dat12$Hum12, main = "Диаграмма размаха Гумус", outline = TRUE, col="blue")

#Проверка данных на нормальностьc с помощью критерия хи-квадрат ( Пирсона)
pearson.test(dat12$ Hum12)
pearson.test(dat12$ Hum12,15)
# Pearson chi-square normality test
# 
# data:  dat12$Hum12
# P = 20.56, p-value = 0.05721
pearson.test(dat12$ Hum12,7)
# Pearson chi-square normality test
# 
# data:  dat12$Hum12
# P = 1.396, p-value = 0.8449

lillie.test(dat12$ Hum12)
#Performs the Shapiro-Wilk test of normality.
shapiro.test(dat12$ Hum12)

##Гистограмма для 15 классов
hist(dat12$Hum12, main = "Гистограмма для содержания гумуса на поле 12", ylab="Плотность вероятности (Density)", xlab="Интервалы классов показателя содержание гумуса, %", prob=TRUE, breaks =15, col="light blue")

curve(dnorm(x, mean=mhum, sd=shum), col ="red", add=TRUE)

##Гистограмма для 7 классов
hist(dat12$Hum12, main = "Гистограмма для содержания гумуса на поле 12", ylab="Плотность вероятности (Density)", xlab="Интервалы классов показателя содержание гумуса, %", prob=TRUE, breaks =7, col="light blue")

curve(dnorm(x, mean=mhum, sd=shum), col ="red", add=TRUE)

# наложим на равномерное распределение
hist(dat12$Hum12, main = "Гистограмма для содержания гумуса на поле 12", ylab="Плотность вероятности (Density)", xlab="Интервалы классов показателя содержание гумуса, %", prob=TRUE, breaks =15, col="light blue")
curve( dunif(x, min(dat12$Hum12)+0.01 , max(dat12$Hum12)-0.01), col ="red", add=TRUE)

# нарисовать график на нормальной вероятностной бумаге
qqnorm(dat12$Hum12, datax=TRUE)
qqline(dat12$Hum12, datax=TRUE)

# #####################################################
# #######Содержание подвижного фосфора
# среднее арифметическое сохраним в переменную mpo
mpo=mean(dat12$P2O512); mpo

# стандартное отклонение сохраним в переменную spo
spo=sd(dat12$P2O512); spo

# построение коробочки с усиками
boxplot(dat12$P2O512, main = "Диаграмма размаха Подвижный Фосфор", outline = TRUE, col="blue")

#Проверка данных на нормальностьc с помощью критерия хи-квадрат ( Пирсона)
pearson.test(dat12$ P2O512)
pearson.test(dat12$ P2O512,15)
# Pearson chi-square normality test
# 
# data:  dat12$P2O512
# P = 215.98, p-value < 2.2e-16
pearson.test(dat12$ P2O512,7)
# Pearson chi-square normality test
# 
# data:  dat12$P2O512
# P = 171.41, p-value < 2.2e-16

lillie.test(dat12$ P2O512)

#Performs the Shapiro-Wilk test of normality.
shapiro.test(dat12$ P2O512)

##Гистограмма для 15 классов
hist(dat12$P2O512, main = "Гистограмма для содержания подвижного фосфора на поле 12", ylab="Плотность вероятности (Density)", xlab="Интервалы классов показателя содержание подвижного фосфора, мг/кг", prob=TRUE, breaks =15, col="light blue")

curve(dnorm(x, mean=mpo, sd=spo), col ="red", add=TRUE)

##Гистограмма для 7 классов
hist(dat12$P2O512, main = "Гистограмма для содержания подвижного фосфора на поле 12", ylab="Плотность вероятности (Density)", xlab="Интервалы классов показателя содержание подвижного фосфора, мг/кг", prob=TRUE, breaks =7, col="light blue")

curve(dnorm(x, mean=mpo, sd=spo), col ="red", add=TRUE)

# наложим на равномерное распределение
hist(dat12$P2O512, main = "Гистограмма для содержания подвижного фосфора на поле 12", ylab="Плотность вероятности (Density)", xlab="Интервалы классов показателя содержание подвижного фосфора, мг/кг", prob=TRUE, breaks =15, col="light blue")
curve( dunif(x, min(dat12$P2O512)+0.01 , max(dat12$P2O512)-0.01), col ="red", add=TRUE)

# нарисовать график на нормальной вероятностной бумаге
qqnorm(dat12$P2O512, datax=TRUE)
qqline(dat12$P2O512, datax=TRUE)
