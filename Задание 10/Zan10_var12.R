# Отчет 10. Регрессионный анализ. Анализ остатков
# Студент Трашко М.Д.
# Вариант 12

#очистим полностью память
rm(list=ls())

# Считаем данные в переменную rdat из файла data10_var12.csv.
rdat = read.csv("data10_var12.csv", sep=";",dec=",")
rdat

# проверим размерность
dim(rdat)

# посмотрим имена
names(rdat)

# Выведем только одну переменную с урожаем
rdat$yield

# Построим матрицу диаграмм размаха для всех переменных из объекта rdat.
plot(rdat)

#красивые графики для визуализации корреляций
library(GGally)
ggpairs(rdat)

# Задание модели для одномерного регрессионного анализа по переменной
# с наибольшей корреляцией
model1 = lm(yield~p2o5, data = rdat); model1

#Краткая информация о модели
summary(model1)

# С помощью команды str можно посмотреть, что в объекте model1 находится
# список (list) из 12 объектов. Первый объект - это coefficients, то есть коэффиценты
# регрессионного уравнения.
str(model1)
#посмотрим эти объекты
model1$coefficients[1]
model1$coefficients[2]

# Выведем вектор предсказанных значений
model1$fitted.values
# Построим график наблюдаемых от предсказанных значений урожая
plot(model1$fitted.values,rdat$yield)
#Добавим линию y=x
abline(a=0, b =1, col="red")

#выведем остатки
model1$residuals

#Найдем сумму остатков
sum(model1$residuals)

#Найдем сумму квадратов остатков
SSR_mod1=sum (model1$residuals^2); SSR_mod1

#RMSE
RMSE_mod1 = sqrt(SSR_mod1/length(rdat$yield)); RMSE_mod1

#AIC
AIC(model1)

# Построим график остатков от наблюдаемых значений урожая
plot( rdat$yield, model1$residuals)
# Нарисуем красным линию, идущую параллельно оси x
abline(a=0, b =0, col="red")
#для поиска коэффициентов для линии зададим модель, связывающую
#остатки и урожай
mo1=lm(model1$residuals~rdat$yield)
abline(a=mo1$coefficients[1], b =mo1$coefficients[2], col="blue", main = "График зависимости остатков от наблюдаемых значений урожая")

# Построим график на нормальной вероятностной бумаге.
library("car")
qqPlot(model1$residuals)

# Задание модели для многомерного регрессионного анализа
model2 = lm(yield~k2o+no3+ph+p2o5+hum, data = rdat)
summary(model2)
plot(model2$fitted.values,rdat$yield)
abline(a=0, b =1, col="red")
model2$residuals
sum(model2$residuals)
SSR_mod2=sum (model2$residuals^2); SSR_mod2
RMSE_mod2 = sqrt(SSR_mod2/length(rdat$yield)); RMSE_mod2
AIC(model2)
plot( rdat$yield, model2$residuals)
abline(a=0, b =0, col="red")
mo2=lm(model2$residuals~rdat$yield)
abline(a=mo2$coefficients[1], b =mo2$coefficients[2], col="blue", main = "График зависимости остатков от наблюдаемых значений урожая")
qqPlot(model2$residuals)

# Задание модели для трехмерного регрессионного анализа
model3 = lm(yield~k2o+no3+p2o5, data = rdat)
summary(model3)
plot(model3$fitted.values,rdat$yield)
abline(a=0, b =1, col="red")
model3$residuals
sum(model3$residuals)
SSR_mod3=sum (model3$residuals^2); SSR_mod3
RMSE_mod3 = sqrt(SSR_mod3/length(rdat$yield)); RMSE_mod3
AIC(model3)
plot(rdat$yield, model3$residuals)
abline(a=0, b =0, col="red")
mo3=lm(model3$residuals~rdat$yield)
abline(a=mo3$coefficients[1], b =mo3$coefficients[2], col="blue", main = "График зависимости остатков от наблюдаемых значений урожая")
qqPlot(model3$residuals)
