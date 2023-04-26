# Отчет 9. Регрессионный анализ
# Студент Трашко М.Д.
# Вариант 12

#очистим полностью память
rm(list=ls())

# Считаем данные в переменную rdat из файла data9_var101.csv.
rdat = read.csv("data9_var12.csv", sep=";",dec=",")
rdat

# проверим размерность
dim(rdat)

# посмотрим имена
names(rdat)

## Выведем только одну переменную с урожаем
rdat$yield

#расчет коэффициентов корреляции
cor(rdat, method="pearson")

library(ellipse)
plotcorr(cor(rdat))# образ в виде эллипсов
plotcorr(cor(rdat), type="lower")
plotcorr(cor(rdat), type="upper")

# Задание модели для одномерного регрессионного анализа по переменной
# с наибольшей корреляцией
model1 = lm(yield~p2o5, data = rdat); model1

#Краткая информация о модели
summary(model1)

# таблица дисперсионного анализа
anova(model1)

# Модель для многомерного регрессионного анализа
model2 = lm(yield~k2o+no3+ph+p2o5+hum, data = rdat)
model2
summary(model2)

# Модель для трехмерного регрессионного анализа
model3 = lm(yield~k2o+no3+p2o5, data = rdat)
model3
summary(model3)
