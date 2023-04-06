# Отчет 7. Дисперсионный анализ
# Студент Трашко М.Д.
# Кафедра физики почв. Вариант 12

adat = read.csv("data12.csv", sep=";",dec=",")
adat
# проверим размерность
dim(adat)
# посмотрим имена
names(adat)
## Выведем только одну переменную с урожаем
adat$Yield
#проверим структуру файла
str(adat)
# Превратим переменые Herbizide и Fertilize в группирующую качественную переменную
adat$Herbizide=factor(adat$Herbizide)
adat$Fertilize=factor(adat$Fertilize)
# Проверим однородность дисперсий - условие применимости ДА
bartlett.test(Yield ~ Herbizide, adat)
bartlett.test(Yield ~ Fertilize, adat)
# Построение однофакторной модели ДА , фак1-
model_herb=lm(Yield ~ Herbizide, dat = adat); model_herb
anova(model_herb)
# Построение однофакторной модели ДА, фактор 2
model_fert=lm(Yield ~ Fertilize, dat = adat); model_fert
anova(model_fert)
# Построение двухфакторной модели ДА без взаимодействия
model_lm=lm(Yield ~ Herbizide+Fertilize, dat = adat); model_lm
anova(model_lm)

# Построение графика средних
# Mean plots
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
# library("ggpubr")
# ggline(adat, x = " Herbizide ", y = " Yield ", add = c("mean_se", "jitter"))

