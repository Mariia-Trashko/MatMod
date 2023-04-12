# Отчет 8. Дисперсионный анализ. Сравнение средних
# Студент Трашко М.Д.
# Кафедра физики почв. Вариант 12

# Считаем данные в переменную adat из файла data9_var100.csv.
adat = read.csv("data8_var12.csv", sep=";",dec=".")
adat
# проверим размерность
dim(adat)
# посмотрим имена
names(adat)
# Выведем только одну переменную с урожаем
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
# Проведем двухфакторный анализ
# Построение модели ДА 1- способ
model_lm=lm(Yield ~ Herbizide+Fertilize, dat = adat); model_lm
anova(model_lm)
# построение модели и ДА 2-ой способ.
# Используется при сравнении средних по методу Тьюки и по НСР
model = aov(Yield ~ Herbizide+Fertilize, dat = adat)
model
anova(model)
# Рассчитаем средние по градациям каждого фактора
tapply(adat$Yield,adat$Herbizide,mean)
tapply(adat$Yield,adat$Fertilize,mean)
# Построим диаграммы рассеивания (коробочки с усиками)
boxplot(Yield ~ Herbizide, data=adat)
boxplot(Yield ~ Fertilize, data=adat)
boxplot(Yield ~ Fertilize, data=adat, col = "green", names = c("Control", "NPK30", "NPK60", "NPK90", "NPK120"))
library("agricolae")
#Сравним средние по градациям фактора по критерию НСР
out_herb=LSD.test(model,"Herbizide", alpha = 0.05, p.adj="none")
out_herb
out_fert=LSD.test(model,"Fertilize", p.adj="none")
out_fert
# Сравнение средних по градациям факторов с помощью критерия Тьюки
TukeyHSD(model, which = c("Herbizide","Fertilize"), ordered = TRUE)
plot(TukeyHSD(model, which = c("Herbizide","Fertilize"), ordered = T))
# построим сразу 2 графика, и зададим графические параметры так,
# чтобы поля не вылезали
par(mar = c(4.5, 8, 4.5, 4.5))
plot(TukeyHSD(model, which = c("Herbizide","Fertilize"), ordered = T), las=1)
#Непараметрический дисперсионный анализ
kruskal.test(Yield ~ Herbizide, data = adat)
kruskal.test(Yield ~ Fertilize, data = adat)
# Рассчитаем медианы по градациям каждого фактора
tapply(adat$Yield,adat$Herbizide, median)
tapply(adat$Yield,adat$Fertilize, median)
#Множественное попарное сравнение медиан между группами
pairwise.wilcox.test(adat$Yield, adat$Herbizide, p.adjust.method = "BH")
pairwise.wilcox.test(adat$Yield, adat$Fertilize, p.adjust.method = "BH")
# Построение графика средних/ медиан
# Mean plots
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(adat, x = "Herbizide", y = "Yield", add = c("median", "jitter"))
ggline(adat, x = "Fertilize", y = "Yield", add = c("median", "jitter"))
