hum101= read.table("4_Variants_zan4_It_2022.csv", header=TRUE, sep=";", dec=",")
hum101 # выводим первые 9 строк каждого столбца
dim(hum101) # число строк и число столбцов
str(hum101) # структура таблицы
summary(hum101) # выводим общую информацию
mydata12=hum101$var12
mydata12 # что содержится в новом векторе
summary(mydata12) # его общая характеристика
length(mydata12) # какова его длина

# выбираем 7 случайных выборок и записываем их в таблицу фрейм 
sam1 = c(sample(mydata12, size = 10, replace = FALSE))
sam2 = c(sample(mydata12, size = 10, replace = FALSE))
sam3 = c(sample(mydata12, size = 10, replace = FALSE))
sam4 = c(sample(mydata12, size = 10, replace = FALSE))
sam5 = c(sample(mydata12, size = 10, replace = FALSE))
sam6 = c(sample(mydata12, size = 10, replace = FALSE))
sam7 = c(sample(mydata12, size = 10, replace = FALSE))
myframe12 = data.frame(sam1 = sam1, sam2= sam2, sam3= sam3, sam4= sam4, sam5= sam5, sam6= sam6, sam7 = sam7)

# считаем статистические характеристики
myframe12_desc = data.frame(row.names = c("average","standart deviation","covar", "med_mist"))
for (i in 1:7) 
{
  average = mean(myframe12[ ,i])
  standart_dev = sd(myframe12[ ,i])
  covar = 100*standart_dev/average
  med_mist = standart_dev/sqrt(10)
  myframe12_desc[,i] = c(average, standart_dev, covar, med_mist)
}

# создаем 7 векторов с номерами для систематического отбора
seq1 = seq(from = 1, by = 100, length.out = 10)
seq2 = seq(from = 23, by = 100, length.out = 10)
seq3 = seq(from = 36, by = 100, length.out = 10)
seq4 = seq(from = 48, by = 100, length.out = 10)
seq5 = seq(from = 52, by = 100, length.out = 10)
seq6 = seq(from = 82, by = 100, length.out = 10)
seq7 = seq(from = 99, by = 100, length.out = 10)

# создаем объект с систематическими выборками
sys1 = mydata12[seq1]
sys2 = mydata12[seq2]
sys3 = mydata12[seq3]
sys4 = mydata12[seq4]
sys5 = mydata12[seq5]
sys6 = mydata12[seq6]
sys7 = mydata12[seq7]
mysysframe12 = data.frame(sys1 = sys1, sys2 = sys2, sys3 = sys3, sys4 = sys4, sys5 = sys5, sys6 = sys6, sys7 = sys7)

# считаем статистические характеристики для систематических выборок
mysysframe12_desc = data.frame(row.names = c("average","standart deviation","covar", "med_mist"))
for (i in 1:7) 
{
  average = mean(mysysframe12[ ,i])
  standart_dev = sd(mysysframe12[ ,i])
  covar = 100*standart_dev/average
  med_mist = standart_dev/sqrt(10)
  mysysframe12_desc[,i] = c(average, standart_dev, covar, med_mist)
}

# соединяем все выборки в одну большую и считаем статистические характеристики
mybigframe = c()
for (i in 1:7) {
  mybigframe = c(mybigframe, myframe12[,i])
}
for (i in 1:7) {
  mybigframe = c(mybigframe, mysysframe12[,i])
}

baverage = mean(mybigframe)
bstandart_dev = sd(mybigframe)
bcovar = 100*standart_dev/average
bmed_mist = standart_dev/sqrt(140)
baverage; bstandart_dev; bcovar; bmed_mist