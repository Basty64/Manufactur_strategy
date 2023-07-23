library(xlsx)


Data_Emissions <- read.xlsx("_path", 1)
Data_KeyInformation <- read.xlsx("_path", 2)
Data_RoseWind <- read.xlsx("_path", 3)


#1 Оценить совокупные затраты на реализацию первой стратегии (установка очистных сооружений). 
#  В совокупные затраты входит как стоимость установки очистных сооружений, так и стоимость ежегодного обслуживания за 5 лет.


#Совокупные затраты на установку очистных сооружений и их обслуживание
KeyInformation <- as.numeric(Data_KeyInformation$Стоимость.системы..тыс.руб.)+as.numeric(Data_KeyInformation$Стоимость.обслуживания..тыс.руб.)*5
KeyInformation_Frame <- data.frame(Вещество = Data_KeyInformation$Вещество,"Стоимость системы тыс.руб."=Data_KeyInformation$Стоимость.системы..тыс.руб., 
                                      "Стоимость обслуживания тыс.руб."=Data_KeyInformation$Стоимость.обслуживания..тыс.руб., 
                                      "Затраты за 5 лет"=KeyInformation)



#2 Оценить степень близости распределений концентраций выбросов к известным теоритическим законам распределения.
#   Обязательная визуализация: гистограмма выборки с наложенной на нее теоретической или эмпирической функцией плотности.

library(EnvStats)

#Нормальное распределение

NormDistribution <- function(EmissionsData, Name){
  xx <- sort(x = unique(x=EmissionsData),decreasing = FALSE)
  
  #Тест Колмогорова-Смирнова для нормального распределения
  Test <- ks.test(x = EmissionsData, y = "pnorm", mean(EmissionsData),sd(EmissionsData))
  
  #Гистограмма и теоретическая функция плотности
  EmissionsData_Scott <- hist(x = EmissionsData , breaks= "scott", freq= FALSE, right = TRUE, border = "black", col= "grey70", main = Name, ylab = "Плотность", xlab = "Выбросы", 
                              ylim =c(0,0.2)                             )
  
  TheorDensity <- dnorm(x= xx,mean = mean(EmissionsData),sd= sd(EmissionsData))
  lines(x = xx, y = TheorDensity, lwd=2, col= "midnightblue")
  
  #Эмпирическая функция плотности
  DataDensity <- density(EmissionsData)
  lines (x =DataDensity$x , y = DataDensity$y, lwd=2, col= "aquamarine4" )
  
  legend (x=0.02, y=38, cex=0.55, fill =c("midnightblue","aquamarine4"),legend = c("Теоретическая функция плотности", "Эмпирическая функция плотности"))
  
  if (Test$p.value >= 0.05) {res <- "Выборка подчинена нормальному закону распределения"} else {res <- "Выборка НЕ подчинена нормальному закону распределения"}
  print(res)
  cat("D = ",Test$statistic, "," ," p-value = ",Test$p.value, fill = TRUE, sep = "")
  print(x = "------------------------------")
}





#Бета-распределение

BetaDistribution <- function(EmissionsData, Name){
  BetaData <- (EmissionsData-min(EmissionsData))/(max(EmissionsData)-min(EmissionsData))
  EBeta <- ebeta(BetaData)
  xxx <- sort(x = unique(x=BetaData),decreasing = FALSE)
  
  #Тест Колмогорова-Смирнова для Beta-распределения
  Test <- ks.test(x = BetaData, y = "pbeta", shape1 = EBeta$parameters[[1]], shape2 = EBeta$parameters[[2]])
  
  #Гистограмма и функция плотности
  EmissionsData_Beta_Scott <- hist(x= BetaData , breaks= "scott", freq= FALSE, right = TRUE, border = "black", col= "grey70", , main = Name, ylab = "Плотность", xlab = "Выбросы",
                                   ylim =c(0,3.5))
  TheorBetaDensity <- dbeta(x = xxx, shape1 = EBeta$parameters[[1]], shape2 = EBeta$parameters[[2]])
  lines(x = xxx, y = TheorBetaDensity, lwd=2, col= "midnightblue")
  
  #Эмпирическая функция плотности
  DataDensity <- density(BetaData)
  lines (x =DataDensity$x , y = DataDensity$y, lwd=2, col= "aquamarine4" )
  
  legend (x=0.02, y=3, cex=0.55, fill =c("midnightblue","aquamarine4"),legend = c("Теоретическая функция плотности", "Эмпирическая функция плотности"))
  
  if (Test$p.value >= 0.05) {res <- "Выборка подчинена закону бета-распределения"} else {res <- "Выборка НЕ подчинена закону бета-распределения"}
  print(res)
  cat("D = ",Test$statistic, "," ," p-value = ",Test$p.value, fill = TRUE, sep = "")
  print(x = "------------------------------")
}




#Гамма-распределение

GammaDistribution <- function(EmissionsData, Name){
  xx <- sort(x = unique(x=EmissionsData),decreasing = FALSE)
  
  Egamma <- egamma(EmissionsData)
  
  #Тест Колмогорова-Смирнова для нормального распределения
  Test <- ks.test(x = EmissionsData, y = "pgamma", shape = Egamma$parameters[[1]], scale = Egamma$parameters[[2]])
  
  #Гистограмма и функция плотности
  EmissionsData_Scott <- hist(x = EmissionsData , breaks= "scott", freq= FALSE, right = TRUE, border = "black", col= "grey70", main = Name, ylab = "Плотность", xlab = "Выбросы",
                              ylim =c(0,0.2))
  
  TheorDensity <- dgamma(x = xx, shape = Egamma$parameters[[1]], scale = Egamma$parameters[[2]])
  lines(x = xx, y = TheorDensity, lwd=2, col= "midnightblue")
  
  #Эмпирическая функция плотности
  DataDensity <- density(EmissionsData)
  lines (x = DataDensity$x , y = DataDensity$y, lwd=2, col= "aquamarine4" )
  
  legend (x=0.02, y=38, cex=0.55, fill =c("midnightblue","aquamarine4"),legend = c("Теоретическая функция плотности", "Эмпирическая функция плотности"))
  
  if (Test$p.value >= 0.05) {res <- "Выборка подчинена закону гамма-распределения"} else {res <- "Выборка НЕ подчинена закону гамма-распределения"}
  print(res)
  cat("D = ",Test$statistic, "," ," p-value = ",Test$p.value, fill = TRUE, sep = "")
  print(x = "------------------------------")
}



#Экспоненциальное распределение

ExpDistribution <- function(EmissionsData, Name){
  xx <- sort(x = unique(x=EmissionsData),decreasing = FALSE)
  
  Eexp <- eexp(EmissionsData)
  
  #Тест Колмогорова-Смирнова для нормального распределения
  Test <- ks.test(x = EmissionsData, y = "pexp", rate = Eexp$parameters[[1]])
  
  #Гистограмма и функция плотности
  EmissionsData_Scott <- hist(x = EmissionsData , breaks= "scott", freq= FALSE, right = TRUE, border = "black", col= "grey70", main = Name, ylab = "Плотность", xlab = "Выбросы",
                              ylim =c(0,0.2))
  
  TheorDensity <- dexp(x = xx, rate = Eexp$parameters[[1]])
  lines(x = xx, y = TheorDensity, lwd=2, col= "midnightblue")
  
  #Эмпирическая функция плотности
  DataDensity <- density(EmissionsData)
  lines (x =DataDensity$x , y = DataDensity$y, lwd=2, col= "aquamarine4" )
  
  legend (x=0., y=38, cex=0.55, fill =c("midnightblue","aquamarine4"),legend = c("Теоретическая функция плотности", "Эмпирическая функция плотности"))
  
  if (Test$p.value >= 0.05) {res <- "Выборка подчинена экспоненциальному закону распределения"} else {res <- "Выборка НЕ подчинена экспоненциальному закону распределения"}
  print(res)
  cat("D = ",Test$statistic, "," ," p-value = ",Test$p.value, fill = TRUE, sep = "")
  print(x = "------------------------------")
}



Results <- function (EmissionsData, Name1, Name2, Name3, Name4){
  NormDistribution(EmissionsData, Name1)
  BetaDistribution(EmissionsData, Name2)
  GammaDistribution(EmissionsData, Name3)
  ExpDistribution(EmissionsData, Name4)
  
}


#1  Диоксид серы
SulfurDioxide_Data <- Data_Emissions$Диоксид.серы
Results(SulfurDioxide_Data, "Диоксид серы (нормальное распределение)","Диоксид серы (бета-распределение)",
        "Диоксид серы  (гамма-распределение)", "Диоксид серы (экспоненциальное распределение)")

Ecdf_Sulf <- ecdf(SulfurDioxide_Data)
plot(Ecdf_Sulf, main = "Эмпирическая функция распределения.Диоксид серы")


#2  Этиленоксид
Etilenoxide_Data <- Data_Emissions$Этиленоксид
Results(Etilenoxide_Data, "Этиленоксид (нормальное распределение)","Этиленоксид (бета-распределение)",
        "Этиленоксид  (гамма-распределение)", "Этиленоксид (экспоненциальное распределение)")

#3  Хлор
Chlorine_Data <- Data_Emissions$Хлор
Results(Chlorine_Data, "Гистограмма. Хлор (нормальное распределение)","Гистограмма. Хлор (бета-распределение)",
        "Гистограмма. Хлор  (экспоненциальное распределение)", "Гистограмма. Хлор (гамма-распределение)")


#4  Диоксид углерода
CarbonDioxide_Data <- Data_Emissions$Диоксид.углерода
Results(CarbonDioxide_Data, "Гистограмма. Диоксид углерода (нормальное распределение)","Гистограмма. Диоксид углерода (бета-распределение)",
        "Гистограмма. Диоксид углерода  (экспоненциальное распределение)", "Гистограмма. Диоксид углерода (гамма-распределение)")

#5  Угарный газ
CarbonMonoxide_Data <- Data_Emissions$Угарный.газ
Results(CarbonMonoxide_Data, "Гистограмма. Угарный газ (нормальное распределение)","Гистограмма. Угарный газ (бета-распределение)",
        "Гистограмма. Угарный газ  (экспоненциальное распределение)", "Гистограмма. Угарный газ (гамма-распределение)")



#3	В случае невозможности отнесения распределения к известным видам — сформировать эмпирическую функцию распределения

Ecdf_Sulf <- ecdf(SulfurDioxide_Data)
plot(Ecdf_Sulf, main = "Эмпирическая функция распределения. Диоксид серы")

Ecdf_Chl <- ecdf(Chlorine_Data)
plot(Ecdf_Chl, main = "Эмпирическая функция распределения.Хлор")



#Оценить вероятность суточного штрафа каждого вида вредных веществ, при постоянном гипотетическом условии наиболее неблагоприятного ветра

Оценка_эффективности <- c(0.0, 0.0, 0.25, 0.5, 0.9, 0.75, 0.4, 0.1)
Data_RoseWind <- cbind(Data_RoseWind, Оценка_эффективности)

PDK <- as.numeric(Data_KeyInformation$ПДК)
max_polution <- max(as.numeric(Data_RoseWind$Оценка_эффективности))

p1 <- (1-Ecdf_Sulf(PDK[1]/max_polution))
p2 <- pnorm(PDK[2]/max_polution, mean(Etilenoxide_Data), sd(Etilenoxide_Data),lower.tail = F)
p3 <- (1-Ecdf_Chl(PDK[3]/max_polution))
p4 <- pnorm(PDK[4]/max_polution, mean(CarbonDioxide_Data), sd(CarbonDioxide_Data),lower.tail = F)
p5 <- pnorm(PDK[5]/max_polution, mean(CarbonMonoxide_Data), sd(CarbonMonoxide_Data),lower.tail = F)

p <- c(p1,p2,p3,p4,p5)

p_Tab <- data.frame(Вещество = Data_KeyInformation$Вещество,"Вероятность суточного штрафа при условии постоянного неблагоприятного ветра" = p)

write.xlsx(p_Tab, "Штрафы_p.xlsx")

#Рассчитать получающийся совокупный штраф. На основании полученного штрафа выделить такие вещества,
#для которых сумма штрафов заведомо меньше затрат на установку и обслуживание очистных сооружений.
Fines <- c(0,0,0,0,0)

for (i in 1:5){
  Fines[i] <- (365*5)*p[i]*as.numeric(Data_KeyInformation$Штраф..тыс.руб)[i]}

Fines_Tab <- data.frame(Вещество = Data_KeyInformation$Вещество,"Совокупный штраф при условии постоянного неблагоприятного ветра тыс.руб" = Fines)

Tab <- data.frame(Вещество = Data_KeyInformation$Вещество,"Совокупный штраф при условии постоянного неблагоприятного ветра тыс.руб" = Fines, 
                  "Совокупные затраты на установку и обслуживание очистных сооружений тыс.руб"=KeyInformation, "Разница тыс.руб" = Fines-KeyInformation)
write.xlsx(Tab, "Выбор веществ.xlsx")

#Для оставшихся веществ — составить выражение полной вероятности получения штрафа при условии разной 
#интенсивности разных направлений ветров. 

Rosewinds <- as.numeric(Data_RoseWind$Дней_в_году)
winds_eff <- as.numeric(Data_RoseWind$Оценка_эффективности)

FullP <- c(0,0,0,0,0)

for (i in 1:8){
  p1.0 <- FullP[1]+(1-Ecdf_Sulf(PDK[1]/winds_eff[i]))*(Rosewinds[i]/365)
  p3.0 <- FullP[3]+(1-Ecdf_Chl(PDK[3]/winds_eff[i]))*(Rosewinds[i]/365)
  p4.0 <- FullP[4]+pnorm(PDK[4]/winds_eff[i], mean(CarbonDioxide_Data), sd(CarbonDioxide_Data),lower.tail = F)*(Rosewinds[i]/365)
  p5.0 <- FullP[5]+pnorm(PDK[5]/winds_eff[i], mean(CarbonMonoxide_Data), sd(CarbonMonoxide_Data),lower.tail = F)*(Rosewinds[i]/365)
  
  FullP[1] <- p1.0
  FullP[3] <- p3.0
  FullP[4] <- p4.0
  FullP[5] <- p5.0
}

Tab_Fullp <- data.frame(Вещество = Data_KeyInformation$Вещество, "Полная вероятность суточного штрафа" = FullP)
write.xlsx(Tab_Fullp, "Полн вер.xlsx")
FullP_Fines <- c(0,0,0,0,0)
for (i in 1:5){
  FullP_Fines[i] <- (365*5)*FullP[i]*as.numeric(Data_KeyInformation$Штраф..тыс.руб)[i]}

FullP_Fines_Tab <- data.frame(Вещество = Data_KeyInformation$Вещество,"Совокупный штраф тыс.руб" = FullP_Fines)

write.xlsx(FullP_Fines_Tab, "Штрафы полн вер.xlsx")

Tab_2 <- data.frame(Вещество = Data_KeyInformation$Вещество,"Совокупный штраф тыс.руб" = FullP_Fines, 
                    "Совокупные затраты на установку и обслуживание очистных сооружений тыс.руб"=KeyInformation, "Разница тыс.руб" = FullP_Fines-KeyInformation)

write.xlsx(Tab_2, "Выбор веществ полн вер.xlsx")

#Подготовить итоговый вывод про все рассматриваемые вещества. Сделать калькуляцию суммарных вероятных затрат предприятия на 5 
#лет при реализации всех выбранных стратегий.

Strategy <- c("","","","","")
Costs <- c(0,0,0,0,0)

for (i in 1:5){
  if ((FullP_Fines-KeyInformation)[i] >0){
    Strategy[i] = "Установка очистных сооружений и их обслуживание"
    Costs[i] = KeyInformation[i]
  } else {
    Strategy[i] = "Выплата штрафов"
    Costs[i] = FullP_Fines[i]
  }
}
Results_Tab <- data.frame("Вещество"= Data_KeyInformation$Вещество, "Стратегия" = Strategy, "Общие затраты,тыс.руб."= Costs)

write.xlsx(Results_Tab, "Вывод.xlsx")
