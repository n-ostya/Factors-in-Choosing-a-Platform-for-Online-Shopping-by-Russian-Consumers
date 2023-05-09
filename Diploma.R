library(lmtest)
library(skedastic)
library(MASS)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(sandwich)
library(plyr)
library(corrplot)
library(AER)
library(nnet)
library(effects)
library(MASS)
library(oglmx)
library(dplyr)
library(pscl)
library(likert)
library(ggcorrplot)
library(readxl)
library(tidyr)
library(ahpsurvey)
library(randomNames)
library(lavaan)
library(lavaanPlot)
library(semPlot)
library(corrplot)

######################################################
###### Статистика по выборке ###########
########################################

Statistic <- read_excel("Statistic.xlsx")
Statistic <- na.omit(Statistic)
View(Statistic)
summary(Statistic)

Age <- Statistic$Age
qplot(Age) + geom_bar(colour="black")
ggplot(Statistic, aes(x=Age)) + geom_bar(colour="black")

################################################################################################
###### SAATY ##########################
#######################################

Saaty <- read_excel("Saaty_R.xlsx")
Index <- Saaty[2:29]
att <- c("Web", "Pay", "Ass", "Deliv", "Peop", "Exp", "Loyal", "Card")

cr <- Index %>% 
  ahp.mat(atts = att, negconvert = TRUE) %>% 
  ahp.cr(att)

table(cr > 0.5)
Index$cr <- cr
Index$ID <- Saaty$ID
Full <- cbind(Index,Saaty[30:36])
Full <- Full %>% select(-cr) %>% subset(Full$cr < 0.5)
Index <- Index %>% select(-cr) %>% subset(Index$cr < 0.5)
ID_Saaty <- Index$ID
Index <- Index %>% select(-ID)

cr <- Index %>% 
  ahp.mat(atts = att, negconvert = TRUE) %>% 
  ahp.cr(att)
mean(cr)

#матрицы парных сравнений
Ind_ahp <- Index %>% 
  ahp.mat(att, negconvert = T)

#Коррекция Харкера
edited <- ahp.harker(Ind_ahp, att, iterations = 3, stopcr = 0.1)
cr <- edited %>% 
  ahp.cr(att)
mean(cr)

error <- ahp.error(edited, att, reciprocal = TRUE)
error

#средние оценки
mean <- Index %>%
  ahp.mat(atts = att, negconvert = TRUE) %>% 
  ahp.aggpref(att, method = "eigen")
mean <- mean/sum(mean)
mean
mean_S <- mean

sd_w <- Index %>%
  ahp.mat(atts = att, negconvert = TRUE) %>% 
  ahp.aggpref(att, method = "eigen", aggmethod = "sd")

#какие результаты усредненные получили
t(data.frame(mean, sd_w))

#скорректированные средние
mean <- edited %>% 
  ahp.aggpref(att, method = "eigen")
mean <- mean/sum(mean)
mean

sd_w <- edited %>% 
  ahp.aggpref(att, method = "eigen", aggmethod = "sd")

#какие результаты усредненные получили
t(data.frame(mean, sd_w))


###############################################################################################
######### Saaty 10/20/30 #############
######################################
cr <- Index %>% 
  ahp.mat(atts = att, negconvert = TRUE) %>% 
  ahp.cr(att)
Index$cr <- cr

Saaty_10 <- Index %>% select(-cr) %>% subset(Index$cr <= 0.1)
Saaty_20 <- Index %>% select(-cr) %>% subset(Index$cr <= 0.2)
Saaty_30 <- Index %>% select(-cr) %>% subset(Index$cr <= 0.3)
Saaty_40 <- Index %>% select(-cr) %>% subset(Index$cr <= 0.4)
Saaty_over <- Index %>% select(-cr) %>% subset(Index$cr > 0.1)

### 10% ###

#средние оценки
mean_10 <- Saaty_10 %>%
  ahp.mat(atts = att, negconvert = TRUE) %>% 
  ahp.aggpref(att, method = "eigen")
sum(mean_10)
mean <- mean_10/sum(mean_10)
mean

### 20% ###

#средние оценки
mean_20 <- Saaty_20 %>%
  ahp.mat(atts = att, negconvert = TRUE) %>% 
  ahp.aggpref(att, method = "eigen")
sum(mean_20)
mean <- mean_20/sum(mean_20)
mean

### 30% ###

#средние оценки
mean_30 <- Saaty_30 %>%
  ahp.mat(atts = att, negconvert = TRUE) %>% 
  ahp.aggpref(att, method = "eigen")
sum(mean_30)
mean <- mean_30/sum(mean_30)
mean

### 40% ###

#средние оценки
mean_40 <- Saaty_40 %>%
  ahp.mat(atts = att, negconvert = TRUE) %>% 
  ahp.aggpref(att, method = "eigen")
sum(mean_40)
mean <- mean_40/sum(mean_40)
mean

### > 10% ####
mean_over <- Saaty_over %>%
  ahp.mat(atts = att, negconvert = TRUE) %>% 
  ahp.aggpref(att, method = "eigen")
sum(mean_over)
mean <- mean_over/sum(mean_over)
mean

################################################################################################
###### Плотности распредления ######
#####################################
new <- matrix(nrow=294, ncol=8) #378
for (i in 1:294) {
  cat("iteration #", i, "\n")
  new[i,] <- Index[i,] %>%
    ahp.mat(atts = att, negconvert = TRUE) %>% 
    ahp.aggpref(att, method = "eigen")
}

new <- as.data.frame(new)
colnames(new) <- c("Web", "Pay", "Ass", "Deliv", "Peop", "Exp", "Loyal", "Card")

ggplot(new, aes(x=Web, y=..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2) +
  geom_density() + xlab("Качество сайта") + ylab("Плотность")

ggplot(new, aes(x=Pay, y=..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2) +
  geom_density() + xlab("Способ оплаты") + ylab("Плотность")

ggplot(new, aes(x=Ass, y=..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2) +
  geom_density() + xlab("Ассортимент") + ylab("Плотность")

ggplot(new, aes(x=Deliv, y=..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2) +
  geom_density() + xlab("Доставка") + ylab("Плотность")

ggplot(new, aes(x=Exp, y=..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2) +
  geom_density() + xlab("Прошлый опыт") + ylab("Плотность")

ggplot(new, aes(x=Peop, y=..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2) +
  geom_density() + xlab("Эффект окружения") + ylab("Плотность")

ggplot(new, aes(x=Loyal, y=..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2) +
  geom_density() + xlab("Программа лояльности") + ylab("Плотность")

ggplot(new, aes(x=Card, y=..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2) +
  geom_density() + xlab("Карточка товара") + ylab("Плотность")

boxplot(new)

data_mean <- as.data.frame(matrix(nrow = 8, ncol = 2))
data_mean$V1 <- t(t(colMeans(new)))
data_mean$V2 <- t(t(sapply(new, sd)))
colnames(data_mean) <- c("mean","sd")
data_mean$names <- c("Web", "Pay", "Ass", "Deliv", "Peop", "Exp", "Loyal", "Card")

ggplot(data_mean) +
  geom_bar(aes(x = names, y = mean), stat="identity", fill="skyblue", alpha = 0.8) +
  geom_errorbar(aes(x = names, y = mean, ymin = mean - 2*sd, ymax = mean + 2*sd),
                width = 0.4, colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Характеристика", y = "Вес")

################################################################################################
###### k-means ##########################
#######################################
new <- matrix(nrow=294, ncol=8) #316
for (i in 1:294) {
  cat("iteration #", i, "\n")
  new[i,] <- Full[i,1:28] %>%
    ahp.mat(atts = att, negconvert = TRUE) %>% 
    ahp.aggpref(att, method = "eigen")
}
new <- as.data.frame(new)
colnames(new) <- c("Web", "Pay", "Ass", "Deliv", "Peop", "Exp", "Loyal", "Card")
new$ID <- ID_Saaty
B <- scale(new[1:8])
fviz_nbclust(B, kmeans, method = "wss")

km <- kmeans(B, centers = 4, nstart = 25)
km
fviz_cluster(km, data = new, geom = "point")
new$cluster <- km$cluster
Full_clust <- new
new <- cbind(new, Full[29:36])
table(km$cluster)

#для средних и отклонений
mean <- aggregate(new, by= list (cluster=km$cluster), mean)
sd <- aggregate(new, by= list (cluster=km$cluster), sd)

data_mean <- as.data.frame(matrix(nrow = 8, ncol = 2))
data_mean$V1 <- t(mean[1,2:9])
data_mean$V2 <- t(sd[1,2:9])
colnames(data_mean) <- c("mean","sd")
data_mean$names <- c("Web", "Pay", "Ass", "Deliv", "Peop", "Exp", "Loyal", "Card")
ggplot(data_mean) +
  geom_bar(aes(x = names, y = mean), stat="identity", fill="skyblue", alpha = 0.8) +
  geom_errorbar(aes(x = names, y = mean, ymin = mean - 2*sd, ymax = mean + 2*sd),
                width = 0.4, colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Характеристика", y = "Вес")

data_mean <- as.data.frame(matrix(nrow = 8, ncol = 2))
data_mean$V1 <- t(mean[2,2:9])
data_mean$V2 <- t(sd[2,2:9])
colnames(data_mean) <- c("mean","sd")
data_mean$names <- c("Web", "Pay", "Ass", "Deliv", "Peop", "Exp", "Loyal", "Card")
ggplot(data_mean) +
  geom_bar(aes(x = names, y = mean), stat="identity", fill="skyblue", alpha = 0.8) +
  geom_errorbar(aes(x = names, y = mean, ymin = mean - 2*sd, ymax = mean + 2*sd),
                width = 0.4, colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Характеристика", y = "Вес")

data_mean <- as.data.frame(matrix(nrow = 8, ncol = 2))
data_mean$V1 <- t(mean[3,2:9])
data_mean$V2 <- t(sd[3,2:9])
colnames(data_mean) <- c("mean","sd")
data_mean$names <- c("Web", "Pay", "Ass", "Deliv", "Peop", "Exp", "Loyal", "Card")
ggplot(data_mean) +
  geom_bar(aes(x = names, y = mean), stat="identity", fill="skyblue", alpha = 0.8) +
  geom_errorbar(aes(x = names, y = mean, ymin = mean - 2*sd, ymax = mean + 2*sd),
                width = 0.4, colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Характеристика", y = "Вес")

data_mean <- as.data.frame(matrix(nrow = 8, ncol = 2))
data_mean$V1 <- t(mean[4,2:9])
data_mean$V2 <- t(sd[4,2:9])
colnames(data_mean) <- c("mean","sd")
data_mean$names <- c("Web", "Pay", "Ass", "Deliv", "Peop", "Exp", "Loyal", "Card")
ggplot(data_mean) +
  geom_bar(aes(x = names, y = mean), stat="identity", fill="skyblue", alpha = 0.8) +
  geom_errorbar(aes(x = names, y = mean, ymin = mean - 2*sd, ymax = mean + 2*sd),
                width = 0.4, colour = "orange", alpha = 0.9, size = 0.8) + theme_bw() +
  labs(x = "Характеристика", y = "Вес")

round(mean[2:9], digits = 4)
round(mean[2:9] + 2*sd[2:9], digits = 3)

################################################################################################
###### Выбор модели для факторного анализа ##########
#####################################################

Ozon <- read_excel("Ozon.xlsx")
Ozon <- Ozon %>% select(-Fool) %>% subset(Ozon$Fool > 5)
View(Ozon)
Ozon <- na.omit(Ozon)
corrplot(cor(Ozon[2:8]))
corrplot(cor(Ozon[9:12])) #post pay minus
corrplot(cor(Ozon[13:15]))
corrplot(cor(Ozon[16:24])) #postpone minus
corrplot(cor(Ozon[25:26]))
corrplot(cor(Ozon[27:29]))
corrplot(cor(Ozon[30:33]))
Ozon$Reg <- Ozon$Regularity
Ozon <- Ozon[2:45]

#### Структурная модель ####
m <- "# факторные модели
Web  =~ NoAnnoy + NoInfNoise	+ Interface + EasyOrder +Filters + Mobile + Categories	
Pay =~ CardLink + Confidentiality + MoneyBack	
Ass =~ Match+ LowPrices + Satisfaction
Deliv =~ Return + NoDamage + Delivery + Pickpoint + Polite + Notification + Rejection + Fit	
Peop =~ Relatives + Friends	
Exp =~ Negative + Support + Response	
Loyal =~ Loyalty + Bonuses + Sale	
Card =~ Photo + Character + Comment
Index =~ Web + Pay + Ass + Deliv + Peop + Exp + Loyal + Card
# регрессия
"
fit1 <- sem(m, data = Ozon)
summary(fit1, standardized=TRUE, fit.measures=TRUE)

semPaths(fit1, "std", edge.label.cex = 0.8, curvePivot = FALSE, rotation = 2, 
         residuals = FALSE, sizeMan = 2, what = "path", label.cex = 2.5, sizeLat = 4,
         edge.color="black")


m2 <- "# факторные модели
Web  =~ NoAnnoy + NoInfNoise	+ Interface + EasyOrder +Filters + Mobile + Categories	
Pay =~ CardLink + Confidentiality + MoneyBack	
Ass =~ Match+ LowPrices + Satisfaction
Deliv =~ Return + NoDamage + Delivery + Pickpoint + Polite + Notification + Rejection + Fit	
Peop =~ Relatives + Friends	
Exp =~ Negative + Support + Response	
Loyal =~ Loyalty + Bonuses + Sale	
Card =~ Photo + Character + Comment	
Index =~ Web + Pay + Ass + Deliv + Peop + Exp + Loyal + Card
# регрессия
Reg ~ Index
"
fit2 <- sem(m2, data = Ozon)
summary(fit2, standardized=TRUE, fit.measures=TRUE)

#визуализация
semPaths(fit2, "std", edge.label.cex = 0.8, curvePivot = FALSE, rotation = 2, 
         residuals = FALSE, sizeMan = 2, what = "path", label.cex = 2.5, sizeLat = 4,
         edge.color="black")

m3 <- "# факторные модели
Web  =~ NoAnnoy + NoInfNoise	+ Interface + EasyOrder +Filters + Mobile + Categories	
Pay =~ CardLink + Confidentiality + MoneyBack	
Ass =~ Match+ LowPrices + Satisfaction
Deliv =~ Return + NoDamage + Delivery + Pickpoint + Polite + Notification + Rejection + Fit	
Peop =~ Relatives + Friends	
Exp =~ Negative + Support + Response	
Loyal =~ Loyalty + Bonuses + Sale	
Card =~ Photo + Character + Comment	
# регрессия
Reg ~ Web + Pay + Ass + Deliv + Peop + Exp + Loyal + Card 

"
fit3 <- sem(m3, data = Ozon)
summary(fit3, standardized=TRUE, fit.measures=TRUE)

#визуализация
semPaths(fit3, "std", edge.label.cex = 0.8, curvePivot = FALSE, rotation = 2, 
         residuals = FALSE, sizeMan = 2, what = "path", label.cex = 2.5, sizeLat = 4,
         edge.color="black")

m4 <- "# факторные модели
Web  =~ NoAnnoy + NoInfNoise	+ Interface + EasyOrder +Filters + Mobile + Categories	
Pay =~ CardLink + Confidentiality + MoneyBack	
Ass =~ Match+ LowPrices + Satisfaction
Deliv =~ Return + NoDamage + Delivery + Pickpoint + Polite + Notification + Rejection + Fit	
Peop =~ Relatives + Friends	
Exp =~ Negative + Support + Response	
Loyal =~ Loyalty + Bonuses + Sale	
Card =~ Photo + Character + Comment	
Index =~ Web + Pay + Ass + Deliv + Peop + Exp + Loyal + Card
# регрессия
Reg ~ Index + JobStat	+ Income	+ Education	+ FamilyStat	+ Children 
"
fit4 <- sem(m4, data = Ozon)
summary(fit4, standardized=TRUE, fit.measures=TRUE)

#визуализация
semPaths(fit4, "std", edge.label.cex = 0.8, curvePivot = FALSE, rotation = 2, 
         residuals = FALSE, sizeMan = 2, what = "path", label.cex = 2.5, sizeLat = 4,
         edge.color="black")

########################################################################
###### Claster демографические характеристики #########
#######################################################
people <- na.omit(Saaty)

#отбор признаков
people_new <- people %>% select(Age, Education, Income)
data_new_scale <- scale(people_new)

#вычисляем расстояния
D <- dist(data_new_scale)
mod <- hclust(D)
plot(mod, ann = FALSE, hang = -1, labels = FALSE)

#сделаем 3 кластера
rect.hclust(mod, k = 3)

#обрежем
Clast_number <- cutree(mod, k = 3)
table(Clast_number)
people$clust <- Clast_number

#визуализируем средние
flexclust::barchart(mod, people_new, k = 3)

###############################################################################
########### Saaty for clust ##############
##########################################
clust1 <- people %>% subset(people$clust == 1)
clust2 <- people %>% subset(people$clust == 2)
clust3 <- people %>% subset(people$clust == 3)

clust1 <- clust1[1:28]
clust2 <- clust2[1:28]
clust3 <- clust3[1:28]

#Cluster 1
#матрицы парных сравнений
Cl1_ahp <- clust1 %>% 
  ahp.mat(att, negconvert = T)
edited_cl1 <- ahp.harker(Cl1_ahp, att, iterations = 10, stopcr = 0.1)

#средние оценки
#скорректированные средние
mean <- edited_cl1 %>% 
  ahp.aggpref(att, method = "eigen")
mean <- mean/sum(mean)
mean

sd_w <- Index %>%
  ahp.mat(atts = att, negconvert = TRUE) %>% 
  ahp.aggpref(att, method = "eigen", aggmethod = "sd")

#Cluster 2
#матрицы парных сравнений
Cl2_ahp <- clust2 %>% 
  ahp.mat(att, negconvert = T)
edited_cl2 <- ahp.harker(Cl2_ahp, att, iterations = 10, stopcr = 0.1)

#средние оценки
#скорректированные средние
mean <- edited_cl2 %>% 
  ahp.aggpref(att, method = "eigen")
mean <- mean/sum(mean)
mean

sd_w <- Index %>%
  ahp.mat(atts = att, negconvert = TRUE) %>% 
  ahp.aggpref(att, method = "eigen", aggmethod = "sd")

########################################################################################
##### Значения индекса через средние ######
###########################################
Sr <- read_excel("Srednee.xlsx")
Vmeste <- read_excel("Vmeste.xlsx")
Sr <- Sr %>% select(-Fool) %>% subset(Sr$Fool>5)
Vmeste <- Vmeste %>% select(-Fool) %>% subset(Vmeste$Fool>5)
Vmeste$Negative <- -1 *Vmeste$Negative
Vmeste$Support <- -1 *Vmeste$Support
Vmeste$Response <- -1 *Vmeste$Response
Sr_new <- Sr
Sr_new$Web <- mean_S[1] * Sr_new$Web
Sr_new$Pay <- mean_S[2] * Sr_new$Pay
Sr_new$Ass <- mean_S[3] * Sr_new$Ass
Sr_new$Deliv <- mean_S[4] * Sr_new$Deliv
Sr_new$Peop <- mean_S[5] * Sr_new$Peop
Sr_new$Exp <- mean_S[6] * Sr_new$Exp
Sr_new$Loyal <- mean_S[7] * Sr_new$Loyal
Sr_new$Card <- mean_S[8] * Sr_new$Card

for (i in 1:797){
  Sr_new$Index[i] <- sum(Sr_new[i,3:10])
}

Srav <- Sr_new[, c(1:2,11)]
Step1 <- Srav
Ozon <- Srav %>% select(-Platform) %>% subset(Srav$Platform == "Ozon")
Wild <- Srav %>% select(-Platform) %>% subset(Srav$Platform == "Wild")

Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")

ggplot(Ozon, aes(x=Ind_Ozon)) + geom_density() + xlab("Индекс для Ozon") + ylab("Плотность")
ggplot(Ozon, aes(x=Ind_Wild)) + geom_density() + xlab("Индекс для Wildberries") + ylab("Плотность")

ggplot(Step1, aes(x=Index, fill=Platform)) + geom_density(alpha=.3)+ xlab("Индекс") + ylab("Плотность")+ labs(fill = "Платформа")

ggplot(Srav) +
  aes(x = Platform, y = Index) +
  geom_boxplot() +
  theme_minimal() + xlab("Платформа") + ylab("Индекс")

t.test(Index ~ Platform, data = Srav)

Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Wildberries победил: 186 против 131
Future <- Ozon

########################################################################################
##### Значения индекса через индивид веса  ######
#################################################
All <- Sr %>% inner_join(new[1:9], by = "ID")
Ind <- matrix(nrow = 486, ncol=1)
for (i in 1:486){
  Ind[i,1] <- as.matrix(All[i, 3:10]) %*% as.matrix(t(All[i, 11:18]))
  cat("iteration #", i, "\n")
}
All$Index <- Ind
Summary <- All[, c(1:2, 19)]
Step2 <- Summary
Ozon <- Summary %>% select(-Platform) %>% subset(Summary$Platform == "Ozon")
Wild <- Summary %>% select(-Platform) %>% subset(Summary$Platform == "Wild")

Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")

ggplot(Ozon, aes(x=Ind_Ozon)) + geom_density() + xlab("Индекс для Ozon") + ylab("Плотность")
ggplot(Ozon, aes(x=Ind_Wild)) + geom_density()+ xlab("Индекс для Wildberries") + ylab("Плотность")

ggplot(Summary) +
  aes(x = Platform, y = Index) +
  geom_boxplot() +
  theme_minimal() + xlab("Платформа") + ylab("Индекс")

t.test(Index ~ Platform, data = Summary)

Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Wildberries победил: 124 против 94

ggplot(Step2, aes(x=Index, fill=Platform)) + geom_density(alpha=.3)+ xlab("Индекс") + ylab("Плотность")+ labs(fill = "Платформа")

########################################################################################
##### Значения индекса через веса из факторного анализа + Саати среднее ######
##############################################################################
Vmeste <- read_excel("Vmeste.xlsx")
Vmeste <- Vmeste %>% select(-Fool) %>% subset(Vmeste$Fool>5)
Vmeste <- na.omit(Vmeste)
Vmeste$Negative <- -1 *Vmeste$Negative
Vmeste$Support <- -1 *Vmeste$Support
Vmeste$Response <- -1 *Vmeste$Response
corrplot(cor(Vmeste[3:9]), addCoef.col = TRUE, p.mat = cor_p$p,sig.level = 0.10, insig='blank')
corrplot(cor(Vmeste[10:13]), addCoef.col = TRUE, p.mat = cor_p$p,sig.level = 0.10, insig='blank') #post pay minus
corrplot(cor(Vmeste[14:16]), addCoef.col = TRUE)
corrplot(cor(Vmeste[17:25]), addCoef.col = TRUE) 
corrplot(cor(Vmeste[26:27]), addCoef.col = TRUE, p.mat = cor_p$p,sig.level = 0.10, insig='blank')
corrplot(cor(Vmeste[28:30]), addCoef.col = TRUE, p.mat = cor_p$p,sig.level = 0.10, insig='blank')
corrplot(cor(Vmeste[31:33]), addCoef.col = TRUE, p.mat = cor_p$p,sig.level = 0.10, insig='blank')
corrplot(cor(Vmeste[34:36]), addCoef.col = TRUE, p.mat = cor_p$p,sig.level = 0.10, insig='blank')
Vmeste <- Vmeste %>% select(-Postpay)
#### Структурная модель ####
m <- "# факторные модели
Web  =~ NoAnnoy + NoInfNoise	+ Interface + EasyOrder +Filters + Mobile + Categories	
Pay =~ CardLink + Confidentiality + MoneyBack	
Ass =~ Match+ LowPrices + Satisfaction
Deliv =~ Return + NoDamage + Delivery + Pickpoint + Polite + Notification + Rejection + Fit	
Peop =~ Relatives + Friends	
Exp =~ Negative + Support + Response	
Loyal =~ Loyalty + Bonuses + Sale	
Card =~ Photo + Character + Comment	
Index =~ Web + Pay + Ass + Deliv + Peop + Exp + Loyal + Card
# регрессия
"
fit <- sem(m, data = Vmeste)
summary(fit, standardized=TRUE, fit.measures=TRUE)

fitmeasures(fit,fit.measures="all",baseline.modle=null)
summary(fit, rsquare = TRUE)

#визуализация
semPaths(fit, "std", edge.label.cex = 0.8, curvePivot = FALSE, rotation = 2, 
         residuals = FALSE, sizeMan = 2, what = "path", label.cex = 2.5, sizeLat = 4,
         edge.color="black")

standardizedSolution(fit)

Latent <- as.data.frame(lavPredict(fit, type = "lv"))
corrplot(cor(Latent[1:8]), addCoef.col = TRUE)

Vmeste <- na.omit(Vmeste)
Latent <- as.data.frame(Latent)
Ind_fac <- cbind(Vmeste[1:2],Latent[1:8])
Latent <- cbind(Vmeste[1:2],Latent[1:8])

Ind_fac$Web <- mean_S[1] * Ind_fac$Web
Ind_fac$Pay <- mean_S[2] * Ind_fac$Pay
Ind_fac$Ass <- mean_S[3] * Ind_fac$Ass
Ind_fac$Deliv <- mean_S[4] * Ind_fac$Deliv
Ind_fac$Peop <- mean_S[5] * Ind_fac$Peop
Ind_fac$Exp <- mean_S[6] * Ind_fac$Exp
Ind_fac$Loyal <- mean_S[7] * Ind_fac$Loyal
Ind_fac$Card <- mean_S[8] * Ind_fac$Card

for (i in 1:796){
  Ind_fac$Index[i] <- sum(Ind_fac[i,3:10])
}

All <- cbind(Vmeste[, 1:2],Ind_fac$Index)
colnames(All)[3] <- "Index"
Step3 <- All
Ozon <- All %>% select(-Platform) %>% subset(All$Platform == "Ozon")
Wild <- All %>% select(-Platform) %>% subset(All$Platform == "Wild")

Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")

ggplot(Ozon, aes(x=Ind_Ozon)) + geom_density() + xlab("Индекс для Ozon") + ylab("Плотность")
ggplot(Ozon, aes(x=Ind_Wild)) + geom_density() + xlab("Индекс для Wildberries") + ylab("Плотность")

ggplot(All) +
  aes(x = Platform, y = Index) +
  geom_boxplot() +
  theme_minimal() + xlab("Платформа") + ylab("Индекс")

t.test(Index ~ Platform, data = All)

Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Wildberries победил: 169 против 148

ggplot(Step3, aes(x=Index, fill=Platform)) + geom_density(alpha=.3)+ xlab("Индекс") + ylab("Плотность")+ labs(fill = "Платформа")

########################################################################################
##### Значения индекса через веса из факторного анализа + Саати индивид ######
##############################################################################
St4 <- Latent[1:10]
All <- St4 %>% inner_join(new[1:9], by = "ID")

Ind <- matrix(nrow=486, ncol=1)
for (i in 1:486){
  Ind[i,1] <- as.matrix(All[i, 3:10]) %*% as.matrix(t(All[i, 11:18]))
  cat("iteration #", i, "\n")
}
All$Index <- Ind
Summary <- All[, c(1:2, 19)]
Step4 <- Summary
Ozon <- Summary %>% select(-Platform) %>% subset(Summary$Platform == "Ozon")
Wild <- Summary %>% select(-Platform) %>% subset(Summary$Platform == "Wild")

Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")

ggplot(Ozon, aes(x=Ind_Ozon)) + geom_density() + xlab("Индекс для Ozon") + ylab("Плотность")
ggplot(Ozon, aes(x=Ind_Wild)) + geom_density() + xlab("Индекс для Wildberries") + ylab("Плотность")

ggplot(Summary) +
  aes(x = Platform, y = Index) +
  geom_boxplot() +
  theme_minimal() + xlab("Платформа") + ylab("Индекс")

t.test(Index ~ Platform, data = Summary)

Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Wildberries победил: 109 против 109

ggplot(Step4, aes(x=Index, fill=Platform)) + geom_density(alpha=.3)+ xlab("Индекс") + ylab("Плотность")+ labs(fill = "Платформа")

########################################################################################
##### Значения индекса через веса из факторного анализа + Веса факторов ######
##############################################################################
Latent <- lavPredict(fit)
Latent<- as.data.frame(Latent)
Latent <- cbind(Vmeste[1:2],Latent)
Fin <- Latent[,c(1:2,11)]
Ozon <- Fin %>% select(-Platform) %>% subset(Fin$Platform == "Ozon")
Wild <- Fin %>% select(-Platform) %>% subset(Fin$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")

ggplot(Ozon, aes(x=Ind_Ozon)) + geom_density() + xlab("Индекс для Ozon") + ylab("Плотность")
ggplot(Ozon, aes(x=Ind_Wild)) + geom_density() + xlab("Индекс для Wildberries") + ylab("Плотность")
ggplot(Fin) +
  aes(x = Platform, y = Index) +
  geom_boxplot() +
  theme_minimal() + xlab("Платформа") + ylab("Индекс")

Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Wildberries победил: 170 против 147
t.test(Index ~ Platform, data = Fin)
Step5 <- Fin
ggplot(Step5, aes(x=Index, fill=Platform)) + geom_density(alpha=.3)+ xlab("Индекс") + ylab("Плотность")+ labs(fill = "Платформа")

##############################################################################
###### Победы внутри кластеров #######
######################################
Clust1_ID <- Full_clust[9:10] %>% subset(Full_clust$cluster == 1)
Clust2_ID <- Full_clust[9:10] %>% subset(Full_clust$cluster == 2)
Clust3_ID <- Full_clust[9:10] %>% subset(Full_clust$cluster == 3)
Clust4_ID <- Full_clust[9:10] %>% subset(Full_clust$cluster == 4)

#кластер 1
Clust11 <- Step1 %>% inner_join(Clust1_ID[1], by = "ID")
Ozon <- Clust11 %>% select(-Platform) %>% subset(Clust11$Platform == "Ozon")
Wild <- Clust11 %>% select(-Platform) %>% subset(Clust11$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы: 40|32
t.test(Index ~ Platform, data = Step1)

Clust11 <- Step2 %>% inner_join(Clust1_ID[1], by = "ID")
Ozon <- Clust11 %>% select(-Platform) %>% subset(Clust11$Platform == "Ozon")
Wild <- Clust11 %>% select(-Platform) %>% subset(Clust11$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы: 40|32
t.test(Index ~ Platform, data = Step2)

Clust11 <- Step3 %>% inner_join(Clust1_ID[1], by = "ID")
Ozon <- Clust11 %>% select(-Platform) %>% subset(Clust11$Platform == "Ozon")
Wild <- Clust11 %>% select(-Platform) %>% subset(Clust11$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы: 42|30
t.test(Index ~ Platform, data = Step3)

Clust11 <- Step4 %>% inner_join(Clust1_ID[1], by = "ID")
Ozon <- Clust11 %>% select(-Platform) %>% subset(Clust11$Platform == "Ozon")
Wild <- Clust11 %>% select(-Platform) %>% subset(Clust11$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы: 40|32
t.test(Index ~ Platform, data = Step4)

Clust11 <- Step5 %>% inner_join(Clust1_ID[1], by = "ID")
Ozon <- Clust11 %>% select(-Platform) %>% subset(Clust11$Platform == "Ozon")
Wild <- Clust11 %>% select(-Platform) %>% subset(Clust11$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы: 42|30
t.test(Index ~ Platform, data = Step5)

#кластер 2
Clust12 <- Step1 %>% inner_join(Clust2_ID[1], by = "ID")
Ozon <- Clust12 %>% select(-Platform) %>% subset(Clust12$Platform == "Ozon")
Wild <- Clust12 %>% select(-Platform) %>% subset(Clust12$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы: 33|29
t.test(Index ~ Platform, data = Step1)

Clust12 <- Step2 %>% inner_join(Clust2_ID[1], by = "ID")
Ozon <- Clust12 %>% select(-Platform) %>% subset(Clust12$Platform == "Ozon")
Wild <- Clust12 %>% select(-Platform) %>% subset(Clust12$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы:  40|22
t.test(Index ~ Platform, data = Step2)

Clust12 <- Step3 %>% inner_join(Clust2_ID[1], by = "ID")
Ozon <- Clust12 %>% select(-Platform) %>% subset(Clust12$Platform == "Ozon")
Wild <- Clust12 %>% select(-Platform) %>% subset(Clust12$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы:26|36
t.test(Index ~ Platform, data = Step3)

Clust12 <- Step4 %>% inner_join(Clust2_ID[1], by = "ID")
Ozon <- Clust12 %>% select(-Platform) %>% subset(Clust12$Platform == "Ozon")
Wild <- Clust12 %>% select(-Platform) %>% subset(Clust12$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы: 27|35
t.test(Index ~ Platform, data = Step4)

Clust12 <- Step5 %>% inner_join(Clust2_ID[1], by = "ID")
Ozon <- Clust12 %>% select(-Platform) %>% subset(Clust12$Platform == "Ozon")
Wild <- Clust12 %>% select(-Platform) %>% subset(Clust12$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы: 28|34
t.test(Index ~ Platform, data = Step5)

#кластер 3
Clust13 <- Step1 %>% inner_join(Clust3_ID[1], by = "ID")
Ozon <- Clust13 %>% select(-Platform) %>% subset(Clust13$Platform == "Ozon")
Wild <- Clust13 %>% select(-Platform) %>% subset(Clust13$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Oзон победил: 28|28
t.test(Index ~ Platform, data = Step1)

Clust13 <- Step2 %>% inner_join(Clust3_ID[1], by = "ID")
Ozon <- Clust13 %>% select(-Platform) %>% subset(Clust13$Platform == "Ozon")
Wild <- Clust13 %>% select(-Platform) %>% subset(Clust13$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Oзон победил: 27|29
t.test(Index ~ Platform, data = Step2)

Clust13 <- Step3 %>% inner_join(Clust3_ID[1], by = "ID")
Ozon <- Clust13 %>% select(-Platform) %>% subset(Clust13$Platform == "Ozon")
Wild <- Clust13 %>% select(-Platform) %>% subset(Clust13$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Oзон победил: 29|27
t.test(Index ~ Platform, data = Step3)

Clust13 <- Step4 %>% inner_join(Clust3_ID[1], by = "ID")
Ozon <- Clust13 %>% select(-Platform) %>% subset(Clust13$Platform == "Ozon")
Wild <- Clust13 %>% select(-Platform) %>% subset(Clust13$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Oзон победил:  27|29
t.test(Index ~ Platform, data = Step4)

Clust13 <- Step5 %>% inner_join(Clust3_ID[1], by = "ID")
Ozon <- Clust13 %>% select(-Platform) %>% subset(Clust13$Platform == "Ozon")
Wild <- Clust13 %>% select(-Platform) %>% subset(Clust13$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Oзон победил: 27|29 
t.test(Index ~ Platform, data = Step5)

#кластер 4
Clust14 <- Step1 %>% inner_join(Clust4_ID[1], by = "ID")
Ozon <- Clust14 %>% select(-Platform) %>% subset(Clust14$Platform == "Ozon")
Wild <- Clust14 %>% select(-Platform) %>% subset(Clust14$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Oзон победил: 20|8
t.test(Index ~ Platform, data = Step1)

Clust14 <- Step2 %>% inner_join(Clust4_ID[1], by = "ID")
Ozon <- Clust14 %>% select(-Platform) %>% subset(Clust14$Platform == "Ozon")
Wild <- Clust14 %>% select(-Platform) %>% subset(Clust14$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Oзон победил: 17|11
t.test(Index ~ Platform, data = Step2)

Clust14 <- Step3 %>% inner_join(Clust4_ID[1], by = "ID")
Ozon <- Clust14 %>% select(-Platform) %>% subset(Clust14$Platform == "Ozon")
Wild <- Clust14 %>% select(-Platform) %>% subset(Clust14$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Oзон победил: 12|16
t.test(Index ~ Platform, data = Step3)

Clust14 <- Step4 %>% inner_join(Clust4_ID[1], by = "ID")
Ozon <- Clust14 %>% select(-Platform) %>% subset(Clust14$Platform == "Ozon")
Wild <- Clust14 %>% select(-Platform) %>% subset(Clust14$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Oзон победил: 15|13
t.test(Index ~ Platform, data = Step4)

Clust14 <- Step5 %>% inner_join(Clust4_ID[1], by = "ID")
Ozon <- Clust14 %>% select(-Platform) %>% subset(Clust14$Platform == "Ozon")
Wild <- Clust14 %>% select(-Platform) %>% subset(Clust14$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Oзон победил: 14|14
t.test(Index ~ Platform, data = Step5)

###################################################################################
####### Победы внутри кластеров по дем характеристикам #####
############################################################
Dem <- people[, c(1,38)]
Clust1 <- Dem %>% subset(Dem$clust ==1)
Clust2 <- Dem %>% subset(Dem$clust ==2)

#кластер 1
Clust21 <- Step1 %>% inner_join(Clust1[1], by = "ID")
Ozon <- Clust21 %>% select(-Platform) %>% subset(Clust21$Platform == "Ozon")
Wild <- Clust21 %>% select(-Platform) %>% subset(Clust21$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы: 82|69
t.test(Index ~ Platform, data = Step1)

Clust21 <- Step2 %>% inner_join(Clust1[1], by = "ID")
Ozon <- Clust21 %>% select(-Platform) %>% subset(Clust21$Platform == "Ozon")
Wild <- Clust21 %>% select(-Platform) %>% subset(Clust21$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы: 64|51
t.test(Index ~ Platform, data = Step2)

Clust21 <- Step3 %>% inner_join(Clust1[1], by = "ID")
Ozon <- Clust21 %>% select(-Platform) %>% subset(Clust21$Platform == "Ozon")
Wild <- Clust21 %>% select(-Platform) %>% subset(Clust21$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы: 73|78
t.test(Index ~ Platform, data = Step3)

Clust21 <- Step4 %>% inner_join(Clust1[1], by = "ID")
Ozon <- Clust21 %>% select(-Platform) %>% subset(Clust21$Platform == "Ozon")
Wild <- Clust21 %>% select(-Platform) %>% subset(Clust21$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы: 52|63
t.test(Index ~ Platform, data = Step4)

Clust21 <- Step5 %>% inner_join(Clust1[1], by = "ID")
Ozon <- Clust21 %>% select(-Platform) %>% subset(Clust21$Platform == "Ozon")
Wild <- Clust21 %>% select(-Platform) %>% subset(Clust21$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы: 73|78
t.test(Index ~ Platform, data = Step5)

#кластер 2
Clust22 <- Step1 %>% inner_join(Clust2[1], by = "ID")
Ozon <- Clust22 %>% select(-Platform) %>% subset(Clust22$Platform == "Ozon")
Wild <- Clust22 %>% select(-Platform) %>% subset(Clust22$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы: 72|41
t.test(Index ~ Platform, data = Step1)

Clust22 <- Step2 %>% inner_join(Clust2[1], by = "ID")
Ozon <- Clust22 %>% select(-Platform) %>% subset(Clust22$Platform == "Ozon")
Wild <- Clust22 %>% select(-Platform) %>% subset(Clust22$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы: 53|34
t.test(Index ~ Platform, data = Step2)

Clust22 <- Step3 %>% inner_join(Clust2[1], by = "ID")
Ozon <- Clust22 %>% select(-Platform) %>% subset(Clust22$Platform == "Ozon")
Wild <- Clust22 %>% select(-Platform) %>% subset(Clust22$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы: 66|47
t.test(Index ~ Platform, data = Step3)

Clust22 <- Step4 %>% inner_join(Clust2[1], by = "ID")
Ozon <- Clust22 %>% select(-Platform) %>% subset(Clust22$Platform == "Ozon")
Wild <- Clust22 %>% select(-Platform) %>% subset(Clust22$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы: 50|37
t.test(Index ~ Platform, data = Step4)

Clust22 <- Step5 %>% inner_join(Clust2[1], by = "ID")
Ozon <- Clust22 %>% select(-Platform) %>% subset(Clust22$Platform == "Ozon")
Wild <- Clust22 %>% select(-Platform) %>% subset(Clust22$Platform == "Wild")
Ozon <- Ozon %>% inner_join(Wild, by = "ID")
colnames(Ozon) <- c("ID", "Ind_Ozon", "Ind_Wild")
Ozon$win <- ifelse(Ozon$Ind_Ozon - Ozon$Ind_Wild > 0, 1, 0)
table(Ozon$win) #Победы: 70|43
t.test(Index ~ Platform, data = Step5)

##################################################
#### Регрессии ########
##################################################
Index_all <- as.data.frame(lavPredict(fit))
Index_all <- data.frame(Vmeste[1:2],Index_all$Index)
colnames(Index_all)[3] <- "Index"

End <- Index_all %>% inner_join(new[1:9], by = "ID")
End <- End %>% inner_join(Saaty[,c(1, 30:37)], by = "ID")
End$Platform <- as.factor(End$Platform)

model <- lm(Index ~ ., data = End)
summary(model)

model2 <- stepAIC(model)
summary(model2)
vif(model2)

model3 <- update(model2, .~. -Job_stat, data = End)
summary(model3)
crPlots(model3)
boxCox(model2)

library(stargazer)
stargazer(model2, 
          title="Влияние предпочтений", type="text", 
          column.labels=c("Индекс качества"), 
          df=FALSE, digits=3, out = "All data ols.txt")


#### Ozon
Index_all <- as.data.frame(lavPredict(fit))
Index_all <- data.frame(Vmeste[1:2],Index_all$Index)
colnames(Index_all)[3] <- "Index"

End <- Index_all %>% inner_join(new[1:9], by = "ID")
End <- End %>% inner_join(Saaty[,c(1, 30:37)], by = "ID")
End$Platform <- as.factor(End$Platform)
End <- End[2:19]
Oz <- End %>% select(-Platform) %>% subset(End$Platform == "Ozon")
cor(na.omit(Oz))
mod <- lm(Index ~ ., data = Oz)
summary(mod)

mod2 <- stepAIC(mod)
summary(mod2)
vif(mod2)

#### Wild
Wl <- End %>% select(-Platform) %>% subset(End$Platform == "Wild")

mod12 <- lm(Index ~ ., data = Wl)
summary(mod12)

mod23 <- stepAIC(mod12)
summary(mod23)
vif(mod23)

stargazer(mod2, mod23, 
          title="Влияние предпочтений", type="text", 
          column.labels=c("Ozon", "Wildberries"), 
          df=FALSE, digits=3, out = "All data ols.txt")

################################################################
###### Корреляционная матрица факторов и весов #######
######################################################
View(new)
Block1 <- Latent %>% inner_join(new[1:9], by = "ID")
corrplot(cor(Block1[3:18]))

cor_p <- cor.mtest(Block1[3:18], conf.level = 0.95)
corrplot(cor(Block1[3:18]), addgrid.col = TRUE, addCoef.col = TRUE,
         type = 'lower', method = 'color',
         p.mat = cor_p$p, sig.level = 0.10, insig='blank')

#################################################################
##### Плотности распределния #################
##############################################
Diag <- Latent[2:10]
DO <- Diag %>% select(-Platform) %>% subset(Diag$Platform == "Ozon")
DW <- Diag %>% select(-Platform) %>% subset(Diag$Platform == "Wild")

ggplot(Diag, aes(x=Web, fill=Platform)) + geom_density(alpha=.3) + xlab("Качество сайта") + ylab("Плотность") + labs(fill = "Платформа")
ks.test(DO$Web, DW$Web)

ggplot(Diag, aes(x=Pay, fill=Platform)) + geom_density(alpha=.3)+ xlab("Способ оплаты") + ylab("Плотность")+ labs(fill = "Платформа")
ks.test(DO$Pay, DW$Pay)

ggplot(Diag, aes(x=Ass, fill=Platform)) + geom_density(alpha=.3)+ xlab("Ассортимент") + ylab("Плотность")+ labs(fill = "Платформа")
ks.test(DO$Ass, DW$Ass)

ggplot(Diag, aes(x=Deliv, fill=Platform)) + geom_density(alpha=.3)+ xlab("Доставка") + ylab("Плотность")+ labs(fill = "Платформа")
ks.test(DO$Deliv, DW$Deliv)

ggplot(Diag, aes(x=Loyal, fill=Platform)) + geom_density(alpha=.3)+ xlab("Программа лояльности") + ylab("Плотность")+ labs(fill = "Платформа")
ks.test(DO$Loyal, DW$Loyal)

ggplot(Diag, aes(x=Card, fill=Platform)) + geom_density(alpha=.3)+ xlab("Карточка товара") + ylab("Плотность")+ labs(fill = "Платформа")
ks.test(DO$Card, DW$Card)

ggplot(Fin, aes(x=Index, fill=Platform)) + geom_density(alpha=.3)+ xlab("Индекс") + ylab("Плотность")+ labs(fill = "Платформа")
FO <-  Fin %>% select(-Platform) %>% subset(Fin$Platform == "Ozon")
FW <-  Fin %>% select(-Platform) %>% subset(Fin$Platform == "Wild")
ks.test(FO$Index, FW$Index)

install.packages("modelsummary")
library(modelsummary)
Sred <- Vmeste
Sred$Platform <- ifelse(Sred$Platform == "Ozon", 1, 0)
datasummary_balance(~Platform, Sred, output = "gt", stars = TRUE)
#stars = TRUE выводит значимость в виде звезд. 
#dinm_statistic = "p.value" – добавляет p-value вместо стандартной ошибки

Sr_O <- Sr
datasummary_balance(~Platform, Sr_O, output = "gt", stars = TRUE, dinm_statistic = "p.value")

####################################################################
###### Альфы Кронбаха ######
############################
install.packages("ltm")
library(ltm)
Vmeste <- na.omit(read_excel("Vmeste.xlsx"))
cronbach.alpha(Latent[3:9])
cronbach.alpha(Vmeste[3:9])
cronbach.alpha(Vmeste[10:13])
cronbach.alpha(Vmeste[, c(10,11,13)])
cronbach.alpha(Vmeste[14:16])
cronbach.alpha(Vmeste[17:25])
cronbach.alpha(Vmeste[26:27])
cronbach.alpha(Vmeste[28:30])
cronbach.alpha(Vmeste[31:33])
cronbach.alpha(Vmeste[34:36])
