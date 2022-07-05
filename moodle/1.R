install.packages('fastDummies')
install.packages('caret')
install.packages('MASS')
install.packages('generalhoslem')
install.packages('psych')
install.packages('cluster')
library(psych)
library('caret')
library('MASS')
library('fastDummies')
library(haven)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend)
library(dplyr)
library(psych)
library(corrplot)
library("psych")
library(ggplot2)
library("MASS")
library('cluster')





set.seed(123)

path = file.path("C://Users//aland//Creative Cloud Files//UOC//Analisis multivariante//Prectica//Nueva carpeta (2)//3339.sav")
df = read_sav(path)

df1 <- df %>% select (SEXO, EDAD, CAPITAL,P16_1,P16_2)
df1 <- na.omit(df1)
df1 <- filter(df1,  P16_1 <= 4)
df1 <- filter(df1,  P16_2 <= 4)

df1 <- dummy_cols(df1, select_columns = "CAPITAL")
df1 <- df1 %>%select(SEXO, EDAD, P16_1,P16_2,CAPITAL_1,CAPITAL_2,CAPITAL_3)
df_scaled <- scale(df1, center = FALSE, scale = TRUE)
fviz_nbclust(df_scaled, FUN = hcut, method = "silhouette")
k_3 <- kmeans(df_scaled, centers = 3, nstart = 25)
df1 %>%mutate(Cluster = k_3$cluster) %>%group_by(Cluster) %>%summarise_all("mean")
 

# codigo para hirerquical
d <- dist(X, method = "euclidean")
hc1 <- hclust(d, method = "complete" )
cut_avg <- cutree(hc1, k = 3)
df %>%mutate(Cluster = cut_avg) %>%group_by(Cluster) %>%summarise_all("mean")
plot(hc1, cex = 0.6, hang = -1)

### Pregunta 2

df2 <- df %>%select(SEXO, EDAD, ESCIDEOL,P18,P2_1)
df2
df2 <- na.omit(df2)
df2 <- data.frame(sapply(df2, haven::as_factor, levels='value'))
# df2$EDAD <- as.numeric(df2$EDAD)
# df2$SEXO <- as.numeric(df2$SEXO)
# df2$ESCIDEOL <- as.numeric(df2$ESCIDEOL)
# df2$ESCIDEOL <- as.numeric(df2$ESCIDEOL)
df2 <- data.frame(sapply(df2, as.numeric))
df2$P2_1 <-ifelse(df2$P2_1 > 1, 0, 1)
df2$SEXO <-ifelse(df2$SEXO > 1, 0, 1)
model <- glm(P2_1 ~ SEXO+EDAD+ESCIDEOL+P18,data=df2)
stepAIC(model, direction = 'both')
model <- glm(formula = P2_1 ~ SEXO + EDAD + ESCIDEOL, data = df2)
summary(model)
varImp(model, scale = TRUE)
logitgof(df2$P2_1, fitted(model))




### pregunta 3
setwd("C:/Users/aland/Creative Cloud Files/UOC/Analisis multivariante/Prectica/Nueva carpeta (2)")
df3 <-  read_sav('3137.sav')
df3 <- df3 %>% select(P27_1,P27_2,P27_3,P27_4,P27_5,P27_6,P27_7,P27_8,P27_9,P27_10)
df3 <- na.omit(df3)


# df3 <- data.frame(sapply(df3, haven::as_factor, levels='value'))

df3 <- filter(df3, P27_1 <=5 & P27_2<=5,P27_3 <=5 & P27_4<=5 & P27_5<=5 & P27_6<=5 & P27_7<=5 & P27_8<=5 & P27_9 <= 5  & P27_10<= 5)

cor_mat <- cor(df3)
ei_val <- eigen(cor_mat)

n_factors <- length(ei_val$values)

scree(df3, pc=TRUE)



# scree <- data.frame(Factor_n =  as.factor(1:n_factors),Eigenvalue = ei_val$values)
# scree
# ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
#   geom_point() + geom_line() +
#   xlab("Number of factors") +
#   ylab("Initial eigenvalue") +
#   labs( title = "Scree Plot", 
#         subtitle = "(Based on the unreduced correlation matrix)")

analisis <- principal(r=df3, nfactors = 4,fm = "pa",rotate= "varimax")
print(analisis, cut = 0.3, digits = 3)

fa.diagram(analisis)


# 9.	Se pone nervioso/a con facilidad 
# 3.	Se deja dominar por el estrés

# 10.	Tiene mucha imaginación
# 5.	Tiene interés por lo artístico 
# 4.	Le resulta fácil ponerse en el lugar de los/as demás 

# 2.	Tiende a ser perezoso/a
# 8.	Realiza concienzudamente las cosas que tiene que hacer 

# 1.	Tiende a ser reservado/a 
# 6.	Es extrovertido/a y sociable 

# 7.	Tiene tendencia a criticar a los/as demás 
fac1 <- c("P27_9","P27_3")
fac2 <- c("P27_10","P27_5","P27_4")
fac3 <- c("P27_8","P27_2")
fac4 <- c("P27_6","P27_1")
alpha(df3[fac4],'check.keys=TRUE' )



analisis <- fa(r=df3[-c(2,4,7)],nfactors = 4)
analisis
fa.diagram(analisis)











path("C:/Users/aland/Creative Cloud Files/UOC/Analisis multivariante/Pec2")



df <- read.csv('CollegeDistance.csv')
# iltramos las columnas que plantea el ejercicio
df<- df[c('score','education','gender','fcollege','mcollege','home','urban','income')]

df$gender<-ifelse(df$gender == 'male', 1, 0)
df$income<-ifelse(df$income == 'high', 1, 0)
df$fcollege<-ifelse(df$fcollege == 'yes', 1, 0)
df$mcollege<-ifelse(df$mcollege == 'yes', 1, 0)
df$home<-ifelse(df$home == 'yes', 1, 0)
df$urban<-ifelse(df$urban == 'yes', 1, 0)
# escalamos los datos ya que Kmeans en sens
clustering <- function(df,k){
  scaled <- scale(df,center=TRUE,scale=TRUE)
  # aplicamos la función k means
  kmeans_df<-kmeans(scaled,center=k)
  # añadimos el vector cluster al dataframe
  df$cluster<-kmeans_df$cluster
  #agrupamos por los grupos generados y obtenemos la media de las columnas
  return(df %>% group_by(cluster) %>% summarise_all("mean"))
}
clustering(df,2)

ç
d.agr <- daisy(agriculture, metric = "euclidean", stand = FALSE)



dat= read.spss("3339.sav",to.data.frame = T)
sub_dat_2= na.omit(dat[,c("P2_1","SEXO","EDAD","CAPITAL","P18")])
# sub_dat_2$EDAD <- as.numeric(sub_dat_2$EDAD)
sub_dat_2$EDAD


sub_dat_2$CAPITAL=factor(sub_dat_2$CAPITAL,labels = c("CC.AA.","Provincia","Otros"))

sub_dat_2$P18=as.numeric(as.character(factor(sub_dat_2$P18,labels=(c(1,2,3,4,5,6,7,8,9,10)))))
sub_dat_2$P2_1=as.numeric(as.character(factor(sub_dat_2$P2_1,labels=(c(0,1)))))

mod_log=(glm(formula = P2_1 ~ SEXO+EDAD+CAPITAL+P18, family = "binomial", data = sub_dat_2))
factores=step(mod_log,direction = "both")
mod_log_new=(glm(formula = P2_1 ~ SEXO+EDAD+CAPITAL, family = "binomial", data = sub_dat_2))
summary(mod_log_new)
