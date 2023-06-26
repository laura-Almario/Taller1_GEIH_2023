set.seed(10101)
##Dividir muestra
sample <- sample.int(n = nrow(df), size = floor(.70*nrow(df)), replace = F)
train <- df[sample, ]
test  <- df[-sample, ]

##5 modelos
#Modelo 1 edad al cuadrado

modelo5_1 <- lm(log_y_total_m ~ age + I(age^2), data = train)

#Modelo 2 sexo

modelo5_2 <- lm(log_y_total_m ~ mujer, data = train)

#Modelo 3 sexo con controles 

modelo5_3 <- lm(log_y_total_m~mujer + formal + age + I(age^2) + maxEducLevel 
            + hoursWorkUsual + oficio + relab + sizeFirm + JHOGAR, data= train)

#Modelo 4 sexo y educacion

modelo5_4 <- lm(log_y_total_m~mujer + age + I(age^2) + maxEducLevel, data= train)

#Modelo 5 

modelo5_5 <- lm(log_y_total_m~mujer + formal + age + I(age^2), data= train)
                
#Comparar resultados

residuales1<-mean(modelo5_1$residuals)
residuales2<-mean(modelo5_2$residuals)
residuales3<-mean(modelo5_3$residuals)
residuales4<-mean(modelo5_4$residuals)
residuales5<-mean(modelo5_5$residuals)

tabla1<-rbind(residuales1,residuales2, residuales3, residuales4, residuales5)
colnames(table1) <- c("value")

#LOOCV

set.seed(10101)
#LOOCV 
error_LOOCV <- c()

for (i in 1:dim(train)[1]) {
  modelo5_d <- lm(log_y_total_m ~ mujer + age + I(age^2) + maxEducLevel, data = train[-i,])
  error_LOOCV[i] <- train$log_y_total_m[i] - predict(modelo5_d, newdata = train[i,])
  print(i)
}

mse_LOOCV_train<-mean(error_LOOCV*error_LOOCV)
CV_LOOCV<-sum(error_LOOCV^2)/nrow(train)

set.seed(10101)
#LOOCV 
error_LOOCV2 <- c()

for (i in 1:dim(train)[1]) {
  modelo5_d2 <- lm(log_y_total_m~mujer + formal + age + I(age^2), data= train[-i,])
  error_LOOCV[i] <- train$log_y_total_m[i] - predict(modelo5_d2, newdata = train[i,])
  print(i)
}

mse_LOOCV_train<-mean(error_LOOCV2*error_LOOCV2)
CV_LOOCV2<-sum(error_LOOCV2^2)/nrow(train)

tabla5_d <- cbind(CV_LOOCV,CV_LOOCV2)
