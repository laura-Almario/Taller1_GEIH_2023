##PUNTO 4##

df <- df %>%
  mutate(mujer = recode(sex, 
                        `0` = "1", 
                        `1` = "0"))
df$mujer <- ifelse(is.na(df$mujer), "0", df$mujer)


#a
modelo2a <- lm(log_y_total_m ~ mujer, data = df)
library(stargazer)
regresion_modelo2a <- stargazer(modelo2a, type="text")
regresion_modelo2a
capture.output(regresion_modelo2a, file="regresion_modelo2a.doc")

#b

modelob<-lm(log_y_total_m~mujer + formal + age + I(age^2) + maxEducLevel 
            + hoursWorkUsual + oficio + relab + sizeFirm + JHOGAR, data= df)
modelob1<-lm(log_y_total_m~sex + formal + age + I(age^2) + maxEducLevel 
            + hoursWorkUsual + oficio + relab + sizeFirm + JHOGAR, data= df)
stargazer(modelob, type = "text")
capture.output(modelob, file="modelob.doc")
stargazer(modelob1, type = "text")
capture.output(modelob1, file="modelob1.doc")

##Teorema FWL

summary(df)

y_cont<-lm(log_y_total_m~ formal + age + I(age^2) + maxEducLevel 
           + hoursWorkUsual + oficio + relab + sizeFirm + JHOGAR, data= df)

mujer_cont<-lm(mujer~ formal + age + I(age^2) + maxEducLevel 
               + hoursWorkUsual + oficio + relab + sizeFirm + JHOGAR, data= df)

df$res_y= y_cont$residuals
df$res_f= mujer_cont$residuals

reg_final<-lm(res_y~ res_f, data=df)

stargazer(modelob, reg_final, type="text")
capture.output(reg_final, file="reg_final.doc")

#c

model2cf <-lm(log_y_total_m~age + I(age^2),data=subset(df,mujer==1))
summary(model2cf)

model2cm <-lm(log_y_total_m~age + I(age^2),data=subset(df,mujer==0))
summary(model2cm)

stargazer(model2cf, model2cm, type="text")
capture.output(model2cf, file="model2cf.doc")

R<-1000
fun_mujer<-function(df,index){
  coef(lm(y_total_m~age + I(age^2),data=subset(df,mujer==1), subset = index))
}
boot(df, fun_mujer, R)

b1_mujer<-modelob$coefficients[2]
b2_mujer<-modelob$coefficients[3]
edad_optima_mujer<--(b1_mujer/(2*b2_mujer))
edad_optima_mujer ##55

fun_hombre<-function(df,index){
  coef(lm(y_total_m~age + I(age^2),data=subset(df,mujer==0), subset = index))
}
boot(df, fun_hombre, R)

b1_hombre<-modelob1$coefficients[2]
b2_hombre<-modelob1$coefficients[3]
edad_optima_hombre<--(b1_hombre/(2*b2_hombre))
edad_optima_hombre ##54

IC_edad_mujer<-confint(modelob, level=0.95)
IC_edad_hombre<-confint(modelob1, level=0.95)

IC_edad_mujer
IC_edad_hombre

df$predict_gender<-ifelse(df$mujer==1, predict(modelob), predict(modelob1))

Base_genero_mujer<-subset(df, mujer==1)
Base_genero_mujer$mujer_pred<-predict(modelob)
g_mujer_p<-ggplot(Base_genero_mujer, aes(x = age, y = mujer_pred)) + geom_point(colour = "orange1") + labs(x = "Edad", y = "Predicción Ingresos Mujer")


Base_genero_hombre<-subset(df, mujer==0)
Base_genero_hombre$hombre_pred<-predict(modelob1)
g_hombre_p<-ggplot(Base_genero_hombre, aes(x = age, y = hombre_pred)) + geom_point(colour = "steelblue1") + labs(x = "Edad", y = " Predicción Ingresos Hombre")


ggarrange(g1, g_mujer_p, g_hombre_p, nrow = 1, ncol = 3)

#Graficas

cor(df$log_y_total_m, df$predict_gender)
ggplot(df, aes(x = predict_gender, y = log_y_total_m)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "green")

g1<-ggplot(df, aes(x = age, y = log_y_total_m)) + geom_point(colour = "darkcyan") + labs(x = "Edad", y = "Ingresos Totales")

g2<-ggplot(df, aes(x = age, y = predict_gender)) + geom_point(colour = "violetred2") + labs(x = "Edad", y = "Ingresos Totales Predicha")
