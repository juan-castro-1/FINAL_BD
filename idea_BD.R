rm(list=ls())
library(rstudioapi)
dir <- getActiveDocumentContext()$path 
setwd(dirname(dir))
dir()

library(data.table)
urbana <- fread("Urbana.csv",header=T,na.strings="?",sep = ";",dec = ",")
rural <- fread("Rural.csv",header=T,na.strings="?",sep = ";",dec = ",")

#C3_E01
#Asiste a la escuela/jardín
#C3_E02 
#Asistió alguna vez a la escuela
urbana <- data.frame(urbana)
rural <- data.frame(rural)
tabla_missin <- sapply(urbana$C3_E02, function(x) sum(is.na(x)))
tabla_missin <-data.frame(tabla_missin)
print(xtable(tabla_missin, type = "csv"), file = "missin")
View(tabla_missin)
table(urbana$C3_E01)

sum(table(urbana$C3_E13))
table(urbana$C3_E16)



table(rural$C3_E01)

urbana['desertor'] <- 0
rural['desertor'] <- 0

urbana$desertor <- ifelse(urbana$C3_E01 ==1, 0,1)
rural$desertor <- ifelse(rural$C3_E01 ==1, 0,1 )

#C2_P03
# sexo==1 varon, sexo==2 mujer
#C2_P02 Edad

library(ggplot2)
urbana$C2_P03 <- as.factor(urbana$C2_P03)
rural$C2_P03 <- as.factor(rural$C2_P03)
library(jpeg)
options(scipen=999)


jpeg("Output/plot_urbana_edad.jpg")
ggplot(urbana[(urbana$desertor==1),], aes(x=C2_P02, group=C2_P03, fill=C2_P03)) +
  geom_density(alpha=0.4) + scale_fill_discrete(name = "Sexo", labels = c("Masculino", "Femenino")) +
  xlim(0,17) +
  ylim(0,0.4) + 
  labs(x="Edad", title = "Población Urbana", y="Densidad") 
dev.off()

jpeg("Output/plot_rural_edad.jpg")
ggplot(rural[(rural$desertor==1),], aes(x=C2_P02, group=C2_P03, fill=C2_P03)) +
  geom_density(alpha=0.4) + scale_fill_discrete(name = "Sexo", labels = c("Masculino", "Femenino")) +
  xlim(0,17) +
  ylim(0,0.4) + 
  labs(x="Edad", title = "Población Rural", y="Densidad")
dev.off()


# CUAL FUE TU ULTIMO AÑO EN EL COLEG
urbana$C3_E15 <- as.factor(urbana$C3_E15)
rural$C2_P03 <- as.factor(rural$C2_P03)

jpeg("Output/plot_urbana_año_D.jpg")
ggplot(urbana, aes(x=C3_E13, group=C3_E15, fill=C3_E15)) +
  geom_density(alpha=0.4) + scale_fill_discrete(name = "Sexo", labels = c("Masculino", "Femenino")) +
  xlim(0,10) +
  ylim(0,0.25) + 
  labs(x="Año de Deserción", title = "Población Urbana", y="Densidad") 
dev.off()

jpeg("Output/plot_rural_año_D.jpg")
ggplot(rural, aes(x=C3_E13, group=C2_P03, fill=C2_P03)) +
  geom_density(alpha=0.4) + scale_fill_discrete(name = "Sexo", labels = c("Masculino", "Femenino")) +
  xlim(0,10) +
  ylim(0,0.25) + 
  labs(x="Año de Deserción", title = "Población Rural", y="Densidad") 
dev.off()

# A QUE EDAD DESERTASTE??
jpeg("Output/plot_urbana_edad.jpg")
ggplot(urbana, aes(x=C3_E16, group=C2_P03, fill=C2_P03)) +
  geom_density(alpha=0.4) + scale_fill_discrete(name = "Sexo", labels = c("Masculino", "Femenino")) +
  xlim(0,17) +
  ylim(0,0.25) + 
  labs(x="Edad de Deserción", title = "Población Urbana", y="Densidad") 
dev.off()

jpeg("Output/plot_rural_edad.jpg")
ggplot(rural, aes(x=C3_E16, group=C2_P03, fill=C2_P03)) +
  geom_density(alpha=0.4) + scale_fill_discrete(name = "Sexo", labels = c("Masculino", "Femenino")) +
  xlim(0,17) +
  ylim(0,0.25) + 
  labs(x="Edad de Deserción", title = "Población Rural", y="Densidad") 
dev.off()


table(urbana[urbana$desertor==1,]$C2_P02)
sum(table(urbana[urbana$desertor==1,]$C2_P02))
# el 87% de los desertores son mayores a 13

table(rural[rural$desertor==1,]$C2_P02)
sum(table(rural[rural$desertor==1,]$C2_P02))


urbana['alumno'] <- 0 
rural['alumno'] <- 0 
urbana$alumno <- ifelse(urbana$C3_E01 ==1, 1,0)
rural$alumno <- ifelse(rural$C3_E01 ==1, 1,0 )

urbana['p_desertor'] <- urbana$desertor*urbana$PONDERA
urbana['p_alumno'] <- urbana$alumno*urbana$PONDERA
rural['p_desertor'] <- rural$desertor*rural$PONDERA
rural['p_alumno'] <- rural$alumno*rural$PONDERA

sum(rural$desertor==1)
sum(rural$desertor==0)

sum(urbana$desertor==1)
sum(urbana$desertor==0)

floor(sum(rural$p_desertor))
floor(sum(rural$p_alumno))
# 84076/(1154107+84076) = 7%

floor(sum(urbana$p_desertor[!is.na(urbana$p_desertor)]))
floor(sum(urbana$p_alumno[!is.na(urbana$p_alumno)]))
# 228150/(228150+7525081) = 3%

# (228150+84076)/(1154107+84076+228150+7525081) = 4%

sum(table(rural[rural$desertor==1,]$C2_P02))
floor(sum(urbana$p_desertor[!is.na(urbana$p_desertor)]))
floor(sum(urbana$p_desertor[urbana$C2_P02==15,]))
floor(sum(urbana$p_alumno[!is.na(urbana$p_alumno)]))

# COHORTE 17
cohorte_17_U <- subset(urbana[urbana$C2_P02==17,], select = c('p_desertor','p_alumno'))
sum(cohorte_17_U$p_desertor[!is.na(cohorte_17_U$p_desertor)])
sum(cohorte_17_U$p_alumno[  !is.na(cohorte_17_U$p_alumno)])
# 88545/(88545+482485) = 16%

cohorte_17_R <- subset(rural[rural$C2_P02==17,], select = c('p_desertor','p_alumno'))
floor(sum(cohorte_17_R$p_desertor[!is.na(cohorte_17_R$p_desertor)]))
floor(sum(cohorte_17_R$p_alumno[  !is.na(cohorte_17_R$p_alumno)]))
# 30279/(30279+70536) = 30%

# COHORTE 16
cohorte_16_U <- subset(urbana[urbana$C2_P02==16,], select = c('p_desertor','p_alumno'))
sum(cohorte_16_U$p_desertor[!is.na(cohorte_16_U$p_desertor)])
sum(cohorte_16_U$p_alumno[!is.na(cohorte_16_U$p_alumno)])
# 55950/(55950+521787) = 10%

cohorte_16_R <- subset(rural[rural$C2_P02==16,], select = c('p_desertor','p_alumno'))
floor(sum(cohorte_16_R$p_desertor[!is.na(cohorte_16_R$p_desertor)]))
floor(sum(cohorte_16_R$p_alumno[!is.na(cohorte_16_R$p_alumno)]))
# 18171/(18171+75248) = 20%

# COHORTE 15
cohorte_15_U <- subset(urbana[urbana$C2_P02==15,], select = c('p_desertor','p_alumno'))
sum(cohorte_15_U$p_desertor[!is.na(cohorte_15_U$p_desertor)])
sum(cohorte_15_U$p_alumno[!is.na(cohorte_15_U$p_alumno)])
# 27488/(27488+558538) = 5%

cohorte_15_R <- subset(rural[rural$C2_P02==15,], select = c('p_desertor','p_alumno'))
floor(sum(cohorte_15_R$p_desertor[!is.na(cohorte_15_R$p_desertor)]))
floor(sum(cohorte_15_R$p_alumno[!is.na(cohorte_15_R$p_alumno)]))
# 15360/(15360+71803) = 18%

# COHORTE 14
cohorte_14_U <- subset(urbana[urbana$C2_P02==14,], select = c('p_desertor','p_alumno'))
sum(cohorte_14_U$p_desertor[!is.na(cohorte_14_U$p_desertor)])
sum(cohorte_14_U$p_alumno[  !is.na(cohorte_14_U$p_alumno)])
# 15636/(15636+557937) = 3%

cohorte_14_R <- subset(rural[rural$C2_P02==14,], select = c('p_desertor','p_alumno'))
floor(sum(cohorte_14_R$p_desertor[!is.na(cohorte_14_R$p_desertor)]))
floor(sum(cohorte_14_R$p_alumno[  !is.na(cohorte_14_R$p_alumno)]))
# 7220/(7220+91823) = 7%

# COHORTE 13
cohorte_13_U <- subset(urbana[urbana$C2_P02==13,], select = c('p_desertor','p_alumno'))
sum(cohorte_13_U$p_desertor[!is.na(cohorte_13_U$p_desertor)])
sum(cohorte_13_U$p_alumno[  !is.na(cohorte_13_U$p_alumno)])
# 6582/(6582+598863) = 1%

cohorte_13_R <- subset(rural[rural$C2_P02==13,], select = c('p_desertor','p_alumno'))
floor(sum(cohorte_13_R$p_desertor[!is.na(cohorte_13_R$p_desertor)]))
floor(sum(cohorte_13_R$p_alumno[  !is.na(cohorte_13_R$p_alumno)]))
# 3463/(3463+87416) = 4%

# COHORTE 12
cohorte_12_U <- subset(urbana[urbana$C2_P02==12,], select = c('p_desertor','p_alumno'))
sum(cohorte_12_U$p_desertor[!is.na(cohorte_12_U$p_desertor)])
sum(cohorte_12_U$p_alumno[  !is.na(cohorte_12_U$p_alumno)])
# 2177/(2177+590427) = 0.3%

cohorte_12_R <- subset(rural[rural$C2_P02==12,], select = c('p_desertor','p_alumno'))
floor(sum(cohorte_12_R$p_desertor[!is.na(cohorte_12_R$p_desertor)]))
floor(sum(cohorte_12_R$p_alumno[  !is.na(cohorte_12_R$p_alumno)]))
# 2071/(2071+102256) = 2%

# COHORTE 11
cohorte_11_U <- subset(urbana[urbana$C2_P02==11,], select = c('p_desertor','p_alumno'))
sum(cohorte_11_U$p_desertor[!is.na(cohorte_11_U$p_desertor)])
sum(cohorte_11_U$p_alumno[  !is.na(cohorte_11_U$p_alumno)])
# 2558/(2558+577877) = 0.4%

cohorte_11_R <- subset(rural[rural$C2_P02==11,], select = c('p_desertor','p_alumno'))
floor(sum(cohorte_11_R$p_desertor[!is.na(cohorte_11_R$p_desertor)]))
floor(sum(cohorte_11_R$p_alumno[  !is.na(cohorte_11_R$p_alumno)]))
# 2053/(2053+91584) = 2%

# COHORTE 10
cohorte_10_U <- subset(urbana[urbana$C2_P02==10,], select = c('p_desertor','p_alumno'))
sum(cohorte_10_U$p_desertor[!is.na(cohorte_10_U$p_desertor)])
sum(cohorte_10_U$p_alumno[  !is.na(cohorte_10_U$p_alumno)])
# 2066/(2066+612264) = 0.3%

cohorte_10_R <- subset(rural[rural$C2_P02==10,], select = c('p_desertor','p_alumno'))
floor(sum(cohorte_10_R$p_desertor[!is.na(cohorte_10_R$p_desertor)]))
floor(sum(cohorte_10_R$p_alumno[  !is.na(cohorte_10_R$p_alumno)]))
# 774/(774+90444) = 0.8%

# COHORTE 9
cohorte_9_U <- subset(urbana[urbana$C2_P02==9,], select = c('p_desertor','p_alumno'))
sum(cohorte_9_U$p_desertor[!is.na(cohorte_9_U$p_desertor)])
sum(cohorte_9_U$p_alumno[  !is.na(cohorte_9_U$p_alumno)])
# 0/(0+634042) = 0%

cohorte_9_R <- subset(rural[rural$C2_P02==9,], select = c('p_desertor','p_alumno'))
floor(sum(cohorte_9_R$p_desertor[!is.na(cohorte_9_R$p_desertor)]))
floor(sum(cohorte_9_R$p_alumno[  !is.na(cohorte_9_R$p_alumno)]))
# 1042/(1042+92662) = 1%


# COHORTE 8
cohorte_8_U <- subset(urbana[urbana$C2_P02==8,], select = c('p_desertor','p_alumno'))
sum(cohorte_8_U$p_desertor[!is.na(cohorte_8_U$p_desertor)])
sum(cohorte_8_U$p_alumno[  !is.na(cohorte_8_U$p_alumno)])
# 0/(0+615957) = 0%

cohorte_8_R <- subset(rural[rural$C2_P02==8,], select = c('p_desertor','p_alumno'))
floor(sum(cohorte_8_R$p_desertor[!is.na(cohorte_8_R$p_desertor)]))
floor(sum(cohorte_8_R$p_alumno[  !is.na(cohorte_8_R$p_alumno)]))
# 568/(568+99214) = 0.5%

# COHORTE 7
cohorte_7_U <- subset(urbana[urbana$C2_P02==7,], select = c('p_desertor','p_alumno'))
sum(cohorte_7_U$p_desertor[!is.na(cohorte_7_U$p_desertor)])
sum(cohorte_7_U$p_alumno[  !is.na(cohorte_7_U$p_alumno)])
# 500/(500+577927) = 0.08%

cohorte_7_R <- subset(rural[rural$C2_P02==7,], select = c('p_desertor','p_alumno'))
floor(sum(cohorte_7_R$p_desertor[!is.na(cohorte_7_R$p_desertor)]))
floor(sum(cohorte_7_R$p_alumno[  !is.na(cohorte_7_R$p_alumno)]))
# 459/(459+95454) = 0.5%

# COHORTE 6
cohorte_6_U <- subset(urbana[urbana$C2_P02==6,], select = c('p_desertor','p_alumno'))
sum(cohorte_6_U$p_desertor[!is.na(cohorte_6_U$p_desertor)])
sum(cohorte_6_U$p_alumno[  !is.na(cohorte_6_U$p_alumno)])
# 6758/(6758+599602) = 1%

cohorte_6_R <- subset(rural[rural$C2_P02==6,], select = c('p_desertor','p_alumno'))
floor(sum(cohorte_6_R$p_desertor[!is.na(cohorte_6_R$p_desertor)]))
floor(sum(cohorte_6_R$p_alumno[  !is.na(cohorte_6_R$p_alumno)]))
# 869/(869+90643) = 1%


# COHORTE 5
cohorte_5_U <- subset(urbana[urbana$C2_P02==5,], select = c('p_desertor','p_alumno'))
sum(cohorte_5_U$p_desertor[!is.na(cohorte_5_U$p_desertor)])
sum(cohorte_5_U$p_alumno[  !is.na(cohorte_5_U$p_alumno)])
# 19890/(19890+597375) = 3%

cohorte_5_R <- subset(rural[rural$C2_P02==5,], select = c('p_desertor','p_alumno'))
floor(sum(cohorte_5_R$p_desertor[!is.na(cohorte_5_R$p_desertor)]))
floor(sum(cohorte_5_R$p_alumno[  !is.na(cohorte_5_R$p_alumno)]))
# 1752/(1752+95019) = 2%

