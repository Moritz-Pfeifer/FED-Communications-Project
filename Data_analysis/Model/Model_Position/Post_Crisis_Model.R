

##################################
##
## Post Crises Model
##
##################################




###
### Modeling little Guy Big Guy to Increase number of observations
###

setwd("/FED-Communications-Project/")

Master_1 <- read.csv("/FED-Communications-Project/Data_analysis/Tones/Tone_data_model.csv")

colnames(Master_1)[1] <- c("Quarter_Year")

Master_1 <- Master_1[1:224,1:10]
colnames(Master_1)


Little_Guy <- read.csv("/FED-Communications-Project/Data_analysis/Tones/Data_With_Position.csv")
colnames(Little_Guy)
colnames(Little_Guy)[1]<- c("Position")
colnames(Little_Guy)[2]<- c("Date")


Little_Guy$Match <- match(Little_Guy$Date, Master_1$Quarter_Year)

Little_Guy$Spread.Households <- Master_1[Little_Guy$Match,]$Spread.Households
Little_Guy$Spread.Firms <- Master_1[Little_Guy$Match,]$Spread.Firms
Little_Guy$Spread.Banks <- Master_1[Little_Guy$Match,]$Spread.Banks
Little_Guy$Spread.Government <- Master_1[Little_Guy$Match,]$Spread.Government


Little_Guy$Periods <- Master_1[Little_Guy$Match,]$Periods
Little_Guy$Position

Little_Guy$Period_Position <- paste0(Little_Guy$Periods,Little_Guy$Position)


colnames(Little_Guy)
table(Little_Guy$Periods)

Post_Crises <- subset(Little_Guy, Little_Guy$Periods== "Post-Crisis and after")
rownames(Post_Crises)<- NULL


Post_Crises <- Post_Crises[with(Post_Crises, order(Post_Crises$Date, decreasing = FALSE)),]
rownames(Post_Crises)<- NULL

Post_Crises$Lead_Helper <- as.numeric(rownames(Post_Crises))+2
Post_Crises$Tone_Household_Lead_1 <- Post_Crises[Post_Crises$Lead_Helper,]$Tone_Household


Check_1 <- Post_Crises[,c("Position", "Date","Tone_Household","Tone_Household_Lead_1")]

colnames(Post_Crises)[3]<- c("Lag_Tone_Household")

Check_1 <- Post_Crises[,c("Position", "Date","Lag_Tone_Household","Tone_Household_Lead_1")]
rm(Check_1)


Post_Crises$Position_Lag_1 <- Post_Crises[Post_Crises$Lead_Helper,]$Position

Check_1 <- Post_Crises[,c("Position", "Position_Lag_1", "Date","Lag_Tone_Household","Tone_Household_Lead_1")]


table(Little_Guy$Periods)
Model_1<- lm(Spread.Households~ Position*Lag_Tone_Household,data = Post_Crises[3:53,])

summary(Model_1)
Model_1$coefficients[3]

lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[3:53,])$coefficients[c(1,3,4)]


pacf(Post_Crises$Spread.Households)

Lag_Tone_Household <- na.omit(Post_Crises$Lag_Tone_Household)
pacf(Lag_Tone_Household, na.action = na.omit,main= "Tone Houseohold during Post Crises Period")
tseries::adf.test(Lag_Tone_Household, alternative = "stationary")

pacf(Post_Crises$Spread.Households, main= "Spread Houseohold during Post Crises Period")
tseries::adf.test(Post_Crises$Spread.Households, alternative = "stationary")



Coeff_Results <- as.data.frame(c(1:58))
colnames(Coeff_Results)[1]<- c("Model_Number")
Coeff_Results$Estimated_Beta_Yes <- paste("")
Coeff_Results$Estimated_Beta_No <- paste("")
Coeff_Results$Estimated_Beta_Intercept <- paste("")


Coeff_Results$Estimated_Beta_Yes[1]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[1:51,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[2]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[2:52,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[3]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[3:53,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[4]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[4:54,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[5]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[5:55,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[6]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[6:56,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[7]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[7:57,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[8]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[8:58,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[9]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[9:59,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[10]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[10:60,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[11]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[11:61,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[12]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[12:62,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[13]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[13:63,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[14]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[14:64,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[15]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[15:65,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[16]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[16:66,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[17]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[17:67,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[18]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[18:68,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[19]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[19:69,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[20]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[20:70,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[21]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[21:71,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[22]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[22:72,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[23]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[23:73,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[24]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[24:74,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[25]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[25:75,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[26]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[26:76,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[27]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[27:77,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[28]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[28:78,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[29]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[29:79,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[30]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[30:80,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[31]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[31:81,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[32]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[32:82,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[33]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[33:83,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[34]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[34:84,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[35]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[35:85,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[36]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[36:86,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[37]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[37:87,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[38]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[38:88,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[39]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[39:89,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[40]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[40:90,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[41]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[41:91,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[42]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[42:92,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[43]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[43:93,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[44]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[44:94,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[45]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[45:95,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[46]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[46:96,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[47]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[47:97,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[48]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[48:98,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[49]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[49:99,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[50]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[50:100,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[51]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[51:101,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[52]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[52:102,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[53]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[53:103,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[54]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[54:104,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[55]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[55:105,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[56]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[56:106,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[57]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[57:107,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[58]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[58:108,])$coefficients[4]


Model_104 <- lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[104:154,])
summary(Model_104)
Model_58 <-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[58:108,])
summary(Model_58)

Coeff_Results$Estimated_Beta_No[1]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[1:51,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[2]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[2:52,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[3]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[3:53,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[4]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[4:54,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[5]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[5:55,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[6]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[6:56,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[7]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[7:57,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[8]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[8:58,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[9]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[9:59,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[10]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[10:60,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[11]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[11:61,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[12]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[12:62,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[13]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[13:63,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[14]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[14:64,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[15]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[15:65,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[16]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[16:66,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[17]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[17:67,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[18]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[18:68,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[19]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[19:69,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[20]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[20:70,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[21]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[21:71,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[22]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[22:72,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[23]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[23:73,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[24]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[24:74,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[25]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[25:75,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[26]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[26:76,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[27]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[27:77,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[28]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[28:78,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[29]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[29:79,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[30]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[30:80,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[31]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[31:81,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[32]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[32:82,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[33]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[33:83,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[34]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[34:84,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[35]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[35:85,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[36]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[36:86,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[37]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[37:87,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[38]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[38:88,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[39]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[39:89,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[40]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[40:90,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[41]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[41:91,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[42]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[42:92,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[43]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[43:93,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[44]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[44:94,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[45]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[45:95,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[46]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[46:96,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[47]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[47:97,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[48]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[48:98,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[49]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[49:99,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[50]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[50:100,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[51]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[51:101,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[52]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[52:102,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[53]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[53:103,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[54]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[54:104,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[55]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[55:105,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[56]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[56:106,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[57]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[57:107,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[58]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[58:108,])$coefficients[3]



Coeff_Results$Estimated_Beta_Intercept[1]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[1:51,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[2]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[2:52,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[3]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[3:53,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[4]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[4:54,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[5]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[5:55,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[6]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[6:56,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[7]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[7:57,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[8]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[8:58,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[9]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[9:59,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[10]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[10:60,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[11]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[11:61,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[12]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[12:62,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[13]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[13:63,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[14]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[14:64,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[15]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[15:65,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[16]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[16:66,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[17]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[17:67,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[18]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[18:68,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[19]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[19:69,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[20]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[20:70,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[21]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[21:71,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[22]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[22:72,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[23]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[23:73,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[24]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[24:74,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[25]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[25:75,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[26]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[26:76,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[27]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[27:77,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[28]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[28:78,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[29]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[29:79,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[30]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[30:80,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[31]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[31:81,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[32]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[32:82,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[33]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[33:83,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[34]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[34:84,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[35]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[35:85,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[36]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[36:86,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[37]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[37:87,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[38]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[38:88,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[39]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[39:89,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[40]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[40:90,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[41]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[41:91,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[42]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[42:92,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[43]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[43:93,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[44]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[44:94,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[45]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[45:95,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[46]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[46:96,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[47]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[47:97,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[48]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[48:98,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[49]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[49:99,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[50]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[50:100,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[51]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[51:101,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[52]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[52:102,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[53]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[53:103,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[54]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[54:104,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[55]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[55:105,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[56]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[56:106,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[57]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[57:107,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[58]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Post_Crises[58:108,])$coefficients[1]



Coeff_Results$Estimated_Beta_Yes_2 <- as.numeric(Coeff_Results$Estimated_Beta_No) + as.numeric(Coeff_Results$Estimated_Beta_Yes)

t.test(as.numeric(Coeff_Results$Estimated_Beta_No[1:58]), alternative = "two.sided")
t.test(as.numeric(Coeff_Results$Estimated_Beta_Yes_2[1:58]), alternative = "two.sided")

mean(as.numeric(Coeff_Results$Estimated_Beta_Yes_2))
mean(as.numeric(Coeff_Results$Estimated_Beta_No))


# Coeff_Results$Estimated_Beta_Yes_2 <- as.numeric(Coeff_Results$Estimated_Beta_Intercept) + as.numeric(Coeff_Results$Estimated_Beta_Yes)
# Coeff_Results$Estimated_Beta_No_2 <- as.numeric(Coeff_Results$Estimated_Beta_Intercept) + as.numeric(Coeff_Results$Estimated_Beta_No)
# 
# 
# 
# mean(as.numeric(Coeff_Results$Estimated_Beta_Yes_2[1:58]))
# mean(as.numeric(Coeff_Results$Estimated_Beta_No_2[1:58]))
# 
# t.test(as.numeric(Coeff_Results$Estimated_Beta_Yes[1:58]), alternative = "two.sided")
# 
# t.test(as.numeric(Coeff_Results$Estimated_Beta_No[1:58]), alternative = "two.sided")
# 
# 
# t.test(as.numeric(Coeff_Results$Estimated_Beta_Yes_2[1:58]), alternative = "two.sided")
# 
# t.test(as.numeric(Coeff_Results$Estimated_Beta_No_2[1:58]), alternative = "two.sided")
# 
# 
# 
# 
# hist(as.numeric(Coeff_Results$Estimated_Beta_No), breaks = 20)
# hist(as.numeric(Coeff_Results$Estimated_Beta_Yes), breaks = 20)


#write.csv(Coeff_Results, file = "Coeff_Results_Post_Crises.csv", row.names = FALSE)
