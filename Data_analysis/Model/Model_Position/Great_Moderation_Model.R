

##################################
##
## Great Moderation Model
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

Great_Moderation <- subset(Little_Guy, Little_Guy$Periods== "Great Moderation")
rownames(Great_Moderation)<- NULL


Great_Moderation <- Great_Moderation[with(Great_Moderation, order(Great_Moderation$Date, decreasing = FALSE)),]
rownames(Great_Moderation)<- NULL

Great_Moderation$Lead_Helper <- as.numeric(rownames(Great_Moderation))+2
Great_Moderation$Tone_Household_Lead_1 <- Great_Moderation[Great_Moderation$Lead_Helper,]$Tone_Household


Check_1 <- Great_Moderation[,c("Position", "Date","Tone_Household","Tone_Household_Lead_1")]

colnames(Great_Moderation)[3]<- c("Lag_Tone_Household")

Check_1 <- Great_Moderation[,c("Position", "Date","Lag_Tone_Household","Tone_Household_Lead_1")]
rm(Check_1)


Great_Moderation$Position_Lag_1 <- Great_Moderation[Great_Moderation$Lead_Helper,]$Position

Check_1 <- Great_Moderation[,c("Position", "Position_Lag_1", "Date","Lag_Tone_Household","Tone_Household_Lead_1")]


table(Little_Guy$Periods)
Model_1<- lm(Spread.Households~ Position*Lag_Tone_Household,data = Great_Moderation[3:53,])

summary(Model_1)
Model_1$coefficients[3]

lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[3:53,])$coefficients[c(1,3,4)]


pacf(Great_Moderation$Spread.Households)

Lag_Tone_Household <- na.omit(Great_Moderation$Lag_Tone_Household)
pacf(Lag_Tone_Household, na.action = na.omit,main= "Tone Houseohold during Great Moderation Period")
tseries::adf.test(Lag_Tone_Household, alternative = "stationary")

pacf(Great_Moderation$Spread.Households, main= "Spread Houseohold during Great Moderation Period")
tseries::adf.test(Great_Moderation$Spread.Households, alternative = "stationary")



Coeff_Results <- as.data.frame(c(1:141))
colnames(Coeff_Results)[1]<- c("Model_Number")
Coeff_Results$Estimated_Beta_Yes <- paste("")
Coeff_Results$Estimated_Beta_No <- paste("")
Coeff_Results$Estimated_Beta_Intercept <- paste("")



Coeff_Results$Estimated_Beta_Yes[1]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[1:51,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[2]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[2:52,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[3]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[3:53,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[4]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[4:54,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[5]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[5:55,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[6]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[6:56,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[7]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[7:57,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[8]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[8:58,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[9]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[9:59,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[10]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[10:60,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[11]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[11:61,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[12]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[12:62,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[13]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[13:63,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[14]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[14:64,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[15]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[15:65,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[16]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[16:66,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[17]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[17:67,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[18]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[18:68,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[19]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[19:69,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[20]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[20:70,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[21]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[21:71,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[22]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[22:72,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[23]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[23:73,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[24]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[24:74,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[25]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[25:75,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[26]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[26:76,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[27]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[27:77,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[28]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[28:78,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[29]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[29:79,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[30]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[30:80,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[31]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[31:81,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[32]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[32:82,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[33]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[33:83,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[34]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[34:84,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[35]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[35:85,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[36]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[36:86,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[37]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[37:87,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[38]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[38:88,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[39]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[39:89,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[40]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[40:90,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[41]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[41:91,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[42]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[42:92,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[43]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[43:93,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[44]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[44:94,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[45]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[45:95,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[46]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[46:96,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[47]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[47:97,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[48]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[48:98,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[49]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[49:99,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[50]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[50:100,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[51]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[51:101,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[52]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[52:102,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[53]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[53:103,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[54]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[54:104,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[55]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[55:105,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[56]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[56:106,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[57]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[57:107,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[58]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[58:108,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[59]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[59:109,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[60]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[60:110,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[61]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[61:111,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[62]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[62:112,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[63]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[63:113,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[64]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[64:114,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[65]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[65:115,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[66]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[66:116,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[67]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[67:117,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[68]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[68:118,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[69]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[69:119,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[70]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[70:120,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[71]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[71:121,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[72]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[72:122,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[73]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[73:123,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[74]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[74:124,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[75]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[75:125,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[76]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[76:126,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[77]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[77:127,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[78]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[78:128,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[79]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[79:129,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[80]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[80:130,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[81]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[81:131,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[82]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[82:132,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[83]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[83:133,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[84]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[84:134,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[85]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[85:135,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[86]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[86:136,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[87]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[87:137,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[88]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[88:138,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[89]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[89:139,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[90]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[90:140,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[91]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[91:141,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[92]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[92:142,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[93]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[93:143,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[94]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[94:144,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[95]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[95:145,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[96]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[96:146,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[97]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[97:147,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[98]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[98:148,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[99]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[99:149,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[100]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[100:150,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[101]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[101:151,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[102]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[102:152,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[103]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[103:153,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[104]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[104:154,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[105]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[105:155,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[106]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[106:156,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[107]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[107:157,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[108]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[108:158,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[109]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[109:159,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[110]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[110:160,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[111]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[111:161,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[112]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[112:162,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[113]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[113:163,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[114]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[114:164,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[115]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[115:165,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[116]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[116:166,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[117]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[117:167,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[118]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[118:168,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[119]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[119:169,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[120]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[120:170,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[121]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[121:171,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[122]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[122:172,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[123]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[123:173,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[124]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[124:174,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[125]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[125:175,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[126]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[126:176,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[127]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[127:177,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[128]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[128:178,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[129]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[129:179,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[130]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[130:180,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[131]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[131:181,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[132]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[132:182,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[133]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[133:183,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[134]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[134:184,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[135]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[135:185,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[136]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[136:186,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[137]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[137:187,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[138]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[138:188,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[139]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[139:189,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[140]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[140:190,])$coefficients[4]
Coeff_Results$Estimated_Beta_Yes[141]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[141:191,])$coefficients[4]



Coeff_Results$Estimated_Beta_No[1]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[1:51,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[2]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[2:52,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[3]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[3:53,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[4]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[4:54,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[5]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[5:55,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[6]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[6:56,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[7]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[7:57,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[8]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[8:58,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[9]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[9:59,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[10]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[10:60,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[11]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[11:61,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[12]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[12:62,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[13]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[13:63,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[14]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[14:64,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[15]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[15:65,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[16]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[16:66,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[17]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[17:67,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[18]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[18:68,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[19]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[19:69,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[20]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[20:70,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[21]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[21:71,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[22]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[22:72,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[23]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[23:73,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[24]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[24:74,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[25]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[25:75,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[26]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[26:76,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[27]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[27:77,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[28]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[28:78,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[29]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[29:79,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[30]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[30:80,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[31]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[31:81,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[32]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[32:82,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[33]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[33:83,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[34]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[34:84,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[35]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[35:85,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[36]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[36:86,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[37]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[37:87,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[38]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[38:88,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[39]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[39:89,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[40]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[40:90,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[41]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[41:91,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[42]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[42:92,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[43]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[43:93,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[44]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[44:94,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[45]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[45:95,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[46]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[46:96,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[47]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[47:97,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[48]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[48:98,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[49]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[49:99,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[50]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[50:100,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[51]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[51:101,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[52]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[52:102,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[53]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[53:103,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[54]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[54:104,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[55]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[55:105,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[56]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[56:106,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[57]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[57:107,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[58]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[58:108,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[59]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[59:109,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[60]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[60:110,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[61]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[61:111,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[62]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[62:112,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[63]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[63:113,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[64]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[64:114,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[65]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[65:115,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[66]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[66:116,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[67]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[67:117,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[68]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[68:118,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[69]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[69:119,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[70]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[70:120,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[71]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[71:121,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[72]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[72:122,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[73]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[73:123,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[74]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[74:124,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[75]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[75:125,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[76]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[76:126,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[77]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[77:127,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[78]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[78:128,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[79]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[79:129,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[80]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[80:130,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[81]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[81:131,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[82]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[82:132,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[83]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[83:133,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[84]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[84:134,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[85]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[85:135,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[86]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[86:136,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[87]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[87:137,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[88]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[88:138,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[89]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[89:139,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[90]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[90:140,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[91]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[91:141,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[92]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[92:142,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[93]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[93:143,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[94]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[94:144,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[95]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[95:145,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[96]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[96:146,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[97]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[97:147,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[98]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[98:148,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[99]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[99:149,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[100]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[100:150,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[101]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[101:151,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[102]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[102:152,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[103]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[103:153,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[104]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[104:154,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[105]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[105:155,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[106]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[106:156,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[107]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[107:157,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[108]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[108:158,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[109]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[109:159,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[110]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[110:160,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[111]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[111:161,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[112]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[112:162,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[113]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[113:163,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[114]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[114:164,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[115]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[115:165,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[116]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[116:166,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[117]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[117:167,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[118]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[118:168,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[119]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[119:169,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[120]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[120:170,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[121]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[121:171,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[122]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[122:172,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[123]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[123:173,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[124]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[124:174,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[125]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[125:175,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[126]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[126:176,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[127]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[127:177,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[128]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[128:178,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[129]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[129:179,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[130]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[130:180,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[131]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[131:181,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[132]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[132:182,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[133]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[133:183,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[134]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[134:184,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[135]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[135:185,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[136]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[136:186,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[137]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[137:187,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[138]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[138:188,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[139]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[139:189,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[140]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[140:190,])$coefficients[3]
Coeff_Results$Estimated_Beta_No[141]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[141:191,])$coefficients[3]



Coeff_Results$Estimated_Beta_Intercept[1]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[1:51,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[2]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[2:52,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[3]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[3:53,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[4]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[4:54,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[5]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[5:55,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[6]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[6:56,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[7]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[7:57,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[8]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[8:58,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[9]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[9:59,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[10]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[10:60,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[11]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[11:61,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[12]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[12:62,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[13]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[13:63,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[14]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[14:64,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[15]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[15:65,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[16]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[16:66,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[17]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[17:67,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[18]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[18:68,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[19]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[19:69,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[20]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[20:70,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[21]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[21:71,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[22]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[22:72,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[23]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[23:73,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[24]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[24:74,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[25]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[25:75,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[26]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[26:76,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[27]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[27:77,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[28]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[28:78,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[29]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[29:79,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[30]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[30:80,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[31]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[31:81,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[32]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[32:82,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[33]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[33:83,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[34]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[34:84,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[35]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[35:85,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[36]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[36:86,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[37]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[37:87,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[38]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[38:88,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[39]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[39:89,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[40]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[40:90,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[41]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[41:91,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[42]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[42:92,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[43]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[43:93,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[44]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[44:94,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[45]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[45:95,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[46]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[46:96,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[47]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[47:97,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[48]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[48:98,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[49]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[49:99,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[50]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[50:100,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[51]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[51:101,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[52]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[52:102,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[53]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[53:103,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[54]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[54:104,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[55]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[55:105,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[56]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[56:106,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[57]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[57:107,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[58]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[58:108,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[59]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[59:109,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[60]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[60:110,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[61]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[61:111,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[62]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[62:112,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[63]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[63:113,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[64]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[64:114,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[65]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[65:115,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[66]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[66:116,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[67]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[67:117,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[68]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[68:118,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[69]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[69:119,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[70]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[70:120,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[71]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[71:121,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[72]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[72:122,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[73]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[73:123,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[74]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[74:124,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[75]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[75:125,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[76]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[76:126,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[77]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[77:127,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[78]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[78:128,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[79]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[79:129,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[80]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[80:130,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[81]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[81:131,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[82]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[82:132,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[83]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[83:133,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[84]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[84:134,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[85]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[85:135,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[86]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[86:136,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[87]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[87:137,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[88]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[88:138,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[89]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[89:139,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[90]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[90:140,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[91]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[91:141,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[92]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[92:142,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[93]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[93:143,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[94]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[94:144,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[95]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[95:145,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[96]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[96:146,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[97]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[97:147,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[98]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[98:148,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[99]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[99:149,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[100]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[100:150,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[101]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[101:151,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[102]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[102:152,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[103]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[103:153,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[104]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[104:154,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[105]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[105:155,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[106]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[106:156,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[107]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[107:157,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[108]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[108:158,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[109]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[109:159,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[110]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[110:160,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[111]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[111:161,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[112]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[112:162,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[113]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[113:163,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[114]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[114:164,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[115]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[115:165,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[116]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[116:166,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[117]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[117:167,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[118]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[118:168,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[119]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[119:169,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[120]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[120:170,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[121]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[121:171,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[122]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[122:172,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[123]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[123:173,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[124]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[124:174,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[125]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[125:175,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[126]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[126:176,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[127]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[127:177,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[128]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[128:178,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[129]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[129:179,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[130]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[130:180,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[131]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[131:181,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[132]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[132:182,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[133]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[133:183,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[134]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[134:184,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[135]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[135:185,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[136]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[136:186,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[137]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[137:187,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[138]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[138:188,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[139]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[139:189,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[140]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[140:190,])$coefficients[1]
Coeff_Results$Estimated_Beta_Intercept[141]<-lm(Spread.Households~Position*Lag_Tone_Household,data = Great_Moderation[141:191,])$coefficients[1]



Coeff_Results$Estimated_Beta_Yes_2 <- as.numeric(Coeff_Results$Estimated_Beta_No) + as.numeric(Coeff_Results$Estimated_Beta_Yes)

t.test(as.numeric(Coeff_Results$Estimated_Beta_No[1:141]), alternative = "two.sided")
t.test(as.numeric(Coeff_Results$Estimated_Beta_Yes_2[1:141]), alternative = "two.sided")

mean(as.numeric(Coeff_Results$Estimated_Beta_Yes_2))
mean(as.numeric(Coeff_Results$Estimated_Beta_No))



# 
# 
# 
# Coeff_Results$Estimated_Beta_Yes_2 <- as.numeric(Coeff_Results$Estimated_Beta_Intercept) + as.numeric(Coeff_Results$Estimated_Beta_Yes)
# Coeff_Results$Estimated_Beta_No_2 <- as.numeric(Coeff_Results$Estimated_Beta_Intercept) + as.numeric(Coeff_Results$Estimated_Beta_No)
# 
# 
# 
# 
# 
# mean(as.numeric(Coeff_Results$Estimated_Beta_Yes))
# mean(as.numeric(Coeff_Results$Estimated_Beta_No))
# 
# t.test(as.numeric(Coeff_Results$Estimated_Beta_Yes[1:141]), alternative = "two.sided")
# 
# t.test(as.numeric(Coeff_Results$Estimated_Beta_No[1:141]), alternative = "two.sided")
# 
# t.test(as.numeric(Coeff_Results$Estimated_Beta_Yes_2[1:141]), alternative = "two.sided")
# 
# t.test(as.numeric(Coeff_Results$Estimated_Beta_No_2[1:141]), alternative = "two.sided")
# 
# 


hist(Coeff_Results$Estimated_Beta_No, breaks = 20)



#write.csv(Coeff_Results, file = "Coeff_Results_Great_Moderation.csv", row.names = FALSE)






