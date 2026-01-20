getwd()
dir()
# Load data.frame
df<-read.csv('NewYorkDataSet.csv')
# Row names
names(df)
#head(df)
###Eliminating some Rows
str(df) 

df<-df[,c(-1,-2,-7,-8,-9,-10,-11,-12,-13,-14,-15)]
mean<-round(colMeans(df),2)
print(mean)

##Calculating Standard Deviation
sapply(df, sd)

###Boxplotting of each variables
df$price_conc<-(df$PRICE)/1000000
boxplot(df$price_conc, main='Price',ylab='Price')
boxplot(df$BEDS,main= 'Beds', ylab = 'Beds')
boxplot(df$BATH, main ='BATH',ylab = 'BATH')
boxplot(df$PROPERTYSQFT,main ='PROPERTYSQFT',ylab = 'PROPERTYSQFT')
boxplot(df$LATITUDE, main = 'LATITUDE',ylab = 'LATITUDE')
boxplot(df$LONGITUDE,main = 'LONGITUDE',ylab = 'LONGITUDE')
##### summarizing 
summary(df)
library (dplyr)
set.seed(007)
n<-nrow(df)
trainingSet<-sample(1:n, size =0.7*n)

train_data<-df[trainingSet,]
test_data<- df[-trainingSet,]

#####Predictive Model 
model1 <- lm(PRICE ~ BEDS + BATH + PROPERTYSQFT + LATITUDE + LONGITUDE, data = df)
summary(model1)

model2 <- lm(PRICE ~ BATH + PROPERTYSQFT + LONGITUDE, data = df)
summary (model2)

model3 <- lm(PRICE~ I(PROPERTYSQFT^2) + I(LONGITUDE^2) + BATH, data = df)
df$sq_PROPE <- df$PROPERTYSQFT^2
df$sq_LONGITUDE <- df$LONGITUDE^2
# View the summary of the polynomial model
summary(model3)

# Generate the summary of the model
model3_summary <- summary(model3)

# Extract R-squared and Adjusted R-squared
r_squared <- model3_summary$r.squared
adjusted_r_squared <- model3_summary$adj.r.squared

# Print the results
cat("R-squared: ", r_squared, "\n")
cat("Adjusted R-squared: ", adjusted_r_squared, "\n")

b_0_simple <- coef(model_simple)[1]  # Intercept
b_1_simple <- coef(model_simple)[2]  # Slope


m<-lm(PRICE~BATH,df)
#m$coefficients
# Plot the data for PropertySqft and Price_Conc
plot(df$BATH, df$PRICE, 
     main = "Regression Line for Price vs BATH",
     xlab = "BATH", 
     ylab = "PRICE",
     pch = 19, col = "black")

# Add regression line
m<-lm(PRICE~PROPERTYSQFT,df)
m$coefficients

# Plot the data for Bath and Price_Conc
plot(df$PROPERTYSQFT, df$PRICE, 
     main = "Regression Line for Price vs LATITUDE",
     xlab = "PROPERTYSQFT", 
     ylab = "PRICE",
     pch = 19, col = "black")

# Add regression line
abline(m$coefficients[1],m$coefficients[2], col = "red")

m<-lm(PRICE~LONGITUDE,df)
m$coefficients
plot(df$LONGITUDE, df$price_conc, 
     main = "Regression Line for Price vs LONGITUDE",
     xlab = "LONGITUDE", 
     ylab = "price_conc",
     pch = 19, col = "black")

# Add regression line
abline(m$coefficients[1],m$coefficients[2], col = "red")

#diagnostics plot
par(mfrow=c(2,2))
plot(model3, col="black", id.n=0)

#individual diagnostic plot
par(mfrow=c(1,1))
plot(model3,which=1, col="Black",  id.n = 0 )
plot(model3,which=2, col="Black",  id.n = 0   )
plot(model3,which=3, col ="Black", id.n = 0)
plot(model3,which=5,col ="Black", id.n = 0 )
