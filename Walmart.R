stores<-read.csv(file.choose())
View(stores)
stores_df <-stores
sales <-read.csv(file.choose())
sales_df <- sales
View(sales)
test1 <- read.csv(file.choose(),header = TRUE, check.names = TRUE)
features_df <- test1
pre_final_df <- merge(stores_df,sales_df,by = "Store")
head(pre_final_df)
final_df <- merge(pre_final_df,features_df,by= c("Store","Date","IsHoliday"))
head(final_df)
final_df$IsHoliday [final_df$IsHoliday == "true"] <- 1
final_df$IsHoliday [final_df$IsHoliday == "false"] <-0
head(final_df)
final_df[is.na(final_df)] <- 0
final_df
subset1 <-subset(final_df$Date,final_df$Weekly_Sales<0)
subset2 <-subset(final_df,select = c("Size","Weekly_Sales","Temperature","Fuel_Price","MarkDown1","MarkDown2","MarkDown3","MarkDown4","MarkDown5","CPI","Unemployment"))

cor(final_df$Weekly_Sales,final_df$IsHoliday,use="everything",method="pearson")
subset1 <- subset(final_df$Date,final_df$Weekly_Sales<0)
mean_markdown1 <- mean(final_df$MarkDown1)
mean_markdown2 <- mean(final_df$MarkDown2)
mean_markdown3 <- mean(final_df$MarkDown3)
mean_markdown4 <- mean(final_df$MarkDown4)
mean_markdown5 <- mean(final_df$MarkDown5)
final_markdown <- mean_markdown1 + mean_markdown2 + mean_markdown3 + mean_markdown4 + mean_markdown5
final_markdown

average_final_markdown <-final_markdown/5
average_final_markdown

install.packages("classInt")
library(classInt)
bin_data <- final_df$Weekly_Sales
bin_data
classIntervals(bin_data,5,style = "equal")

classIntervals(bin_data,5,style="quantile")
fore_data <- ts(final_df$Weekly_Sales, start=2010, end=2012,frequency=12)
plot(fore_data)

install.packages("forecast")
library(forecast)
fore_data <- ts(final_df$Weekly_Sales, start=2010, end=2012,frequency=12)
plot(fore_data)
hw <- HoltWinters(fore_data)
plot(hw)

install.packages("ggplot2")
library(ggplot2)
install.packages("reshape")
library(reshape)
install.packages("dplyr")
library(dplyr)
subset2 <- subset(final_df,select= c("Size","Weekly_Sales","Temperature","Fuel_price","MarkDown1","MarkDown2","MarkDown3","MarkDown4","MarkDown5","CPI","Unemployment"))

res <-cor(subset2)
install.packages("corrplot")
library(corrplot)
corrplot(res,type = "upper",order = "hclust",tl.col = "black",tl.srt = 45)

col <-colorRampPalette(c("blue","white","red"))(20)
heatmap(x = res, col = col, symm = TRUE )


input<-final_df[c("Weekly_Sales","Temperature","Fuel_price","MarkDown1","MarkDown2","MarkDown3","MarkDown4","MarkDown5","CPI","Unemployment"),]
head(input)
model <-lm(Weekly_Sales~Temperature+Fuel_Price+MarkDown1+MarkDown2+MarkDown3+MarkDown4+MarkDown5+CPI+Unemployment,data= final_df)
print(model)

cat("# # # # The Coefficeint value # # #","\n")
a <- coef(model)[1]
print(a)

XTemperature <- coef(model)[2]
XFuel_Price <- coef(model)[3]
XMarkDown1 <- coef(model)[4]
XMarkDown2 <- coef(model)[5]
XMarkDown3 <- coef(model)[6]
XMarkDown4 <- coef(model)[7]
XMarkDown5 <- coef(model)[8]
XCPI <- coef(model)[9]
XUnemployment <- coef(model)[10]

print(XTemperature)
print(XFuel_Price)     
print(XMarkDown1)
print(XMarkDown2)
print(XMarkDown3)
print(XMarkDown4)
print(XMarkDown5)
print(XCPI)
print(XUnemployment)

x1= 41.17
x2 = 2.562
x3 = 16305.11
x4 = 3551.41
x5=16.16
x6 = 3611.60
x7 = 1240.2
x8 = 220.806
x9 = 7.931

y=a+XTemperature*x1+XFuel_Price*x2+XMarkDown1*x3+XMarkDown2*x4+XMarkDown3*x5+XMarkDown4*x6+XMarkDown5*x7+XCPI*x8+XUnemployment*x9
print(y)

