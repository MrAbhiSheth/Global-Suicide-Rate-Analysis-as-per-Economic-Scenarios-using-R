library(tidyverse) # general
library(ggalt) # dumbbell plots
library(countrycode) # continent
library(rworldmap) # quick region-level heat maps
library(gridExtra) # plots
library(broom) # significant trends within countries
library(packHV)
library(moments)
# data visualization
library(RColorBrewer) # For ready-to-use color palettes
library(ggridges) # provides two main geoms, geom_ridgeline and geom_density_ridges
library(cowplot) # functions to align plots and arrange them into complex compound figures
library(ggplot2) # a system for declaratively creating graphics, based on The Grammar of Graphics.
# general data manipulation
library(dplyr) # data transformation
library(glue) # An implementation of interpreted string literals
library(stringr) # strings manipulation
# specific visualisation
library(hrbrthemes) # Additional Themes
library(ggthemes) # Additional Themes
# Statistics
library(DescTools) # Tools for Descriptive Statistics
# Table formate
library(knitr)
library(kableExtra)
library(ggplot2)
main_data <- read_csv("C:/Users/jayesh kartik/Desktop/master22.csv")
summary(main_data)
#convert using lapply function
main_data[c(1,2,3,4)] <- lapply(main_data[c(1,2,3,4)],factor)
main_data$year <- factor(main_data$year,ordered = TRUE)
#check
sapply(main_data, class)
data <- main_data %>%
  mutate(age = str_remove(age,'years'))
data <- data %>%
  mutate(age = str_remove(age," "))
head(data$age,n=6)
labels_1 <- c(
  '0 -| 1332',
  '1332 -| 2652',
  '2652 -| 3962',
  '3962 -| 5282',
  '5282 -| 6592',
  '6592 -| 7902',
  '7902 -| 9222',
  '9222 -| 10522',
  '10522 -| 11822',
  '11822 -| 13122',
  '13122 -| 14522',
  '14522 -| 15822',
  '15822 -| 17122',
  '17122 -| 18422',
  '18422 -| 19722',
  '19722 -| 21000',
  '21000 -| 22400'
)
suicide_frq <- cbind(Frequency = table(cut(x = data$suicides_no, breaks = 17,labels = labels_1, include.lowest = TRUE)),Percent = prop.table(table(cut(x = data$suicides_no,breaks = 17,labels = labels_1,include.lowest = TRUE)))*100)
suicide_frq
kable(suicide_frq) %> kable_styling(bootstrap_options = "striped", full_width = F)

hist(suicide_frq)

region_freq <- data.frame(cbind(Frequency = table(x = data$region), Percent = prop.table(table(data$region))*100))
region_freq <- region_freq[order(-region_freq$Frequency),]
kable(region_freq) %>% kable_styling(bootstrap_options = "striped", full_width = F)

glue("Number of unique year : {count(unique(data['year']))}")
glue("years are from: {min(data$year)} to {max(data$year)}")
year_freq <- data.frame(cbind(Frequency = table(x = data$year), Percent = prop.table(table(data$year))*100))
year_freq <- year_freq[order(-year_freq$Frequency),]
kable(year_freq) %>% kable_styling(bootstrap_options = "striped", full_width = F,)

sex_freq <- cbind(Frequency = table(data$sex), Percent = prop.table(table(data$sex)) *100)
kable(sex_freq) %>% kable_styling(bootstrap_options = "striped", full_width = F)

age_freq <- cbind(Frequency = table(data$age), Percent = prop.table(table(data$age)) * 100)
head(age_freq)

avg_per_region <- data %>%
  group_by(region) %>%
  summarise(suicide_avg = round(mean(suicides_no),2))
avg_per_region <- avg_per_region[order(-avg_per_region$suicide_avg),]
avg_per_region_15 <- head(avg_per_region,15)
avg_per_region_vis <- avg_per_region_15 %>%
  ggplot(aes(x = region , y = suicide_avg)) +
  geom_bar(stat = "identity",aes(fill = region , color = region),size = 1.1,alpha = 0.7) +
  geom_label(aes(label = suicide_avg) , size = 4,fill = "#F5FFFA", fontface = "bold") +
  coord_flip()+
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#DCFCE6", color = "#66CDAA")) +
  ylab("") +
  xlab("") +
  ggtitle("10 Countries with the Highset Sucicde Rate")

avg_per_region_vis

avg_per_sex <- data %>%
  group_by(sex) %>%
  summarise(suicide_avg = round(mean(data$suicides_no),2))
head(avg_per_sex)

#To check whether the data is normally distributedpar(mfrow=c(2,2))
options(repr.plot.width=14, repr.plot.height=6)
hist_boxplot(main_data$gdp_per_capita...., main = "GDP per Capita", col = "lightblue", xlab =
               "GDP per Capita");
hist_boxplot(main_data$gdp_for_year...., main = "GDP ", col = "lightblue", xlab = "GDP ");
hist_boxplot(main_data$suicides_no, main = "Suicides", col = "lightblue",xlab = "Tax");
hist_boxplot(main_data$suicides.100k.pop, main = "Suicide Rate", col = "lightblue", xlab = "Suicides per Capita (100K)");

#Skew of the Distirbutions#
skewness(main_data$gdp_for_year....) # > 1 = highly skewed towards the right

skewness(main_data$suicides_no) # > 1 = highly skewed towards the right

skewness(main_data$suicides.100k.pop) # > 1 = skewed towards the right

plot1 <-
  ggplot(main_data, aes(year, gdp_per_capita....)) + geom_boxplot(fill = "#C7E9B4") +
  labs(title = "GDP per Capita by Year", x = "", y = "GDP per Capita")
plot2 <-
  ggplot(main_data, aes(year, suicides.100k.pop)) + geom_boxplot(fill = "#1D91C0") +
  labs(title = "Suicide Rate by Year", x = "", y ="Suicides per 100,000")
plot_grid(plot1,plot2, ncol = 2, nrow = 1)

#Checking the Correlation
cor(main_data$suicides_no, main_data$gdp_per_capita....)
cor(main_data$suicides.100k.pop, main_data$gdp_for_year....)

#By gender
ggplot(main_data,aes(x = main_data$sex, y = main_data$suicides.100k.pop, fill = sex)) +
  geom_bar(stat = "identity") +
  labs(title = "Global suicides (per 100k), by Sex",
       x = "Sex",
       y = "Suicides per 100k") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 25), minor_breaks = F)

#By Continent
main_data$continent <- countrycode(sourcevar = main_data$region,
                                   origin = "country.name",
                                   destination = "continent")
ggplot(main_data, aes(x = main_data$continent, y = suicides.100k.pop, fill = region)) +
  geom_bar(stat = "identity") +
  labs(title = "Global Suicides (per 100k), by region",
       x = "region",
       y = "Suicides per 100k",
       fill = "continent") +
  theme(legend.position = "none", title = element_text(size = 10)) +
  scale_y_continuous(breaks = seq(0, 20, 1), minor_breaks = F)

#By region
ggplot(main_data, aes(x = region, y = suicides.100k.pop, fill = continent)) +
geom_bar(stat = "identity") +
  labs(title = "Global suicides per 100k, by region",
       x = "region",
       y = "Suicides per 100k",
       fill = "Continent") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 45, 2)) +
  theme(legend.position = "bottom")

#Suicides count by age group
a = ggplot(main_data, aes(age, suicides_no, fill = age)) +
  geom_bar(stat = "identity", show.legend = F) +
  labs(title = "Suicides Count by Age", x = "", y = "Suicides")

a

b = ggplot(main_data, aes(age, suicides.100k.pop, fill = age)) +
  geom_bar(stat = "identity", show.legend = F) +
  labs(title = "Suicide Rate by Age", x = "", y = "Suicides per 100,000")
options(repr.plot.width=16, repr.plot.height=7)
b

#GDP and Suicides
plot3= ggplot(main_data, aes(main_data$gdp_for_year....,main_data$suicides_no, color = age))+
  geom_point(pch = 21, cex = 3, stroke = 2, show.legend = F) +
  geom_smooth(method = "lm", color = "#225EA8", lwd = 3, formula = "y~x") +
  labs(x = "Gross Domestic Product",
       y = "Total Number of Suicides",
       title = "GDP & Suicide Rates")
plot3


# GDP per Capita and Suicide Rates
ggplot(main_data, aes(gdp_per_capita...., suicides.100k.pop, color = age)) +
  geom_point(pch = 21, cex = 3, stroke = 2) +
  geom_smooth(method = "lm", color = "#225EA8", lwd = 3, formula = "y~x") +
  labs(y = "Suicides per 100,000",
       title = "GDP per Capita & Suicide Rates")
View(main_data)


#Linear Regression-

lm1 = lm(formula = suicides_no ~ continent, data = main_data)
summary(lm1)

lm2= lm(suicides_no ~ region, data = main_data)
summary(lm2)

lm3= lm(formula =  suicides_no ~ as.numeric(sex) , data = main_data)
summary(lm3)

lm4= lm(formula =suicides_no  ~  gdp_per_capita....  , data = main_data)
summary(lm4)

lm5 = lm(formula = main_data$suicides_no ~ main_data$age)
summary(lm5)


#Model 1 Adjusted R-Squared =  0.005743 Target = Suicide_no
#Model 2 Adjusted R-Squared =  0.4074 Target = Suicide_no 
#Model 3 Adjusted R-Squared =  0.02092 Target = Suicide_no
#Model 4 Adjusted R-Squared =  0.003761 Target = Suicide_no 
#Model 5 Adjusted R-Squared =  0.03341 Target = Suicide_no

#calculating the MSE of models-
#Model 1  
mean(lm1$residuals^2)

#Model 2
mean(lm2$residuals^2)

#Model 3  
mean(lm3$residuals^2)

#Model 4  
mean(lm4$residuals^2)

#Model 5  
mean(lm5$residuals^2)

#Assessing the Outliers
par(mfrow=c(4,1))
hist_boxplot(main_data$suicides_no, col = "grey50", main = "Suicides Number") # right skewed
hist_boxplot(main_data$suicides.100k.pop, col = "grey50", main = "Suicides Per 100k population") # right skewed
hist_boxplot(main_data$gdp_for_year...., col = "grey50", main = "GDP") # right skewed
hist_boxplot(main_data$gdp_per_capita...., col = "grey50", main = "GDP per Capita") # right skewed

#Identifying and Removing the outliers-

#Suicides Number
Q <- quantile(main_data$suicides_no, probs=c(.25, .75), na.rm = T)
iqr <- IQR(main_data$suicides_no, na.rm = T)
main_data1 <- main_data %>% filter( suicides_no > (Q[1] - 1.5*iqr) &
                        suicides_no < (Q[2] + 1.5*iqr))

par(mfrow=c(1,1))
boxplot(main_data$suicides_no, col = "red", horizontal = T,
        main = "Suicides_no- Before Removing Outliers")
boxplot(main_data1$suicides_no, col = "red", horizontal = T,
        main = "Suicides_no - After Removing Outliers")

#Suicides per 100 k population
Q <- quantile(main_data$suicides.100k.pop, probs=c(.25, .75), na.rm = T)
iqr <- IQR(main_data$suicides.100k.pop, na.rm = T)
main_data1 <- main_data %>% filter( suicides.100k.pop > (Q[1] - 1.5*iqr) &
                                      suicides.100k.pop < (Q[2] + 1.5*iqr))

par(mfrow=c(1,1))
boxplot(main_data$suicides_no, col = "red", horizontal = T,
        main = "Suicides per 100k population- Before Removing Outliers")
boxplot(main_data1$suicides_no, col = "red", horizontal = T,
        main = "Suicides per 100k population - After Removing Outliers")

#GDP Per Capita
Q <- quantile(main_data$gdp_per_capita...., probs=c(.25, .75), na.rm = T)
iqr <- IQR(main_data$gdp_per_capita...., na.rm = T)
main_data1 <- main_data %>% filter( gdp_per_capita.... > (Q[1] - 1.5*iqr) &
                                      gdp_per_capita.... < (Q[2] + 1.5*iqr))

par(mfrow=c(1,1))
boxplot(main_data$suicides_no, col = "red", horizontal = T,
        main = "GDP per capita - Before Removing Outliers")
boxplot(main_data1$suicides_no, col = "red", horizontal = T,
        main = "GDP per capita- After Removing Outliers")

#GDP Per Year
Q <- quantile(main_data$gdp_for_year...., probs=c(.25, .75), na.rm = T)
iqr <- IQR(main_data$gdp_for_year...., na.rm = T)
main_data1 <- main_data %>% filter( gdp_for_year.... > (Q[1] - 1.5*iqr) &
                                      gdp_for_year.... < (Q[2] + 1.5*iqr))

par(mfrow=c(1,1))
boxplot(main_data$suicides_no, col = "red", horizontal = T,
        main = "GDP per year- Before Removing Outliers")
boxplot(main_data1$suicides_no, col = "red", horizontal = T,
        main = "GDP per year - After Removing Outliers")




