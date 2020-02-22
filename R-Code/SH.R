#Packages ####
library(readr)
library(tidyverse)
library(psych)
library(corrplot)
library(imputeTS)
library(ggfortify) #PCA plot biplot
library(MASS)
library(klaR) # LDA Plot

#Data Cleaning ####
AB <- read_csv("F:/Graduate/Depaul/Classes/DSC 424/datasets/AB_NYC_2019.csv")

AB = as.data.frame(AB)

ddd <-  c("id", "host_id")
AB[ddd] <- NULL

head(AB)
summary(AB)
str(AB)

#Character columns to factor columns 
names_to_factor <- c("host_name", "neighbourhood_group", "neighbourhood", "room_type")
AB[names_to_factor] <- map(AB[names_to_factor], as.factor)

#Convert to date format
AB[c("last_review")] <- AB[c("last_review")] %>% map(~lubridate::ymd(.x))

#Check
glimpse(AB)

#Missing Data
Missing <- AB %>% summarise_all(~(sum(is.na(.))/n()))
Missing <- gather(Missing, key = "variables", value = "percent_missing")
AB$reviews_per_month = na_mean(AB$reviews_per_month)
Missing

#Numeric
ABC <- AB %>% select_if(is.numeric)
ABC[ddd] <- NULL

#Data Vis ####
a_cor <- ABC[complete.cases(ABC), ]
cor_a <- cor(a_cor, method = 'spearman') 
corrplot(cor_a, method = 'ellipse', order = "AOE", diag = FALSE)

#Histogram Latitude
ggplot(AB, aes(latitude)) +
  geom_histogram(bins = 15, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.2, fill = "purple") +
  ggtitle("Distribution of latitude")
  theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_vline(xintercept = round(mean(AB$latitude), 2), size = 2, linetype = 3)

#Histogram longitude  
ggplot(AB, aes(longitude)) +
    geom_histogram(bins = 70, aes(y = ..density..), fill = "purple") + 
    geom_density(alpha = 0.2, fill = "purple") +
    ggtitle("Distribution of longitude")
  theme(axis.title = element_text(), axis.title.x = element_text()) +
    geom_vline(xintercept = round(mean(AB$longitude), 2), size = 2, linetype = 3)

#Boxplot Latitude
ggplot(AB, aes(x = room_type, y = latitude)) +
  geom_boxplot(aes(fill = room_type)) + scale_y_log10() +
  xlab("Room type") + 
  ylab("latitude") +
  ggtitle("Boxplots of latitude by room type",
        subtitle = "Shared Room highest avg latitude") 


#Variable Selction ####  
str(ABC)
summary(ABC)

#PCA with Numerical variables only
P_A <- principal(ABC, nfactors = 4, covar = TRUE)
print(P_A)

print(P_A$loadings, cutoff=.25)

#PCA Cor- Plot Analysis
plot.psych(cor(P_A))
cor
cor.plot(P_A, sort = TRUE)
plot.psych(P_A)

#PCA Plot Analysis
biplot.psych(P_A)
biplot(P_A)


#LDA by Room Type
AB <- as.data.frame(AB)
LDA_A <- lda(AB$room_type ~ AB$latitude + AB$longitude + AB$neighbourhood_group + AB$availability_365
             , data = AB)
print(LDA_A)

#LDA by Neighborhood group
LDA_B <- lda(AB$neighbourhood_group ~ AB$room_type + AB$price + AB$minimum_nights 
             + AB$availability_365, data = AB)
print(LDA_B)

help("plot.psych")
paa = as.data.frame(P_A$scores)    

# Make a dataset
paa$ID_dataset = ABC$id
paa

#LDA Cor- Plot Analysis
LDAA <- as.data.frame(LDA_A)
LDBB <- as.data.frame(LDA_B)
plot.psych(cor(LDA_B))
cor
#Assesment into neighborhood group
plot(LDA_B, sort = TRUE)
plot.psych(LDA_B)

plot(LDA_B$x[,1], LDA_B$x[,2])
text(LDA_B$x[,1], LDA_B$x[,2], Type, cex = 0.7, pos = 4, col = "red")

# Machine Learning ####
fit = lm(y ~ x2 + x1, data=d)
summary(fit)