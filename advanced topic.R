getwd()
setwd("/Users/sijinzhou/Documents/2020Spring/719/dplyr project")
library(dplyr)
library(stringr)
library(ggplot2)

my.dir <- "/Users/sijinzhou/Documents/2020Spring/719/dplyr project/google-play-store-apps/"
list.files(my.dir)
googleplay <- read.csv(paste0(my.dir, "googleplaystore.csv")
                     , header = TRUE
                     , stringsAsFactors = FALSE)

colnames(googleplay)

#clean data
str(googleplay)
googleplay <- googleplay[-10473,]
googleplay$Price <- as.numeric(gsub("\\$","",googleplay$Price))
googleplay$Installs <- gsub("\\+","",googleplay$Installs)
googleplay$Installs <- gsub(",", "", googleplay$Installs)
googleplay$Installs <- as.numeric(googleplay$Installs)
str(googleplay)
googleplay <- googleplay[-which(googleplay$Type=="NaN"),]

#filter
test <- filter(googleplay, Category=="ART_AND_DESIGN")

#sample
sample_n(googleplay,10)
sample_frac(googleplay,0.1)


#select
test <- select(googleplay, ends_with("ver"))
test <- select(googleplay, contains("type"))

#mutate
googleplay <- mutate(googleplay,
                     sale=Price * Installs)

#which category has the highest sales
dat <- googleplay %>% group_by(Category) %>% summarise(sales=sum(sale))
dat <- dat[-which(dat$sales==0),]
dat

ggplot(dat) + aes(x=Category, y=sales, fill=Category, group=factor(1)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x =  element_text(angle = 45 ,hjust=1))

#group_by
test <- group_by(googleplay, Type)
test <- summarise(test, count=n())
test
ggplot(test, aes(x="", y=count, fill=Type)) + geom_bar(stat = "identity") +
  coord_polar(theta = "y")


#arrange and select
test <- arrange(googleplay, desc(Installs))

#which app in family category has the highest installments
colnames(googleplay)
family <- select(googleplay, App, Category, Installs, Rating)
family <- family[family$Category=="FAMILY",]
family <- arrange(family, desc(Installs))
top.app <- family[1:7,]
top.app
p <- ggplot(top.app) + aes(x = App, y= Installs, fill=Rating) + geom_bar(stat="identity")+
  theme(axis.text.x =  element_text(angle = 45 ,hjust=1))
p

#Relationship between reviews and installments
class(googleplay$Reviews)
class(googleplay$Installs)
googleplay$Reviews <- as.numeric(googleplay$Reviews)
by_name <- group_by(googleplay, App)
application <- summarise(by_name,
                         count=n(),
                         Reviews=sum(Reviews),
                         Installment=sum(Installs))
ggplot(application, aes(Installment, Reviews)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

#Relationship between reviews and rating, using filter
class(googleplay$Rating)
application <- summarise(by_name,
                         count=n(),
                         Reviews=mean(Reviews, na.rm = TRUE),
                         Rating=mean(Rating, na.rm = TRUE))

ggplot(application, aes(Rating, Reviews)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

#piping + filter +select
googleplay %>% group_by(App) %>% select(Reviews, Rating) %>% 
  summarise(Reviews=mean(Reviews, na.rm = TRUE),
            Rating=mean(Rating, na.rm = TRUE),
            ) %>% filter(Rating > 3 | Reviews > 20000000)















