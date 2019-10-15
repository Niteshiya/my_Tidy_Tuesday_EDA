#Library
library(dplyr)
library(tidyverse)
library(tidytext)
#load data
data <- read.csv("big_mtcars.csv",stringsAsFactors = F)
names(data)
#lets make it more like mtcars data set to view 
data2 <- mtcars
str(mtcars)
str(data)
summary(data)

df<- data %>% mutate(fuel = paste0(fuelType1,",",fuelType2),
                        city = paste0(city08,",",cityA08),
                        highway = paste0(highway08,",",highwayA08),
                        Cdo = paste0(co2,",",co2A),
                     Pet_consum = paste0(barrels08,",",barrelsA08),
                     C_mpg =paste(comb08,",",combA08)) %>%
  select(make,model,year,type=VClass,displ,cylinders,drive,fuel,
         city,highway,Cdo,save=youSaveSpend,Pet_consum,C_mpg)
df
graph1 <- df %>% count(make,year) %>% arrange(year)

main_manufacturers<-c("Audi", "BMW", "Chevrolet", "Chrysler", "Dodge", 
                      "Ford", "GMC", "Honda", "Hyundai", "Jeep", 
                      "Mazda", "Mercedes-Benz", "Mitsubishi", "Nissan",
                      "Porsche", "Subaru", "Toyota", "Volkswagen", "Volvo")
top_manufacturers <- c("Chevrolet","Ford","Dodge","GMC","Toyota",
                       "BMW","Mercedes-Benz","Nissan","Volkswagen")
df %>% count(make,sort=T)
graph1 <- df %>% count(make,year) %>% filter(make %in% main_manufacturers) %>% arrange(year)
g1 <- ggplot(graph1,aes(x=year,y=n))+geom_col(aes(fill=make),col="black",show.legend = T)+xlab("Year")+ylab("Count")+ggtitle("Cars manufactured")


g1
g2<-df %>% count(type,year) %>% arrange(year) %>% group_by(type) %>% top_n(n=10,n) %>% ggplot(aes(x=year,y=n,fill=type))+geom_col(col="black")


g2
g3 <- df %>% select(make,save) %>% filter(make %in% top_manufacturers) %>% mutate(spend=save*(-1)) %>%
  group_by(make) %>% ggplot(aes(x=make,y=spend,fill=make))+geom_boxplot() +theme_minimal()+
  theme(axis.text.x = element_blank()) + xlab("Top Manufactures(1984-2020)") + ylab("Maintainance Cost")+ggtitle("Maintainance Cost over 5 years compared to an average car ($)")
g3
top_models <- df %>% select(make,model) %>% filter(make %in% top_manufacturers) %>% count(make,model) %>%
  group_by(make) %>% top_n(n=6,n)
g4<-ggplot(top_models,aes(fill=make))+geom_col(aes(x=model,y=n),show.legend = F)+coord_flip()+facet_wrap(~make,scales = "free_y")+
  xlab("Count")+ylab("Model")+ggtitle("Most sold models(1984-2020)")
g4
names(df)
str(df)
df_ <- df %>% separate_rows(fuel,city,highway,Cdo,Pet_consum,C_mpg,sep=",")%>%
  filter(fuel !="NA",city !=0,Pet_consum !=0,C_mpg !=0) %>%
  mutate(city=as.numeric(city),
         highway = as.numeric(highway),
         Pet_consum=as.numeric(Pet_consum),
         C_mpg = as.numeric(C_mpg),
         Cdo = as.numeric(Cdo),
         Cdo = na_if(Cdo,-1))%>% arrange(year)
str(df_)
no_cars <- df_ %>% select(make,year,Pet_consum) %>% filter(make %in% top_manufacturers) %>%
  group_by(make,year) %>% count(make,year)
tc <- df_ %>% select(make,year,Pet_consum) %>% filter(make %in% top_manufacturers) %>%
  group_by(make,year) %>% summarize(Tpc=sum(Pet_consum))
tc$new <- tc$Tpc/no_cars$n
g5 <- ggplot(tc,aes(x=year,y=new,col=make))+geom_line(show.legend = F)+
  facet_wrap(~make,ncol=3,scales = "free_y")+xlab("Year")+ylab("Fuel Consumption")+
  ggtitle("Average Fuel Consumption")
g1
g3
g4
g5
ggsave("one.png",g1,width=20,height = 20,units = "cm")
ggsave("two.png",g3)
ggsave("three.png",g4,width=35,height = 20,units = "cm")
ggsave("four.png",g5,width=35,height = 20,units = "cm")
