### Tiempo de Compra: Eduard Bernà y Raúl Domínguez ###

### Paquetes ####

install.packages("shiny")
install.packages("dplyr")
if(!require("tidyverse")) {
  install.packages("tidyverse", repos="https://cloud.r-project.org/",
                   quiet=TRUE, type="binary")
  library("tidyverse")
}
install.packages("chron")
install.packages("plotly")
install.packages("forcats")
install.packages("ggplot2")

### Librerias ####
library(shiny)
library(dplyr)
library(chron)
library(plotly)
library(forcats)
library(ggplot2)

### Leemos los datos ####
TrainData <- read.table("train_trips.csv", sep=",", dec=".", quote = "\"'",
                        header=TRUE, skip = 0, na.strings = "NA")
OrderData <- read.table("order_items.csv", sep=",", dec=".", quote = "\"'",
                        header=TRUE, skip = 0, na.strings = "NA")
TestData <- read.table("test_trips.csv", sep=",", dec=".", quote = "\"'",
                       header=TRUE, skip = 0, na.strings = "NA")


### Cambio de formato #
TrainData$shopping_started_at <- as.POSIXlt(TrainData$shopping_started_at)
TrainData$shopping_ended_at <- as.POSIXlt(TrainData$shopping_ended_at)
TrainData$DayOfWeek <- weekdays.POSIXt(TrainData$shopping_started_at)
TrainData$HourofDay <- unclass(TrainData$shopping_started_at)$hour

resumOrder <- OrderData %>%
  group_by(trip_id) %>%
  summarise(TotalQuantity = sum(quantity))


resumTrain <- TrainData %>%
  group_by(trip_id, store_id) %>%
  summarise(shopping_time = difftime(shopping_ended_at,shopping_started_at, units = "mins"),
            DayWeek = DayOfWeek)

allTrainData <- merge(resumOrder,resumTrain, by="trip_id")

data1 <- allTrainData %>%
  group_by(shopper_id) %>%
  summarise(meanTime = mean(shopping_time)
  )

##Tiempo medio de compra por tienda)
df<-allTrainDataHour %>% group_by(DayWeek) %>% summarise(meanST=mean(shopping_time, na.rm=T))

df$DayWeek<-factor(df$DayWeek,levels=c("lunes","martes","miércoles","jueves","viernes","sábado","domingo"),ordered=TRUE)

#compras dia semana
ggplot(df, aes(x = DayWeek,y=meanST ))+geom_bar(stat="identity")+
  labs(x=" ", y="Media del tiempo de compra")


allTrainDataHour1<-allTrainDataHour[allTrainDataHour$TotalQuantity<=100,]
allTrainDataHour2<-allTrainDataHour[allTrainDataHour$TotalQuantity>100,]


# Compras <=100
ggplot(allTrainDataHour1,aes(x = TotalQuantity)) + geom_histogram(mapping = NULL,data = NULL,
                                                                  stat = "bin",
                                                                  position = "stack",
                                                                  binwidth = 3,
                                                                  bins = 200,
                                                                  na.rm = FALSE,
                                                                  orientation = NA,
                                                                  show.legend = NA,
                                                                  inherit.aes = TRUE)+
  ggtitle("                        Compras de menos de 100 euros")

# Compras 100-500
ggplot(allTrainDataHour2,aes(x = TotalQuantity)) + geom_histogram(mapping = NULL,
                                                                  data = NULL,
                                                                  stat = "bin",
                                                                  position = "stack",  
                                                                  binwidth = 50,
                                                                  bins = 200,
                                                                  na.rm = FALSE,
                                                                  orientation = NA,
                                                                  show.legend = NA,
                                                                  inherit.aes = TRUE, fill="yellow")+
  ggtitle("                         Compras de más de 100 euros ")+
  labs(x="Dinero gastado en compras", y="Número de compras al dia")

# Compras segons la hora del dia
ggplot(allTrainDataHour, aes(x=shopping_hour)) +
  geom_bar(fill="orange")+
  ggtitle("        Evolución de las compras según la hora del dia")+
  labs(x="Horas que abre el supermercado", y="Cantidad de productos vendidos")+
  scale_x_continuous(breaks=seq(min(allTrainDataHour$shopping_hour),max(allTrainDataHour$shopping_hour),by=1))


dfHours<-data.frame(table(allTrainDataHour$shopping_hour))

ggplot(dfHours)+geom_line(aes(x=as.numeric(Var1),y=Freq),stat="identity") +
  geom_point(aes(x=as.numeric(Var1),y=Freq))+
  scale_x_discrete(breaks=seq(min(allTrainDataHour$shopping_hour),max(allTrainDataHour$shopping_hour),by=1))


# Productes mes venuts ###
ProductTypeOrdenatTop5<-ProductTypeOrdenat[77:81,]

ggplot(ProductTypeOrdenatTop5, aes(x=department_name, y=TotalQuantityProductClean))+
        geom_bar(stat = "identity",na.rm=TRUE,fill="blue")+
        ggtitle("                           Ventas por departamento")+
        theme(axis.text.x=element_text(angle=90, size=rel(1.5), vjust=0.5),
                 axis.text.y=element_text(size=rel(.8), hjust=0.5),
                 legend.text = element_text(size=rel(.8)),
                 legend.title = element_blank(),
                 strip.background = element_rect(fill="#000000"),
                 strip.text = element_text(color="black",size=rel(.8)),
                 panel.background = element_rect(fill="white"),
                 axis.line = element_line(color="blue"))+
        labs(x="Top 5 departamentos con más ventas", y="Cantidad de productos vendidos")
        