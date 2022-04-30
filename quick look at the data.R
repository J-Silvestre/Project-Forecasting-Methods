data<- read.csv("data/GlobalTemperatures.csv")
data2 <-na.omit(data)

data2 %>% mutate(Date = yearmonth(dt)) %>% as_tsibble(index=Date) %>% select(Date, everything()) %>% 
  select(-dt) ->cleanedData

cleanedData %>% model(
  classical_decomposition(LandAverageTemperature, type="additive")
) %>% components() %>% autoplot



#filter(Date >= as.Date("2000-01-01"), Date <= as.Date("2005-01-01"))%>% autoplot(LandAverageTemperature)


cleanedData%>%  filter( year(Date)> 2014) %>% autoplot(LandAverageTemperature)



GlobalTemperatures

tsibble<-function(data){
  data<- data %>% mutate(Date = yearmonth(dt)) %>% as_tsibble(index=Date) %>% select(Date, everything()) %>% 
    select(-dt)
  return(data)
}


data_join <- list.files(pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  reduce(full_join, by = "dt")                      # Full-join data sets into one data set 
data_join                                           # Print data to RStudio console

data_join %>% filter(Country != "Åland") %>%
select(-AverageTemperatureUncertainty, -LandAverageTemperatureUncertainty, -LandMaxTemperatureUncertainty, -LandMinTemperatureUncertainty, -LandAndOceanAverageTemperatureUncertainty, -AverageTemperatureUncertainty)




GlobalTemperature2 <-read.csv("GlobalTemperatures2.csv")


GlobalTemperature<- GlobalTemperature%>% mutate(Date = yearmonth(dt)) %>% as_tsibble(index=Date) %>% select(Date, everything()) %>% select(-dt, -LandAndOceanAverageTemperatureUncertainty, -LandMaxTemperatureUncertainty, -LandMinTemperatureUncertainty, -LandAverageTemperatureUncertainty) 

GlobalTemperatureCountry <- GlobalTemperatureCountry %>% mutate(Date= yearmonth(dt)) %>% as_tsibble(index=Date, key =Country) %>%  select(-dt, -AverageTemperatureUncertainty ) %>% select(Date, Country, AverageTemperature)




#cleaning nas
GlobalTemperature <- na.omit(GlobalTemperature)
GlobalTemperatureCountry <- na.omit(GlobalTemperatureCountry)



ggplot()+
  geom_line(data=GlobalTempV ,aes(Date, LandAvgTemp, col= "Global"),lwd=1.5)+
  geom_line(data=PortugalTemp, aes(Date, AvgTemp, col= "Portugal"))+
  geom_line(data=GreenlandTemp, aes(Date, AvgTemp, col= "GreenLand"))+
  geom_line(data=MaliTemp, aes(Date, AvgTemp, col= "Mali"))+
  labs(title= "Global Land Temperature",
            subtitle = "Average",
            y= "LandAvgTemp Cº")+
  ylim(-30,40)+
  theme(legend.position = "right")



ggplot()+
  geom_line(data=GlobalTempV ,aes(Date, LandAvgTemp, col= "Global"),lwd=1.5)+
  geom_line(data=PortugalTempV, aes(Date, LandAvgTemp, col= "Portugal"))+
  geom_line(data=GreenlandTempV, aes(Date, LandAvgTemp, col= "GreenLand"))+
  geom_line(data=MaliTempV, aes(Date, LandAvgTemp, col= "Mali"))+
  labs(title= "Global Land Temperature",
       subtitle = "Average",
       y= "LandAvgTemp Cº")+
  ylim(-30,40)+
  theme(legend.position = "right")

