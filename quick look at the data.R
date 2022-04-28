data<- read.csv("GlobalTemperatures.csv")
data2 <-na.omit(data)

data2 %>% mutate(Date = yearmonth(dt)) %>% as_tsibble(index=Date) %>% select(Date, everything()) %>% 
  select(-dt) ->cleanedData

cleanedData %>% model(
  classical_decomposition(LandAverageTemperature, type="additive")
) %>% components() %>% autoplot



#filter(Date >= as.Date("2000-01-01"), Date <= as.Date("2005-01-01"))%>% autoplot(LandAverageTemperature)


cleanedData%>%  filter( year(Date)> 2014) %>% autoplot(LandAverageTemperature)
