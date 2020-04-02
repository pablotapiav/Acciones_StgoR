rm(list = ls())


#install.packages("rtsdata")
library("rtsdata")
library("dplyr")
library("lubridate")
library(readxl)

nombres <- as.data.frame(read_excel("nombres.xlsx", col_names = TRUE))
nombres$Nemo <- gsub("\\..*","",nombres$Nemo)

###########

block <- as.data.frame(cbind(seq(from = as.Date("2014-12-31"), to = Sys.Date(), by = "day"), weekdays(seq(from = as.Date("2014-12-31"), to = Sys.Date(), by = "day"))) )
colnames(block) <- c("Date", "dia")
block$Date <- as.Date(seq(from = as.Date("2014-12-31"), to = Sys.Date(), by = "day"))
block <-  block %>% filter(dia != "sábado" & dia != "domingo") %>% select(Date)

resumen <- NULL

for (i in 1:nrow(nombres)) {
  
  nemo = nombres[i,1]
  
    data <- as.data.frame(ds.getSymbol.yahoo(paste(nombres[i,1], ".SN", sep = ""), from = "2014-12-31", to = Sys.Date()))
  data <- as.data.frame(cbind(Date = rownames(data), as.numeric(data[,4])), stringsAsFactors = FALSE)
  
  data$V2 <- as.numeric(data$V2)
  
    row.names(data) <- NULL
    data$Date <- as.Date(ymd(data$Date))

  colnames(data) <- c("Date", nemo)
  
  block <- left_join(block,data, by = "Date")
  #block <-  as.data.frame(merge(as.data.frame(block), data, by="Date", all = T))
  #block <-  as.data.frame(cbind(as.data.frame(block), data))

  
  
  
  
  prom2019 <- round(mean( (data %>% filter(Date >  "2018-12-31" & Date < "2020-12-31"))[,2] ),2)
  
  pxhoy <- round(tail(data[,2], n=1),2)
  
  vec <- c(nemo,prom2019,pxhoy, round((pxhoy/prom2019)*100,2) )
  

  resumen <- (rbind(resumen, vec))
  
    
}

resumen <- as.data.frame(resumen, stringsAsFactors = FALSE)

row.names(resumen) <- NULL
colnames(resumen) <- c("Nemo", "Prom2019", "Px_Hoy", "prop")

resumen$Prom2019 <- as.numeric(resumen$Prom2019)
resumen$Px_Hoy <- as.numeric(resumen$Px_Hoy)
resumen$prop <- as.numeric(resumen$prop)

resumen <- resumen %>% arrange(prop)



