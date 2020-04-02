library(readxl)
lista <- read_excel("nombres.xlsx")
library("readr")


#Beta = Covariance(Stock's % Change, Index's % Change)/Variance(Index % Change)

# IPSA -----
ipsa <- read_delim("data Ipsa DAily mar2019.csv", 
                   ";", escape_double = FALSE, col_types = cols(`% var.` = col_skip(), 
                                                                Fecha = col_date(format = "%d.%m.%Y"), 
                                                                `Maximo` = col_skip(), `Minimo` = col_skip(), 
                                                                Vol. = col_skip()), trim_ws = TRUE)

ipsa <- as.data.frame(ipsa)
colnames(ipsa) <- c("Date", "Close","Open")



ipsa <- ipsa %>% mutate(
  Var_porc = 100*(log(x = Close/Open ))
  ) %>% select(Date, Close, Var_porc) %>% ungroup()


# Beta IPSA ----

# data$var_porc <- 100*log(data$VAPORES.SN.Close/lag(data$VAPORES.SN.Close)) 
# 
# join <- left_join(ipsa, data, by = "Date")
# 
# colnames(join) <- c("Date", "Ipsa", "Var_ipsa", "Acc_Close", "Acc_Vol", "Acc_var")
# 
# 
# beta <- round(cov(join$Acc_var, join$Var_ipsa)/var(join$Var_ipsa),2)
# 
# corr <- cor(join$Acc_var, join$Var_ipsa)

######### Beta por industria 


            

# Industrias ----




  

t <- 0
y <- 0
yy <- 0

levels <- unique(nombres$Industria)

for (i in 1:length(levels)) {

  
x <- (nombres %>% filter(Industria == levels[i]) %>% ungroup())[1]

#names <- names(block)[(names(block) %in% x[,1])]

ind <- cbind(block[1], block[, x[,1]])

z <- ind %>% 
  mutate(total = rowSums(ind[1: nrow(x)+1], na.rm = TRUE),
         ind_var_porc = 100*(log(total/lag(total)))
         ) 

colnames(z)[length(z)-1] <- c(paste(levels[i], "_abs", sep = ""))
colnames(z)[length(z)] <- c(paste(levels[i], "_var", sep = ""))

t <- cbind(t,z[length(z)-1] ) #Variacion Industria abs
y <- cbind(y,z[length(z)] ) # Variacion Industria %
}


for(i in 2:length(block)) {
  
  
var_temp <- log(block[,i]/lag(block[,i]))*100



yy  <- cbind(yy, var_temp)
  
  colnames(yy)[i] <- c(paste(nombres[i-1,1], "_var", sep = "")) 

}

yy <- as.data.frame(yy, stringsAsFactors = FALSE)

yy$yy <- block$Date

y$y <- block$Date
y <- y %>% filter(Banca_var != Inf & Banca_var != -Inf)


test <- left_join(y, yy, by = c("y" = "yy"))[(length(y)+1):(length(yy)+length(y)-1)] # Var empresa

test <- cbind(y$y, test, stringsAsFactors = FALSE)


#Beta = Covariance(Stock's % Change, Index's % Change)/Variance(Index % Change)

beta <- 0

i <- 45
m <- 5
vec_beta <- 0
tabla_beta <- 0
tabla_final <- 0

# Test = Var Empresa
# y = Var Ind

for(m in 2:length(y)) {
for(i in 2:length(test)) {
  
  
    
  beta <- cov(test[i], y[m], use = "na.or.complete")/var(y[m])
  
  vec_beta <- c(nombres[i-1,1], beta)
  
  tabla_beta <- rbind(tabla_beta, vec_beta)
  
  
} 
  
  tabla_final <- cbind(tabla_final, tabla_beta[2:length(tabla_beta)])
  
}


###########################

for(i in 2:length(test)) {
  
  tabla_beta <- 0
for(m in 2:length(y)) {

    
    beta <- cov(test[i], y[m], use = "na.or.complete")/var(y[m])
    
    #vec_beta <- c(beta)
    
    tabla_beta <- cbind(tabla_beta, beta)
    
    
  } 
  
  tabla_final <- rbind(tabla_final,tabla_beta[])
  
}

