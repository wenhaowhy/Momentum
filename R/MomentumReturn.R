setwd("/Users/MasonHinckley/Documents/QuantFinance/Momentum")

library(ws.data)
library(ggplot2)
library(dplyr)
library(RcppRoll)
library(devtools)
library(Momentum)

data("daily.1998")
data("daily.1999")
data("daily.2000")
data("daily.2001")
data("daily.2002")
data("daily.2003")
data("daily.2004")
data("daily.2005")
data("daily.2006")
data("daily.2007")
data("yearly")
data("secref")

daily.data <- rbind(daily.1998, daily.1999, daily.2000, daily.2000, daily.2001, daily.2002, daily.2003, daily.2004, daily.2005, daily.2006, daily.2007)
daily.data$v.date <- as.Date(daily.data$v.date)
daily.data$year <- format(daily.data$v.date, "%Y")
daily.data <- merge(daily.data, yearly)
daily.data <- merge(daily.data, secref)
daily.data$monyear <- format(daily.data$v.date, "%m-%Y")

unq.id <- unique(daily.data$id)
id.vect <- numeric()
my.vect <- character()
mr.vect <- numeric()
pu.vect <- numeric()

for(i in 1:length(unq.id)){
  temp.frame <- daily.data %>% select(id, price.unadj, monyear, v.date) %>% filter(id == unq.id[i]) %>% arrange(v.date)
  unq.monyear <- unique(temp.frame$monyear)
  for (j in 1:length(unq.monyear)){
    temp.2 <- temp.frame %>% filter(monyear == unq.monyear[j])
    id.vect <- append(id.vect, temp.2$id[1])
    my.vect <- append(my.vect, unq.monyear[j])
    mr.vect <- append(mr.vect, (temp.2$price.unadj[nrow(temp.2)] - temp.2$price.unadj[1])/temp.2$price.unadj[1])
    pu.vect <- append(pu.vect, temp.2$price.unadj[nrow(temp.2)])
  }
}

monthly.returns <- data.frame(id = id.vect, monyear = my.vect, monreturn = mr.vect, price.unadj.end = pu.vect)
monthly.returns$price.unadj.end <- as.numeric(monthly.returns$price.unadj.end)
monthly.returns$monreturn <- as.numeric(monthly.returns$monreturn)

unq.monyear <- unique(monthly.returns$monyear)
nums <- 1:length(unq.monyear)

for (i in 1:nrow(monthly.returns)){
  monthly.returns$mon.num[i] <- match(monthly.returns$monyear[i], unq.monyear)
}


unq.monyear <- as.character(unique(monthly.returns$monyear))
id.vect <- character()
my.vect <- character()
tmr.vect <- numeric()
port.vect <- numeric()

for (i in 13:length(unq.monyear)){
  temp.frame <- monthly.returns %>% filter(mon.num >= i-12 & mon.num < i-1)
  unq.id <- as.character(unique(temp.frame$id))
  for(j in 1:length(unq.id)){
    temp.2 <- temp.frame %>% filter(id == unq.id[j]) %>% arrange(mon.num)
    id.vect <- append(id.vect, unq.id[j])
    my.vect <- append(my.vect, unq.monyear[i])
    port.vect <- append(port.vect, (i-12))
    tmr.vect <- append(tmr.vect, (temp.2$price.unadj.end[nrow(temp.2)] - (temp.2$price.unadj.end[1]))/temp.2$price.unadj.end[1])
  }
}

port.frame <- data.frame(id = id.vect, monyear = my.vect, ten.month.return = tmr.vect, port.num = port.vect)

unq.port <- unique(port.frame$port.num)
final.port.frame <- data.frame(id = character(), monyear = character(), ten.month.return = numeric(), port.num = numeric(), rank = numeric())

for (i in 1:length(unq.port)){
  temp.frame <- port.frame %>% filter(port.num == i) %>% mutate(rank = ntile(ten.month.return, 10))
  temp.frame <- temp.frame %>% filter(rank == 1 | rank == 10)
  final.port.frame <- rbind(final.port.frame, temp.frame)
}

for(i in 1:nrow(final.port.frame)){
  final.port.frame$monyear[i] <- paste(strsplit(as.character(final.port.frame$monyear[i]), "-")[[1]][2], strsplit(as.character(final.port.frame$monyear[i]), "-")[[1]][1], sep = "")
}

final.frame <- merge(final.port.frame, monthly.returns, by = c("id", "monyear"))

for (i in 1:nrow(final.frame)){
  if (final.frame$rank[i] == 1){
    final.frame$mon.return.dollars[i] = final.frame$mon.return.dollars[i]*(-1)
  }
}

save(final.frame, file="data/portfolios.RData")

WML <- final.frame %>% group_by(port.num) %>% summarise(wml.return.percent = (sum(mon.return.dollars) + sum(price.unadj.beg))/sum(price.unadj.beg), investment = sum(price.unadj.beg), rtrn.dollars = sum(mon.return.dollars))
WML <- WML %>% mutate(cumreturn = (cumsum(investment) + cumsum(rtrn.dollars))/cumsum(investment) - 1)
WML <- left_join(WML, final.frame)
ggplot() + geom_line(aes(WML$port.num, WML$cumreturn))




