list.of.packages <- c("data.table", "anytime", "ggplot2", "scales", "bsts", "dplyr", "Hmisc","reshape2","splitstackshape")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

wd = "/home/alex/git/IATI-Covid/output/"
setwd(wd)

agg <- fread("iati_unfiltered_agg.csv")
agg = subset(agg, secondary_reporter %in% c("0","false"))
agg = subset(agg, usd_disbursement > 0)
comm = c("C", "2")
disb = c("E", "D", "3", "4")
incom = c("11")

# Analytics

t.april = subset(agg,budget_or_transaction=="Transaction" & year==2020)
t.april$transaction_date = anydate(t.april$transaction_date)
t.april$month = month(t.april$transaction_date)
t.april = subset(t.april, month==4)
t.april.publishers = unique(t.april$publisher)

t.april.commits = subset(
  t.april,
  transaction_type %in% comm &
    !(publisher %in% c("undp", "afd"))
)
undp.t.april.comm <- subset(
  t.april,
  transaction_type %in% comm &
    publisher == "undp"
)
undp.t.april.incom <- subset(
  t.april,
    transaction_type %in% incom &
    publisher == "undp"
)
undp.t.april.incom$usd_disbursement = -1 * undp.t.april.incom$usd_disbursement
undp.t.april = rbind(undp.t.april.comm, undp.t.april.incom)
t.april.commits = rbind(t.april.commits, undp.t.april)
t.april.sum = t.april.commits[,.(total.spend=sum(usd_disbursement, na.rm=T)),by=.(publisher)]
t.april.sum = t.april.sum[order(t.april.sum$total.spend),]
t.april.sum$publisher = factor(t.april.sum$publisher,levels=rev(t.april.sum$publisher))
ggplot(t.april.sum,aes(x=publisher,y=total.spend)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=dollar) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(
    y="Total April 2020 commitments (USD)",
    x=""
  )

# b.april = subset(agg,budget_or_transaction=="Budget" & year==2020)
# b.april$budget_period_start = anydate(b.april$budget_period_start)
# b.april$month = month(b.april$budget_period_start)
# b.april = subset(b.april, month==4)
# b.april.publishers = unique(b.april$publisher)

# Test charts ####

t <- subset(
  agg,
  budget_or_transaction=="Transaction" &
    year>=2008 &
    year<=2020 &
    transaction_type %in% comm &
    publisher %in% t.april.publishers &
    !(publisher %in% c("undp", "afd"))
)
undp.t.comm <- subset(
  agg,
  budget_or_transaction=="Transaction" &
    year>=2008 &
    year<=2020 &
    transaction_type %in% comm &
    publisher == "undp"
)
undp.t.incom <- subset(
  agg,
  budget_or_transaction=="Transaction" &
    year>=2008 &
    year<=2020 &
    transaction_type %in% incom &
    publisher == "undp"
)
undp.t.incom$usd_disbursement = -1 * undp.t.incom$usd_disbursement
undp.t = rbind(undp.t.comm, undp.t.incom)
t = rbind(t, undp.t)

t$transaction_date = anydate(t$transaction_date)
t$month = month(t$transaction_date)
t$year = year(t$transaction_date)
t.sum = t[,.(total.spend=sum(usd_disbursement, na.rm=T)),by=.(month, year)]
t.sum = t.sum[order(t.sum$year, t.sum$month),]
t.sum$year = factor(t.sum$year)
ggplot(t.sum, aes(x=month,y=total.spend,group=year,color=year)) +
  geom_point() + 
  geom_line() +
  scale_x_discrete(limits=month.abb) +
  scale_y_continuous(labels=dollar) +
  theme_bw() +
  labs(
    y="Total commitments (USD)",
    x="Month of commitment",
    color="Year of commitment",
    title="\"Excess\" commitments published to IATI\nas a result of COVID-19"
  )

b <- subset(agg,budget_or_transaction=="Budget" & year>=2008 & year<=2020)
b$budget_period_start = anydate(b$budget_period_start)
b$budget_period_end = anydate(b$budget_period_end)
b$month = month(b$budget_period_start)
b$year = year(b$budget_period_start)
b.sum = b[,.(total.budget=sum(usd_disbursement, na.rm=T)),by=.(month, year)]
b.sum = b.sum[order(b.sum$year, b.sum$month),]
b.sum$year = factor(b.sum$year)
ggplot(b.sum, aes(x=month,y=total.budget,group=year,color=year)) +
  geom_point() +
  geom_line() +
  scale_x_discrete(limits=month.abb) +
  scale_y_continuous(labels=dollar) +
  theme_bw() +
  labs(
    y="Total budgets (USD)",
    x="Month of budget",
    color="Year of budget"
  )

# Numeric Analysis ####
t$transaction_date = t$transaction_date - mday(t$transaction_date) + 1
t.sum = t[,.(total.spend=sum(usd_disbursement, na.rm=T)),by=.(transaction_date)]
t.sum = t.sum[order(t.sum$transaction_date),]

t.sum.pre = subset(t.sum, transaction_date<as.Date("2020-01-01"))
t_y = ts(t.sum.pre$total.spend, frequency=12, start=c(2008,1))

### Run the bsts model
t_ss <- AddLocalLinearTrend(list(), t_y)
t_ss <- AddSeasonal(t_ss, t_y, nseasons = 12)
t_bsts.model <- bsts(t_y, state.specification = t_ss, niter = 500, ping=0, seed=2016)

### Get a suggested number of burn-ins
t_burn <- SuggestBurn(0.1, t_bsts.model)

# Predict until December 2024
t_p <- predict.bsts(t_bsts.model, horizon = 12, burn = t_burn, quantiles = c(.025, .975))
t_p.ts = ts(rep(NA,12),frequency=12,start=c(2020,1))
### Actual versus predicted
t_d2 <- data.frame(
  # fitted values and predictions
  c(as.numeric(-colMeans(t_bsts.model$one.step.prediction.errors[-(1:t_burn),])+t_y),  
    as.numeric(t_p$mean)),
  # actual data and dates 
  as.numeric(c(t_y,rep(NA,12))),
  c(as.Date(time(t_y)),as.Date(time(t_p.ts))))
names(t_d2) <- c("Fitted", "Actual", "Date")

### 95% forecast credible interval
t_posterior.interval <- cbind.data.frame(
  as.numeric(t_p$interval[1,]),
  as.numeric(t_p$interval[2,]), 
  subset(t_d2, Date>=as.Date("2020-01-01"))$Date)
names(t_posterior.interval) <- c("LL", "UL", "Date")

### Join intervals to the forecast
t_d3 <- left_join(t_d2, t_posterior.interval, by="Date")
t_d3$Fitted[which(t_d3$Fitted<0)] = 0
t_d3$LL[which(t_d3$LL<0)] = 0
t_d3$UL[which(t_d3$UL<0)] = 0

prediction = subset(t_d3,Date==as.Date("2020-04-01"))
actual = subset(t.sum,transaction_date==as.Date("2020-04-01"))
actual_total = actual$total.spend
predicted_total = prediction$Fitted
predicted_ll = prediction$LL
predicted_ul = prediction$UL
probable_extra = actual_total - predicted_total
probable_extra_perc = probable_extra/predicted_total

message(
  paste0(
    "The observed April 2020 commitment total was $",
    format(round(actual_total),big.mark=","), ".\n",
    "The Baysean time-series projection for April 2020 based on data of the same publishers from",
    " 2008 to 2019 was $",
    format(round(predicted_total),big.mark=","), " with",
    " a 95% confidence interval lower limit of $",
    format(round(predicted_ll),big.mark=","), " and an upper limit of $",
    format(round(predicted_ul),big.mark=","), ".\n",
    "From this we can infer that the \"excess\" COVID-19 commitment total is probably $",
    format(round(probable_extra),big.mark=",",scientific=F), " (+",percent(probable_extra_perc, big.mark=""),")."
  )
)

t_d4 = t_d3
t_d4$transaction_date = t_d4$Date
t_d4 = merge(t_d4, t.sum, by="transaction_date")
t_d4$Actual[which(is.na(t_d4$Actual))] = t_d4$total.spend[which(is.na(t_d4$Actual))]
ggplot(data=t_d4, aes(x=Date)) +
  geom_line(aes(y=Actual, colour = "Actual"), size=1.2) +
  geom_line(data=subset(t_d4,Date>=as.Date("2020-04-01")),aes(y=Fitted, colour = "Estimate"), size=1.2, linetype=1) +
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") +
  geom_vline(xintercept=as.numeric(as.Date("2020-04-01")), linetype=2) + 
  geom_ribbon(data=subset(t_d4,Date>=as.Date("2020-04-01")),aes(ymin=LL, ymax=UL, fill="95% conf."), alpha=0.5) +
  scale_fill_manual("", values=c("95% conf."="grey")) +
  scale_y_continuous(labels=scales::dollar) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

# Bill Analysis
b.dat = subset(agg,budget_or_transaction=="Transaction" & year==2020 & transaction_type %in% comm)
b.dat$transaction_date = anydate(b.dat$transaction_date)
keep = c("iati_identifier","reporting_org_ref","publisher","humanitarian",
         "transaction_date", "usd_disbursement", "transaction_sector_code",
         "transaction_sector_percentage", "transaction_sector_vocabulary", "transaction_type")
b.dat = b.dat[,keep,with=F]
b.dat$transaction.id = c(1:nrow(b.dat))
names(b.dat) = gsub("_",".",names(b.dat))
original_names = names(b.dat)
b.dat.split = cSplit(b.dat,c("transaction.sector.code", "transaction.sector.percentage", "transaction.sector.vocabulary"),",")
new_names = setdiff(names(b.dat.split),original_names)
b.dat.split.long =reshape(b.dat.split, varying=new_names, direction="long", sep="_")
b.dat.split.long[ , `:=`( max_count = .N , count = 1:.N ) , by = transaction.id ]
b.dat.split.long=subset(b.dat.split.long, !is.na(transaction.sector.code) | max_count==1 | count==1)
b.dat.split.long$usd.value=(b.dat.split.long$transaction.sector.percentage/100)*b.dat.split.long$usd.disbursement
b.dat.split.long$usd.value[which(is.na(b.dat.split.long$usd.value))] = b.dat.split.long$usd.disbursement[which(is.na(b.dat.split.long$usd.value))]
b.dat.split.long[,c("usd.disbursement", "max_count", "count", "transaction.id", "id", "time")] = NULL
fwrite(b.dat.split.long, "bill_data.csv")
