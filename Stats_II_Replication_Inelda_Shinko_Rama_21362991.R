set.seed(345)
# load the require libraries
library(ggplot2)

##### BASIC ANALYSIS #####
##### Correlation #####
# datasets
tot_mes <- read.csv("local-national-tot_mes_replication.csv",header = T)
head(tot_mes)

summary(tot_mes[,5:9])

aggregate(tot_mes[,5:9], by=list(tot_mes$Type),FUN=mean)
aggregate(tot_mes[,5:9], by=list(tot_mes$Type),FUN=sd)
aggregate(tot_mes[,5:9], by=list(tot_mes$Year),FUN=mean)
aggregate(tot_mes[,5:9], by=list(tot_mes$Year),FUN=sd)

#First column
# split the data and find correlation different years 
year_split <- split(tot_mes, tot_mes$Year)

# create a function find correlation and linear regression between variables 
correlator.fn <-function(col1,col2){
  z <- as.data.frame(cbind(col1,col2))
  a.cor <- paste("The correlation is", cor(z$col1,z$col2))
  c.lm <- summary(lm(z$col2~z$col1))
  return(list(a.cor,c.lm))
}

# call the function and pass arguments
mod11 <- correlator.fn(year_split[[1]]$E,year_split[[1]]$EN)


mod12 <- correlator.fn(year_split[[2]]$E,year_split[[2]]$EN)
mod12

mod13 <- correlator.fn(year_split[[3]]$E,year_split[[3]]$EN)
mod13

#Second column
# subset the data into different years
tot_mes1910<-subset(tot_mes,tot_mes$Year=="1910")
tot_mes191214<-subset(tot_mes,tot_mes$Year=="191214")
tot_mes191618<-subset(tot_mes,tot_mes$Year=="191618")
head(tot_mes1910)

mod21<-lm(SN~S, data=tot_mes1910)
plot(SN~S, data=tot_mes1910, col=factor(Type), main="Correlation between SN and S for year 1910")
abline(mod21, lty=2)
summary(mod21)

sym <- c(22, 24)
cols <- c("green",blue)

mod22<-lm(SN~S, data=tot_mes191214)
plot(SN~S, data=tot_mes191214, pch= sym[factor(Type)],col=cols[factor(Type)], main="Correlation between SN and S for year 1912-14")
abline(mod22, lty=2)
summary(mod22)

cols18 <- c("red", "yellow")
mod23<-lm(SN~S,data = tot_mes191618)
plot(SN~S, data=tot_mes191618, pch= sym[factor(Type)],col=cols18[factor(Type)], main="Correlation between SN and S for year 1916-18")
abline(mod23, lty=2)
summary(mod23)

#Last Column
mod31<-lm(SoceN~Soce, data = tot_mes1910)
plot(SoceN~Soce, data=tot_mes1910, pch= sym[factor(Type)],col=factor(Type), main="Correlation between SoceN and Soce for year 1910")
abline(mod31, lty=2)
summary(mod31)

mod32<-lm(SoceN~Soce, data=tot_mes191214)
plot(SoceN~Soce, data=tot_mes191214, pch= sym[factor(Type)],col=factor(Type), main="Correlation between SoceN and Soce for year 1912-14")
abline(mod32, lty=2)
summary(mod32)

mod33<-lm(SoceN~Soce, data = tot_mes191618)
plot(SoceN~Soce, data=tot_mes191618, pch= sym[factor(Type)],col=factor(Type), main="Correlation between SoceN and Soce for year 1916-18")
abline(mod33, lty=2)
summary(mod33)




###### REPLICATION ANALYSIS ######
#  read in the data set
dat<- read.csv("2ab_replication.csv",header = TRUE)


# plot a box plot 
plot(sd~factor(Party2), 
     data=dat, 
     main="General Standard deviation for different Party", 
     col=c("grey70", "green"), 
     xlab="Party 2", 
     ylab="Standard deviation")



# subset the data into rural and urban and plot it

dat_urban <- subset(dat,dat$Type=="Urban")

plot(sd~factor(Party2), 
     data=dat_urban, 
     main="Standard deviation for Urban", 
     col=c("#689edf", "#2724da"), 
     xlab="Party 2", 
     ylab="Standard deviation")

dat_rural<- subset(dat,dat$Type=="rural")

plot(sd~factor(Party2), 
     data=dat_rural, 
     main="Standard deviation for Rural", 
     col=c("#6c094f", "#aece5c"), 
     xlab="Party 2", 
     ylab="Standard deviation")

# plot the income distribution for urban registered voters 
labs_u<-c(100, 200, 200,300,500,600,800,1000,1500,5000,9500,11500,15000)
dist_u<-c(0.09647266,0.20567017,0.29874679,0.38777379,0.45923167,0.52166165,0.58701277,0.65924060,0.78233496,0.89471692,0.95056172,0.98369572,1.00000000)

plot1<-ggplot(dat_urban, aes(x=dist, y=Value, group=PartyY, color=Party2)) +
  geom_step(aes(linetype=factor(Year2))) + geom_point(aes(shape=factor(Year)),show.legend = FALSE)+
  scale_color_manual(values=c('blue',"green"))+
  scale_x_continuous(breaks=c(dist_u),labels=labs_u)+
  scale_y_continuous(limits=c(0, 1))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  labs(title = "Urban Income Distribution for Registered voters in Sweden", x = "Income (Cum Distribution of Registered Population)", y = "Share of Registered Voters", color = "\n",linetype = " Election Year") 
plot1


# plot the income distribution for rural registered voters
labs_r<-c(c(100,200,300,400,500,600,800,1000,1500,3000,5500,15500,30000))
dist_r<-c(0.2718833,0.4374807, 0.5510673, 0.6337851,0.6978094,0.7512661,0.8097766,0.8539019,0.9221030,0.9702318,0.9874676,0.9971107,1.0000000)
plot2<-ggplot(dat_rural, aes(x=dist, y=Value, group=PartyY, color=Party2)) +
  geom_step(aes(linetype=factor(Year2))) + geom_point(aes(shape=factor(Year)),show.legend = FALSE)+
  scale_color_manual(values=c('orange',"red"))+
  scale_x_continuous(breaks=c(dist_r),labels=labs_r)+
  scale_y_continuous(limits=c(0, 1))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ 
  labs(title = "Rural Income Distribution for Registered voters in Sweden", x = "Income (Cum Distribution of Registered Population)", 
       y = "Share of Registered Voters", color = "\n",linetype = " Election Year") 
plot2


# data set 2
dat_all<- read.csv("3ab_replication.csv",header = T)
#head(dat_all)

# convert the Party_lab to factor
#dat_all$Party_lab <- factor(dat_all$Party_lab)

plot(sd~factor(Party_lab), 
     data=dat_all, 
     main="General Standard deviation for Urban and Rural", 
     col=c("white", "grey70", "red"), 
     xlab="Party lab", 
     ylab="Standard deviation")

# subset the data into different types
dat_urban_all<- subset(dat_all,dat_all$Type=="Urban")
#head(dat_urban_all)

# plot the urban boxplot distribution for different party lab
plot(Value~factor(Party_lab), 
     data=dat_urban_all, 
     main="Distribution Value for Urban Party lab", 
     col=c("blue", "green", "white"), 
     xlab="Party lab", 
     ylab="Value")


dat_rural_all<- subset(dat_all,dat_all$Type=="rural")

# plot the rural boxplot distribution for different party lab
plot(Value~factor(Party_lab), 
     data=dat_rural_all, 
     main="Distribution Valur for Rural Party lab", 
     col=c("orange", "white", "magenta"), 
     xlab="Party lab", 
     ylab="Value")


# urban plot
labs.u<-c("300","600","1500","15000")
cols <- c("blue", "green", "red")
plot_urban<-ggplot(data = dat_urban_all, aes(x = dist, y = Value)) +
  geom_point(aes(color=Year)) +
  geom_step(aes(color=Year,linetype=Year)) +
  scale_x_continuous(breaks=c(0.25,0.5,0.75,1),labels=c(as.character(labs.u)))+
  facet_grid(. ~ Party_lab) +
  scale_color_grey(start= 0.8, end =0.2) + theme_bw()+
  labs(title = "Urban Registered Voters income in Sweden for different Parties", color = " Year",linetype = " Year",x="Income (Figures mark income quantiles)",y="Share of Eligible Voters")
plot_urban

# rural plot
labs <- c("200","400","800","15500")

plot_rural <- ggplot(data = dat_rural_all, aes(x = dist, y = Value)) +
  geom_point(aes(color=Year)) +
  geom_step(aes(color=Year,linetype=Year)) +
  facet_grid(. ~ Party_lab) +
  scale_x_continuous(breaks=c(0.25,0.5,0.75,1),labels=c(as.character(labs)))+
  scale_color_grey(start= 0.8, end =0.2) + theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Rural Registered Voters income in Sweden for different Parties",x="Income (Figures mark income quantiles)",y="Share of Eligible Voters")
plot_rural





