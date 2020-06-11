rm(list=ls())
dev.off()

library (gtrendsR)
keywords=c("Vaseline","Nivea", "Ponds")
country=c("GB")
time=("2010-01-01 2020-05-30")
channel='web'

trends = gtrends(keywords, gprop =channel,geo=country, time = time )
#select only interest over time 
time_trend=trends$interest_over_time

library(ggplot2)
# raw plot can be a bit messy
plot<-ggplot(data=time_trend, aes(x=date, y=hits,group=keyword,col=keyword))+
  geom_line()+xlab('Time')+ylab('Relative Interest')+ theme_bw()+
  theme(legend.title = element_blank(),legend.position="bottom",legend.text=element_text(size=12))+ggtitle("Google Search Volume")
plot

# Sometimes there are outliers which makes it difficult to analyze, therefore we can remove hits
# that are too high based on the dataset for example we can set the limit to 45
time_trend2=time_trend[time_trend$hits<100,]
plot2<-ggplot(data=time_trend2, aes(x=date, y=hits,group=keyword,col=keyword))+
  geom_line()+xlab('Time')+ylab('Relative Interest')+ theme_bw()+
  theme(legend.title = element_blank(),legend.position="bottom",legend.text=element_text(size=12))+ggtitle("Google Search Volume")
plot2

# Remove seasonality to only display trend by using geom_smooth()
plot3<-ggplot(data=time_trend2, aes(x=date, y=hits,group=keyword,col=keyword))+
  geom_smooth(span=0.5,se=FALSE)+xlab('Time')+ylab('Relative Interest')+
  theme_bw()+theme(legend.title = element_blank(),legend.position="bottom",
                   legend.text=element_text(size=12))+ggtitle("Google Search Volume")
plot3

#Trend of first keyword
trendTS <- ts(time_trend2$hits[time_trend2$keyword=="Nivea"], frequency=12,start=c(2010, 1))
head(trendTS)
decomTS <- decompose(trendTS, type='multiplicative')
plot(decomTS$trend)

trendTS2<- ts(time_trend2$hits[time_trend2$keyword=="Vaseline"], frequency=12,start=c(2010, 1))
head(trendTS2)
decomTS2<- decompose(trendTS2, type="multiplicative")
plot(decomTS2$trend)
par(mfrow= c(1,1))

plot(decomTS$trend, ylim= c(10, 55), col='blue', main="Google trend",
     ylab="hits")
lines(decomTS2$trend, col='red')
legend("topright",c("Nivea","Vaseline"), fill= c("blue", "red"))


cor(decomTS$trend, decomTS2$trend)
decomTS$trend
