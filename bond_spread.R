# Step 1: Obtain Moodys Aaa and Baa Corporate Index Yield Data from FRED
library(xts)
library(quantmod)
library(plotly)

aaa<-read.csv("data/aaa.csv",header=TRUE)
aaa$date<-as.Date(aaa$DATE,"%Y-%m-%d")
aaa$AAA<-as.numeric(as.character(aaa$AAA))
aaa<-xts(aaa$AAA,order.by=aaa$date)
names(aaa)<-paste("AAA")


baa<-read.csv("data/baa.csv",header=TRUE)
baa$date<-as.Date(baa$DATE,"%Y-%m-%d")
baa$BAA<-as.numeric(as.character(baa$BAA))
baa<-xts(baa$BAA,order.by=baa$date)
names(baa)<-paste("BAA")

#Step 2: Combine Data and Subset to Only Yields from 2015 to 2020
moodys<-merge(aaa,baa)

#moodys<-subset(moodys,index(moodys)>="2015-01-01" & index(moodys)<="2020-03-01")
moodys[c(1:3,nrow(moodys)),]

#Step 3: Setup Additional Variables for Plotting
moodys<-cbind(data.frame(index(moodys)),data.frame(moodys))
names(moodys)[1]<-paste("date")
moodys[c(1:3,nrow(moodys)),]

#Step 4: Plot theYields and Shade the Spread

#Base plot
plot(x=moodys$date,xlab="Date",y=moodys$AAA,ylab="Yield (Percent)",ylim=range(2:8),type="l",lwd=1,lty=1,main="Moodys Aaa and Baa Index Yields From 2015 to 2020")
lines(x=moodys$date,y=moodys$AAA,lty=1,lwd=3)
lines(x=moodys$date,y=moodys$BAA,lty=3,lwd=3)
polygon(c(moodys$date,rev(moodys$date)),c(moodys$BAA,rev(moodys$AAA)),col="gray50",density=20,border=NA)
abline(v=c(as.Date("2010-01-01"),as.Date("2020-03-01")),lty=1)
legend("topright",c("Aaa","Baa"),lwd=c(3,3),lty=c(1,3))

#Plotly plot
x_axis <- list(title="Date", size=14, color="black")
y_axis <- list(title="Rating", size=14, color="black")
yield_plot <- plot_ly(data=moodys, x=~date, y=~AAA, name="AAA", type="scatter", mode="lines")
yield_plot <- yield_plot %>% add_trace(data=moodys, y=~BAA, name="BAA",mode="lines") %>% layout(xaxis=x_axis, yaxis=y_axis)
yield_plot
