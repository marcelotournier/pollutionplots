library(plyr)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#plot1
library(plyr)
plot1=ddply(NEI, .(year), summarise, emi=sum(Emissions))
png(filename = 'plot1.png', width = 480, height = 480, units = 'px')
plot(x = plot1$year, y = plot1$emi, type = "l",lwd=4, bty="n", xlab = "Year", ylab = "Total Emissions", main = "PM2.5 Emissions - United States", col = 'red')
dev.off()

#plot2
library(plyr)
library(dplyr)
plot2=filter(NEI, fips == "24510")
plot2=ddply(plot2, .(year), summarise, emi=sum(Emissions))
png(filename = 'plot2.png', width = 480, height = 480, units = 'px')
plot(x = plot2$year, y = plot2$emi, type = "l",lwd=4, bty="n", xlab = "Year", ylab = "Total Emissions", main = "PM2.5 Emissions - Baltimore City, Maryland", col = "purple")
dev.off()

#plot3
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
plot3=filter(NEI, fips == "24510")
plot3 <- melt(plot3, id.vars=c("type", "Emissions", "year"))
plot3=ddply(plot3, .(type, year), summarise, Emissions=sum(Emissions))
png(filename = 'plot3.png', width = 480, height = 480, units = 'px')
ggplot(data=plot3, aes(x=year, y=Emissions, group = type, colour = type))+geom_line()+geom_point( size=4, shape=2)+ggtitle("PM2.5 Emissions per type of Source - Baltimore City, Maryland")
dev.off()

#plot4
library(plyr)
library(dplyr)
library(stringr)
coal=filter(SCC,(str_detect(Short.Name, "Coal")))
coalscc=unique(coal$SCC)
neicoal=NEI[NEI$SCC %in% coalscc,]
plot4=ddply(neicoal, .(year), summarise, emi=sum(Emissions))
png(filename = 'plot4.png', width = 480, height = 480, units = 'px')
plot(x = plot4$year, y = plot4$emi, type = "l",lwd=4, bty="n", xlab = "Year", ylab = "Emissions",main = "Coal combustion-related Emissions - United States", col = 'grey')
dev.off()

#plot 5
library(plyr)
library(dplyr)
library(stringr)
plot5=filter(NEI, fips == "24510")
gas=filter(SCC,(str_detect(SCC.Level.Two, "Vehicle")))
gasscc=unique(gas$SCC)
neigas=NEI[NEI$SCC %in% gasscc,]
plot5=ddply(neigas, .(year), summarise, emi=sum(Emissions))
png(filename = 'plot5.png', width = 480, height = 480, units = 'px')
plot(x = plot5$year, y = plot5$emi, type = "l",lwd=4, bty="n", xlab = "Year", ylab = "Emissions",main = "Motor vehicle Emissions - Baltimore City, Maryland", col = 'brown')
dev.off()

#plot 6
library(plyr)
library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
gas=filter(SCC,(str_detect(SCC.Level.Two, "Vehicle")))
gasscc=unique(gas$SCC)
neigas=NEI[NEI$SCC %in% gasscc,]
plot6=neigas[neigas$fips %in% c("24510","06037"),]
plot6 <- melt(plot6, id.vars=c("fips", "Emissions", "year"))
plot6=ddply(plot6, .(fips, year), summarise, Emissions=sum(Emissions))
plot6$fips=gsub("24510","Baltimore City", plot6$fips)
plot6$fips=gsub("06037","Los Angeles County", plot6$fips)
png(filename = 'plot6.png', width = 800, height = 450, units = 'px')
ggplot(data=plot6, aes(x=year, y=Emissions, group = fips, colour = fips))+geom_line()+geom_point( size=3, shape=7)+ggtitle("Motor Vehicle Emissions - Baltimore & Los Angeles")+theme(legend.title = element_blank())
dev.off()
