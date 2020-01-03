
library(ggplot2)
library(dplyr)
library(sqldf)
library(ggmap)
library(openxlsx)
library(plotrix)
file = 'C:/Users/KC/Documents/Metro College/R/project/Crimes_2019.csv' 
cc_df=read.csv(file)
#defining colors
c=c("red","black","aquamarine", "blueviolet",	"orange",	"blanchedalmond",	"blue",	"brown",	"burlywood",	"cadetblue",	"chartreuse",	"chocolate",	"coral",	"cornflowerblue",		"cyan",	"darkblue",	"darkcyan",	"darkgoldenrod",	"darkgray",	"darkgreen",	"darkgrey",	"darkkhaki",	"darkmagenta",	"darkolivegreen",	"darkorange",	"darkorchid",	"darkred",	"darksalmon",	"darkseagreen",	"darkslateblue",	"darkslategray",	"darkslategrey",	"darkturquoise",	"darkviolet",	"deeppink",	"deeppin"
)

is.na(cc_df$color)
getwd()
setwd("C:/Users/KC/Documents/Metro College/R/project")
# library(odbc)
# library(DBI)
# kc_rcon = dbConnect(odbc(),
#                     Driver ="SQL Server",
#                     Server ="DESKTOP-NK9F7RG",
#                     Database="kc",
#                     SCHEMA ="R")
# dbWriteTable(kc_rcon,"crime",cc_df)
nrow(cc_df)
#column names
names(cc_df)
cc_df=cc_df[-c(1,2)]
summary(cc_df)

#types of crimes
levels(cc_df$Primary.Type)
levels(cc_df$Description)
#plot the frequencies of different types of crimes


#-----------------------------------------------------------------------


#Q1. How many different types of crimes have been Registered in Chicago in 2019.
arr_df = sqldf("SELECT [Primary.Type], COUNT(*) AS [TOTAL_COUNT]
               FROM[cc_df]
               GROUP BY [Primary.Type]")
arr_df$Primary.Type
#arr_df$Primary.Type[which(arr_df$Primary.Type %in% c("CONCEALED CARRY LICENSE VIOLATION","INTERFERENCE WITH PUBLIC OFFICER","OFFENSE INVOLVING CHILDREN","LIQUOR LAW VIOLATION ","OTHER NARCOTIC VIOLATION","PUBLIC PEACE VIOLATION"))] =c('CNCL WPN VIO','INF PBLC OFFCR','OFFNS INV CHLDRN','OTH NRC VIOL','PBLC PCE VIOL') 


levels(cc_df$District)

pt=levels(cc_df$Primary.Type)
i=1;j=1
rownames()
for(i in 1: length(cc_df$Primary.Type)){
  for(j in 1:length(pt)){
    if(cc_df$Primary.Type[i]==pt[j]){
      cc_df[i,"color"]=c[j]
      break;
    }
  }
}
anyNA(cc_df$color)

jpeg("diff_crime.jpg", width = 1400, height = 700)



p1 <- ggplot(arr_df, aes(Primary.Type,TOTAL_COUNT)) +
  geom_bar(stat="identity",color=c[1:31],fill=c[1:31]) +
  labs(title = "Number of Crimes committed in 2019")
theme(axis.text.x = element_text(angle = 45, hjust = 0.5))
p1 +theme(axis.text.x = element_text(angle = 75, hjust = 1))
dev.off()

#-----------------------------------------------------------------------

# Q2. What is the arrest ratio for each crime

arr_sol_df =
sqldf(
 "WITH [ARR_TOT] AS (
    SELECT [Primary.Type], COUNT(*) AS [TOTAL_COUNT]
    FROM[cc_df]
    GROUP BY [Primary.Type]),
  [ARR_TRUE] AS (
    SELECT [Primary.Type], COUNT(*) AS [ARR_COUNT]
    FROM[cc_df]
    WHERE [Arrest]='true'
    GROUP BY [Primary.Type])
  SELECT [ARR_TOT].[Primary.Type],[TOTAL_COUNT],[ARR_COUNT] 
  FROM [ARR_TOT]left join [ARR_TRUE] ON [ARR_TOT].[Primary.Type] = [ARR_TRUE].[Primary.Type]"
)
#typeof(arr_sol_df$ARR_COUNT)
#Filling All NA Values 
arr_sol_df$ARR_COUNT[which(is.na(arr_sol_df$ARR_COUNT))]=0
jpeg("crime_solRatio.jpg", width = 1400, height = 700)
p2 <- ggplot(arr_sol_df, aes(Primary.Type,ARR_COUNT/TOTAL_COUNT)) +
  geom_bar(stat="identity",color=c[1:31],fill=c[1:31]) +
  labs(title = "Ratio of Crimes solved in 2019")
#theme(axis.text.x = element_text(angle = 45, hjust = 1))
p2 +theme(axis.text.x = element_text(angle = 75, hjust = 1))
dev.off()

#-----------------------------------------------------------------------


#Q3 - show the crimes on the Map

#AIzaSyAI-xYzcTfpNv_Xcbm_7I44dPlA-bviGxI
register_google(key="AIzaSyAI-xYzcTfpNv_Xcbm_7I44dPlA-bviGxI")
names(cc_df)
cc_df=cc_df[!is.na(cc_df$Latitude)|!is.na(cc_df$Longitude), ]
#mean(cc_df$X.Coordinate,na.rm=T)
#cc_df$Y.Coordinate  
mean(cc_df$Latitude,na.rm=T)
mean(cc_df$Longitude,na.rm=T)
summary(cc_df$Latitude)

jpeg("crime_geoplot.jpg", width = 1400, height = 1000)
p <- ggmap(get_googlemap(center = c(lon = mean(cc_df$Longitude,na.rm=T), lat = mean(cc_df$Latitude,na.rm=T)),
                         zoom = 12,  scale = 2,
                         maptype ="terrain",
                         color = 'color'))
p + geom_point(aes(x = Longitude, y = Latitude,  colour =factor(Primary.Type)), data = cc_df, size = 0.5)
  +theme(legend.position="right",legend.direction="vertical")
dev.off()


#-----------------------------------------------------------------------


#Q4 what are the number of crimes commited month wise.
month=c(as.numeric(substring(cc_df$Date,1,2)))
#length(month)
#length(rownames(cc_df))
cc_df['month'] = month.abb[month]
month_sum =sqldf("SELECT month, COUNT(*) AS tot FROM cc_df group by month")
piepercent<- round(100*month_sum$tot/sum(month_sum$tot), 1)
jpeg("crime_month.jpg", width = 700, height = 700)
pie(month_sum$tot,labels = piepercent,
    col=c[1:11],main="Crimes commited per month in 2019")
legend("topright", legend=month_sum$month, cex = 0.8,
       fill = c[1:11])
dev.off()
# jpeg("crime_month.jpg", width = 1400, height = 700)
# ggplot(month_sum, aes(month,tot)) +
#   geom_bar(stat="identity",color=c[1:11],fill=c[1:11]) +
#   labs(title = "Crimes commited per month in 2019")
# dev.off()


#-----------------------------------------------------------------------

#Q5. Summary of Crimes commited per month per crime

#month_summary=table(cc_df$Primary.Type,cc_df$month)
write.xlsx(table(cc_df$Primary.Type,cc_df$month),file= 'month_crime_summary.xlsx', sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

x=sqldf("SELECT month, [Primary.Type],count(*) as count from cc_df group by month, [Primary.Type] ")

#HEAT MAP
jpeg("heatmap.jpg", width = 1400, height = 1000)
ggplot(x, aes(month,Primary.Type,fill=count)) + 
  geom_tile(colour = "white") + 
  scale_fill_gradient(low = "orange", high = "red")
dev.off()


#-----------------------------------------------------------------------


#Q6. Top 15 location types where crime is highest
#names(cc_df)
Loc_summ =sqldf("SELECT [Location.Description], COUNT(*) AS tot 
                FROM cc_df 
                GROUP BY [Location.Description] 
                ORDER BY tot DESC")
Loc_summ=Loc_summ[1:15,]
length(Loc_summ$Location.Description)
jpeg("crime_location.jpg", width = 1400, height = 700)
p3=ggplot(Loc_summ, aes(Location.Description,tot)) +
   geom_histogram(stat="identity",color=c[1:15],fill=c[1:15]) +   
  labs(title = "Crimes commited per location type in 2019")
p3 +theme(axis.text.x = element_text(angle = 75, hjust = 1))
dev.off()
names(cc_df)


#-----------------------------------------------------------------------



#Q7.Which Beat made the highest number of arrests
beat_count=sqldf("SELECT beat, count(*) as count from cc_df WHERE Arrest='true' group by beat order by count desc")
#total number of crimes registered per beat.
beat_count_tot=sqldf("SELECT beat, count(*) as count from cc_df group by beat order by count desc")
#Best Beat
best_beat=sqldf("SELECT bc.beat AS Beat, bc.count As Arrests, bct.count As Crimes
              FROM beat_count bc
              JOIN beat_count_tot bct
              ON bc.beat=bct.beat") 
best_beat$ArrestRate= as.numeric(best_beat$Arrests)/as.numeric(best_beat$Crimes)

#-----------------------------------------------------------------------


#Q8. Which Police district contributed the highest number of arrests
district_count=sqldf("SELECT District, count(*) as count from cc_df WHERE Arrest='true' group by District order by count desc")
#total number of crimes registered per beat.
district_count_tot=sqldf("SELECT District, count(*) as count from cc_df group by District order by count desc")
#best District 
best_district=sqldf("SELECT dc.District  AS District , dc.count As Arrests, dct.count As Crimes
              FROM district_count dc
              JOIN district_count_tot dct
              ON dc.District=dct.District") 
best_district$ArrestRate= as.numeric(best_district$Arrests)/as.numeric(best_district$Crimes)

#-----------------------------------------------------------------------

#Q9. Which ward has the safest to live in?

ward_count =  sqldf("SELECT ward, count(*) as count from cc_df WHERE Arrest='true' group by ward order by count desc")
#total number of crimes registered per beat.
ward_count_tot=sqldf("SELECT ward, count(*) as count from cc_df group by ward order by count desc")
#best District 
best_ward=sqldf("SELECT wc.ward  AS Ward , wc.count As Arrests, wct.count As Crimes
              FROM ward_count wc
              JOIN ward_count_tot wct
              ON wc.ward=wct.ward") 
best_ward$ArrestRate= as.numeric(best_ward$Arrests)/as.numeric(best_ward$Crimes)

#best ward is the ward with least number of crimes reported? 

best_ward[order(best_ward$Crimes),] 
#38 is the best ward.

#-----------------------------------------------------------------------

#Q10. Find the number of charges that involve Financial identity theft OVER $ 300

length(which(substring(cc_df$Description,26,36)=='OVER $ 300'))
#total of 3303


