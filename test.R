ufo<-read.delim("data/ufo/ufo_awesome.tsv",sep="\t",stringsAsFactors=FALSE,header = FALSE,na.strings="")
names(ufo)<-c("DateOccurred","DateReported","Location","ShortDescription","Duration","LongDescription")

errorDateRows <- which(nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateReported)!=8)
errorDateData_1 <- ufo[errorDateRows,1]

good.rows <- which(nchar(ufo$DateOccurred)==8 & nchar(ufo$DateReported)==8)
ufo=ufo[good.rows,]

ufo$DateOccurred<-as.Date(ufo$DateOccurred,format="%Y%m%d")
ufo$DateReported<-as.Date(ufo$DateReported,format="%Y%m%d")




location.exception <- function(e)
{
	return(c(NA,NA))
}


get.location <- function(location)
{
	split.location <- tryCatch(strsplit(location,",")[[1]],error=location.exception)
	clean.location <- gsub("^ ","", split.location);
	if(length(clean.location) >2)
	{
		return(c(NA,NA))
	}
	else
	{
		return(clean.location);
	}
}

city.state <- lapply(ufo$Location,get.location);
location.matrix <- do.call(rbind,city.state)
ufo <- transform(ufo,USCity=location.matrix[,1],USState = toupper(location.matrix[,2]),stringsAsFactors=FALSE)

us.states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
		"MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RL","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
ufo$USState <- us.states[match(ufo$USState,us.states)];
ufo$USCity[is.na(ufo$USState)] <- NA

non.us.rows <- is.na(ufo$USCity);
us.ufo <- ufo[!non.us.rows,]
quick.history <- ggplot(us.ufo,aes(x=DateOccurred))+geom_histogram()
#+scale_x_date(breaks="50 years")
#print(quick.history);
us.ufo <- subset(us.ufo,DateOccurred >= as.Date("1990-01-01"))

us.ufo$YearMonth <- strftime(us.ufo$DateOccurred,format="%Y-%m")

quick.history <- ggplot(us.ufo,aes(x=DateOccurred))+geom_histogram()
#print(quick.history);
require("plyr")

sighting.count <- ddply(us.ufo,.(USState,YearMonth),nrow)


date.range <- seq.Date(from=as.Date(min(us.ufo$DateOccurred)), to = as.Date(max(us.ufo$DateOccurred)),stringsAsFactors = FALSE,by="month")
date.strings <- strftime(date.range,format="%Y-%m")

state.dates <- lapply(us.states,function(s) {cbind(s,date.strings)})
state.dates <- data.frame(do.call(rbind,state.dates),stringsAsFactors = FALSE)

all.sightings<- merge(state.dates,sighting.count,by.x=c("s","date.strings"),by.y=c("USState","YearMonth"),all=TRUE)
names(all.sightings) <- c("USState","YearMonth","Sightings")
all.sightings$Sightings[is.na(all.sightings$Sightings)] <- 0

all.sightings$YearMonth <- as.Date(rep(date.range,length(us.states)))
all.sightings$USState <- as.factor(all.sightings$USState)


state.plot <- ggplot(all.sightings, aes(x = YearMonth,y = Sightings)) +
  geom_line(aes(color = "darkblue")) +
  facet_wrap(~USState, nrow = 10, ncol = 5) + 
  theme_bw() + 
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +
  scale_x_date(breaks = "5 years", labels = date_format('%Y')) +
  xlab("Years") +
  ylab("Number of Sightings") +
  ggtitle("Number of UFO sightings by Month-Year and U.S. State (1990-2010)")





