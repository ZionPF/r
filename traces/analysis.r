#for(file_name in list.files("./univ1_csv")){
#	file_dir <- paste("~/r/traces/univ1_csv/",file_name,sep="")
#	print(file_dir)
#	csv_data <- read.csv(file=file_dir,row.names=NULL,head=TRUE,sep=",")
#	print(head(csv_data))
#}

#csv_data <- read.csv(file="~/r/traces/univ1_csv/univ1_pt1.csv",row.names=NULL,head=TRUE,sep=",")

#select flows for certain pair
#csv_data[csv_data$ip.src=="41.177.117.184" & csv_data$ip.dst=="41.177.3.224",]

#To turn a colume into real number (no factor number),do it like this:
#time_vector <- as.numeric(as.character(csv_data$frame.time_relative))


#To read big tables, do like this:
#http://www.biostat.jhsph.edu/~rpeng/docs/R-large-tables.html

library(stringr)
dat <- "~/r/traces/univ1_csv/univ1_pt1.csv"
tab5rows <- read.csv(dat, header = TRUE, nrows = 5, sep=",")
classes <- sapply(tab5rows,class)
lines <- readLines(dat)
#Now delete the lines with wrong colume number
lines <- lines[str_count(lines,",")<4]
csv_data <- read.csv(text=lines, header = TRUE, sep=",", colClasses=classes)


