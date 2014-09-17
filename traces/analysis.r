#for(file_name in list.files("./univ1_csv")){
#	file_dir <- paste("~/r/traces/univ1_csv/",file_name,sep="")
#	print(file_dir)
#	csv_data <- read.csv(file=file_dir,row.names=NULL,head=TRUE,sep=",")
#	print(head(csv_data))
#}

csv_data <- read.csv(file="~/r/traces/univ1_csv",row.names=NULL,head=TRUE,sep=",")

