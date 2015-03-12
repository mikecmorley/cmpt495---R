#outputdir -- directory for output (boxplots, etc) and for where you have the aoc and rhino out_results folders
outputdir<-"DIRECTORY HERE"
setwd(outputdir)

#opens library
library(multcomp)
library(multcompView)

#threshhold value, change to whatever
thresh <- 10

#scans directory and creates a list of all files and sub directories for AOC and rhino datasets
directories<-list.dirs(path=paste(outputdir, "/out_results", sep=""), full.names=TRUE, recursive=TRUE)
directories2<-list.dirs(path=paste(outputdir, "/out_results_rhino", sep=""), full.names=TRUE, recursive=TRUE)

#empty vectors to store file information
files<-vector()
files2<-vector()
files3<-vector()
files4<-vector()

#loops to store all files within marked_search and prf_results into the files and files2 vectors (for the AOC set)
#and files3 and files4 for the rhino set
for (i in 1:length(directories)) {
	if (grepl("marked_search", directories[i])==TRUE)
		files <- append(files, list.files(path=directories[i], full.names=TRUE))
}

for (i in 1:length(directories)) {
	if (grepl("prf_results", directories[i])==TRUE)
		files2 <- append(files2, list.files(path=directories[i], full.names=TRUE))
}

for (i in 1:length(directories2)) {
	if (grepl("marked_search", directories2[i])==TRUE)
		files3 <- append(files3, list.files(path=directories2[i], full.names=TRUE))
}

for (i in 1:length(directories2)) {
	if (grepl("prf_results", directories2[i])==TRUE)
		files4 <- append(files4, list.files(path=directories2[i], full.names=TRUE))
}

#---------------------------------------------------AOC DATASET LOOP
#-------------------------------------------------------------------

#empty data frame to store all prfmap data
prfdataframe<-data.frame()

#loop to read in files from AOC set
for (i in 1:length(files)) {
	data<-read.table(files[i], sep="", nrows=thresh, col.names=c(sub(".marked_search.(.*)", "", 
		files[i]), sub("(.*?)marked_search.", "", files[i])))
	data2<-read.table(files2[i], header=F, skip=1, sep="", col.names=c(sub(".prf_results.(.*)", "", 
		files2[i]), sub("(.*?)prf_results.", "", files2[i]), "tp", "p", "r", "f", "fp", "fp-rate", "tn"))
	colnames(data)[1] <- sub("(.*?)out_results.", "", colnames(data)[1])
	colnames(data2)[1] <- sub("(.*?)out_results.", "", colnames(data2)[1])

#counters for P/R/F/MAP calculations
counter<-0
lastvalue<-0
rank<-0
map<-0

#loops through data (dataframe) looking for asterisks, keeps track of how many occur within threshhold
for (i in 1:nrow(data)) {
	if (pmatch("*", data[1][i,], nomatch=FALSE, duplicates.ok=FALSE)==1) {
		counter<-counter + 1 
		}
}

#calculates MAP via prf files
for (i in 1:nrow(data2)){
	if (data2$tp[i] != lastvalue) {
		map<-map + ((data2$tp[i]-lastvalue) * data2$p[i])
		lastvalue<-data2$tp[i]
		rank<-rank + 1
		}
}

#adds P/R/F/MAP to the data dataframe
data$P<-(counter/thresh)
	if (data$P[1] == "NaN" || data$P[1] == "Inf")
		data$P<-0
data$R<-(counter/data2$tp[length(data2$tp)])
	if (data$R[1] == "NaN" || data$R[1] == "Inf")
		data$R<-0
data$F<-(2*((data$P[1] * data$R[1])/(data$P[1] + data$R[1])))
	if (data$F[1] == "NaN" || data$F[1] == "Inf")
		data$F<-0
data$MAP<-(map/data2$tp[length(data2$tp)]) / 100
	if (data$MAP[1] == "NaN" || data$MAP[1] == "Inf")
		data$MAP<-0	

#adds concern name to the dataframe
if (grepl("add_text", names(data[2])) == TRUE) {
	data$Concern <- "Textfield"
  data$Subject <- "J"
	}
if (grepl("create_textfield", names(data[2])) == TRUE) {
	data$Concern <- "Textfield"
	data$Subject <- "C"
	}
if (grepl("text_field_tool", names(data[2])) == TRUE) {
	data$Concern <- "Textfield"
	data$Subject <- "I"
	}
if (grepl("textfield", names(data[2])) == TRUE) {
	data$Concern <- "Textfield"
	data$Subject <- "N"
	}
if (grepl("textfieldReportElement", names(data[2])) == TRUE) {
	data$Concern <- "Textfield"
  data$Subject <- "P"
	}
if (grepl("textfieldReportElement1", names(data[2])) == TRUE) {
	data$Concern <- "Textfield"
	data$Subject <- "E"
	}
if (grepl("IReportCompiler", names(data[2])) == TRUE) {
	data$Concern <- "Compile"
  data$Subject <- "A"
	}
if (grepl("compile", names(data[2])) == TRUE) {
	data$Concern <- "Compile"
  data$Subject <- "M"
	}
if (grepl("compile_report", names(data[2])) == TRUE) {
	data$Concern <- "Compile"
	data$Subject <- "P"
	}
if (grepl("compile_report1", names(data[2])) == TRUE) {
	data$Concern <- "Compile"
	data$Subject <- "J"
	}
if (grepl("compile_report2", names(data[2])) == TRUE) {
	data$Concern <- "Compile"
	data$Subject <- "H"
	}
if (grepl("compile_report3", names(data[2])) == TRUE) {
	data$Concern <- "Compile"
	data$Subject <- "F"
	}
if (grepl("add_auction", names(data[2])) == TRUE) {
	data$Concern <- "Add Auction"
	data$Subject <- "B"
	}
if (grepl("add_auction1", names(data[2])) == TRUE) {
	data$Concern <- "Add Auction"
	data$Subject <- "F"
	}
if (grepl("add_auction2", names(data[2])) == TRUE) {
	data$Concern <- "Add Auction"
	data$Subject <- "H"
	}
if (grepl("add_auction3", names(data[2])) == TRUE) {
	data$Concern <- "Add Auction"
	data$Subject <- "K"
	}
if (grepl("add_auction4", names(data[2])) == TRUE) {
	data$Concern <- "Add Auction"
	data$Subject <- "O"
	}
if (grepl("add_auction5", names(data[2])) == TRUE) {
	data$Concern <- "Add Auction"
	data$Subject <- "R"
	}
if (grepl("add_snipe", names(data[2])) == TRUE) {
	data$Concern <- "Set Snipe"
  data$Subject <- "Q"
	}
if (grepl("do_you_wish_to_snipe", names(data[2])) == TRUE) {
	data$Concern <- "Set Snipe"
  data$Subject <- "K"
	}
if (grepl("set_snipe", names(data[2])) == TRUE) {
	data$Concern <- "Set Snipe"
  data$Subject <- "Q"
	}
if (grepl("snipe", names(data[2])) == TRUE) {
	data$Concern <- "Set Snipe"
	data$Subject <- "H"
	}
if (grepl("snipe1", names(data[2])) == TRUE) {
	data$Concern <- "Set Snipe"
	data$Subject <- "M"
	}
if (grepl("sniping", names(data[2])) == TRUE) {
	data$Concern <- "Set Snipe"
	data$Subject <- "A"
	}
if (grepl("save", names(data[2])) == TRUE) {
	data$Concern <- "Save Auctions"
	data$Subject <- "L"
	}
if (grepl("save_auctions", names(data[2])) == TRUE) {
	data$Concern <- "Save Auctions"
	data$Subject <- "I"
	}
if (grepl("save_auction", names(data[2])) == TRUE) {
	data$Concern <- "Save Auctions"
	data$Subject <- "E"
	}
if (grepl("save_auction1", names(data[2])) == TRUE) {
	data$Concern <- "Save Auctions"
	data$Subject <- "C"
	}
if (grepl("save_auction2", names(data[2])) == TRUE) {
	data$Concern <- "Save Auctions"
	data$Subject <- "Q"
	}
if (grepl("save_auction3", names(data[2])) == TRUE) {
  data$Concern <- "Save Auctions"
  data$Subject <- "I"
	}
if (grepl("find", names(data[2])) == TRUE) {
	data$Concern <- "Theaters"
	data$Subject <- "O"
	}
if (grepl("get_theater", names(data[2])) == TRUE) {
	data$Concern <- "Theaters"
	data$Subject <- "B"
	}
if (grepl("movie", names(data[2])) == TRUE) {
	data$Concern <- "Theaters"
	data$Subject <- "P"
	}
if (grepl("movie", names(data[2])) == TRUE) {
  data$Concern <- "Theaters"
	data$Subject <- "G"
	}
if (grepl("theater", names(data[2])) == TRUE) {
	data$Concern <- "Theaters"
	data$Subject <- "D"
	}
if (grepl("view_listing", names(data[2])) == TRUE) {
	data$Concern <- "Theaters"
	data$Subject <- "J"
	}
if (grepl("search", names(data[2])) == TRUE) {
	data$Concern <- "Search"
	data$Subject <- "I"
	}
if (grepl("search1", names(data[2])) == TRUE) {
	data$Concern <- "Search"
	data$Subject <- "M"
	}
if (grepl("search2", names(data[2])) == TRUE) {
	data$Concern <- "Search"
	data$Subject <- "A"
	}
if (grepl("search3", names(data[2])) == TRUE) {
	data$Concern <- "Search"
	data$Subject <- "L"
	}
if (grepl("search4", names(data[2])) == TRUE) {
	data$Concern <- "Search"
	data$Subject <- "R"
	}
if (grepl("search_file", names(data[2])) == TRUE) {
	data$Concern <- "Search"
	data$Subject <- "D"
	}
if (grepl("open_file", names(data[2])) == TRUE) {
	data$Concern <- "Play"
	data$Subject <- "F"
	}
if (grepl("play", names(data[2])) == TRUE) {
	data$Concern <- "Play"
	data$Subject <- "R"
	}
if (grepl("play_track", names(data[2])) == TRUE) {
	data$Concern <- "Play"
	data$Subject <- "N"
	}
if (grepl("play_file", names(data[2])) == TRUE) {
	data$Concern <- "Play"
	data$Subject <- "G"
	}
if (grepl("play_file1", names(data[2])) == TRUE) {
	data$Concern <- "Play"
	data$Subject <- "B"
	}
if (grepl("start", names(data[2])) == TRUE) {
	data$Concern <- "Play"
	data$Subject <- "L"
	}

#creates a single dataframe with all of the P/R/F/MAP data for each technique/query/concern combo		
prfdataframe<-rbind(prfdataframe, data.frame("Technique"=names(data[1]), "Concern"=data$Concern[1],
	"Query"=names(data[2]), "Subject"=data$Subject[1], "P"=data$P[1], "R"=data$R[1], "F"=data$F[1], "MAP"=data$MAP[1]))
	
}

#---------------------------------------------------RHINO DATASET LOOP
#---------------------------------------------------------------------

#empty data frame to store all prfmap data
prfdataframe2<-data.frame()

#loop to read in files from rhino set and formats the names of the columns (this is pretty ghetto but it works)
for (i in 1:length(files3)) {
	data3<-read.table(files3[i], sep="", nrows=thresh, col.names=c(sub(".marked_search.(.*)", "", 
		files3[i]), sub("(.*?)marked_search.", "", files3[i]), sub("(.*?)marked_search.", "", files3[i])))
	data4<-read.table(files4[i], header=F, skip=1, sep="", col.names=c(sub(".prf_results.(.*)", "", 
		files4[i]), sub("(.*?)prf_results.", "", files4[i]), "tp", "rank-rel", "p", "r", "f"))
	colnames(data3)[1] <- sub("(.*?)out_results_rhino.", "", colnames(data3)[1])
	colnames(data3)[2] <- sub(".[0-9].out", "", colnames(data3)[2])
	colnames(data3)[2] <- unlist(strsplit(colnames(data3)[2], ".", TRUE))[[1]]
	colnames(data3)[3] <- unlist(strsplit(colnames(data3)[3], colnames(data3)[2], TRUE))[[2]]
	colnames(data3)[3] <- sub(".", "", colnames(data3)[3])
	colnames(data3)[3] <- sub(".[0-9].out.(.*)", "", colnames(data3)[3])
	colnames(data4)[1] <- colnames(data3)[1]
	colnames(data4)[2] <- colnames(data3)[2]
	colnames(data4)[4] <- colnames(data3)[3]

#counters for P/R/F/MAP calculations
counter<-0
lastvalue<-0
rank<-0
map<-0
lastvalue<-0

#loops through data (dataframe) looking for asterisks, keeps track of how many occur within threshhold
for (i in 1:nrow(data3)) {
	if (pmatch("*", data3[2][i,], nomatch=FALSE, duplicates.ok=FALSE)==1) {
		counter<-counter + 1 
		}
}

#calculates MAP via prf files
for (i in 1:nrow(data4)){
	if (data4$tp[i] != lastvalue) {
		map<-map + ((data4$tp[i]-lastvalue)*data4$p[i])
		lastvalue<-data4$tp[i]
		rank<-rank + 1
		}
}

#adds P/R/F/MAP to the data dataframe
data3$P<-(counter/thresh)
	if (data3$P[1] == "NaN" || data3$P[1] == "Inf")
		data3$P<-0
data3$R<-(counter/data4$tp[length(data4$tp)])
	if (data3$R[1] == "NaN" || data3$R[1] == "Inf")
		data3$R<-0
data3$F<-(2*((data3$P[1] * data3$R[1])/(data3$P[1] + data3$R[1])))
	if (data3$F[1] == "NaN" || data3$F[1] == "Inf")
		data3$F<-0
data3$MAP<-(map/data4$tp[length(data4$tp)]) / 100
	if (data3$MAP[1] == "NaN" || data3$MAP[1] == "Inf")
		data3$MAP<-0	

#creates a single dataframe with all of the P/R/F/MAP data for each technique/query/concern combo		
prfdataframe2<-rbind(prfdataframe2, data.frame("Technique"=names(data3[1]), "Concern"=names(data3[3]), 
"Query"=names(data3)[2], "P"=data3$P[1], "R"=data3$R[1], "F"=data3$F[1], "MAP"=data3$MAP[1]))
	
}

#empty data frame for p/r/f/map stats
mapstats<-data.frame()
rhinomapstats<-data.frame()
pstats<-data.frame()
rhinopstats<-data.frame()
rstats<-data.frame()
rhinorstats<-data.frame()
fstats<-data.frame()
rhinofstats<-data.frame()

#calculates mean/std/var/median/quartiles for p/r/f/map AOC data 
for (i in c("both_swum_weighted", "sig_swum", "sig_bow_boolean", "sig_bow_tf", "sig_at", "both_bow_tf", "both_bow_boolean_weighted", "body_bow_boolean", 
	"body_swum", "both_bow_tf_weighted", "body_bow_tf")) {
		
		ci<-subset(prfdataframe, prfdataframe[1]==i)
		
		mapstats<-rbind(mapstats, data.frame("Technique"=i, "Mean"=mean(ci$MAP), "Median"=quantile(ci$MAP,.50), 
			"First.Quartile"=quantile(ci$MAP,.25), "Third.Quartile"=quantile(ci$MAP,.75),"STD"=sd(ci$MAP), 
			"Var"=var(ci$MAP), "Min"=min(ci$MAP), "Max"=max(ci$MAP), row.names=NULL))
		
		pstats<-rbind(pstats, data.frame("Technique"=i, "Mean"=mean(ci$P), "Median"=quantile(ci$P,.50),
			"First.Quartile"=quantile(ci$P,.25), "Third.Quartile"=quantile(ci$P,.75), "STD"=sd(ci$P),
			"Var"=var(ci$P), "Min"=min(ci$P), "Max"=max(ci$P), row.names=NULL))
			
		rstats<-rbind(rstats, data.frame("Technique"=i, "Mean"=mean(ci$R), "Median"=quantile(ci$R,.50),
			"First.Quartile"=quantile(ci$R,.25), "Third.Quartile"=quantile(ci$R,.75), "STD"=sd(ci$R),
			"Var"=var(ci$R), "Min"=min(ci$R), "Max"=max(ci$R), row.names=NULL))
			
		fstats<-rbind(fstats, data.frame("Technique"=i, "Mean"=mean(ci$F), "Median"=quantile(ci$F,.50),
			"First.Quartile"=quantile(ci$F,.25), "Third.Quartile"=quantile(ci$F,.75), "STD"=sd(ci$F),
			"Var"=var(ci$F), "Min"=min(ci$F), "Max"=max(ci$F), row.names=NULL))
}

#calculates mean/std/var/median/quartiles for p/r/f/map Rhino data
#for (i in c("body_bow", "body_bow_TF_NoSig", "body_swum", "bow", "bowBooleanNew", "bowTFNew", "sig_at", "sig_bow", "sig_lex", "sig_swum", "swum")) {
	
for (i in c("both_swum_weighted", "sig_swum", "sig_bow_boolean", "sig_bow_tf", "sig_at", "both_bow_tf", "both_bow_boolean_weighted", "body_bow_boolean", 
	"body_swum", "both_bow_tf_weighted", "body_bow_tf")) {
	
	ci<-subset(prfdataframe2, prfdataframe2[1]==i)
	
	rhinomapstats<-rbind(rhinomapstats, data.frame("Technique"=i, "Mean"=mean(ci$MAP), "Median"=quantile(ci$MAP,.50), 
			"First.Quartile"=quantile(ci$MAP,.25), "Third.Quartile"=quantile(ci$MAP,.75),"STD"=sd(ci$MAP), 
			"Var"=var(ci$MAP), "Min"=min(ci$MAP), "Max"=max(ci$MAP), row.names=NULL))
			
	rhinopstats<-rbind(rhinopstats, data.frame("Technique"=i, "Mean"=mean(ci$P), "Median"=quantile(ci$P,.50),
			"First.Quartile"=quantile(ci$P,.25), "Third.Quartile"=quantile(ci$P,.75), "STD"=sd(ci$P),
			"Var"=var(ci$P), "Min"=min(ci$P), "Max"=max(ci$P), row.names=NULL))
	
	rhinorstats<-rbind(rhinorstats, data.frame("Technique"=i, "Mean"=mean(ci$R), "Median"=quantile(ci$R,.50),
			"First.Quartile"=quantile(ci$R,.25), "Third.Quartile"=quantile(ci$R,.75), "STD"=sd(ci$R),
			"Var"=var(ci$R), "Min"=min(ci$R), "Max"=max(ci$R), row.names=NULL))
			
	rhinofstats<-rbind(rhinofstats, data.frame("Technique"=i, "Mean"=mean(ci$F), "Median"=quantile(ci$F,.50),
			"First.Quartile"=quantile(ci$F,.25), "Third.Quartile"=quantile(ci$F,.75), "STD"=sd(ci$F),
			"Var"=var(ci$F), "Min"=min(ci$F), "Max"=max(ci$F), row.names=NULL))
}

#making body/sig/weighted/swum/bowboolean/bowtf columns and making them all zero in prfdataframe and prfdataframe2
prfdataframe$Location <- "N/A"
prfdataframe$BodyScore <- "N/A"
prfdataframe$BodyType <- "N/A"
prfdataframe$SigScore <- "N/A"
prfdataframe$SigType <- "N/A"
prfdataframe$Weighted <- "N/A"
prfdataframe2$Location <- "N/A"
prfdataframe2$BodyScore <- "N/A"
prfdataframe2$BodyType <- "N/A"
prfdataframe2$SigScore <- "N/A"
prfdataframe2$SigType <- "N/A"
prfdataframe2$Weighted <- "N/A"

for (i in 1:nrow(prfdataframe)) {
	if (prfdataframe$Technique[i] == "body_bow_boolean") {
	prfdataframe$BodyScore[i] <- "bow" 
	prfdataframe$BodyType[i] <- "boolean" 
	prfdataframe$Location[i] <- "body"}
	
	if(prfdataframe$Technique[i] == "body_swum") {
	prfdataframe$BodyScore[i] <- "swum" 
	prfdataframe$BodyType[i] <- "normal" 
	prfdataframe$Location[i] <- "body" }
	
	if(prfdataframe$Technique[i] == "body_bow_tf") {
	prfdataframe$BodyScore[i] <- "bow" 
	prfdataframe$BodyType[i] <- "tf" 
	prfdataframe$Location[i] <- "body" }
	
	if(prfdataframe$Technique[i] == "sig_bow_tf") {
	prfdataframe$SigScore[i] <- "bow" 
	prfdataframe$SigType[i] <- "tf" 
	prfdataframe$Location[i] <- "sig" }
	
	if(prfdataframe$Technique[i] == "sig_bow_boolean") {
	prfdataframe$SigScore[i] <- "bow" 
	prfdataframe$SigType[i] <- "boolean"
	prfdataframe$Location[i] <- "sig" }
	
	if(prfdataframe$Technique[i] == "sig_swum") {
	prfdataframe$SigScore[i] <- "swum"
	prfdataframe$SigType[i] <- "normal"
	prfdataframe$Location[i] <- "sig" }
	
	if(prfdataframe$Technique[i] == "sig_at") {
	prfdataframe$SigScore[i] <- "swum" 
	prfdataframe$SigType[i] <- "at"
	prfdataframe$Location[i] <- "sig" }
	
	if (prfdataframe$Technique[i] == "both_swum_weighted") {
	prfdataframe$SigScore[i] <- "swum"
	prfdataframe$SigType[i] <- "normal"
	prfdataframe$BodyScore[i] <- "bow" 
	prfdataframe$BodyType[i] <- "tf"
	prfdataframe$Location[i] <- "both"
	prfdataframe$Weighted[i] <- "weighted" }	
	
	if (prfdataframe$Technique[i] == "both_bow_boolean_weighted") {
	prfdataframe$SigScore[i] <- "bow" 
	prfdataframe$SigType[i] <- "boolean"
	prfdataframe$BodyScore[i] <- "bow" 
	prfdataframe$BodyType[i] <- "boolean"	
	prfdataframe$Location[i] <- "both"
	prfdataframe$Weighted[i] <- "weighted" }
	
	if (prfdataframe$Technique[i] == "both_bow_tf") {
	prfdataframe$SigScore[i] <- "bow" 
	prfdataframe$SigType[i] <- "tf"
	prfdataframe$BodyScore[i] <- "bow" 
	prfdataframe$BodyType[i] <- "tf"
	prfdataframe$Location[i] <- "both" }
	
	if (prfdataframe$Technique[i] == "both_bow_tf_weighted") {
	prfdataframe$SigScore[i] <- "bow" 
	prfdataframe$SigType[i] <- "tf"
	prfdataframe$BodyScore[i] <- "bow" 
	prfdataframe$BodyType[i] <- "tf"
	prfdataframe$Location[i] <- "both"
	prfdataframe$Weighted[i] <- "weighted" }
}

for (i in 1:nrow(prfdataframe2)) {
	if (prfdataframe2$Technique[i] == "body_bow_boolean") {
	prfdataframe2$BodyScore[i] <- "bow" 
	prfdataframe2$BodyType[i] <- "boolean"
	prfdataframe2$Location[i] <- "body" }
	
	if(prfdataframe2$Technique[i] == "body_swum") {
	prfdataframe2$BodyScore[i] <- "swum" 
	prfdataframe2$BodyType[i] <- "normal"
	prfdataframe2$Location[i] <- "body" }
	
	if(prfdataframe2$Technique[i] == "body_bow_tf") {
	prfdataframe2$BodyScore[i] <- "bow" 
	prfdataframe2$BodyType[i] <- "tf"
	prfdataframe2$Location[i] <- "body" }
	
	if(prfdataframe2$Technique[i] == "sig_bow_tf") {
	prfdataframe2$SigScore[i] <- "bow" 
	prfdataframe2$SigType[i] <- "tf"
	prfdataframe2$Location[i] <- "sig" }
	
	if(prfdataframe2$Technique[i] == "sig_bow_boolean") {
	prfdataframe2$SigScore[i] <- "bow" 
	prfdataframe2$SigType[i] <- "boolean"
	prfdataframe2$Location[i] <- "sig" }
	
	if(prfdataframe2$Technique[i] == "sig_swum") {
	prfdataframe2$SigScore[i] <- "swum"
	prfdataframe2$SigType[i] <- "normal"
	prfdataframe2$Location[i] <- "sig" }
	
	if(prfdataframe2$Technique[i] == "sig_at") {
	prfdataframe2$SigScore[i] <- "swum" 
	prfdataframe2$SigType[i] <- "at"
	prfdataframe2$Location[i] <- "sig" }
	
	if (prfdataframe2$Technique[i] == "both_swum_weighted") {
	prfdataframe2$SigScore[i] <- "swum"
	prfdataframe2$SigType[i] <- "normal"
	prfdataframe2$BodyScore[i] <- "bow" 
	prfdataframe2$BodyType[i] <- "tf"
	prfdataframe2$Location[i] <- "both"
	prfdataframe2$Weighted[i] <- "weighted" }	
	
	if (prfdataframe2$Technique[i] == "both_bow_boolean_weighted") {
	prfdataframe2$SigScore[i] <- "bow" 
	prfdataframe2$SigType[i] <- "boolean"
	prfdataframe2$BodyScore[i] <- "bow" 
	prfdataframe2$BodyType[i] <- "boolean"	
	prfdataframe2$Location[i] <- "both"
	prfdataframe2$Weighted[i] <- "weighted" }
	
	if (prfdataframe2$Technique[i] == "both_bow_tf") {
	prfdataframe2$SigScore[i] <- "bow" 
	prfdataframe2$SigType[i] <- "tf"
	prfdataframe2$BodyScore[i] <- "bow" 
	prfdataframe2$BodyType[i] <- "tf"
	prfdataframe2$Location[i] <- "both" }
	
	if (prfdataframe2$Technique[i] == "both_bow_tf_weighted") {
	prfdataframe2$SigScore[i] <- "bow" 
	prfdataframe2$SigType[i] <- "tf"
	prfdataframe2$BodyScore[i] <- "bow" 
	prfdataframe2$BodyType[i] <- "tf"
	prfdataframe2$Weighted[i] <- "weighted"
	prfdataframe2$Location[i] <- "both" }
}

#prints out summary statistics for aoc and rhino data and saves them to files
sink(paste(outputdir, "/prfdataframe.txt", sep=""))
options(max.print = 99999999, width=999)
print("-----------prfdataframe - aoc set-------------")
prfdataframe
sink()

sink(paste(outputdir, "/prfdataframe2.txt", sep=""))
options(max.print = 99999999, width=999) 
print("-----------prfdataframe2 - rhino set-------------")
prfdataframe2
sink()

sink(paste(outputdir, "/precisionStatsAOC.txt", sep=""))
print("-----------precision AOC summary-------------")
pstats
sink()

sink(paste(outputdir, "/precisionStatsRhino.txt", sep=""))
print("-----------precision Rhino summary-------------")
rhinopstats
sink()

sink(paste(outputdir, "/recallStatsAOC.txt", sep=""))
print("------------recall AOC summary--------------")
rstats
sink()

sink(paste(outputdir, "/recallStatsRhino.txt", sep=""))
print("------------recall Rhino summary--------------")
rhinorstats
sink()

sink(paste(outputdir, "/fmeasureStatsAOC.txt", sep=""))
print("-----------f-measure AOC summary-------------")
fstats
sink()

sink(paste(outputdir, "/fmeasureStatsRhino.txt", sep=""))
print("-----------f-measure Rhino summary-------------")
rhinofstats
sink()

sink(paste(outputdir, "/mapStatsAOC.txt", sep=""))
print("-------------map AOC summary---------------")
mapstats
sink()

sink(paste(outputdir, "/mapStatsRhino.txt", sep=""))
print("-------------map Rhino summary---------------")
rhinomapstats
sink()

sink(paste(outputdir, "/TukeyResultsAOC.txt", sep=""))
print("-------------tukeyhsd/aov AOC--------------")
analysis<-aov(formula=MAP~Technique, data=prfdataframe)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)

tukeyframe<-data.frame(tukey$Technique)

sigresults<-data.frame()
for (i in 1:length(tukeyframe[,1]))
if (tukeyframe$p.adj[i] < .05)
	sigresults<-rbind(sigresults, tukeyframe[i,])

print("-------------tukey significant results-------------")
print(sigresults)
sink()

sink(paste(outputdir, "/TukeyResultsRhino.txt", sep=""))
print("-------------tukeyhsd/aov Rhino--------------")
analysis2<-aov(formula=MAP~Technique, data=prfdataframe2)
summary(analysis2)
tukey2<-TukeyHSD(analysis2)
print(tukey2)

tukeyframe2<-data.frame(tukey2$Technique)

sigresults2<-data.frame()
for (i in 1:length(tukeyframe2[,1]))
if (tukeyframe2$p.adj[i] < .05)
	sigresults2<-rbind(sigresults2, tukeyframe2[i,])

print("-------------tukey significant results-------------")
print(sigresults2)
sink()

pdf(paste(outputdir, "/RhinoBoxplotWithTukeyGroups.pdf", sep=""))
multcompBoxplot(MAP~Technique, data=prfdataframe2,
      horizontal=FALSE, compFn="TukeyHSD",
      sortFn="mean", decreasing=FALSE,
      plotList=list(
      	 boxplot=list(fig=c(.35, 1, 0, 1), las=2, ylab="MAP", main="MAP~Technique w/ Tukey Groups", col="lightgrey"),
         multcompTs=list(fig=c(0.02, 0.15, 0, 1)),
         multcompLetters=list(fig=c(0.40, 0.97, 0.03, 0.98), fontsize=0,fontface="bold")))
#		 means<-tapply(prfdataframe2[[7]], prfdataframe2[,1], mean)
#		 points(means, col="red")
dev.off()            

pdf(paste(outputdir, "/AOCBoxplotWithTukeyGroups.pdf", sep=""))
multcompBoxplot(MAP~Technique, data=prfdataframe,
      horizontal=FALSE, compFn="TukeyHSD",
      sortFn="mean", decreasing=FALSE,
      plotList=list(
      	 boxplot=list(fig=c(.35, 1, 0, 1), las=2, ylab="MAP", main="MAP~Technique w/ Tukey Groups", col="lightgrey"),
         multcompTs=list(fig=c(0.02, 0.15, 0, 1)),
         multcompLetters=list(fig=c(0.40, 0.97, 0.03, 0.98), fontsize=0,fontface="bold")))
#    	 means<-tapply(prfdataframe[[7]], prfdataframe[,1], mean)
#		 points(means, col="red")
dev.off()      

#----------------------------------------------AOC BOXPLOTS
#----------------------------------------------------------

#creates directory to store boxplots
dir.create(paste(outputdir, "/BoxplotsAOC",sep=""), showWarnings = FALSE)

#loop to create boxplots (map/p/r/f vs technique and concern)

for (i in 5:8) {
	for (x in 1:4) {
		prfdataframe[,x]<-with(prfdataframe, reorder(prfdataframe[,x], prfdataframe[[i]], mean))
		name<-paste(outputdir, "/BoxplotsAOC/", names(prfdataframe[i]), "_", 
			names(prfdataframe[x]), "boxplot.pdf", sep="")
		main<-paste(names(prfdataframe[i]),"~", names(prfdataframe[x]), sep="")
		pdf(name)
		par(mar=c(13,4,2,1))
		boxplot(prfdataframe[[i]]~prfdataframe[[x]], data=prfdataframe, ylab=names(prfdataframe[i]), col="lightgray", 
			las=2, main=main, cex.axis=1)
		means<-tapply(prfdataframe[[i]], prfdataframe[,x], mean)
		points(means, col="red")	
		dev.off()
		}
	}
	
#creates directory to store boxplots	
dir.create(paste(outputdir, "/BoxplotsByConcernAOC", sep=""), showWarnings = FALSE)

#loop to create boxplots (map/p/r/f vs technique by one concern at a time)
for (c in c("Textfield", "Compile", "Add Auction", "Set Snipe", "Save Auctions", "Theaters", "Search", "Play")) {
	concernframe<-subset(prfdataframe, prfdataframe$Concern == c)
	for (i in 5:8) {
		concernframe[,1]<-with(concernframe, reorder(concernframe[,1], concernframe[[i]], mean))
		name<-paste(outputdir, "/BoxplotsByConcernAOC/", names(concernframe[i]), "_", 
			names(concernframe[1]), "_", c,"_","boxplot.pdf", sep="")
		main<-paste(names(concernframe[i]),"~", names(concernframe[1]), "(", c, ")", sep="")
		pdf(name)
		par(mar=c(13,4,2,1))
		boxplot(concernframe[[i]]~concernframe[[1]], data=concernframe, ylab=names(concernframe[i]),
			col="lightgray", las=2, main=main, cex.axis=1)
		means<-tapply(concernframe[[i]], concernframe[,1], mean)
		points(means, col="red")
		dev.off()	
		}
	}
	
#direcory for paired boxplots
dir.create(paste(outputdir, "/PairedBoxplotsAOC", sep=""), showWarnings = FALSE)	
	
#Sig-Only box plot
sigframe<-subset(prfdataframe, prfdataframe$Technique=="sig_swum" | prfdataframe$Technique == "sig_at" 
	| prfdataframe$Technique == "sig_bow_tf" | prfdataframe$Technique == "sig_bow_boolean")
	for (i in 5:8) {
		sigframe[,1]<-with(sigframe, reorder(sigframe[,1], sigframe[[i]], mean))
		name<-paste(outputdir, "/PairedBoxplotsAOC/", "SigOnly_", names(sigframe[i]), "_", 
			names(sigframe[1]), "_","boxplot.pdf", sep="")
		main<-paste(names(sigframe[i]),"~", names(sigframe[1]), sep="")
		pdf(name)
		par(mar=c(13,4,2,1))
		boxplot(sigframe[[i]]~droplevels(sigframe[[1]]), data=sigframe, ylab=names(sigframe[i]),
			col="lightgray", las=2, main=main, cex.axis=1)
		means<-tapply(sigframe[[i]], sigframe[,1], mean)
		points(means, col="red")
		dev.off()	
		}
		
#Body-Only box plot
bodyframe<-subset(prfdataframe, prfdataframe$Technique=="body_swum" | prfdataframe$Technique == "body_bow_boolean" 
	| prfdataframe$Technique == "body_bow_tf")
	for (i in 5:8) {
		bodyframe[,1]<-with(bodyframe, reorder(bodyframe[,1], bodyframe[[i]], mean))
		name<-paste(outputdir, "/PairedBoxplotsAOC/", "BodyOnly_", names(bodyframe[i]), "_", 
			names(bodyframe[1]), "_","boxplot.pdf", sep="")
		main<-paste(names(bodyframe[i]),"~", names(bodyframe[1]), sep="")
		pdf(name)
		par(mar=c(13,4,2,1))
		boxplot(bodyframe[[i]]~droplevels(bodyframe[[1]]), data=bodyframe, ylab=names(bodyframe[i]),
			col="lightgray", las=2, main=main, cex.axis=1)
		means<-tapply(bodyframe[[i]], bodyframe[,1], mean)
		points(means, col="red")
		dev.off()	
		}

#Both box plot
bothframe<-subset(prfdataframe, prfdataframe$Technique=="both_swum_weighted" | prfdataframe$Technique == "both_bow_boolean_weighted" 
	| prfdataframe$Technique == "both_bow_tf" | prfdataframe$Technique == "both_bow_tf_weigthed")
	for (i in 5:8) {
		bothframe[,1]<-with(bothframe, reorder(bothframe[,1], bothframe[[i]], mean))
		name<-paste(outputdir, "/PairedBoxplotsAOC/", "Both_", names(bothframe[i]), "_", 
			names(bothframe[1]), "_","boxplot.pdf", sep="")
		main<-paste(names(bothframe[i]),"~", names(bothframe[1]), sep="")
		pdf(name)
		par(mar=c(13,4,2,1))
		boxplot(bothframe[[i]]~droplevels(bothframe[[1]]), data=bothframe, ylab=names(bothframe[i]),
			col="lightgray", las=2, main=main, cex.axis=1)
		means<-tapply(bothframe[[i]], bothframe[,1], mean)
		points(means, col="red")
		dev.off()	
		}

#----------------------------------------------RHINO BOXPLOTS
#------------------------------------------------------------

dir.create(paste(outputdir, "/BoxplotsRhino",sep=""), showWarnings = FALSE)

#loop to create boxplots (map/p/r/f vs technique and concern)

for (i in 4:7) {
	for (x in 1:2) {
		prfdataframe2[,x]<-with(prfdataframe2, reorder(prfdataframe2[,x], prfdataframe2[[i]], mean))
		name<-paste(outputdir, "/BoxplotsRhino/", names(prfdataframe2[i]), "_", 
			names(prfdataframe2[x]), "boxplot.pdf", sep="")
		main<-paste(names(prfdataframe2[i]),"~", names(prfdataframe2[x]), sep="")
		pdf(name)
		par(mar=c(13,4,2,1))
		boxplot(prfdataframe2[[i]]~prfdataframe2[[x]], data=prfdataframe2, ylab=names(prfdataframe2[i]), col="lightgray", 
			las=2, main=main, cex.axis=1)
		means<-tapply(prfdataframe2[[i]], prfdataframe2[,x], mean)
		points(means, col="red")	
		dev.off()
		}
	}
	
#creates directory to store boxplots	
dir.create(paste(outputdir, "/BoxplotsByConcernRhino", sep=""), showWarnings = FALSE)

#loop to create boxplots (map/p/r/f vs technique by one concern at a time)
for (c in levels(prfdataframe2$Concern)) {
	concernframe2<-subset(prfdataframe2, prfdataframe2$Concern == c)
	for (i in 4:7) {
		concernframe2[,1]<-with(concernframe2, reorder(concernframe2[,1], concernframe2[[i]], mean))
		name<-paste(outputdir, "/BoxplotsByConcernRhino/", names(concernframe2[i]), "_", 
			names(concernframe2[1]), "_", c,"_","boxplot.pdf", sep="")
		main<-paste(names(concernframe2[i]),"~", names(concernframe2[1]), "(", c, ")", sep="")
		pdf(name)
		par(mar=c(13,4,2,1))
		boxplot(concernframe2[[i]]~concernframe2[[1]], data=concernframe2, ylab=names(concernframe2[i]),
			col="lightgray", las=2, main=main, cex.axis=1)
		means<-tapply(concernframe2[[i]], concernframe2[,1], mean)
		points(means, col="red")
		dev.off()	
		}
	}
	
#direcory for paired boxplots
dir.create(paste(outputdir, "/PairedBoxplotsRhino", sep=""), showWarnings = FALSE)	
	
#Sig-Only box plot
sigframe2<-subset(prfdataframe2, prfdataframe2$Technique=="sig_swum" | prfdataframe2$Technique == "sig_at" 
	| prfdataframe2$Technique == "sig_bow_tf" | prfdataframe2$Technique == "sig_bow_boolean")
	for (i in 4:7) {
		sigframe2[,1]<-with(sigframe2, reorder(sigframe2[,1], sigframe2[[i]], mean))
		name<-paste(outputdir, "/PairedBoxplotsRhino/", "SigOnly_", names(sigframe2[i]), "_", 
			names(sigframe2[1]), "_","boxplot.pdf", sep="")
		main<-paste(names(sigframe2[i]),"~", names(sigframe2[1]), sep="")
		pdf(name)
		par(mar=c(13,4,2,1))
		boxplot(sigframe2[[i]]~droplevels(sigframe2[[1]]), data=sigframe2, ylab=names(sigframe2[i]),
			col="lightgray", las=2, main=main, cex.axis=1)
		means<-tapply(sigframe2[[i]], sigframe2[,1], mean)
		points(means, col="red")
		dev.off()	
		}
		
#Body-Only box plot
bodyframe2<-subset(prfdataframe2, prfdataframe2$Technique=="body_swum" | prfdataframe2$Technique == "body_bow_boolean" 
	| prfdataframe2$Technique == "body_bow_tf")
	for (i in 4:7) {
		bodyframe2[,1]<-with(bodyframe2, reorder(bodyframe2[,1], bodyframe2[[i]], mean))
		name<-paste(outputdir, "/PairedBoxplotsRhino/", "BodyOnly_", names(bodyframe2[i]), "_", 
			names(bodyframe2[1]), "_","boxplot.pdf", sep="")
		main<-paste(names(bodyframe2[i]),"~", names(bodyframe2[1]), sep="")
		pdf(name)
		par(mar=c(13,4,2,1))
		boxplot(bodyframe2[[i]]~droplevels(bodyframe2[[1]]), data=bodyframe2, ylab=names(bodyframe2[i]),
			col="lightgray", las=2, main=main, cex.axis=1)
		means<-tapply(bodyframe2[[i]], bodyframe2[,1], mean)
		points(means, col="red")
		dev.off()	
		}

#Both box plot
bothframe2<-subset(prfdataframe2, prfdataframe2$Technique=="both_swum_weighted" | prfdataframe2$Technique == "both_bow_boolean_weighted" 
	| prfdataframe2$Technique == "both_bow_tf" | prfdataframe2$Technique == "both_bow_tf_weighted")
	for (i in 4:7) {
		bothframe2[,1]<-with(bothframe2, reorder(bothframe2[,1], bothframe2[[i]], mean))
		name<-paste(outputdir, "/PairedBoxplotsRhino/", "Both_", names(bothframe2[i]), "_", 
			names(bothframe2[1]), "_","boxplot.pdf", sep="")
		main<-paste(names(bothframe2[i]),"~", names(bothframe2[1]), sep="")
		pdf(name)
		par(mar=c(13,4,2,1))
		boxplot(bothframe2[[i]]~droplevels(bothframe2[[1]]), data=bothframe2, ylab=names(bothframe2[i]),
			col="lightgray", las=2, main=main, cex.axis=1)
		means<-tapply(bothframe2[[i]], bothframe2[,1], mean)
		points(means, col="red")
		dev.off()	
		}

dir.create(paste(outputdir, "/DistributionsAOC",sep=""), showWarnings = FALSE)
dir.create(paste(outputdir, "/DistributionsAOC/BodyOnly",sep=""), showWarnings = FALSE)
dir.create(paste(outputdir, "/DistributionsAOC/SigOnly",sep=""), showWarnings = FALSE)
dir.create(paste(outputdir, "/DistributionsAOC/BodySig",sep=""), showWarnings = FALSE)
dir.create(paste(outputdir, "/DistributionsRhino",sep=""), showWarnings = FALSE)
dir.create(paste(outputdir, "/DistributionsRhino/BodyOnly",sep=""), showWarnings = FALSE)
dir.create(paste(outputdir, "/DistributionsRhino/SigOnly",sep=""), showWarnings = FALSE)
dir.create(paste(outputdir, "/DistributionsRhino/BodySig",sep=""), showWarnings = FALSE)

pdf(paste(outputdir, "/DistributionsAOC/all_F.pdf", sep=""))
hist(prfdataframe$F, main="ALL_F", xlab="F")
dev.off()

pdf(paste(outputdir, "/DistributionsAOC/all_R.pdf", sep=""))
hist(prfdataframe$R, main="ALL_R", xlab="R")
dev.off()

pdf(paste(outputdir, "/DistributionsAOC/all_P.pdf", sep=""))
hist(prfdataframe$P, main="ALL_P", xlab="P")
dev.off()

pdf(paste(outputdir, "/DistributionsAOC/all_MAP.pdf", sep=""))
hist(prfdataframe$MAP, main="ALL_MAP", xlab="MAP")
dev.off()

pdf(paste(outputdir, "/DistributionsRhino/all_F.pdf", sep=""))
hist(prfdataframe$F, main="ALL_F", xlab="F")
dev.off()

pdf(paste(outputdir, "/DistributionsRhino/all_R.pdf", sep=""))
hist(prfdataframe$R, main="ALL_R", xlab="R")
dev.off()

pdf(paste(outputdir, "/DistributionsRhino/all_P.pdf", sep=""))
hist(prfdataframe$P, main="ALL_P", xlab="P")
dev.off()

pdf(paste(outputdir, "/DistributionsRhino/all_MAP.pdf", sep=""))
hist(prfdataframe$MAP, main="ALL_MAP", xlab="MAP")
dev.off()

bodyframe<-subset(prfdataframe, prfdataframe$Location=="body")

pdf(paste(outputdir, "/DistributionsAOC/BodyOnly/F.pdf", sep=""))
hist(bodyframe$F, main="BodyOnly_F", xlab="F")
dev.off()

pdf(paste(outputdir, "/DistributionsAOC/BodyOnly/R.pdf", sep=""))
hist(bodyframe$R, main="BodyOnly_R", xlab="R")
dev.off()

pdf(paste(outputdir, "/DistributionsAOC/BodyOnly/P.pdf", sep=""))
hist(bodyframe$P, main="BodyOnly_P", xlab="P")
dev.off()

pdf(paste(outputdir, "/DistributionsAOC/BodyOnly/MAP.pdf", sep=""))
hist(bodyframe$MAP, main="BodyOnly_MAP", xlab="MAP")
dev.off()

sigframe<-subset(prfdataframe, prfdataframe$Location=="sig")

pdf(paste(outputdir, "/DistributionsAOC/SigOnly/F.pdf", sep=""))
hist(sigframe$F, main="SigOnly_F", xlab="F")
dev.off()

pdf(paste(outputdir, "/DistributionsAOC/SigOnly/R.pdf", sep=""))
hist(sigframe$R, main="SigOnly_R", xlab="R")
dev.off()

pdf(paste(outputdir, "/DistributionsAOC/SigOnly/P.pdf", sep=""))
hist(sigframe$P, main="SigOnly_R", xlab="R")
dev.off()

pdf(paste(outputdir, "/DistributionsAOC/SigOnly/MAP.pdf", sep=""))
hist(sigframe$MAP, main="SigOnly_MAP", xlab="MAP")
dev.off()

sigbodyframe<-subset(prfdataframe, prfdataframe$Location=="both")

pdf(paste(outputdir, "/DistributionsAOC/BodySig/F.pdf", sep=""))
hist(sigbodyframe$F, main="BodySig_F", xlab="F")
dev.off()

pdf(paste(outputdir, "/DistributionsAOC/BodySig/R.pdf", sep=""))
hist(sigbodyframe$R, main="BodySig_R", xlab="R")
dev.off()

pdf(paste(outputdir, "/DistributionsAOC/BodySig/P.pdf", sep=""))
hist(sigbodyframe$P, main="BodySig_P", xlab="P")
dev.off()

pdf(paste(outputdir, "/DistributionsAOC/BodySig/MAP.pdf", sep=""))
hist(sigbodyframe$MAP, main="BodySig_MAP", xlab="MAP")
dev.off()
#

bodyframe2<-subset(prfdataframe2, prfdataframe2$Location=="body")

pdf(paste(outputdir, "/DistributionsRhino/BodyOnly/F.pdf", sep=""))
hist(bodyframe2$F, main="BodyOnly_F", xlab="F")
dev.off()

pdf(paste(outputdir, "/DistributionsRhino/BodyOnly/R.pdf", sep=""))
hist(bodyframe2$R, main="BodyOnly_R", xlab="R")
dev.off()

pdf(paste(outputdir, "/DistributionsRhino/BodyOnly/P.pdf", sep=""))
hist(bodyframe2$P, main="BodyOnly_P", xlab="P")
dev.off()

pdf(paste(outputdir, "/DistributionsRhino/BodyOnly/MAP.pdf", sep=""))
hist(bodyframe2$MAP, main="BodyOnly_MAP", xlab="MAP")
dev.off()

sigframe2<-subset(prfdataframe2, prfdataframe2$Location=="sig")

pdf(paste(outputdir, "/DistributionsRhino/SigOnly/F.pdf", sep=""))
hist(sigframe$F, main="SigOnly_F", xlab="F")
dev.off()

pdf(paste(outputdir, "/DistributionsRhino/SigOnly/R.pdf", sep=""))
hist(sigframe2$R, main="SigOnly_R", xlab="R")
dev.off()

pdf(paste(outputdir, "/DistributionsRhino/SigOnly/P.pdf", sep=""))
hist(sigframe2$P, main="SigOnly_P", xlab="P")
dev.off()

pdf(paste(outputdir, "/DistributionsRhino/SigOnly/MAP.pdf", sep=""))
hist(sigframe2$MAP, main="SigOnly_MAP", xlab="MAP")
dev.off()

sigbodyframe2<-subset(prfdataframe2, prfdataframe2$Location=="both")

pdf(paste(outputdir, "/DistributionsRhino/BodySig/F.pdf", sep=""))
hist(sigbodyframe2$F, main="BodySig_F", xlab="F")
dev.off()

pdf(paste(outputdir, "/DistributionsRhino/BodySig/R.pdf", sep=""))
hist(sigbodyframe2$R, main="BodySig_R", xlab="R")
dev.off()

pdf(paste(outputdir, "/DistributionsRhino/BodySig/P.pdf", sep=""))
hist(sigbodyframe2$P, main="BodySig_P", xlab="P")
dev.off()

pdf(paste(outputdir, "/DistributionsRhino/BodySig/MAP.pdf", sep=""))
hist(sigbodyframe2$MAP, main="BodySig_MAP", xlab="MAP")
dev.off()


#AOC Data
#
#
#
#
#comparison 1
#
#
#
dir.create(paste(outputdir, "/SigOnlyScoresComparisonAOC",sep=""), showWarnings = FALSE)
sigscoreSwum<-subset(prfdataframe, prfdataframe$SigScore=="swum" & prfdataframe$BodyScore=="N/A")
pdf(paste(outputdir, "/SigOnlyScoresComparisonAOC/SigSwum_vs_SigAt.pdf", sep=""))
		boxplot(sigscoreSwum$MAP~droplevels(sigscoreSwum$Technique),ylab="MAP",
			col="lightgray", main="Sig_swum vs sig_at, AOC", cex.axis=1)
dev.off()

sink(paste(outputdir, "/SigOnlyScoresComparisonAOC/SigSwum_vs_SigAt_TukeyResults.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=sigscoreSwum)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

sigscoreBow<-subset(prfdataframe, prfdataframe$SigScore=="bow" & prfdataframe$BodyScore=="N/A")
pdf(paste(outputdir, "/SigOnlyScoresComparisonAOC/SigBowBoolean_vs_SigBowTF.pdf", sep=""))
		boxplot(sigscoreBow$MAP~droplevels(sigscoreBow$Technique),ylab="MAP",
			col="lightgray", main="Sig_bow_boolean vs sig_bow_tf, AOC", cex.axis=1)
dev.off()

sink(paste(outputdir, "/SigOnlyScoresComparisonAOC/SigBowBoolean_vs_SigBowTF.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=sigscoreBow)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

sigswumsigbowtf<-subset(prfdataframe, prfdataframe$Technique=="sig_swum" | prfdataframe$Technique=="sig_bow_tf")
pdf(paste(outputdir, "/SigOnlyScoresComparisonAOC/SigSwum_vs_SigBowTF.pdf", sep=""))
boxplot(sigswumsigbowtf$MAP~droplevels(sigswumsigbowtf$Technique),ylab="MAP",
			col="lightgray", main="Sig_swum vs sig_bow_tf, AOC", cex.axis=1)
dev.off()

sink(paste(outputdir, "/SigOnlyScoresComparisonAOC/SigSwum_vs_SigBowTF.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=sigswumsigbowtf)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()
#
#
#
#comparison 2
#
#
#
dir.create(paste(outputdir, "/BodyOnlyScoresComparisonAOC",sep=""), showWarnings = FALSE)
bodyscoreBow<-subset(prfdataframe, prfdataframe$BodyScore=="bow" & prfdataframe$SigScore=="N/A")
pdf(paste(outputdir, "/BodyOnlyScoresComparisonAOC/BodyBowBoolean_vs_BodyBowTF.pdf", sep=""))
		boxplot(bodyscoreBow$MAP~droplevels(bodyscoreBow$Technique),ylab="MAP",
			col="lightgray", main="Body_bow_boolean vs body_bow_tf, AOC", cex.axis=1)
dev.off()

sink(paste(outputdir, "/BodyOnlyScoresComparisonAOC/BodyBowBoolean_vs_BodyBowTF.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bodyscoreBow)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

bodybowtfbodyswum<-subset(prfdataframe, prfdataframe$Technique=="body_swum" | prfdataframe$Technique=="body_bow_tf")
pdf(paste(outputdir, "/BodyOnlyScoresComparisonAOC/BodySwum_vs_BodyBowTF.pdf", sep=""))
		boxplot(bodybowtfbodyswum$MAP~droplevels(bodybowtfbodyswum$Technique),ylab="MAP",
			col="lightgray", main="Body_swum vs body_bow_tf, AOC", cex.axis=1)
dev.off()

sink(paste(outputdir, "/BodyOnlyScoresComparisonAOC/BodySwum_vs_BodyBowTF.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bodybowtfbodyswum)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()
#
#
#
#
#comparison 3
#
#
#
#
dir.create(paste(outputdir, "/BothOnlyScoresComparisonAOC",sep=""), showWarnings = FALSE)

bothbowbooltf<-subset(prfdataframe, prfdataframe$Technique=="both_bow_boolean_weighted" | prfdataframe$Technique=="both_bow_tf_weighted")
pdf(paste(outputdir, "/BothOnlyScoresComparisonAOC/BothBow_boolean_W_vs_tf_W.pdf", sep=""))
		boxplot(bothbowbooltf$MAP~droplevels(bothbowbooltf$Technique),ylab="MAP",
			col="lightgray", main="Both_bow_boolean_W vs both_bow_tf_W, AOC", cex.axis=1)
dev.off()

sink(paste(outputdir, "/BothOnlyScoresComparisonAOC/BothBow_boolean_W_vs_tf_W.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bothbowbooltf)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

bothbowboolwtf<-subset(prfdataframe, prfdataframe$Technique=="both_bow_boolean_weighted" | prfdataframe$Technique=="both_bow_tf")
pdf(paste(outputdir, "/BothOnlyScoresComparisonAOC/BothBow_boolean_W_vs_tf.pdf", sep=""))
		boxplot(bothbowbooltf$MAP~droplevels(bothbowboolwtf$Technique),ylab="MAP",
			col="lightgray", main="Both_bow_boolean_W vs both_bow_tf, AOC", cex.axis=1)
dev.off()

sink(paste(outputdir, "/BothOnlyScoresComparisonAOC/BothBow_boolean_W_vs_tf.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bothbowboolwtf)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

bothbowtf<-subset(prfdataframe, prfdataframe$Technique=="both_bow_tf_weighted" | prfdataframe$Technique=="both_bow_tf")
pdf(paste(outputdir, "/BothOnlyScoresComparisonAOC/BothBow_tf_W_vs_tf.pdf", sep=""))
boxplot(bothbowtf$MAP~droplevels(bothbowtf$Technique),ylab="MAP",
			col="lightgray", main="Both_bow_tf_W vs both_bow_tf, AOC", cex.axis=1)
dev.off()

sink(paste(outputdir, "/BothOnlyScoresComparisonAOC/BothBow_tf_W_vs_tf.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bothbowtf)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

bothbowtfswum<-subset(prfdataframe, prfdataframe$Technique=="both_bow_tf_weighted" | prfdataframe$Technique=="both_swum_weighted")
pdf(paste(outputdir, "/BothOnlyScoresComparisonAOC/BothBow_tf_W_vs_swum_W.pdf", sep=""))
boxplot(bothbowtfswum$MAP~droplevels(bothbowtfswum$Technique),ylab="MAP",
			col="lightgray", main="Both_bow_tf_W vs both_swum_W, AOC", cex.axis=1)
dev.off()

sink(paste(outputdir, "/BothOnlyScoresComparisonAOC/BothBow_tf_W_vs_swum_W.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bothbowtfswum)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

#RHINO data
#
#
#
#
#comparison 1
#
#
#

dir.create(paste(outputdir, "/SigOnlyScoresComparisonRhino",sep=""), showWarnings = FALSE)
sigscoreSwum2<-subset(prfdataframe2, prfdataframe2$SigScore=="swum" & prfdataframe2$BodyScore=="N/A")
pdf(paste(outputdir, "/SigOnlyScoresComparisonRhino/SigSwum_vs_SigAt.pdf", sep=""))
boxplot(sigscoreSwum2$MAP~droplevels(sigscoreSwum2$Technique),ylab="MAP",
			col="lightgray", main="Sig_swum vs sig_at, Rhino", cex.axis=1)
dev.off()

sink(paste(outputdir, "/SigOnlyScoresComparisonRhino/SigSwum_vs_SigAt_TukeyResults.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=sigscoreSwum2)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

sigscoreBow2<-subset(prfdataframe2, prfdataframe2$SigScore=="bow" & prfdataframe2$BodyScore=="N/A")
pdf(paste(outputdir, "/SigOnlyScoresComparisonRhino/SigBowBoolean_vs_SigBowTF.pdf", sep=""))
boxplot(sigscoreBow2$MAP~droplevels(sigscoreBow2$Technique),ylab="MAP",
			col="lightgray", main="Sig_bow_boolean vs sig_bow_tf, Rhino", cex.axis=1)
dev.off()

sink(paste(outputdir, "/SigOnlyScoresComparisonRhino/SigBowBoolean_vs_SigBowTF.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=sigscoreBow2)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

sigswumsigbowbol2<-subset(prfdataframe2, prfdataframe2$Technique=="sig_swum" | prfdataframe2$Technique=="sig_bow_boolean")
pdf(paste(outputdir, "/SigOnlyScoresComparisonRhino/SigSwum_vs_SigBowBoolean.pdf", sep=""))
boxplot(sigswumsigbowbol2$MAP~droplevels(sigswumsigbowbol2$Technique),ylab="MAP",
			col="lightgray", main="Sig_swum vs sig_bow_boolean, Rhino", cex.axis=1)
dev.off()

sink(paste(outputdir, "/SigOnlyScoresComparisonRhino/SigSwum_vs_SigBowBoolean.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=sigswumsigbowbol2)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()
#
#
#
#comparison 2
#
#
#
dir.create(paste(outputdir, "/BodyOnlyScoresComparisonRhino",sep=""), showWarnings = FALSE)
bodyscoreBow2<-subset(prfdataframe2, prfdataframe2$BodyScore=="bow" & prfdataframe2$SigScore=="N/A")
pdf(paste(outputdir, "/BodyOnlyScoresComparisonRhino/BodyBowBoolean_vs_BodyBowTF.pdf", sep=""))
boxplot(bodyscoreBow2$MAP~droplevels(bodyscoreBow2$Technique),ylab="MAP",
			col="lightgray", main="Body_bow_boolean vs body_bow_tf, Rhino", cex.axis=1)
dev.off()

sink(paste(outputdir, "/BodyOnlyScoresComparisonRhino/BodyBowBoolean_vs_BodyBowTF.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bodyscoreBow2)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

bodybowtfbodyswum2<-subset(prfdataframe2, prfdataframe2$Technique=="body_swum" | prfdataframe2$Technique=="body_bow_tf")
pdf(paste(outputdir, "/BodyOnlyScoresComparisonRhino/BodySwum_vs_BodyBowTF.pdf", sep=""))
boxplot(bodybowtfbodyswum2$MAP~droplevels(bodybowtfbodyswum2$Technique),ylab="MAP",
			col="lightgray", main="Body_swum vs body_bow_tf, Rhino", cex.axis=1)
dev.off()

sink(paste(outputdir, "/BodyOnlyScoresComparisonRhino/BodySwum_vs_BodyBowTF.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bodybowtfbodyswum2)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

#
#
#
#comparison 3

dir.create(paste(outputdir, "/BothOnlyScoresComparisonRhino",sep=""), showWarnings = FALSE)
bothbowbooltf2<-subset(prfdataframe2, prfdataframe2$Technique=="both_bow_boolean_weighted" | prfdataframe2$Technique=="both_bow_tf_weighted")
pdf(paste(outputdir, "/BothOnlyScoresComparisonRhino/BothBow_boolean_W_vs_tf_W.pdf", sep=""))
boxplot(bothbowbooltf2$MAP~droplevels(bothbowbooltf2$Technique),ylab="MAP",
			col="lightgray", main="Both_bow_boolean_W vs both_bow_tf_W, Rhino", cex.axis=1)
dev.off()
  
sink(paste(outputdir, "/BothOnlyScoresComparisonRhino/BothBow_boolean_W_vs_tf_W.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bothbowbooltf2)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

bothbowboolwtf2<-subset(prfdataframe2, prfdataframe2$Technique=="both_bow_boolean_weighted" | prfdataframe2$Technique=="both_bow_tf")
pdf(paste(outputdir, "/BothOnlyScoresComparisonRhino/BothBow_boolean_W_vs_tf.pdf", sep=""))
boxplot(bothbowbooltf2$MAP~droplevels(bothbowboolwtf2$Technique),ylab="MAP",
			col="lightgray", main="Both_bow_boolean_W vs both_bow_tf, Rhino", cex.axis=1)
dev.off()
  
sink(paste(outputdir, "/BothOnlyScoresComparisonRhino/BothBow_boolean_W_vs_tf.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bothbowboolwtf2)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()
  
bothbowtf2<-subset(prfdataframe2, prfdataframe2$Technique=="both_bow_tf_weighted" | prfdataframe2$Technique=="both_bow_tf")
pdf(paste(outputdir, "/BothOnlyScoresComparisonRhino/BothBow_tf_W_vs_tf.pdf", sep=""))
boxplot(bothbowtf2$MAP~droplevels(bothbowtf2$Technique),ylab="MAP",
			col="lightgray", main="Both_bow_tf_W vs both_bow_tf, Rhino", cex.axis=1)
dev.off()
  
sink(paste(outputdir, "/BothOnlyScoresComparisonRhino/BothBow_tf_W_vs_tf.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bothbowtf2)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

bothbowbolswum2<-subset(prfdataframe2, prfdataframe2$Technique=="both_bow_boolean_weighted" | prfdataframe2$Technique=="both_swum_weighted")
pdf(paste(outputdir, "/BothOnlyScoresComparisonRhino/BothBow_boolean_W_vs_swum_W.pdf", sep=""))
boxplot(bothbowbolswum2$MAP~droplevels(bothbowbolswum2$Technique),ylab="MAP",
			col="lightgray", main="Both_bow_boolean_W vs both_swum_W, Rhino", cex.axis=1)
dev.off()
  
sink(paste(outputdir, "/BothOnlyScoresComparisonRhino/BothBow_boolean_W_vs_swum_W.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bothbowbolswum2)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

#testing2<-subset(prfdataframe2, prfdataframe2$Technique=="both_swum_weighted" | prfdataframe2$Technique=="sig_swum" | prfdataframe2$Technique=="body_bow_tf")
#boxplot(testing2$MAP~droplevels(testing2$Technique))

#
#
#
#
# Reading in the data to split up the RHINO data set between action and non-action
# oriented queries. All action oriented queiries are stored in action.queries, while
# all (total) queries are stored in all.queries
#
# The following code will read in action.queries and make a data frame with the information.
# It will then divide the prfdataframe2 (primary rhino data frame) into two subsections:
# action and non-action oriented queries. We will then be able to make plots with only action oriented
# queries and only non-action oriented queries.
#
#
#
#
#
actionframe<-data.frame()
actionframe<-read.table(paste(outputdir, "/verbs/action.queries", sep=""), sep="\t", na.strings="NA", fill=T, col.names=c("Concern", "first", "second", "third"))

prfdataframe2$Action <- "No"

for (i in 1:nrow(prfdataframe2)) {
	for (j in 1:nrow(actionframe)) {
		if (as.character(prfdataframe2$Concern[[i]])==as.character(actionframe$Concern[[j]])) {
			if (as.character(prfdataframe2$Query[[i]])==as.character(actionframe$first[[j]]) | as.character(prfdataframe2$Query[[i]])==as.character(actionframe$second[[j]]) | as.character(prfdataframe2$Query[[i]])==as.character(actionframe$third[[j]])) {
				prfdataframe2$Action[[i]] <- "Yes"
			}
		}
	}
}
#
#
#
# subsets
#
#
prfdataframe2action<-data.frame()
prfdataframe2noaction<-data.frame()
prfdataframe2action<-subset(prfdataframe2, prfdataframe2$Action == "Yes")
prfdataframe2noaction<-subset(prfdataframe2, prfdataframe2$Action == "No")
#
#
#
#RHINO ACTION data
#
#
#
#
#comparison 1
#
#
#

dir.create(paste(outputdir, "/SigOnlyScoresComparisonRhino/action",sep=""), showWarnings = FALSE)
sigscoreSwum2action<-subset(prfdataframe2action, prfdataframe2action$SigScore=="swum" & prfdataframe2action$BodyScore=="N/A")
pdf(paste(outputdir, "/SigOnlyScoresComparisonRhino/action/SigSwum_vs_SigAt.pdf", sep=""))
boxplot(sigscoreSwum2action$MAP~droplevels(sigscoreSwum2action$Technique),ylab="MAP",
			col="lightgray", main="Sig_swum vs Sig_at, Rhino/Action", cex.axis=1)
dev.off()

sink(paste(outputdir, "/SigOnlyScoresComparisonRhino/action/SigSwum_vs_SigAt_TukeyResults.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=sigscoreSwum2action)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

sigscoreBow2action<-subset(prfdataframe2action, prfdataframe2action$SigScore=="bow" & prfdataframe2action$BodyScore=="N/A")
pdf(paste(outputdir, "/SigOnlyScoresComparisonRhino/action/SigBowBoolean_vs_SigBowTF.pdf", sep=""))
boxplot(sigscoreBow2action$MAP~droplevels(sigscoreBow2action$Technique),ylab="MAP",
			col="lightgray", main="Sig_bow_boolean vs Sig_bow_TF, Rhino/Action", cex.axis=1)
dev.off()

sink(paste(outputdir, "/SigOnlyScoresComparisonRhino/action/SigBowBoolean_vs_SigBowTF.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=sigscoreBow2action)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

sigswumsigbowbol2action<-subset(prfdataframe2action, prfdataframe2action$Technique=="sig_swum" | prfdataframe2action$Technique=="sig_bow_boolean")
pdf(paste(outputdir, "/SigOnlyScoresComparisonRhino/action/SigSwum_vs_SigBowBoolean.pdf", sep=""))
boxplot(sigswumsigbowbol2action$MAP~droplevels(sigswumsigbowbol2action$Technique),ylab="MAP",
			col="lightgray", main="Sig_swum vs Sig_bow_boolean, Rhino/Action", cex.axis=1)
dev.off()

sink(paste(outputdir, "/SigOnlyScoresComparisonRhino/action/SigSwum_vs_SigBowBoolean.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=sigswumsigbowbol2action)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()
#
#
#
#comparison 2
#
#
#
dir.create(paste(outputdir, "/BodyOnlyScoresComparisonRhino/action",sep=""), showWarnings = FALSE)
bodyscoreBow2action<-subset(prfdataframe2action, prfdataframe2action$BodyScore=="bow" & prfdataframe2action$SigScore=="N/A")
pdf(paste(outputdir, "/BodyOnlyScoresComparisonRhino/action/BodyBowBoolean_vs_BodyBowTF.pdf", sep=""))
boxplot(bodyscoreBow2action$MAP~droplevels(bodyscoreBow2action$Technique),ylab="MAP",
			col="lightgray", main="Body_bow_boolean vs Body_bow_TF, Rhino/Action", cex.axis=1)
dev.off()

sink(paste(outputdir, "/BodyOnlyScoresComparisonRhino/action/BodyBowBoolean_vs_BodyBowTF.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bodyscoreBow2action)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

bodybowtfbodyswum2action<-subset(prfdataframe2action, prfdataframe2action$Technique=="body_swum" | prfdataframe2action$Technique=="body_bow_tf")
pdf(paste(outputdir, "/BodyOnlyScoresComparisonRhino/action/BodySwum_vs_BodyBowTF.pdf", sep=""))
boxplot(bodybowtfbodyswum2action$MAP~droplevels(bodybowtfbodyswum2action$Technique),ylab="MAP",
			col="lightgray", main="Body_swum vs Body_bow_TF, Rhino/Action", cex.axis=1)
dev.off()

sink(paste(outputdir, "/BodyOnlyScoresComparisonRhino/action/BodySwum_vs_BodyBowTF.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bodybowtfbodyswum2action)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()


#
#
#
#comparison 3

dir.create(paste(outputdir, "/BothOnlyScoresComparisonRhino/action",sep=""), showWarnings = FALSE)
bothbowbooltf2action<-subset(prfdataframe2action, prfdataframe2action$Technique=="both_bow_boolean_weighted" | prfdataframe2action$Technique=="both_bow_tf_weighted")
pdf(paste(outputdir, "/BothOnlyScoresComparisonRhino/action/BothBow_boolean_W_vs_tf_W.pdf", sep=""))
boxplot(bothbowbooltf2action$MAP~droplevels(bothbowbooltf2action$Technique),ylab="MAP",
			col="lightgray", main="Both_bow_boolean_W vs Both_bow_tf_W, Rhino/Action", cex.axis=1)
dev.off()
  
sink(paste(outputdir, "/BothOnlyScoresComparisonRhino/action/BothBow_boolean_W_vs_tf_W.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bothbowbooltf2action)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

bothbowboolwtf2action<-subset(prfdataframe2action, prfdataframe2action$Technique=="both_bow_boolean_weighted" | prfdataframe2action$Technique=="both_bow_tf")
pdf(paste(outputdir, "/BothOnlyScoresComparisonRhino/action/BothBow_boolean_W_vs_tf.pdf", sep=""))
boxplot(bothbowbooltf2action$MAP~droplevels(bothbowboolwtf2action$Technique),ylab="MAP",
			col="lightgray", main="Both_bow_boolean_W vs Both_bow_tf, Rhino/Action", cex.axis=1)
dev.off()
  
sink(paste(outputdir, "/BothOnlyScoresComparisonRhino/action/BothBow_boolean_W_vs_tf.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bothbowboolwtf2action)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()
  
bothbowtf2action<-subset(prfdataframe2action, prfdataframe2action$Technique=="both_bow_tf_weighted" | prfdataframe2action$Technique=="both_bow_tf")
pdf(paste(outputdir, "/BothOnlyScoresComparisonRhino/action/BothBow_tf_W_vs_tf.pdf", sep=""))
boxplot(bothbowtf2action$MAP~droplevels(bothbowtf2action$Technique),ylab="MAP",
			col="lightgray", main="Both_bow_tf_W vs Both_bow_tf, Rhino/Action", cex.axis=1)
dev.off()
  
sink(paste(outputdir, "/BothOnlyScoresComparisonRhino/action/BothBow_tf_W_vs_tf.txt", sep=""))
print("-------------tukeyhsd/aov AOC--------------")
analysis<-aov(formula=MAP~Technique, data=bothbowtf2action)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

bothbowbolswum2action<-subset(prfdataframe2action, prfdataframe2action$Technique=="both_bow_boolean_weighted" | prfdataframe2action$Technique=="both_swum_weighted")
pdf(paste(outputdir, "/BothOnlyScoresComparisonRhino/action/BothBow_boolean_W_vs_swum_W.pdf", sep=""))
boxplot(bothbowbolswum2action$MAP~droplevels(bothbowbolswum2action$Technique),ylab="MAP",
			col="lightgray", main="Both_bow_boolean_W vs Both_swum_W, Rhino/Action", cex.axis=1)
dev.off()
  
sink(paste(outputdir, "/BothOnlyScoresComparisonRhino/action/BothBow_boolean_W_vs_swum_W.txt", sep=""))
print("-------------tukeyhsd/aov AOC--------------")
analysis<-aov(formula=MAP~Technique, data=bothbowbolswum2action)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()
#
#
#
#
#
#
#RHINO NOACTION data
#
#
#
#
#comparison 1
#
#
#

dir.create(paste(outputdir, "/SigOnlyScoresComparisonRhino/noaction",sep=""), showWarnings = FALSE)
sigscoreSwum2noaction<-subset(prfdataframe2noaction, prfdataframe2noaction$SigScore=="swum" & prfdataframe2noaction$BodyScore=="N/A")
pdf(paste(outputdir, "/SigOnlyScoresComparisonRhino/noaction/SigSwum_vs_SigAt.pdf", sep=""))
boxplot(sigscoreSwum2noaction$MAP~droplevels(sigscoreSwum2noaction$Technique),ylab="MAP",
			col="lightgray", main="Sig_swum vs Sig_at, Rhino/No-Action", cex.axis=1)
dev.off()

sink(paste(outputdir, "/SigOnlyScoresComparisonRhino/noaction/SigSwum_vs_SigAt_TukeyResults.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=sigscoreSwum2noaction)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

sigscoreBow2noaction<-subset(prfdataframe2noaction, prfdataframe2noaction$SigScore=="bow" & prfdataframe2noaction$BodyScore=="N/A")
pdf(paste(outputdir, "/SigOnlyScoresComparisonRhino/noaction/SigBowBoolean_vs_SigBowTF.pdf", sep=""))
boxplot(sigscoreBow2noaction$MAP~droplevels(sigscoreBow2noaction$Technique),ylab="MAP",
			col="lightgray", main="Sig_bow_boolean vs Sig_bow_tf, Rhino/No-Action", cex.axis=1)
dev.off()

sink(paste(outputdir, "/SigOnlyScoresComparisonRhino/noaction/SigBowBoolean_vs_SigBowTF.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=sigscoreBow2noaction)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

sigswumsigbowbol2noaction<-subset(prfdataframe2noaction, prfdataframe2noaction$Technique=="sig_swum" | prfdataframe2noaction$Technique=="sig_bow_boolean")
pdf(paste(outputdir, "/SigOnlyScoresComparisonRhino/noaction/SigSwum_vs_SigBowBoolean.pdf", sep=""))
boxplot(sigswumsigbowbol2noaction$MAP~droplevels(sigswumsigbowbol2noaction$Technique),ylab="MAP",
			col="lightgray", main="Sig_swum vs Sig_bow_boolean, Rhino/No-Action", cex.axis=1)
dev.off()

sink(paste(outputdir, "/SigOnlyScoresComparisonRhino/noaction/SigSwum_vs_SigBowBoolean.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=sigswumsigbowbol2noaction)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()
#
#
#
#comparison 2
#
#
#
dir.create(paste(outputdir, "/BodyOnlyScoresComparisonRhino/noaction",sep=""), showWarnings = FALSE)
bodyscoreBow2noaction<-subset(prfdataframe2noaction, prfdataframe2noaction$BodyScore=="bow" & prfdataframe2noaction$SigScore=="N/A")
pdf(paste(outputdir, "/BodyOnlyScoresComparisonRhino/noaction/BodyBowBoolean_vs_BodyBowTF.pdf", sep=""))
boxplot(bodyscoreBow2noaction$MAP~droplevels(bodyscoreBow2noaction$Technique),ylab="MAP",
			col="lightgray", main="Body_bow_boolean vs Body_bow_tf, Rhino/No-Action", cex.axis=1)
dev.off()

sink(paste(outputdir, "/BodyOnlyScoresComparisonRhino/noaction/BodyBowBoolean_vs_BodyBowTF.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bodyscoreBow2noaction)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

bodybowtfbodyswum2noaction<-subset(prfdataframe2noaction, prfdataframe2noaction$Technique=="body_swum" | prfdataframe2noaction$Technique=="body_bow_tf")
pdf(paste(outputdir, "/BodyOnlyScoresComparisonRhino/noaction/BodySwum_vs_BodyBowTF.pdf", sep=""))
boxplot(bodybowtfbodyswum2noaction$MAP~droplevels(bodybowtfbodyswum2noaction$Technique),ylab="MAP",
			col="lightgray", main="Body_swum vs Body_bow_tf, Rhino/No-Action", cex.axis=1)
dev.off()

sink(paste(outputdir, "/BodyOnlyScoresComparisonRhino/noaction/BodySwum_vs_BodyBowTF.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bodybowtfbodyswum2noaction)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

#
#
#
#comparison 3

dir.create(paste(outputdir, "/BothOnlyScoresComparisonRhino/noaction",sep=""), showWarnings = FALSE)
bothbowbooltf2noaction<-subset(prfdataframe2noaction, prfdataframe2noaction$Technique=="both_bow_boolean_weighted" | prfdataframe2noaction$Technique=="both_bow_tf_weighted")
pdf(paste(outputdir, "/BothOnlyScoresComparisonRhino/noaction/BothBow_boolean_W_vs_tf_W.pdf", sep=""))
boxplot(bothbowbooltf2noaction$MAP~droplevels(bothbowbooltf2noaction$Technique),ylab="MAP",
			col="lightgray", main="Both_bow_boolean_W vs Both_bow_tf_W, Rhino/No-Action", cex.axis=1)
dev.off()
  
sink(paste(outputdir, "/BothOnlyScoresComparisonRhino/noaction/BothBow_boolean_W_vs_tf_W.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bothbowbooltf2noaction)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

bothbowboolwtf2noaction<-subset(prfdataframe2noaction, prfdataframe2noaction$Technique=="both_bow_boolean_weighted" | prfdataframe2noaction$Technique=="both_bow_tf")
pdf(paste(outputdir, "/BothOnlyScoresComparisonRhino/noaction/BothBow_boolean_W_vs_tf.pdf", sep=""))
boxplot(bothbowbooltf2noaction$MAP~droplevels(bothbowboolwtf2noaction$Technique),ylab="MAP",
			col="lightgray", main="Both_bow_boolean_W vs Both_bow_tf, Rhino/No-Action", cex.axis=1)
dev.off()
  
sink(paste(outputdir, "/BothOnlyScoresComparisonRhino/noaction/BothBow_boolean_W_vs_tf.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bothbowboolwtf2noaction)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()
  
bothbowtf2noaction<-subset(prfdataframe2noaction, prfdataframe2noaction$Technique=="both_bow_tf_weighted" | prfdataframe2noaction$Technique=="both_bow_tf")
pdf(paste(outputdir, "/BothOnlyScoresComparisonRhino/noaction/BothBow_tf_W_vs_tf.pdf", sep=""))
boxplot(bothbowtf2noaction$MAP~droplevels(bothbowtf2noaction$Technique),ylab="MAP",
			col="lightgray", main="Both_bow_tf_W vs Both_bow_tf, Rhino/No-Action", cex.axis=1)
dev.off()
  
sink(paste(outputdir, "/BothOnlyScoresComparisonRhino/noaction/BothBow_tf_W_vs_tf.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bothbowtf2noaction)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()

bothbowbolswum2noaction<-subset(prfdataframe2noaction, prfdataframe2noaction$Technique=="both_bow_boolean_weighted" | prfdataframe2noaction$Technique=="both_swum_weighted")
pdf(paste(outputdir, "/BothOnlyScoresComparisonRhino/noaction/BothBow_boolean_W_vs_swum_W.pdf", sep=""))
boxplot(bothbowbolswum2noaction$MAP~droplevels(bothbowbolswum2noaction$Technique),ylab="MAP",
			col="lightgray", main="Both_bow_boolean_W vs Both_swum_W, Rhino/No-Action", cex.axis=1)
dev.off()
  
sink(paste(outputdir, "/BothOnlyScoresComparisonRhino/noaction/BothBow_boolean_W_vs_swum_W.txt", sep=""))
analysis<-aov(formula=MAP~Technique, data=bothbowbolswum2noaction)
summary(analysis)
tukey<-TukeyHSD(analysis)
print(tukey)
sink()
#
#
#

pdf(paste(outputdir, "/BothOnlyScoresComparisonRhino/RhinoBoxplotWithTukeyGroupsACTION.pdf", sep=""))
multcompBoxplot(MAP~Technique, data=prfdataframe2action,
      horizontal=FALSE, compFn="TukeyHSD",
      sortFn="mean", decreasing=FALSE,
      plotList=list(
      	 boxplot=list(fig=c(.35, 1, 0, 1), las=2, ylab="MAP", main="MAP~Technique w/ Tukey Groups Action", col="lightgrey"),
         multcompTs=list(fig=c(0.02, 0.15, 0, 1)),
         multcompLetters=list(fig=c(0.40, 0.97, 0.03, 0.98), fontsize=0,fontface="bold")))
dev.off() 

pdf(paste(outputdir, "/BothOnlyScoresComparisonRhino/RhinoBoxplotWithTukeyGroupsNOACTION.pdf", sep=""))
multcompBoxplot(MAP~Technique, data=prfdataframe2noaction,
      horizontal=FALSE, compFn="TukeyHSD",
      sortFn="mean", decreasing=FALSE,
      plotList=list(
      	 boxplot=list(fig=c(.35, 1, 0, 1), las=2, ylab="MAP", main="MAP~Technique w/ Tukey Groups No Action", col="lightgrey"),
         multcompTs=list(fig=c(0.02, 0.15, 0, 1)),
         multcompLetters=list(fig=c(0.40, 0.97, 0.03, 0.98), fontsize=0,fontface="bold")))
dev.off() 


sink(paste(outputdir, "/BothOnlyScoresComparisonRhino/RhinoAnalysis.txt", sep=""))
for (i in 1:length(levels(prfdataframe2$Technique))) {
	samplezz<-subset(prfdataframe2, prfdataframe2$Technique==levels(prfdataframe2$Technique)[i])
	analysis<-aov(formula=MAP~Action, data=samplezz)
	summary(analysis)
	tukey<-TukeyHSD(analysis)
	print(levels(prfdataframe2$Technique)[i])
	print(tukey)
}
sink()

#
# Looking at the differences between body_swum and both_swum_W
# within the AOC and rhino data set. Looking at the abs of the top 5 and
# bottom 5 results.
#
# AOC

body_swum_rhino<-subset(prfdataframe2, prfdataframe2$Technique=="body_swum")
both_swum_W_rhino<-subset(prfdataframe2, prfdataframe2$Technique=="both_swum_weighted")
attach(body_swum_rhino)
attach(both_swum_W_rhino)
body_swum_rhino_sorted<-body_swum_rhino[order(Query),]
both_swum_W_rhino_sorted<-both_swum_W_rhino[order(Query),]

both_swum_W_AOC<-subset(prfdataframe, prfdataframe$Technique=="both_swum_weighted")
body_swum_AOC<-subset(prfdataframe, prfdataframe$Technique=="body_swum")
attach(both_swum_W_AOC)
attach(body_swum_AOC)
both_swum_W_AOC_sorted<-both_swum_W_AOC[order(Query),]
body_swum_AOC_sorted<-body_swum_AOC[order(Query),]

AOCdifference<-data.frame("Both_swum_W"=both_swum_W_AOC_sorted$Query, "MAP1"=both_swum_W_AOC_sorted$MAP, "body_swum"=body_swum_AOC_sorted$Query, 
	"MAP2"=body_swum_AOC_sorted$MAP)
	
AOCdifference$Difference=AOCdifference$MAP1-AOCdifference$MAP2
attach(AOCdifference)
AOCdifference<-AOCdifference[order(Difference),]


RHINOdifference<-data.frame("Both_swum_W"=both_swum_W_rhino_sorted$Concern, "MAP1"=both_swum_W_rhino_sorted$MAP,
	"body_swum"=body_swum_rhino_sorted$Concern, "MAP2"=body_swum_rhino_sorted$MAP, "Subject"=body_swum_rhino_sorted$Query)
	
RHINOdifference$Difference=RHINOdifference$MAP1-RHINOdifference$MAP2
attach(RHINOdifference)
RHINOdifference<-RHINOdifference[order(Difference),]

AOCdifference<-AOCdifference[c("Difference","Both_swum_W", "MAP1", "body_swum", "MAP2")]
RHINOdifference<-RHINOdifference[c("Difference", "Subject","Both_swum_W", "MAP1", "body_swum", "MAP2")]

SUBSETTING TO GET TOP/BOTTOM DATA
toprhinoframe<-subset(prfdataframe2, prfdataframe2$Concern=="15.9.5.32_._setMinutes" & prfdataframe2$Query=="subject1")

bottomrhinoframe<-subset(prfdataframe2, prfdataframe2$Concern=="8.6.2.2_._..Put.." & prfdataframe2$Query=="subject5")

aoctop<-subset(prfdataframe, prfdataframe$Query=="view_listing")