#A truck load of code snippets #################################################################################################################################################################################### ####################################################################################################################################################################################19:31
Actions
#######################R environment things##################

#Make sure the R packages are in the correct place at the start of the script
.libPaths()
.libPaths("S:\\Gastroenterology\\Seb\\R\\R-3.3.1\\library")
.libPaths()


#A truck load of code snippets

####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
################################################ System tasks ######################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################


#To call sed and awk for general cleaning up tasks
system("sed -i '' 's/\"//g' /Users/sebastianzeki/Desktop/SequencingScripts/bedtools/bedtools2-master/CohortComparisons/AkagiT_ChromosomeAbnormEA.bed")
#To call bedtools and other command line type things use, make sure the command is in the command path and then use (system("<command>"))
system("bedtools etc etc blablabla")
# How to run perl and pass it a filename as a variable for it work on. Sed as well
#How to reference things in system(paste()) from the R script included
system(paste("perl -p -i -e 's/ /\t/g'",filename1))


#Write directly to change an external config file by inserting a filename
system(paste("perl -i -p -e's{file1=\"\"}{\"/User/me/ct.txt\"}g' /Users/sebastianzeki/Desktop/tbb.conf"))
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
############################################################ Subset   ##############################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################


#Subsetting data: #Can subset by column by referencing the column name or number or whatever.
newdata <- subset(total , total[,5] == 'g' & total[,8] == 'd')
#Subset on basis of colmeans
totalB_lowDiv2[,colMeans(totalB_lowDiv2)>30]
#Get rid of rows with NA in a specific column
rainbow2<-subset(rainbow,!(is.na(rainbow["CopyNumber"])))
#Get rid of non adjacent columns
resultOrderProportion[-c(2:6,8)]
#Dont need to subset necessarily....
resultReidBE[resultReidBE$ValuesReidBE<2,]
#Subset by non adjacent columns
WholeSVNoTrans2 <- data.frame(WholeSVNoTrans[2],WholeSVNoTrans[4])
#delete a column from a dataframe
data$col <- NULL
#Only select certain columns from one dataframe to make another one
df3 <- data.frame(df2[3:4],df2[9])
SelfComp <- data.frame(Self["PatientID"],Self["Findings"],Self["EndoscopDiagn"],Self["Dx"])


#delete a row from r
df = df[-1,]
#move columns to certain positions without getting rid of them
df <- df[ ,c(1,4:6,2:3)]
#COunt the number of rows
nrow(SVProp)
# Show unique repeat entries
unique(df[duplicated(df),])


#Get rid of all duplicates entirely
df[!(duplicated(df) | duplicated(df, fromLast = TRUE)), ]


#rounding up in r
df2$Means<-round(df2[3])


#Switch row to column
data.frame(t(df))
#How to create a dataframe
n=c(2,3,4)
s=c("aa","bb","dd")
b=c(TRUE,FALSE,FALSE)
df=data.frame(n,s,b)
#Remove first row in r
df = df[-1,]
#Remove columns based on dataframe list of values
df1[setdiff(names(df1),df2$ID)]
#omit rows which are na
df[!(is.na(df$start_pc)
     #or
final[complete.cases(final),]
#Replace specific value
df[df==""]<-NA
#Replace na with zero in R
d[is.na(d)] <- 0


#Create random numbers in a column in a dataframe
CN_PerSampleOrder$number<-sample(100, size = nrow(CN_PerSampleOrder), replace = TRUE)


#Remove NAs in a certain column in r
OCForCorr[complete.cases(OCForCorr$UICC.stage)]


#Subtract one dataframe from another in R
library(dplyr)
setdiff(BigDF, SmallDF)
want<-anti_join(org_df,sub_df)


#See if elements of one dataframe are in another dataframe (or not in another dataframe)


Rule3NoRepeats<-data.frame(EndoSubsetOnSurveilPreDOIHospNum[which(!EndoSubsetOnSurveilPreDOIHospNum$x %in% EndoSubsetOnSurveilPostDOIHospNum$x),])




#Proportion by subset in R
DF<-ddply(DF,.(category1,category2),transform,prop=number/sum(number))

#Summarise by group:
grp=group_by(MyColonDataADRByInstrumentGrp,Instrument)
summarise(grp,mean=mean(PropAdenomas),sd=sd(PropAdenomas))
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
############################################################ reshape data and dplyr ##############################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
library(reshape)
mdata <- melt(resultOrderProportion, id=c("Names","DivergenceOfRpts"))


#How to group rows according to whatever you want and then perform something on them
library(dplyr)
GroupedTableProportion <- RptCountsAll %>% group_by(Rptname = substr(Rptname, 1, 4)) %>% summarise(freq = sum(freq))
#How to group rows according to whatever you want and then perform something on them (group_by(dataframe,columnInDataFrame))
bySex <- group_by(OCCAMSdbTumCNOAC,Sex)
(sumSex <- summarize(bySex,count=n()))
#Combine columns of different length
library(plyr)
combined <- rbind.fill(mtcars[c("mpg", "wt")], mtcars[c("wt", "cyl")])


#Reshaping data with tidyr
library(dplyr)
library(tidyr)


long_data =
  res_matrix %>%
  as.data.frame %>%
  mutate(rowname = rownames(.) ) %>%
  gather(variable, value, AH_026C:WG_019C)


matches =
  long_data %>%
  filter(rowname == variable) %>%
  select(rowname, control_value = value)


long_result =
  long_data %>%
  left_join(matches) %>%
  mutate(ratio = value / control_value)


wide_result =
  long_result %>%
  select(-value, -control_value) %>%
  spread(variable, ratio)




#group results


library(dplyr)
sumdf <- yourdf %>%
  group_by(ID) %>%
  summarise_each(funs(sum))


#Merge the same rows using aggregate
total2<-aggregate(. ~ Rptname, data = total, FUN = sum)
#Average the duplicates
TTypedAllControlTBBX<-aggregate(TTypedAllControlTBB$CNI,by=list(name=TTypedAllControlTBB$Sample,etc1=TTypedAllControlTBB$TissueType,etc2=TTypedAllControlTBB$Extraction),data=TTypedAllControlTBB,FUN=mean)




#Convert table to matrix
tr<-as.data.frame.matrix(table(df3))


#Get minumum of a group
NAOGCName_Num_CStage_MStage<-NAOGCName_Num_CStage_MStage %>%
  slice(which.min(VisitDate))


#split-apply-combine problems
#split a dataframe up according to a row qith categories in it ######SPLIT
PClst<-split(MyColonData,MyColonData$IndicationsFroExamination)
names(PClst)<-paste0('df',gsub("\\s+","",names(PClst)))
#Gives the basic output of the number for each indication
numPC<-data.frame(t(lapply(PClst,nrow))) ###############APPLY
#THis will give you a list of dataframes which you can name
#THen you can refer to the name by using ########REFER TO INDIVIDUAL dataframe from the list
View(PClst[["dfUrgency"]])

#Iterate through a list and print each one out as a separate spreadsheet

library(knitr)


a=c("boo","yaka","shaaa")
b=c("shut","yer","maaaf")
c=c("23","22","5345")
df<-data.frame(a,b,c)


d=c("bofo","yakfa","shaafa")
e=c("shfut","yefr","maafaf")
f=c("23f","22f","5f345")
df1<-data.frame(d,e,f)


g=c("bood","yakad","shaaad")
h=c("shutd","yerd","maaafd")
i=c("23d","22d","5345d")
df2<-data.frame(g,h,i)




ldf<-list(df,df1,df2)
ldf <- mget(ls(pattern = "^df"))
names(ldf)
ReportOp<-function(n) {
  #Note you pass the names of the dataframes in the list to the function and then you extract the dataframe from the list
  this_is_a_name <- n;
  print(this_is_a_name)
  this_is_my_data <- ldf[[n]]
  paste0(this_is_a_name,"Yes")
  write.table(this_is_my_data,paste0("/Users/sebastianzeki/Desktop/",this_is_a_name,".txt"))

}
lapply(names(ldf), ReportOp)



######################################## ######################################## ########################################
######################################## ######################################## ########################################
######################################## ######################################## ########################################
######################################## Row comparisons ########################################
######################################## ######################################## ########################################
######################################## ######################################## ########################################
######################################## ######################################## ########################################
#To get comparison with adjacent rows
Upstage<-EndoSubset %>%
  group_by(HospNum_Id) %>%
  mutate(ind = EVENT=="nothing" & lead(EVENT)=="EMR") %>%
  slice(sort(c(which(ind),which(ind)+1)))

#To get the minimum of something in a group
NAOGCName_Num_CStage_MStage<-NAOGCName_Num_CStage_MStage %>%
slice(which.min(VisitDate>DOI))





####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
############################################################ String searches  ##############################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################


##Find nd replace text
counts2$chr <- gsub("chr", "",counts2$chr)


# How to use grep to subset data
irisSubset <- mydataWhole[grep("L1M.*", mydataWhole$Rptname), ]
#Find and replace with a regular expression in a column- this one inserts a 'per' at the beginning of the line.
newdataOrder[1] = sub("^","per",newdataOrder[1])




#Select rows where does not match the grep
RepeatAlusSequences3 <- as.data.frame(RepatAlusSequences[grep("^>chr(.*)", RepatAlusSequences$V1, invert=TRUE), ])
#Or as follows where he new column is called strip and the original is called chrom
#Split the string based on a pattern and put into another dataframe
library(stringr)
RepeatAlusSequencesdfMatch[c('Sequence', 'new_split_col')] <-  do.call(rbind,lapply(str_split(RepeatAlusSequencesdfMatch$Sequence,
                                                                                              perl('(?=ggtcaggagttcgagaccag)'), 2), function(x) c(x[1],substr(x[2],1,146))))


require(stringr);counts$strip <- str_replace(counts$chrom, "chr", "")


#Trim whitespace from rows
library(stringr); df1[] <- lapply(df1, str_trim)


#Pick only certain columns based on grep
names(totsMerge) <- sub('.*?_(TC0\\d{2}[^_]*).*', '\\1', names(totsMerge))


#Quick find and replace
require(stringr);dfZ$chr <- str_replace(dfZ$chr, "chr", "hs")


#Rename columns based on a find replace names. You have to matchthe whole line.
names(cbind6Tum) <- gsub('.*(OCCAMS_[A-Z]{2}_[0-9]{3}_).*', '\\1', names(cbind6Tum))


#Crete a new column based on the values in another column
library(stringr)
MeDysplasia$Grade <- str_extract(MeDysplasia$SName, '[A-Z]GD')


#Match multiple samples
TTypedAllChol<-TTypedAll2[grepl("CholangioCarcinoma",TTypedAll2$TissueType)|grepl("Controls",TTypedAll2$TissueType),]


#Data cleaningas per here:  http://www.r-bloggers.com/three-quick-and-simple-data-cleaning-helper-functions-december-2013/
library(DataCombine)
ABNewDF <- FindReplace(data = ABData, Var = "a", replaceData = Replaces, from = "from", to = "to", exact = FALSE)


#Grep stringthat does not contain something
chromLength<- chromLength[!grepl("_", chromLength[,1]), ]


#Flagging when a string matches in a separate column with several conditions


Therap$EVENT <- ifelse(grepl("HALO|RFA|APC", Therap$ERPROCEDUREPERFORMED), "RFA",
                       ifelse(grepl("EMR", Therap$Diagnosis), "EMR",
                              "nothing"))
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
############################################################ Pasting strings  ##############################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################




##Add string to dataframe column values
data$Chromosome <- paste("chr",as.character(data$Chromosome), sep="")


#Rename columns- in this case rename the first three. names is a function
names(newdata2) <- c("chrom", "chromStart", "chromEnd")
#Rename just one column specifically
names(df1)[1] <- c("chr")
#Export a file- tab delimited
write.table(mydata,"/Volumes/SeagateBackupPlusDrive/SequencingRawFiles/TumourOesophagealOCCAMS/SequencingScripts/countsAmendedInterestingColumnOnly.txt", sep="\t", row.names=FALSE, col.names=FALSE)
#Rename a named column with dplyr
MyBariumData<-dplyr::rename(MyBariumData,HospNum_Id=Best.Hosp.No.)
#Rename a specific column
names(df)[names(df) == 'old.var.name']
#To concatenate rows together use paste, same with rows
AllAluSx$MergeCol<-paste(AllAluSx$chr, AllAluSx$chrStart, AllAluSx$chrEnd, sep=":")
#Concatentate columns together and re-add to the dataframe
TIMEdbMerge<-paste(TIMEdb$Study, TIMEdb$Sample,TIMEdb$StudySample, sep="")
TIMEdb<-cbind(TIMEdbMerge,TIMEdb)
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
############################################################# Sorting ########################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################


#Sorting in R- dont forget the comma before the bracket
newdata3 <- newdata2[order(newdata2$chrom) , ]
#And sort by more than one column
newdata3 <- newdata2[order(newdata2$chrom ,newdata2$chromStart) , ]
#sort top to bottom
sortedDiscoveryTumours <- tup[order(-tup[,1]),]
#Order a column with mixed text and numbers by the numbers
library(gtools)
df2<-df[mixedorder(df$chr),]
#Reverse the order of something according to a column
TTypedAll$Sample<-rev(TTypedAll$Sample)


#Group and then slect the minimum of any column in that group using dplyr


NAOGCName_Num_CStage_MStage<-NAOGCAll %>%
  arrange(HospNum_Id, as.Date(NAOGCAll$VisitDate, '%d/%m/%y')) %>%
  group_by(HospNum_Id) %>%
  slice(which.min(VisitDate))


####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
################################################ Descriptive stats #####################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################






#Count frequencies in a column (where the thing inside the "" is the column name)
library(dplyr)
count(newdata2, "Rptname")
#Group and count in one column
table(ZerofLINENameseq$V7)




#Group and get freq by group
DilatationsTable<-Dilatations %>%
  group_by(PATIENTRECORDNUMBER) %>%
  summarise (n=n())


####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
################################################ merge cbind and rbind #####################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################




#merging datasets (this is by two columns)
total <- merge(data frameA,data frameB,by=c("chr","leftPos"))


#How to add a row to a dataframe
result = data.frame(Names = names(result), Values = result)
x <- data.frame(Names="FastSeq",Values=FastSeqSVs)
result <- rbind(x,result)


#Merge datasets and keep everything in both. Can also do inner and outer joins with this
library(dplyr)
rainbow<-merge(totsMerge,TIMEdb,all = TRUE)


####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
################################################ ggplot ############################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################


#ggplot example
ggplot() +
  geom_point(aes(mergedGroup4$Rptname, mergedGroup4$PercentageChangeForSVStart, color = "red"), mergedGroup4) +
  geom_point(shape=1) +
  geom_point(aes(mergedGroup4$Rptname, mergedGroup4$PercentageChangeForSVEnd, color = "green"), mergedGroup4) +
  geom_point(size=3, colour="#CC0000")  +
  labs(title="Number of repetitive elements (as a proportion of total genomic number) per \n family at the start and end of Oesophageal adenocarcinoma SV's\n", x="TY [?C]", y="Txxx") +
  scale_color_manual("",labels = c("SVStart", "SVEnd"), values = c("blue", "red")) +
  #geom_point(aes(mergedGroup4$Rptname, mergedGroup4$PercentageChangeForWholeSV), mergedGroup4) + theme(axis.text.x=element_text(angle=-90)) +
  xlab("Repetitive elements") +
  ylab("Percentage of all repeat elements") +
  theme(axis.text.x=element_text(angle=-90)) +
  theme(legend.position="top")


#How to order stuff with ggplot
ggplot(resultOrder) +
  geom_point(aes(reorder(resultOrder$Names, resultOrder$Proportion, mean), y=resultOrder$Proportion)) +
  coord_flip() +

  #Order the x axis in ggplot
  DF1R$WorstH_path <- as.character(DF1R$WorstH_path)
#Then turn it back into an ordered factor
DF1R$WorstH_path <- factor(DF1R$WorstH_path,  levels=c("No_IM", "IM", "IGD","LGD","HGD","T1a","SM1","SM2","T1b_Unspec"))

#Get the data into long format in ggplot and in one line plot all the variable as a line.
  library(reshape)
mdata <- melt(resultOrderProportion, id=c("Names","DivergenceOfRpts"))
ggplot(mdata) +
  geom_line(aes(mdata$Names, mdata$value, group=mdata$variable, colour=mdata$variable)) +
  theme(legend.position="bottom") +
  labs(title="Number of SVs each repeat element is found in (as a percentage, filtered for >30%)", x="TY [?C]", y="Txxx") +
  xlab("Repetitive elements") +
  ylab("Percentage of SVs") +
  theme(axis.text.x=element_text(angle=-90))


#ggplot geom_bar
ggplot(pg_mean, aes(x=group, y=weight)) +
  geom_bar(stat="identity", fill="lightblue", colour="black")

#Stacked bar chart with ggplot- just make the fill the stacking variable
ggplot(mydf,aes(x=year,y=n,fill=Requestor))+geom_bar(stat="identity")

#Put a plot within a plot and other neat bits and peices- data is from CopyNumberCountsWithLoops.R


aplot<-ggplot() +
  geom_point(aes(TTypedAll$Sample, TTypedAll$V1, group=TTypedAll$TissueType,colour=TTypedAll$TissueType),size=5) +
  labs(title="Copy number index for oesophageal adenocarcinoma, \nnon-dysplastic Barrett's and normal blood", x="Sample", y="Copy number index") +
  theme(axis.text=element_text(size=16)) +
  theme(axis.title=element_text(size=18))+
  scale_x_discrete(breaks=NULL) +
  theme(legend.title=element_blank())+
  theme(legend.position = "none")+
  theme(plot.title = element_text(size=18,lineheight=.8, face="bold"))




d=data.frame(x=c(1), y=c(38), vx=c(2), vy=c(0))
l=data.frame(x=c(1), y=c(33), vx=c(0), vy=c(5))
r=data.frame(x=c(3), y=c(33), vx=c(0), vy=c(5))


p1 <- ggplot(aes(y = TTypedAll$V1, x = factor(TTypedAll$TissueType)), data = TTypedAll)
p1<-p1 + geom_boxplot(aes(fill=TTypedAll$TissueType))+
  geom_segment(data=d, mapping=aes(x=x, y=y, xend=x+vx, yend=y+vy), size=1, color="black") +
  geom_segment(data=l, mapping=aes(x=x, y=y, xend=x+vx, yend=y+vy), size=1, color="black") +
  geom_segment(data=r, mapping=aes(x=x, y=y, xend=x+vx, yend=y+vy), size=1, color="black") +
  geom_jitter(position=position_jitter(width=.2), size=1) +
  geom_text(aes(2, 40, label="*"),size=10)+
  xlab("Sample") +
  ylab("Copy number index")+
  theme(axis.text=element_text(size=14)) +
  theme(axis.title=element_text(size=14))+
  scale_x_discrete(breaks=NULL) +
  scale_fill_discrete(breaks=c("trt1","ctrl","trt2"))
theme(legend.title=element_blank())+
  theme(legend.position = c(0.9, 0.9))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


vp <- viewport(width = 0.6, height = 0.5, x = 0.673, y = 0.655)
#Just draw the plot twice
print(aplot)
print(p1, vp = vp)


#Get a histogram plot of whatever you want in ggplot


df<-subset(df,df[8]==0)
table<-table(df[7])
table1<-as.data.frame(table)
table2 <- table1[order(table1$Freq) , ]
table3<- tail(table2,50)


library(ggplot2)
ggplot() +
  geom_point(aes(table3$Var1, table3$Freq, color = "red"), table3) +
  geom_point(shape=1)


#Reorder a ggplot according to what you want. Have to use factors. This example reorders TTypedfTumvsB$Sample according to TTypedfTumvsB$TissueType
TTypedfTumvsB$Sample <- factor(TTypedfTumvsB$Sample, levels = TTypedfTumvsB$Sample[order(TTypedfTumvsB$TissueType)])
#To order by y axis as a continuous variable
resultOrder <- result[order(result$Freq), ]
resultOrder$Names <- factor(resultOrder$Names, levels = resultOrder$Names[order(resultOrder$Freq)])


#How to create a legend
legend(700,0.8,c("Low CN","Medium CN","High CN"), cex=0.8,lty=c(1,1), lwd=c(2.5,2.5),col=c("red","green","black"))# puts text in the legend




####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
################################################ non-ggplot stuff ############################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################




#Histogram plot of a column frequency with a subset for the most frequent
barplot(sort(table((ZerofLINENameseq$V7)[200:250])),cex.axis = 1, cex.names = 0.3, space= 12,main="Absolute Numbers of LINE elements found within SVs", las=2)


#Boxplot for grouped data
boxplot(df$sd ~df$chr)


# Example of a Bagplot -For continuous variables -like a boxplot for continuous variables
library(aplpack)
attach(mtcars)
bagplot(wt,mpg, xlab="Car Weight", ylab="Miles Per Gallon",
        main="Bagplot Example")


#Annotate a boxplot
fn<-boxplot2(rainbow$CopyNumber~rainbow$AFI,plot=FALSE)$stats
par(mar=c(4,6,4,2))
boxplot(rainbow$CopyNumber~rainbow$AFI,xlab="AFI status",ylab="Copy Number Change",outline=FALSE,col=c("red","green","black"))
text(1.15, fn[1], paste("Minimum Value =", fn[3]), adj=0, cex=.7)
text(1.15, fn[2], paste("Lower Quartile =", fn[2]), adj=0, cex=.7)
text(1.15, fn[3], paste("Median =", fn[3]), adj=0, cex=.7)
text(1.15, fn[4], paste("Upper Quartile =", fn[4]), adj=0, cex=.7)
text(1.15, fn[5], paste("Maximum Value =", fn[5]), adj=0, cex=.7)
arrows(1.14, fn[1], 1.02, fn[1])
arrows(1.14, fn[2], 1.02, fn[2])
arrows(1.14, fn[3], 1.02, fn[3])
arrows(1.14, fn[4], 1.02, fn[4])
arrows(1.14, fn[5], 1.02, fn[5])
title("TIME sample AFI \nvs copy number changes")




#To have a plot within a plot
library(lattice)
library(gridBase)
library(grid)


plot.new()
pushViewport(viewport())
xvars <- rnorm(25)
yvars <- rnorm(25)
plot(unlist(cbind6),type="p",cex=1.5,xlab="Sample", ylab="Copy Number Index", pch=21,bg="red", main= "Copy number Index for Combined HiSeq and MiSeq runs")
pushViewport(viewport(x=.6,y=.8,width=.25,height=.25,just=c("left","top")))
grid.rect()
par(plt = gridPLT(), new=TRUE)
pp<-boxplot(combined,main="Tumour,Diploid & Barrett's Normalised Copy Number Values using all \n LINE1 PCR loci", cex=0.5, xlab="Samples", ylab="Copy Number Value",pch=20, outline=FALSE,col=c("red","green","black"))
popViewport(2)


require(ggplot2)
require(grid)
plot(unlist(cbind6),type="p",cex=1.5,xlab="Sample", ylab="Copy Number Index", pch=21,bg="red", main= "Copy number Index for Combined HiSeq and MiSeq runs")
print(pp, vp=viewport(.8, .75, .2, .2))




#Import an image
img <- readPNG("sochi-logo.png")
g <- rasterGrob(img, interpolate=TRUE)
plot1 <- ggplot(mdat, aes(x = Count, y = Country, colour = Place))
+ geom_point()
+ facet_grid(.~Place) + theme_bw()
+ annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
+ scale_colour_manual(values=c("#CC6600", "#999999", "#FFCC33", "#000000"))


####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
################################################ importing and exporting things ####################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################


#To get a specific sheet from excel into R
library(XLConnect)
wk = loadWorkbook("/Users/sebastianzeki/Desktop/SequencingScripts/Plots/Results/Cellularities/cellularities_Sebastian.xlsx")
dfw = readWorksheet(wk, sheet="Sheet3",header=TRUE)
#To get the sheet names
library(gdata)
sheetNames("/Users/sebastianzeki/Dropbox/Work/Medical/Clinical/Gastro/Data_Infrastruc/Eosinophil/GSTT_HistoEndo_Eosinophil07toAug2016Report.xlsx")

#write to xls
writeWorksheetToFile("~\\MyBariumDataWithHRMNoDupsLab.xlsx",data=MyBariumDataWithHRMNoDupsLab,sheet="blabla",startRow=3,startCol=4)

#Subset by row
GroupTbbM[as.logical(rowSums(GroupTbbM > 0)), ]

#Add row.names as a proper column
myDF <- cbind(Row.Names = rownames(myDF), myDF)

#Subset groups
df[which(df$col2 > 25), ]




####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
##################################  Statistics #####################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################


#To get Kaplan-Meier curves
library(survival)
OCCAMSdbTumCN$D_Surv<-as.integer(OCCAMSdbTumCN$DOD-OCCAMSdbTumCN$DODg)
OCCAMSdbTumCN$Dead <- as.integer(OCCAMSdbTumCN$Dead)
mfit <- survfit(Surv(D_Surv, Dead )~1, data = OCCAMSdbTumCN)
par
plot(mfit)


####################################################################################################################################################################################
####################################################################################################################################################################################
############################################################### functions ###################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################






#Using functions for looping through data


library(irr)
lst <- mget(ls(pattern='total\\d+'))


classify_cnv = function (column)
  ifelse(column < 2, 1, ifelse(column > 2, 3, 2))


classify_all_cnvs = function (df) {
  df$CopyNumber.x = classify_cnv(df$CopyNumber.x)
  df$CopyNumber.y = classify_cnv(df$CopyNumber.y)
  df
}


result = lapply(lst, classify_all_cnvs)


lapply(result, function(df){
  kappa2(df[,c(5,8)], "squared")})


View(apply(AluJb,2,function(x){sum(x>0)}))


####################################################################################################################################################################################
####################################################################################################################################################################################
############################################################### Convert files ###################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################


#Convert a file to a csv
file <- '~/Downloads/PING CONCOURS DONNES.csv'
lines <- readLines(file)
columns <- strsplit(lines, ';')
headers <- sapply(columns, '[[', 1)
data <- lapply(columns, '[', -1)
df <- do.call(cbind, data)
colnames(df) <- headers
print(head(df))


#Convert data to JSON file format
library(rjson)
apply(X[,-1], 1, toJSON)


####################################################################################################################################################################################
####################################################################################################################################################################################
############################################################### Conditionals ###################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################


#ifelse with different conditions
OCCAMSdbTumCNOAC$CNSTATUS <- ifelse(OCCAMSdbTumCNOAC$CNI < 15, 1, ifelse(OCCAMSdbTumCNOAC$CNI >= 20 & OCCAMSdbTumCNOAC$CNI < 30, 2, 3))
#Several if statements put together
OCCAMSdbTumCN$CNSTATUS <- ifelse(OCCAMSdbTumCN$CN < 10, 1, ifelse(OCCAMSdbTumCN$CN >= 10 & OCCAMSdbTumCN$CN < 20, 2, 3))


####################################################################################################################################################################################
####################################################################################################################################################################################
############################################################### Dates stuff ###################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################


#Date tricks Get difference between current date and another column. Has a condition for na values
OCCAMSdbTumCNOAC$D_Surv = ifelse(!is.na(OCCAMSdbTumCNOAC$DOD),difftime(OCCAMSdbTumCNOAC$DOD,OCCAMSdbTumCNOAC$DODg,units="days"),difftime(Sys.time(),OCCAMSdbTumCNOAC$DODg,units="days"))
#Convert to date format from string (the format is the format the date is currently in as a string with hyphens but could be any punctuation)
OCCAMSdb$DOD<-as.Date(OCCAMSdb$DOD,format="%Y-%m-%d")


#Get difference between two dates in consecutive rows
DateSurveill<-EndoSubsetOnSurveil %>% arrange(HospNum_Id, VisitDate) %>% group_by(HospNum_Id) %>%
  mutate(diffDate = difftime(VisitDate, lag(VisitDate,1)))

#Plotting over time:
#group by month using dplyr, then just plot it

#Create time plot where date is on the x-axis:



####################################################################################################################################################################################
####################################################################################################################################################################################
############################################################### Lists stuff ###################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################


#Put a load of dataframes into a list
df_list = lapply(names(dfMatched)[-(1:2)],
                 FUN = function(x) dfMatched[, c(names(dfMatched)[1:2], x)])
names(df_list) = names(dfMatched)[-(1:2)]


#Renaming columns in a list
colnames <- c("chr","leftPos","Means")
for (i in seq_along(df_list)){
  colnames(df_list[[i]]) <- colnames
}


#Convert list to a datatable
library(data.table); DT <- rbindlist(df_list, idcol = TRUE)


#Extract data from a list
df <- data.frame(name=names(more),
                 value=sapply(more, function(x) x$value))
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
################################################ dput ####################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################


#Limited dput function
dput(droplevels(df1[1:6, 1:6]))


#Get the last columns in R
dput(HMapSampGenes[1:8,tail(colnames(HMapSampGenes),5)])


#Find all values where something is true
It is really quite easy because R can use logical indexing. So if drift.N already contains TRUE/FALSE, then simply:

  yourdata[yourdata[, "drift.N"], ]
should work. Basically, pass the column vector yourdata[, "drift.N"] as the row subset you want from your whole data frame, yourdata. The rows where drift.N == TRUE will be returned.


#Find values anywhere in dataframe
DAT[apply(DAT[, -1], MARGIN = 2, function(x) any(x < 0)), ]


####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
################################################ Package management  ####################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################




#Install a new package version
install.version('tidyr', '0.3.1',repos = "http://cran.us.r-project.org")


####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
################################################ DNA snippets ####################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
# Complement strand


p<-read.table("/Users/sebastianzeki/Dropbox/Work/Medical/Lab/BarrettsResearch/BarrettExperimentAndPresentation/ProjectCambridgePloidy/FSeqSeboligoTemplateForR.csv")
#If that doesnt work then just import using Rstudio tools
p$rev<-chartr("ATGC","TACG",p$Oligo_Sequence_5Prime_to_3Prime)


library(Biostrings)
dna = DNAStringSet(p$Oligo_Sequence_5Prime_to_3Prime)
p$rev<-complement(dna)


####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
################################################# Dataframe manipulations #########################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################


#Add a new column to an existing datafrme
saida["MY_NEW_COLUMN"] <- NA
####################################################################################################################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################
################################################# Statistics#########################################################################################
####################################################################################################################################################################################
####################################################################################################################################################################################


pie(table(Rule3$ERRECALLREASON1Grouped),cex=2.5,main="Decision path for patients not followed up",cex.main=3.5)




######Ti debug an error use:


options(error=utils::recover)


########################################################################## ##########################################################################
########################################################################## ##########################################################################
########################################################################## ##########################################################################
########################################################################## ##########################################################################
############################### Text mining algorithms ##########################################################################
########################################################################## ##########################################################################
########################################################################## ##########################################################################
########################################################################## ##########################################################################
########################################################################## ##########################################################################
########################################################################## ##########################################################################


# **To start,** install the packages you need to mine text
#      You only need to do this step once.

Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust",
            "cluster", "igraph", "fpc")
install.packages(Needed, dependencies=TRUE)

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

# If you get the following message:
#       Update all/some/none? [a/s/n]:
#   enter "a" and press return
##########################################################################################
##########################################################################################
#                                  Loading Texts  and Text Mining                        #
##########################################################################################
#
#     Start by saving your text files in a folder titled:    "texts"
#     This will be the "corpus" (body) of texts you are mining.
#
#     Next, choose the type of computer you have...



######
# **On a Mac**, save the folder to your *desktop* and use the following code chunk:
######
cname <- file.path("~", "Desktop", "texts")
cname
dir(cname)   # Use this to check to see that your texts have loaded.



######
# *On a PC*, save the folder to your *C: drive* and use the following code chunk:
######
cname <- file.path("C:", "texts")
cname
dir(cname)
##########################################################################################
##########################################################################################Run the Analyses##########################################################################################
#                                Start Your Analyses                                     #
##########################################################################################
# **Load the R package for text mining and then load your texts into R.**
library(tm)
docs <- Corpus(DirSource(cname))
## Preprocessing
docs <- tm_map(docs, removePunctuation)   # *Removing punctuation:*
docs <- tm_map(docs, removeNumbers)      # *Removing numbers:*
docs <- tm_map(docs, tolower)   # *Converting to lowercase:*
docs <- tm_map(docs, removeWords, stopwords("english"))   # *Removing "stopwords"
library(SnowballC)
docs <- tm_map(docs, stemDocument)   # *Removing common word endings* (e.g., "ing", "es")
docs <- tm_map(docs, stripWhitespace)   # *Stripping whitespace
docs <- tm_map(docs, PlainTextDocument)
## *This is the end of the preprocessing stage.*


### Stage the Data
dtm <- DocumentTermMatrix(docs)
tdm <- TermDocumentMatrix(docs)

### Explore your data
freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq)
m <- as.matrix(dtm)
dim(m)
write.csv(m, file="DocumentTermMatrix.csv")
### FOCUS - on just the interesting stuff...
#  Start by removing sparse terms:
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.
### Word Frequency
head(table(freq), 20)
# The above output is two rows of numbers. The top number is the frequency with which
# words appear and the bottom number reflects how many words appear that frequently.
#
tail(table(freq), 20)
# Considering only the 20 greatest frequencies
#
# **View a table of the terms after removing sparse terms, as above.
freq <- colSums(as.matrix(dtms))
freq
# The above matrix was created using a data transformation we made earlier.
# **An alternate view of term frequency:**
# This will identify all terms that appear frequently (in this case, 50 or more times).
findFreqTerms(dtm, lowfreq=50)   # Change "50" to whatever is most appropriate for your data.
#
#
#
### Plot Word Frequencies
# **Plot words that appear at least 50 times.**
library(ggplot2)
wf <- data.frame(word=names(freq), freq=freq)
p <- ggplot(subset(wf, freq>50), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p
#
## Relationships Between Terms
### Term Correlations
# See the description above for more guidance with correlations.
# If words always appear together, then correlation=1.0.
findAssocs(dtm, c("question" , "analysi"), corlimit=0.98) # specifying a correlation limit of 0.98
#
# Change "question" & "analysi" to terms that actually appear in your texts.
# Also adjust the `corlimit= ` to any value you feel is necessary.
#
#
### Word Clouds!
# First load the package that makes word clouds in R.
library(wordcloud)
dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)
freq <- colSums(as.matrix(dtm)) # Find word frequencies
dark2 <- brewer.pal(6, "Dark2")
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)

### Clustering by Term Similarity

### Hierarchal Clustering
dtms <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space.
library(cluster)
d <- dist(t(dtms), method="euclidian")   # First calculate distance between words
fit <- hclust(d=d, method="ward")
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters

### K-means clustering
library(fpc)
library(cluster)
dtms <- removeSparseTerms(dtm, 0.15) # Prepare the data (max 15% empty space)
d <- dist(t(dtms), method="euclidian")
kfit <- kmeans(d, 2)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

######################################################################################################################################################
################################################# Dealing with negatives in free text##########################################################################################
######################################################################################################################################################

MyBariumData$Dysmotility <- ifelse(grepl("[Dd]ysmotility",MyBariumData$Rep.Text,perl=TRUE)&
                                     !grepl("[Nn]o.*[Dd]ysmotility?\\n",MyBariumData$Rep.Text,perl=TRUE),"Yes","NO")

#################################Data cleaning with tidyR##################################################
#Tidy timepoint dataeg in order to get the following:

#row    TimePoint1      Timepoint2      Timepoint3        Timepoint4


EGJSwallows2$rowid<-rownames(EGJSwallows2)
EGJSwallows2DCIVariation<-EGJSwallows2 %>%
  select(TimePoint1,Timepoint2,Timepoint3,Timepoint4,rowid)%>%
  gather(variable, value, DistallatencyMapSwallowsNum1:DistallatencyMapSwallowsNum4)



#Convert NA to zeros
MyBariumDataWithHRM$dx[is.na(MyBariumDataWithHRM$dx)]=0


#SQL snippets so you can get the linked tables results:


.libPaths()
.libPaths("S:\\Gastroenterology\\Seb\\R\\R-3.3.1\\library")
.libPaths()

library(RODBC)
channel <- odbcConnectAccess("S:\\Gastroenterology\\Seb\\JavaPortableLauncher\\PhysiPopDONOTTOUCH\\Physiology.mdb")
dataBT <- sqlQuery( channel , "SELECT BreathTests.*FROM BreathTests",stringsAsFactors=F)


#Convert a bunch of rows to long format

library(XLConnect)
wk = loadWorkbook("/Users/sebastianzeki/Dropbox/Work/Medical/Clinical/Gastro/Research/Eosinophilic/EoETreatmentsMASTER.xls")
dfw = readWorksheet(wk, sheet="Sheet1",header=TRUE)




names(dfw) <- gsub("([1-5])[a-z]{2}", "\\1", names(dfw))
#dfw[, 1:116][is.na(dfw[, 1:116])] <- "NA"


names(dfw) <- gsub("(\\d)(\\w*)", "\\2\\.\\1", names(dfw))
#ncol(dfw[grep(".*\\.5$",names(dfw))])






library(data.table)
setDT(dfw)


try<-melt(dfw, id.vars = "HospNum_Id",measure = list(
  Tx_Tried =  grep("Tx_Tried", names(dfw)),
  Tx_Location=  grep("Tx_Location", names(dfw)),
  Tx_Comments=  grep("Tx_Comments", names(dfw)),
  Details = grep("Tx_ResponseHistol", names(dfw)),
  Tx_ResponseClinical = grep("Tx_ResponseClinical", names(dfw)),
  Tx_Date= grep("Tx_Date",names(dfw)),
  Tx_YN_Budesonide= grep("Tx_YN_Budesonide",names(dfw)),
  Tx_YN_Fluticasone= grep("Tx_YN_Fluticasone",names(dfw)),
  Tx_FlutiComments= grep("Tx_FlutiComments",names(dfw)),
  Tx_YN_PPIs= grep("Tx_YN_PPIs",names(dfw)),
  Tx_PPIsComments= grep("Tx_PPIsComments",names(dfw)),
  Tx_YN_SFED= grep("Tx_YN_SFED",names(dfw)),
  Tx_SFEDComments= grep("Tx_SFEDComments",names(dfw)),
  Tx_YN_TED= grep("Tx_YN_TED",names(dfw)),
  Tx_TEDComments= grep("Tx_TEDComments",names(dfw)),
  Tx_YN_ED= grep("Tx_YN_ED",names(dfw)),
  Tx_YN_OtherDiet= grep("Tx_YN_OtherDiet",names(dfw)),
  Tx_OtherDietComments= grep("Tx_OtherDietComments",names(dfw)),
  Tx_BxHistolResponseYN= grep("Tx_BxHistolResponseYN",names(dfw)),
  Tx_BxClinicalResponse= grep("Tx_BxClinicalResponse",names(dfw)),
  Tx_BxClinical_ResponseComments= grep("Tx_BxClinical_ResponseComments",names(dfw)),
  Tx_PeriphEosinophDate= grep("Tx_PeriphEosinophDate",names(dfw)),
  Tx_PeriphEosinophResults= grep("Tx_PeriphEosinophResults",names(dfw))),
  value.name = c("Tx_Tried","Tx_Location","Tx_Comments","Tx_ResponseHistol","Tx_ResponseClinical","Tx_Date","Tx_YN_Budesonide","Tx_YN_Fluticasone","Tx_FlutiComments","Tx_YN_PPIs","Tx_PPIsComments","Tx_YN_SFED","Tx_SFEDComments","Tx_YN_TED","Tx_TEDComments","Tx_YN_ED","Tx_YN_OtherDiet","Tx_OtherDietComments","Tx_BxHistolResponseYN","Tx_BxClinicalResponse","Tx_BxClinical_ResponseComments","Tx_PeriphEosinophDate","Tx_PeriphEosinophResults"))


#Detach all packages:
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)

#One package detach
detach("package:vegan", unload=TRUE)

--------------------------------------------------------------------------------

#Split a column with lots of symptoms in it into separate symptoms:

IndicVsBx<-data.frame(cSplit(x,"IndicationsFroExamination","\n","long")[, .(Avg=mean(NumbOfBxs)),by=IndicationsFroExamination])

#Alternative way
s<-strsplit(as.character(x$IndicationsFroExamination),'\n')
IndicVsBx<-data.frame(director=unlist(s),AB=rep(x$NumbOfBxs,sapply(s,FUN=length)))
IndicVsBx$director<-as.character(IndicVsBx$director)
IndicVsBx2<-IndicVsBx %>%
  group_by(director) %>%
  dplyr::summarise(mean=mean(AB,na.rm=T))


#A way to split column according to a delimiter:

within(dfw,FOO<-data.frame(do.call('rbind',strsplit(as.character(PatientName),',',fixed=T))))

#Create matrix or cross tabulate from a data frame (usually for a heatmap)

matPoss<-xtabs(Freq~Reason+AB,data=matPossdf)
pheatmap(matPoss)

#Useful date things
library(lubridate)
ymd_hms(x$Histo_ResultPerformed)



#Extract rows using a list of words
foo<-foo[grepl(paste(myNotableWords, collapse='|'), foo$X2,perl=TRUE),]



#Group words in a list:
sapply(myNotableWords, function(x) sum(foo$Prop[grep(x, foo$X2)]))

OR
library(stringr)
library(dplyr)
df1 %>%
  group_by(grp = str_extract(X2, paste(myNotableWords, collapse="|"))) %>%
  summarise(Prop = sum(Prop)) %>%
  na.omit()

#Remove empty dataframes from a list of dataframes
zz<-data.frame(Filter(Negate(function(x) is.null(unlist(x))),zz))


#Remove column where the whole column is NA
dataImpWhole<-dataImpWhole[,colSums(is.na(dataImpWhole))<nrow(dataImpWhole)]

#Get all rows where the value anywhere is over a certain level in specific columns("RSAPNonacid")
names(dfwNon=dataImpWhole[which(apply(dataImpWhole[grepl("RSAPNonacid",names(dataImpWhole))],1,max,na.rm=T)>50),]

#Convert things to lower case
gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(x), perl=TRUE)

#str_extract_all
Self$FindingsAlgoNegs<-str_extract_all(Self$Findings,"[Nn]o .*?[\\.\\n]|.* [Nn]ormal.*?[\\.\\n]|There (?:is|was|are) no.*?\\.|.*None.*?[\\.\\n]|.*unremarkable?\\.|^Negative.*?[\\.\\n]|[Oo]therwise (?:normal|unremarkable).*?[\\.\\n]")

#List all the files in a folder and subfolders and get information about them:

#####Get all the file names
myfileNames<-list.files(path="I:\\OesophagealLabReports\\1-All reports",recursive=T)
#####Get all the file info
finf<-file.info(list.files(path="I:\\OesophagealLabReports\\1-All reports", full.names=TRUE,recursive=T))
finf$FileName<-row.names(finf)

#Dates in multiple formats:
tbb$Endo_ResultEntered<-as.Date(ifelse(grepl("-", tbb$Endo_ResultEntered), as.Date(tbb$Endo_ResultEntered, format = c("%d-%b-%Y")), as.Date(tbb$Endo_ResultEntered, format = c("%d_%m_%Y"))), origin = "1970-01-01")

#Get week of the year with dyplr and lubridate:

df_opAttend%>% mutate(week=week(df_opAttend$Appt.Date))


#Random sample of numbers:

sample(1:10, 20,replace=T)

#Manual package installation:
install.packages("H:/Downloads/clickme-master.zip", repos = NULL)


#Get large or multiple spreadsheets into R (for xlsx):

setwd("S:\\Gastroenterology\\Seb\\R\\Data\\SelfAudit\\EndoscopyFrom2007ToApr1_2017//")
filenames <- list.files(pattern=".xlsx")
library(plyr)
df.list <- lapply(filenames, function(x) read.xlsx(x, sheet = 1, startRow = 1, colNames = TRUE))
final.df <- rbind.fill(df.list)


#To get a timeline ggplot done
#Get the date column from the dataframe. Use lubridate and dplyr library
OGDfinal.df<-OGDfinal.df%>% mutate(week=week(OGDfinal.df$Endo_ResultEntered))
OGDfinal.df<-OGDfinal.df%>% mutate(month=month(OGDfinal.df$Endo_ResultEntered))
OGDfinal.df<-OGDfinal.df%>% mutate(year=year(OGDfinal.df$Endo_ResultEntered))
#TO the get table
OGDfinal.df<-OGDfinal.df%>% group_by(year,month)%>%summarise(n=n())
#Then plot it
ggplot(ndf) + geom_bar(aes(x=month,y=n),stat="identity")  + facet_wrap(~year,nrow=1)


#For excel imports use:
library(readxl)
######################################### Data acquisiton#########################################
read_excel("S:\\Gastroenterology\\Seb\\R\\Data\\SelfAudit\\OrigEndoscopyFrom2007ToApr1_2017//GSTT_TCR2232_AllEndoscopyProcedure_since_2007.xlsx")


#################### Time series analysis with xts####################
#Get dataframe with a date time in it. If isnt date then make it so
#Then create the object (the output is a matrix with the dat column as the row.name). order by specifies the date column you want to be the index column

library(xts)
Myxts<-xts(df, order.by=df$Date)



############# Faceting time series############# ############# ############# ############# ############# ############# #############
library(tidyquant)
#From this tutorial "http://www.business-science.io/timeseries-analysis/2017/07/02/tidy-timeseries-analysis.html"


#Starting table format.......
## # A tibble: 1,629 x 3
## # Groups:   package [9]
##          date count package
##  *     <date> <dbl>   <chr>
##  1 2017-01-01   873   tidyr
##  2 2017-01-02  1840   tidyr
##  3 2017-01-03  2495   tidyr
##  4 2017-01-04  2906   tidyr
##  5 2017-01-05  2847   tidyr
##  6 2017-01-06  2756   tidyr
##  7 2017-01-07  1439   tidyr
##  8 2017-01-08  1556   tidyr
##  9 2017-01-09  3678   tidyr
## 10 2017-01-10  7086   tidyr
## # ... with 1,619 more rows


#Use tq_transmute to get the apply function from xts
#Basically this groups according to week in this example and gets the mean
mean_tidyverse_downloads_w <- tidyverse_downloads %>%
  tq_transmute(
    select     = count,
    mutate_fun = apply.weekly,
    FUN        = mean,
    na.rm      = TRUE,
    col_rename = "mean_count"
  )

#THis can then be plotted as a facet wrapped ggplot:
library(ggplot2)
mean_tidyverse_downloads_w %>%
  ggplot(aes(x = date, y = mean_count, color = package)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(title = "tidyverse packages: Average daily downloads by week", x = "",
       y = "Mean Daily Downloads by Week") +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  expand_limits(y = 0) +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")

#Can also use a custom function instead of just the mean:

# Custom function to return mean, sd, quantiles
custom_stat_fun <- function(x, na.rm = TRUE, ...) {
  # x     = numeric vector
  # na.rm = boolean, whether or not to remove NA's
  # ...   = additional args passed to quantile
  c(mean    = mean(x, na.rm = na.rm),
    stdev   = sd(x, na.rm = na.rm),
    quantile(x, na.rm = na.rm, ...))
}

probs <- c(0, 0.025, 0.25, 0.5, 0.75, 0.975, 1)

stats_tidyverse_downloads_w <- tidyverse_downloads %>%
  tq_transmute(
    select = count,
    mutate_fun = apply.weekly,
    FUN = custom_stat_fun,
    na.rm = TRUE,
    probs = probs
  )

stats_tidyverse_downloads_w %>%
  ggplot(aes(x = date, y = `50%`, color = package)) +
  # Ribbon
  geom_ribbon(aes(ymin = `25%`, ymax = `75%`),
              color = palette_light()[[1]], fill = palette_light()[[1]], alpha = 0.5) +
  # Points
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  # Aesthetics
  labs(title = "tidyverse packages: Median daily downloads by week", x = "",
       subtitle = "Range of 1st and 3rd quartile to show volatility",
       y = "Median Daily Downloads By Week") +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  expand_limits(y = 0) +
  scale_color_tq(theme = "dark") +
  theme_tq() +
  theme(legend.position="none")






#Dates in the past 7 days:
myfileNames_df[myfileNames_df$ctime>=(as.Date(Sys.Date(),format='%Y-%m-%D')-7),]


#To create whole website in rmarkdown do:
rmarkdown::render_site("S://Gastroenterology//Seb//R//Scripts//TBB_website")


#To get sample of procedure do:
sample(c("EMR","RFA","Biopsies"), 100, replace = TRUE)
#Sample dates
sample(seq(as.Date('2013/01/01'), as.Date('2017/05/01'), by="day"), 100)
#Generate 20 hospital numbers in no particular order:
sample(c("P433224","P633443","K522332","G244224","S553322","D0739033","U873352","P223333","Y763634","I927282","P223311","P029834","U22415","U234252","S141141","O349253","T622722","J322909","F630230","T432452"), 100, replace = TRUE)
#Generate random numbers
sample(1:100, 3, replace=TRUE)


#Add a link to rmarkdown:

<http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram#visualization-methods>

internal links in rmarkdown:
  [GroupByDates](/GroupByDates/)








