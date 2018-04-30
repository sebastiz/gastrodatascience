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






######  Introduction ###### 
#Using the book Text mining with R Jan Wijfiels: jwijfiels@bnosac.be   R_Textmining.pdf

options(width=120)  # make the printing fit on the page
options(scipen=3)
oldwd <- getwd()


mystrings
Encoding(mystrings)

Sys.getlocale("LC_CTYPE")
localeToCharset()

iconvlist()

##### Encoding Detection #################### 
library(stringi)
stri_enc_detect(str = "I want to detect this encoding")
# You can easily convert your string to another encoding using iconv.
# If you know your data is in Latin-1 and want to convert it in UTF-8

mystring <- mystrings[1]
mystring
Encoding(mystring)
x <- iconv(mystring, from = "latin1", to = "UTF-8")
x 
Encoding(x)
#If you convert to another encoding, make sure the character exists

mystring <- mystrings[1:2]
mystring
iconv(mystring, from = "latin1", to = "ASCII", sub = "?")
iconv(mystring, from = "latin1", to = "ASCII")


# If you want to get rid of special characters and want to translate special
# characters to ASCII, you can use translate to ASCII. This will translate
# special characters from another encoding which do not exist in the ASCII
# encoding to the ASCII encoding.You can change the encoding of text as follows allowing you to either
# mess up your text data or set it correctly.

iconv(mystring, from = "latin1", to = "ASCII//TRANSLIT")

Encoding(mystrings)
mystrings
Encoding(mystrings) <- "UTF-8"
mystrings
Encoding(mystrings) <- "latin1"
# When importing data, know the Encoding of your le and you specify it
# I readLines has argument encoding to indicate latin1 or UTF-8
# I read.table has argument encoding to assume latin1 or UTF-8
# Languages can be identied by N-gram proles.
# Function textcat form the textcat package allows this.

library(textcat)
mystrings
textcat(mystrings)
textcat(mystrings, 
        p = TC_char_profiles[c("dutch","english","french","german")],
        method = "CT")

# Methodology explained at www.jstatsoft.org/v52/i06
# I Categorize texts by computing their n-gram proles, and nding the
# closest category n-gram prole.
# I N-gram prole db is constructed from a known corpus for a language
# and top 300 n-grams are taken
# I Next, compute n-gram on your own text
# I Calculate distance to the n-gram prole of the database
# I Default distance method CT: Cavnar and Trenkle approach which
# counts the number of rank inversions between the proles
textcat("Marieke gaat om water", p = textcat::ECIMCI_profiles)
textcat("Marieke gaat om water", p = textcat::TC_char_profiles)
textcat("Marieke gaat om water", p = textcat::TC_byte_profiles)

# Similar approach used by R package cld2 (Google's Compact Language
# Detector)
#                                          I Corpus was constructed based on scraped web pages where they
#                                          knew the language of the web page
#                                          I N-grams are constructed based on the HTML.
#                                          I A naive bayesian classier is constructed based on this data
#                                          I The model is applied to new data
#                                          I Advantage: faster and good if your data is html, disadvantage you
#                                          can not use your own corpus
library(cld2)
x <- c("To be or not to be?", "Ce n'est pas grave.", "Nou breekt mijn klomp!")
detect_language(x)



##### ??????????????? Basic text processing ################################################
# 2 functions that you ought to know already
# I paste: collapsing several strings in 1 or combining several strings
# I sprintf: inject a string into another
library(udpipe)
data("brussels_reviews", package = "udpipe")
brussels_reviews$feedback[1:2]


##### paste and sprintf ##### 
x <- c("abc", "def", "123", "go ahead")
paste(x, collapse = "_")

paste(x, 1:4, sep = "-")

sprintf("INSERT INTO customers VALUES(%s, '%s')", 
        1:2, 
        c("person1", "xyz"))


############## Regular expressions ########################################## 

# Detecting (search): grep
# I searches for matches to argument pattern within each element of a
# character vector.
# I returns the vector elements where it has found the pattern
# argument ignore.case allows you to match exactly (including upper
#                                                   case/lower case)
# I argument invert allows you to get the indexes where the match is
# not found
# I argument value will return the character strings with a match
# grep returns the index of the match in the character vector while grepl
# returns a logical vector of length(x) indicating the match location.
grep(pattern="B", x=c("A", "B", "C", "D"))

x <- c("LStat","lstat","Leuven Statistics Research Center",
       "BNOSAC", "Belgium", "Waffels in Belgium", "Manneken Pis @ Atomium")
grep(pattern="stat", x=x, ignore.case=TRUE)
grep(pattern="stat", x=x, ignore.case=TRUE, invert=TRUE)
# I argument ignore.case allows you to match exactly (including upper
#                                                     case/lower case)
# I argument invert allows you to get the indexes where the match is
# not found
# I argument value will return the character strings with a match
# grep returns the index of the match in the character vector while grepl
grep(pattern="lstat", x=x, ignore.case=TRUE, value=TRUE)
grepl(pattern="lstat", x=x, ignore.case=TRUE)


x <- c("LStat","lstat","Leuven Statistics Research Center",
       "BNOSAC", "Belgium", "Waffels in Belgium", "Manneken Pis @ Atomium")
grep(pattern="^Bel", x=x, value=TRUE)
grep(pattern="stat$", x=x, value=TRUE)
grep(pattern="^Bel|Stat$", x=x, value=TRUE)


grep(pattern="Go{4}l", x=c("Gooool","Goooool"), value=TRUE)
grep(pattern="Go+a", x=c("Gooooaaal","Goooooobbbl"), value=TRUE)
grep(pattern="^GG?o*a", x=c("Gooooaaal","GGooooaaal"), value=TRUE)
grep(pattern="G.*a", x=c("Gooooaaal","Gooaaal"), value=TRUE)
grep(pattern="(Ik haat){2}", x=c("Ik haat smurfen","Ik haatIk haat smurfen"), value=TRUE)


metacharacters = c("$","*","+",".","?","[","^","{","|","(","\\")
grep(pattern=".", "testing", value=TRUE)
grep(pattern="\\.", "testing", value=TRUE)
grep(pattern="\\.", "testing.out", value=TRUE)

input1 <- c("nose", "letter38", "window9", "apple0")
grep("[[:digit:]]", input1, value = TRUE)
grep("[nco]", input1, value = TRUE)
grep("[39]", input1, value = TRUE)

input2 <- c("abcdef", "ABCDEFG", "IJK")
grep("[a-cA-D]", input2, value = TRUE)
grep("[[:lower:]]", input2, value = TRUE)


gsub(pattern="Statistics", replacement="Statistiek", x="Leuven Statistics Research")
gsub(pattern="Sta|Research", replacement="", x="Leuven Statistics Research")
gsub(pattern=" +", replacement=" ", x="Leuven      Statistics")
sub(pattern="\\.",  replacement="", x="abc...def")
gsub(pattern="\\.", replacement="", x="abc...def")

x <- "I want to break free"
gsub(pattern = "(.+)(break)(.+)", replacement = "\\1", x)
gsub(pattern = "(.+)(break)(.+)", replacement = "\\2", x)
gsub(pattern = "(.+)(break)(.+)", replacement = "\\3", x)


gregexpr(pattern="\\.", text="abc...def")
txt <- "Leuven Statistics Research Center"
gregexpr(pattern="Sta|Research",  text=txt)
regexpr(pattern="Sta|Research",  text=txt)

# regexpr and gregexpr give detailed information where the
# matches are found.
# I regexpr gives the starting position of the rst match.
# I gregexpr the starting positions of every (disjoint) match is given.
txt <- "Leuven Statistics Research Center"
regmatches(x=txt, gregexpr(pattern="Sta.+ |Research",  text=txt), invert=FALSE)
regmatches(x=txt, gregexpr(pattern="Sta.+ |Research",  text=txt), invert=TRUE)
regmatches(x=txt, regexpr(pattern="Statis",  text=txt)) <- "Analy"
txt
# The result of a gregexpr can be fed to regmatches in order to extract
# or replace the text

###### strsplit #####
#strsplit splits text data by a character or a regular expression


strsplit(x=c("abc.   def", ""), split=" ")
strsplit(x=c("abc.   def", ""), split="\\.")

##### strtrim #####
#use substr and strtrim to extract element of character data
substr(x=c("abc. def", "123456789"), start=3, stop=4)
strtrim(x=c("abc", "123456789"), width=4)

d1 <- data.frame(id...of....patient = c(1, 2), patient....age = c(1, 2))
d <- data.frame(
  id = c(11, 22, 33, 44, 55, 66, 77), 
  drug = c("vitamin E", "vitamin ESTER-C", "  vitamin E ", "vitamin E(ointment)", "", "provitamin E\n", "vit E"), 
  text = c("", " ", "3 times a day after meal", "once a day", " ", "\t", "\n "), 
  stringsAsFactors = FALSE)

##### String distances #############################################  

# Package stringdist allows to compute distances
# between 2 strings.
# Application domains:
#   I replace strings with another string
# I fuzzy merging
# http://journal.r-project.org/archive/
#   2014-1/loo.pdf
# I edit-based distances: for short string matching
# I q-gram based distances: for very long text
# strings
# I heuristic distances: for matching
# human-typed, relatively short strings
# base package functions: agrep & adist
# I adist: allows to compute the generalized Levenshtein distance
# between strings.
# I agrep: allows to get approximate regulare expressions.
# Levenshtein distances: combination of transformations 1, 2, 3
# 1. Substitution of a character, as in 'belgium' >'belhium'.
# 2. Deletion of a character, as in 'belgium' >'elgium'.
# 3. Insertion of a character, as in 'belgium' >'beltgium'.
# 4. Transposition of two adjacent characters, as in 'belgium' >'beglium'.


wb <- c("Warner Bros Pictures", "Warner Bros.", "Warner Brothers", 
        "Warner Bro. Pictures", "Warners Bros. Pictures",
        "Universal / Warner Bros.")
adist(x = "Warner Bros Pictures", y = wb)
agrep(pattern = "Warner Bros Pictures", x = wb, 
      max.distance = 0.1, value=TRUE)

agrep(pattern = "Warner Bros Pictures", x = wb, 
      costs = list(insertions = 0.1, deletions = 0.5, substitutions = 1), 
      max.distance = 0.1, value=TRUE)

# Always based on UTF-8 encoding.
# I Hamming distance: only character substitutions
# I Levenshtein distance (weighted): counting the weighted number of
# insertions, deletions and substitutions necessary to turn one string
# into another.
# I Restricted Damerau-Levenshtein distance (weighted, a.k.a. Optimal
#                                            String Alignment): extension of the Levenshtein distance that allows
# for transpositions of adjacent characters
# I Longest Common Substring distance: counts the number of deletions
# and insertions necessary to transform one string into another

require(stringdist)
## Get Hamming/Levehnstein distances
stringdist(a = "Warner Bros Pictures", wb, method = "hamming")
stringdist(a = "Warner Bros Pictures", wb, method = "lv")
## Optimal String Alignment and Longest common substring distance
stringdist(a = "Warner Bros Pictures", wb, method = "osa")
stringdist(a = "Warner Bros Pictures", wb, method = "lcs")

#Approximate in operators based on string matching

wb %in% "Warner Bros Pictures"
ain(x=wb, table="Warner Bros Pictures", maxDist = 4, method = "lv")

# A q-gram is a string consisting of q consecutive characters.
# I Jaccard distance for q-gram count vectors (= 1-Jaccard similarity)
# where Jaccard similarity: listing unique q-grams in two strings and
# compare which ones they have in common
# I Q-gram distance: The q-gram distance is obtained by tabulating the
# q-grams occurring in the two strings and taking the absolute value
# of the row differences
qgrams(a = "leilali", b = "leelala", 
       q = 2)
stringdist('leilali', 'leelala', method='jaccard', q = 2) # 1 - ((3/8) in common)
stringdist('leilali', 'leelala', method='qgram', q = 2) # sum of abs(row 1 - row 2)
# Jaro, and Jaro-Winker distance:
#   Used to match name + address data:
#   I character mismatches and transpositions are caused by typing-errors
# I matches between remote characters are unlikely to be caused by a
# typing error
# I measures the number of matching characters between two strings
# that are not too many positions apart
# I adds a penalty for matching characters that are transposed
# I an extra penalty for character mismatches in the rst four characters
# dropped a letter at the end, low penalty
stringdist('Jan Wijffels', 'Jan Wijfel', method='jw')
# low penality on swapping
stringdist('Jan Wijffels', 'Jna Wijffesl', method='jw')
# mistyping higher penalty than swapping
stringdist('Jan Wijffels', 'Jen Wijffels', method='jw')

##### ??????????????? Graphical representation ################ 

# I frequencies
# I wordclouds
# I correlations
# I associations word network plots


#Working on a dataset with movies extracted using the omdbapi package

load("../data/mymovies.RData")
str(mymovies)
# n-grams

#I 1-gram: 1 word/term
# I bi-gram: one word/term in combination with another word
# I tri-gram: three words/terms in a row

oldpar <- par(no.readonly = FALSE)



library(tau)
out <- textcnt(x = mymovies$Plot, tolower = TRUE, 
               method = "string", n = 1, decreasing=TRUE)
barplot(rev(head(out, 20)), col = "lightblue", 
        horiz = TRUE, las = 2, main = "Frequency of terms")


##### Wordcloud #####
# Wordclouds are frequency statistics. Mark that data preparation is key
# (e.g. ltering irrelevant words)
# I scale: Set sizes of the words
# I max.words and min.freq: Limit the number of words plotted.
# I random.order: words with the highest frequency are plotted rst
# I rot.per: Fraction of words that are plotted vertically.
# I colors: color paletteslibrary(wordcloud)
wordcloud(names(out), freq = out, scale=c(10, .75), min.freq = 10, max.words = 100,
          random.order = FALSE, colors = brewer.pal(6,"Dark2"))

##### Correlations and association plots #####

# These kinds of plots require a document-term matrix which needs some
# data preparation.


library(tm)
library(NLP)
mymovies$txt <- tolower(mymovies$tomatoConsensus)
mycorpus <- Corpus(x = VectorSource(mymovies$txt))  
mydtm <- DocumentTermMatrix(mycorpus, 
                            control = list(tokenize = "words"))
mydtm

## show some rows
as.matrix(mydtm[1:3, c("and", "the", "movie")])

# Mark: you can also get that document/term/matrix with other packages
# like udpipe, tidytext, quanteda

mymovies$doc_id <- 1:nrow(mymovies)

## Use the udpipe package to create a document/term/matrix
library(udpipe)
dtm <- document_term_frequencies(x = mymovies$txt, document = mymovies$doc_id, split = "[[:space:][:punct:]]+")
dtm <- document_term_matrix(dtm)

## Use the tidytext package to create a document/term/matrix
library(tidytext)
dtm <- unnest_tokens(mymovies, output = "term", input = "txt", token = "words")
dtm <- count(dtm, doc_id, term, sort = TRUE)
dtm <- ungroup(dtm)
dtm <- cast_dtm(dtm, document = doc_id, term = term, value = n)

## Use the quanteda package to create a document/term/matrix
library(quanteda)
dtm <- mymovies$txt
names(dtm) <- mymovies$doc_id
dtm <- dfm(dtm, what = "word")

#Correlation plots
library(corrplot)
interestedin <- c("action", "may", "enough", "special", "movie", "humor", 
                  "effects", "will", "fans", "best", "one")
m <- cor(as.matrix(mydtm[, interestedin]), 
         method = "pearson", use = "pairwise.complete.obs")
corrplot(m, method="circle", type="lower", diag=TRUE, order = "original", 
         addCoef.col="black", 
         main = "\n\nCorrelation between words")

interestedin <- c("action", "may", "enough", "special", "movie", "humor", 
                  "effects", "will", "fans", "best", "one", "boasts", "cast", "make", "funny", "story", "talented")
plot(mydtm, terms = interestedin, 
     corThreshold = 0.075, weighting = TRUE,
     attrs = list(node = list(shape = "circle", fontsize = 15), 
                  edge = list(color = "steelblue")))

par(oldpar)

# Calculate word co-occurences in each sentence (using cooccurrence
#                                                from the udpipe package)
# I How many times does each word occur in the same sentence with
# another word
library(udpipe)
load("../data/brussels_reviews_pos.RData")
x <- subset(brussels_reviews_pos, language %in% "french" & word.type %in% c("NN"))
head(x[, c("id", "sentence.id", "language", "word.lemma")], 3)
word_cooccurences <- cooccurrence(x, term="word.lemma", group=c("id", "sentence.id"))
head(word_cooccurences)

##### Network co-occurrences #####

# Visualise the word co-occurences as a graphlibrary(magrittr) #

library(ggraph)
library(igraph)
set.seed(123456789)
head(word_cooccurences, 50) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link0(aes(edge_alpha = cooc, edge_width = cooc)) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.8, col = "darkgreen") +
  ggtitle("Co-occurrence of nouns") +
  theme_void()




########### Stemming ################################# 
# Stemming: reducing in
# ected (or sometimes derived) words to their word
# stem, base or root form-generally a written word form
# Snowball:
#   Snowball stemmer allows stemming of languages: danish, dutch, english, nnish,
# french, german, hungarian, italian, norwegian, porter, portuguese, romanian, russian, spanish,
# swedish, turkish
# Procedure for each language is explained in
# http://snowball.tartarus.org

library(SnowballC)
wordStem(c("winner", "winners", "winning", "won"), language = "english")
stemSentence <- function(x, language){
  x <- strsplit(x, "[[:blank:]]")[[1]]
  x <- wordStem(x, language)
  paste(x, collapse = " ")
}
mymovies$Plot[1]
stemSentence(mymovies$Plot[1], language = "english")

##### ??????????????? POS tagging ######## 

# POS tagging: also called grammatical tagging or word-category
# disambiguation
# I is the process of marking up a word in a text (corpus) as
# corresponding to a particular part of speech
# I based on both its denition, as well as its context-i.e. relationship
# with adjacent and related words in a phrase, sentence, or paragraph.
# Categories: noun, verb, article, adjective, preposition, pronoun, adverb,
# conjunction and interjection.
# Modelling: tagging is done based on a corpus and a learner (Hidden
#                                                             Markov models, Viterbi algorithm, Brill Tagger, the Baum-Welch
#                                                             forward-backward algorithm, SVM, Perceptron, Nearest Neighbour,
#                                                             Maximum Entropy)

# Possibility 1: UDPipe: Language-agnostic tokenisation, lemmatisation,
# POS tagging, dependency parsing
# I For more than 50 languages
# I Universal POS tags categories as dened at
# http://universaldependencies.org/u/pos/index.html


library(udpipe)
ud_model <- udpipe_download_model(language = "dutch")
ud_model <- udpipe_load_model(ud_model$file_model)
txt <- c("Ik ga op reis en ik neem mee, een tandenborstel, boeken en mijn goed humeur.",
         "Ik ga niet op reis. Mijn geld is op.")
x <- udpipe_annotate(ud_model, txt)
x <- as.data.frame(x)
str(x)

# UDPipe logic
# 1. Tokenisation: uses a deep learning model (GRU) to predict for each
# token if it is the last one in a sentence, the last one of a token, not
# the last one
# 2. POS/UPOS/Morphological Features tagging: guesser makes possible
# combinations of which one is chosen based on simple neural network
# 3. Details: http://dx.doi.org/10.18653/v1/K17-3009
# For a denition of the POS tags:library(NLP)
help(Universal_POS_tags, package = "NLP")
Universal_POS_tags
Penn_Treebank_POS_tags


# Possibility 2: openNLPmodels.en/da/de/es/nl/pt/se
# I A series of models written in Apache OpenNLP
# (http://opennlp.sourceforge.net/models-1.5)
# I available as R packages at https://datacube.wu.ac.at
# install.packages(
#   openNLPmodels.en
#   , repos = "http://datacube.wu.ac.at", type = "source")
# I Gives POS tags based on the treebanks tags
# (English: https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html)

library(NLP)
library(openNLP)
library(openNLPmodels.en)
## Make sentence/word/POS annotators
sent_token_annotator <- Maxent_Sent_Token_Annotator(language = "en")
word_token_annotator <- Maxent_Word_Token_Annotator(language = "en")
pos_annotator <- Maxent_POS_Tag_Annotator(language = "en")
## Annotate the text
s <- as.String(mymovies$Plot[1])
a1 <- NLP::annotate(s, sent_token_annotator)
a2 <- NLP::annotate(s, word_token_annotator, a=a1)
a3 <- NLP::annotate(s, pos_annotator, a=a2)
a3


#Get the type of POS as a data.frame
library(data.table)
taggedwords <- subset(a3, type == "word")
tags <- data.frame(term = s[taggedwords],
                   pos.tag = sapply(taggedwords$features, FUN=function(x) head(x$POS, 1)),
                   stringsAsFactors=FALSE)
head(tags)

##### ??????????????? Lemmatization #############################################
# Lemmatisation in linguistics is the process of grouping together the
# different inected forms of a word so they can be analysed as a single
# item
# Closely related to stemming. Stemmer operates on a single word without
# knowledge of the context, and therefore cannot discriminate between
# words which have different meanings depending on part of speech.
# https://en.wikipedia.org/wiki/Lemmatisation

# Possibility 1: UDPipe (MPL license) Logic: generates possible in
# ections (lemma rules + UPOS) based
# on last 4 letters and the word prex and lets a simple neural network
# decide on which is the best based on the training data
# I How: run udpipe annotate
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
x <- udpipe_annotate(ud_model, x = mymovies$Plot[1:5])
x <- as.data.frame(x)
x[, c("token_id", "token", "lemma", "upos", "xpos", "dep_rel", "feats")]


# Possibility 2: Pattern (BSD license): Other option for Parts of Speech
# tagging + lemmatisation: use R package pattern.nlp
# I https://github.com/bnosac/pattern.nlp
# I POS tagging for Dutch, French, English, German, Spanish, Italian
# according to Penn Treebank
# I POS tagger is Brill tagger (classication tree)
# I Sentiment analysis for Dutch, French, English
library(pattern.nlp)
txt <- "Dus godvermehoeren met pus in alle puisten, 
zei die schele van Van Bukburg en hij had nog gelijk ook.
Er was toen dat liedje van tietenkonttieten kont tieten kontkontkont,
maar dat hoefden we geenseens niet te zingen"
pattern_pos(x = txt, language = 'dutch', core = TRUE)



##### ??????????????? Dependency parsing ######################################## 

# Dependency is the notion that linguistic units, e.g. words, are connected
# to each other by directed links which have a specic relation.
# I For example in the sentence 'The economy is weak but the outlook
# is bright', the term 'economy' is related to the term 'weak' as
# economy is the subject of weak.
# I Every word in a sentence is linked to a head word with a certain
# relationship. This is called dependency parsing.
# I Type of relationships are dened at
# http://universaldependencies.org/u/dep/index.html
# I Use cases: question/answering, semantic labelling, chatbots.
library(udpipe)
x <- udpipe_annotate(ud_model, "The economy is weak but the outlook is bright")
x <- as.data.frame(x)
x[, c("token_id", "token", "head_token_id", "dep_rel")]

# If you want to visualise these dependencies in R, you can do as follows.
library(igraph)
edges <- subset(x, head_token_id != 0, select = c("token_id", "head_token_id", "dep_rel"))
edges$label <- edges$dep_rel
g <- graph_from_data_frame(edges,
                           vertices = x[, c("token_id", "token", "lemma", "upos")],
                           directed = TRUE)
plot(g, vertex.label = x$token)

##### ??????????????? Text summarisation #####

# If you have annotated text with POS tags, basic keyword detection
# algorithms are
# I Cooccurrences
# I RAKE (Rapid Automatic Keyword Extraction)
# I Parts of Speech phrase sequence detection
# I Textrank (Google Pagerank based on text)
# I Collocations (ordering based Pointwise Mutual Information)
# Examples shown below on annotated client feedback on movie. Keyword
# detection is part of the udpipe R package.
# 

load("../data/mymovies_consensus.RData")
head(mymovies_consensus[, c("doc_id", "sentence_id", "token", "lemma", "upos")])



#Co-occurrence analysis

# Co-occurrences show how many times word occur together
# I Within a sentence
# I Next to each other
# I Within a range of skipgram words
library(udpipe)
## Nouns in the same sentence
cooc <- cooccurrence(subset(mymovies_consensus, upos %in% "NOUN"),
                     group = c("doc_id", "sentence_id"), term = "lemma")

## Nouns/adverbs next to each other
cooc <- cooccurrence(mymovies_consensus$lemma, 
                     relevant = mymovies_consensus$upos %in% c("NOUN", "ADJ"))

## Nouns/adverbs next to each other even if we would skip 2 words
cooc <- cooccurrence(mymovies_consensus$lemma, 
                     relevant = mymovies_consensus$upos %in% c("NOUN", "ADJ"), 
                     skipgram = 2)

##skipgram allows you to see the co-occurrence between words which are commonly separated from each other
# RAKE:
#   I keywords are dened as a sequence of words following one another
# I among the words of the candidate keywords the algorithm looks
# I how many times each word is occurring
# I and how many times it co-occurs with other words
# I each word gets a score which is the ratio of the word degree (how
#                                                                 many times it co-occurs with other words) to the word frequency
# I a RAKE score for the full candidate keyword is calculated by
# summing up the
head(cooc)

stats <- keywords_rake(x = mymovies_consensus,
                       term = "lemma", group = c("xpos", "upos"),
                       relevant = mymovies_consensus$upos %in% c("NOUN", "ADJ"))
head(subset(stats, freq > 5))

# Phrases: To detect phrases (a sequence of POS tags) one can use
# keywords phrases.
# This merely looks based on a supplied regular expression to a sequence of
# POS tags. Allows to detect
# I Noun phrases
# I Verb phrases
mymovies_consensus$phrase_tag <- as_phrasemachine(mymovies_consensus$upos, type = "upos")
stats <- keywords_phrases(x = mymovies_consensus$phrase_tag,
                          term = mymovies_consensus$token, 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*",
                          is_regex = TRUE, detailed = FALSE)
head(subset(stats, freq < 100 & ngram > 1))


# Textrank:
#   I the textrank algorithm constructs a word network.
# I A link is set up between two words if they follow one another
# I the link gets a higher weight if these 2 words occur more frequenctly
# next to each other in the text.
# I On top of the network apply Google's Pagerank to get the
# importance of each word.
# I The top 1/3 of all these words are kept and are considered relevant.
# I After this, a keywords table is constructed by combining the relevant
# words together if they appear following one another in the text.
library(textrank)
stats <- textrank_keywords(mymovies_consensus$token,
                           relevant = mymovies_consensus$upos %in% c("NOUN", "ADJ"))
stats <- subset(stats$keywords, freq > 5 & ngram > 1)
wordcloud(words = stats$keyword, freq = stats$freq, max.words = 15)


# Collocations
#   Theorem
# Collocations is a sequence of words or terms that co-occur more often
# than would be expected by chance
# https://en.wikipedia.org/wiki/Collocation
# Common measures of collocation are:
#   I PMI (pointwise mutual information): I(w1;w2) = log2( P(w1w2)
#                                                          P(w1)(w2) )
# where P(w) is the probability of a word
# I MD (mutual dependency): MD(w1;w2) == log2( P2(w1w2)
#                                              P(w1)(w2) )
# I LFMD (log-frequency biased mutual dependency):
#   DLF(w1;w2) = D(w1;w2) + log2P(w1w2)
# In R this can be accomplished with the udpipe R package or with the
# text2vec R package

stats <- keywords_collocation(x = mymovies_consensus, 
                              term = "lemma", group = c("doc_id", "sentence_id"))
head(subset(stats, freq > 5))

##  Textrank can also be used to summarise sentences:
#   I A graph is constructed where
# I the vertices of the graph represent each sentence in a document
# I and the edges between sentences are based on content overlap, by
# calculating the number of words that 2 sentences have in common
# I Use Google Pagerank to identify the most important sentences.
# I When we want to extract a summary of the text, we can now take
# only the most important sentences.

data(joboffer)
sentences <- unique(mymovies_consensus[, c("sentence_id", "sentence")])
terminology <- subset(joboffer, upos %in% c("NOUN", "ADJ"))
stats <- textrank_sentences(data = sentences,
                            terminology = terminology[, c("sentence_id", "lemma")])
stats

################### ???????????????  Entity recognition ########################################## 

# Named-entity recognition (NER) (also known as entity identication,
#                                 entity chunking and entity extraction) is a subtask of information
# extraction that seeks to locate and classify elements in text into
# pre-dened categories such as the names of persons, organizations,
# locations, expressions of times, quantities, monetary values, percentages,
# etc
# Can be done with the openNLP packages.
library(NLP)
library(openNLP)
library(openNLPmodels.en)


######### Named Entity Recognition ############

# Theorem
# Named-entity recognition (NER) (also known as entity identifcation,
#                                 entity chunking and entity extraction) is a subtask of information
# extraction that seeks to locate and classify elements in text into
# pre-defined categories such as the names of persons, organizations,
# locations, expressions of times, quantities, monetary values, percentages,
# etc
# Can be done with the openNLP packages


s <- as.String(mymovies$Plot[1])

## Need sentence and word token annotations before finding persons
sent_token_annotator <- Maxent_Sent_Token_Annotator(language = "en")
word_token_annotator <- Maxent_Word_Token_Annotator(language = "en")
personannotator <- Maxent_Entity_Annotator(language = "en", kind = "person", probs = FALSE)
a1 <- NLP::annotate(s, sent_token_annotator)
a2 <- NLP::annotate(s, word_token_annotator, a=a1)
a3 <- NLP::annotate(s, personannotator, a=a2)
a3

tags <- data.frame(term = s[a3],
                   is.person = sapply(a3$features, FUN=function(x){
                     "kind" %in% names(x) && x$kind %in% "person"
                   }),
                   stringsAsFactors=FALSE)
subset(tags, is.person == TRUE)

################### ???????????????  Sentiment analysis: ########################

# Sentiment analysis: identify and extract subjective
# information in source materials
# In it's basic form, this comes down to word
# frequencies and assigning a value to a word.
# Reference: www.cs.uic.edu/~liub/FBS/
# SentimentAnalysis-and-OpinionMining.pdf
# Generally 2 approaches:
# 1. supervised: predictive modelling where
# sentences were annotated
# 2. using a lexicon-based sentiment dictionary

library(qdap)
data(key.pol)
head(key.pol, 3)
table(key.pol$y)

# OpenNER lexicons: https://github.com/opener-project/public-sentiment-lexicons
# Sentometrics: https://github.com/sborms/sentometrics/tree/master/data-raw
# MPQA Subjectivity Lexicon: http://mpqa.cs.pitt.edu/lexicons/subj_lexicon -
#   GNU licensed
# SentiWordNet: http://sentiwordnet.isti.cnr.it - Non-commercial

# Computes sentiment scores between -1 and 1 by
# 1. identifying the words from a dictionary with
# positive/negative/ampliers/de-amplier terms
# 2. polarised words are weighted by the -1/+1.
# 3. If a valence shifter occurs in the neighbourhood, the word is
# weighted extra with the shifters
# 4. Summed up and divided by
# p
# wordcount
# 5. Constraint to put it into a -1/+1 scale:
#   ((1 􀀀 (1=(1 + exp(polarity))))  2) 􀀀 1



#Compute the sentiment score on a [􀀀1; 1] range.
oldpar <- par(no.readonly = FALSE)

mymovies2<-head(mymovies,100)

txt <- tolower(mymovies2$tomatoConsensus)
sentiments <- polarity(txt,
                       polarity.frame = qdapDictionaries::key.pol,
                       negators = qdapDictionaries::negation.words, n.before = 4, n.after = 2,
                       amplifiers = qdapDictionaries::amplification.words,
                       deamplifiers = qdapDictionaries::deamplification.words, amplifier.weight = 0.8,
                       constrain = TRUE)

## Add a column called sentiment to the data.frame
mymovies2$sentiment <- sentiments$all$polarity

## General output
sentiments$group

## Structure of the detailed output
str(sentiments$all)

# Distribution of the sentiment - standard a lot of zero's.
# Is the sentiment linked to the popularity - yes.
library(corrplot)
library(MASS)
par(mfrow = c(1, 2))
truehist(mymovies$sentiment, col = "lightblue", main = "Sentiment distribution")
m <- cor(mymovies[c("sentiment", "tomatoMeter", "tomatoRating", "imdbRating", "imdbVotes")],
         method = "pearson", use = "pairwise.complete.obs")
corrplot(m, method="number", main = "\n\nCorrelation sentiment vs movie metrics")

idx <- sample(which(sentiments$all$polarity != 0), 10)
sentiments$all[idx, c("all", "wc", "polarity", "pos.words", "neg.words")]

par(oldpar)
set.seed(123456789)

idx <- sample(which(sentiments$all$polarity != 0), 10)
sentiments$all[idx, c("all", "wc", "polarity", "pos.words", "neg.words")]

# For reference: other less advanced options for sentiment analysis with R
# 1. R package tidytext (just use a lookup dictionary, without valence
#                        shifters (ampliers/negators))
# 2. R package sentimentr https://github.com/trinker/sentimentr
# 3. R package pattern.nlp https://github.com/bnosac/pattern.nlp

####### To############ ??????????????? pic detection ########

# Objective: identify topics (combinations of words
#                             used in the text) which form a business-relevant
# topic.
# Models:
#   1. Latent Dirichlet Allocation (LDA) (package
#                                         topicmodels)
# 2. Correlated Topics Model (package
#                             topicmodels)
# 3. Other options:
#   I Non-negative matrix factorisation (package
#                                        NMF)
# I Other options: Structural Topic Model
# (package stm)
# Part of family of mixed-membership topic models where:
#   I documents are not assumed to belong to single topics, but to
# simultaneously belong to several topics
# I the topic distributions vary over documents.
# Starts with bag-of-words matrix of document X words/terms
# generative model/process for a document is dened as follows:
#   set the number of topics k as a xed a-priori
# Step 1: A topic emits words/terms ( is the term distribution of topics and
#                                      contains the probability of a word occurring in a given topic). The
# term distribution  is determined for each topic by
#  Dirichlet():
#   Step 2: To which topic does a document belong is being set by . The
# proportions  of the topic distribution for the document w are
# determined by
#   Dirichlet():
#   Step 3: For each of the N words wi
# Step 1:.1 Choose a topic zi  Multinomial().
# Step 2:.2 Choose a word wi from a multinomial probability distribution
# conditioned on the topic zi: p(wijzi; ).
# Common data preparation is
# I remove irrelevant terms (numbers, punctuation, stopwords, . . . )
# I focus on nouns or only adjectives
# I only positive/negative sentiments
# I only terms with enough frequency
# I term frequency inverse document frequency
# Where
# I tf = how many times does term appear in the document
# I idf = inverse document frequency = log(number of documents /
#                                            number of documents containing the term)
# The tf-idf value increases proportionally to the number of times a
# word appears in the document, but is oset by the frequency of the
# word in the corpus, which helps to adjust for the fact that some
# words appear more frequently in general.
# Data preparation for the document/term matrix

library(topicmodels)
library(udpipe)
library(tm)
library(slam)
txt <- tolower(mymovies$tomatoConsensus)
txt <- removeWords(txt, words = stopwords("english"))
mycorpus <- Corpus(x = VectorSource(txt))  
mycorpus <- tm_map(mycorpus, FUN=function(x) stripWhitespace(x))
## Build Document Term matrix
mydtm <- DocumentTermMatrix(mycorpus, 
                            control = list(wordLengths = c(2, Inf), 
                                           bounds = list(global = c(5, Inf))))
## tfidf
term_tfidf <- tapply(mydtm$v/row_sums(mydtm)[mydtm$i], mydtm$j, mean) * 
  log2(nrow(mydtm)/col_sums(mydtm > 0))
mydtm <- mydtm[, term_tfidf >= quantile(term_tfidf, 0.5)]
## Work only on the top 500 words if more words are occuring
termfreq <- col_sums(mydtm)
termfreq <- sort(termfreq, decreasing = TRUE)
termfreq <- head(termfreq, 500)
mydtm <- mydtm[, intersect(colnames(mydtm), names(termfreq))]
mydtm <- mydtm[, col_sums(mydtm) > 5]
mydtm <- mydtm[row_sums(mydtm) > 0, ]
dim(mydtm)

# Important parameter in the t is  which indicates if a document can
# belong to 1 topic or several ones.
# Output of LDA contains
# I 
# gamma is the posterior topic distribution of each document
# I beta is the log of the word distribution of each topic

set.seed(123456789)
m <- LDA(x = mydtm, k = 7, method = "VEM",
         control = list(alpha = 0.1))
m
slotNames(m)


# Posterior probability document belongs to topic

x <- posterior(m, newdata = mydtm)$topics
x[1:2, ]


#Words emitted by each topic

x <- posterior(m, newdata = mydtm)$terms
apply(x, MARGIN=1, FUN=function(x, top = 5){
  idx <- order(x, decreasing = TRUE)
  out <- data.frame(term = names(x)[idx], prob = x[idx])
  head(out, n = top)
})

# Evaluation of model fit + Visualisation of each topic


logLik(m)
topicmodels::perplexity(m)
terms(m, 5)

#Interactive visualisation of word emmittance.

library(LDAvis)
library(servr)
json <- createJSON(phi = posterior(m)$terms,
                   theta = posterior(m)$topics,
                   doc.length = row_sums(mydtm),
                   vocab = colnames(mydtm),
                   term.frequency = col_sums(mydtm))
serVis(json)



library(tm)
library(slam)
x <- subset(mymovies, !is.na(BoxOffice))
x$txt <- tolower(paste(x$Plot, x$tomatoConsensus, sep=" "))
xcorpus <- Corpus(VectorSource(x$txt))
mydtm <- DocumentTermMatrix(xcorpus) 
dim(mydtm)
mydtm <- mydtm[, col_sums(mydtm) > 5]
dim(mydtm)

##### Pr############## ??????????????? edictive modelling with text #####

# raw txt
# 2. data cleaning &
#   NLP
# 3. create DTM
# (document-term)
# matrix
# 4. reduce data
# 5. further modelling
# Reduction: with
# penalised regression
# Linear models estimated with least squares minimise the sum of the
# squares of the error of the t. Namely:
#   RSS =
#   Pn
# i=1(yi 􀀀 ^ yi)2
# RSS =
#   Pn
# i=1
# 
# yi 􀀀 0 􀀀
# Pp
# j=1 jxij
# !2
# Idea: add a penalty term called  to each  term
# 1. Ridge regression: penalty on the squared  values
# 2. Lasso (least absolute shrinkage): penalty on the absolute  values
# 3. Elastic net: penalty on both the squared  values and the absolute
# values

# Elastic Net:
#   Put a penalty on both the squared  values and the absolute  values
# Minimise
# Pn
# i=1
# 
# yi 􀀀0 􀀀
# Pp
# j=1 jxij
# !2
# +
# Pp
# j=1[ 1
#      2 (1􀀀)2
#      j +jj j]
# I  is the penalisation parameter
# I  is a mixing parameter between lasso and ridge.
# I The elastic net with  = 1 􀀀  for some small  > 0 performs much
# like the lasso, but removes any degeneracies and wild behavior
# caused by extreme correlations.

# In LARS/Ridge/Best subset selection.
# Minimising subject to a certain budget s for the  values.
# Best subset: no more than s coecients can be non-zero. But
# computationally infeasible
# 􀀀p
# s

# Lasso closer to best subset as it puts constraints on total  values.
# minimize
# 
# Pn
# i=1
# 
# yi 􀀀 0 􀀀
# Pp
# j=1 jxij
# !2
# subject to Ridge :
#   Pp
# j=1 2
# j  s
# Lasso :
#   Pp
# j=1 jj j  s
# Bestsubset :
#   Pp
# j=1 I(j 6= 0)  s
# Jan Wijels:
# Put a penalty on the absolute  values. This is called to the L1 norm.
# Minimise
# Pn
# i=1
# 
# yi 􀀀 0 􀀀
# Pp
# j=1 jxij
# !2
# + 
# Pp
# j=1 jj j
# Lasso tends to pick 1 of correlated variables.
# Lasso better in setting where relatively small nr of predictors with large
# coecients.
# Ridge regression better if many predictors with similar eects.
# Lasso: variable reduction. Ridge: shrinkage.
# Fit a penalised regression model

library(glmnet)
library(caret)
#hist(x$BoxOffice, col = "lightblue", breaks = 30)
mymodel <- cv.glmnet(y = log(x$BoxOffice), 
                     x = as.matrix(mydtm), 
                     family = "gaussian", nfolds = 10, alpha = 1)
plot(mymodel)

## Extract relevant predictive words
relevant <- predict(mymodel, s="lambda.min", type = "coefficients")[, 1]
relevant <- relevant[relevant != 0]
relevant <- sort(relevant)
relevant <- relevant[setdiff(names(relevant), "(Intercept)")]
## Plot relevant predictive words
par(las = 2,  mar=c(5, 7, 4, 2) + 0.1)
barplot(relevant, horiz=TRUE, cex.names = 0.75, col = "lightblue",
        main = "Regression coefficients at optimal lambda")
##Predict or benchmark new movie plots
## Use the model to predict based on an existing Document/Term matrix 
x$boxoffice.estimate <- predict(mymodel, newx = as.matrix(mydtm), 
                                s="lambda.min", type = "response")
hist(x$boxoffice.estimate, breaks = 30, col = "lightblue", xlab = "Predicted box office", 
     main = "Distribution of predicted log(box office)")
postResample(x$boxoffice.estimate, log(x$BoxOffice))


##### Text similarit############## ??????????????? y #####

# Types of analysis:
#   1. Plagiarism: Identify fraudulent (as in copy-pasted) thesis / papers
# 2. Align text where they are overlapping (like in DNA sequence
#                                           alignment)
# 3. Code comparison of your own R code to other R code
# 4. Duplicate document detection (emails, web pages) in corporate
# environments
# 5. How much do CV's match with Job descriptions
# Jaccard similarity of sets S and T is: jS \ Tj=jS [ Tj
# I You can also see how many of the words are in one text versus the
# other (ratio of matches)
# I The Jaccard bag similarity looks how many of the total number of
#  elements in S and T are overlapping.


library(textreuse)
jaccard_similarity(1:6, 3:10) # 3, 4, 5, 6 are overlapping of all 10 elements
ratio_of_matches(1:6, 3:10) # items in right which are also in left 3,4,5,6 out of 3,4,5,6,7,8,9,10
jaccard_bag_similarity(1:6, 3:10) # 4/14 elements are overlapping

textreuse:::jaccard_similarity.default
textreuse:::ratio_of_matches.default
textreuse:::jaccard_bag_similarity.default


align_local("The answer is blowin' in the wind.",
            "As the Bob Dylan song says, the answer is blowing in the wind.",
            match = 2, mismatch = -1, gap = -1,
            edit_mark = "#")


gpl2 <- readLines(url("https://www.gnu.org/licenses/gpl-2.0.txt"))
gpl3 <- readLines(url("https://www.gnu.org/licenses/gpl-3.0.txt"))
gpl2 <- paste(gpl2, collapse = " ")
gpl3 <- paste(gpl3, collapse = " ")


invisible(load("../data/mymovies.RData"))

library(text2vec)
tokens <- mymovies$tomatoConsensus %>% tolower %>% word_tokenizer
vocabulary <- create_vocabulary(it = itoken(tokens))
vocabulary <- prune_vocabulary(vocabulary, term_count_min = 8)
vectorizer <- vocab_vectorizer(vocabulary = vocabulary)
tcm <- create_tcm(it = itoken(tokens), vectorizer = vectorizer, skip_grams_window = 5)

dim(tcm)
class(tcm)

rownames(tcm)[1:5]
colnames(tcm)[1:5]

myglovemodel <- GlobalVectors$new(word_vectors_size = 50, vocabulary = vocabulary, 
                                  x_max = 10, alpha = 0.75)
myglovemodel$fit_transform(tcm, n_iter = 40)

wordvectors <- myglovemodel$components
dim(wordvectors)
head(wordvectors[, 1:5])


## Visualise evolution of cost fit over iterations
plot(myglovemodel$get_history()$cost_history)


wordvectors <- t(wordvectors)
cos_sim <- sim2(x = wordvectors, y = wordvectors["remake", , drop = FALSE], 
                method = "cosine", norm = "l2")
head(sort(cos_sim[, 1], decreasing = TRUE), 10)


cos_sim <- sim2(x = wordvectors, 
                y = wordvectors["action", , drop = FALSE] + 
                  wordvectors["zombie", , drop = FALSE], 
                method = "cosine", norm = "l2")
head(sort(cos_sim[, 1], decreasing = TRUE), 10)



