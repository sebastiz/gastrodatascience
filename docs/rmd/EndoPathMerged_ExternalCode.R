library(dplyr)
library(stringr)
library(fuzzyjoin)
rpts<-10000
#Now create the endoscopy reports:
Endoscopist<-list(x1="Dr Jonny Begood",x2="Dr Elvis Presley",x3="Dr Bilbo Baggins",x4="Dr Elmo Fudd",x5="Dr Jimminey Cricket",x6="Dr Davy Jones",x7="Dr Bugs Bunny",x8="Dr Rara Rasputin",x9="Dr Chubby Checker",x10="Dr Frank Sinatra",x11="Dr Charles Dickens",x12="Dr Joseph Conrad",x13="Dr Florence Nightingale",x14="Dr Sal Addin",x15="Dr King Richard III")

Midazolam<-list(x="1mg",x="2mg",x="3mg",x="4mg",x="5mg",x="6mg",x="7mg",x="8mg")

Fentanyl<-list(x="12.5mcg",x="25mcg",x="50mcg",x="75mcg",x="100mcg",x="125mcg",x="150mcg")

Indication<-list(x1="Therapeutic- Dilatation",x2="Other-",x3="Follow-up ULCER HEALING",x4="Haematemesis or Melaena/Blood PR",x5="Previous OGD ? 8 months ago",x6="Dysphagia/Odynophagia",x7="Surveillance-Barrett's",x8="Nausea and/or Vomiting",x9="Weight Loss",x10="Dysphagia/intermittent for a few months",x11="Other-",x12="Small Bowel Biopsy",x13="Dyspepsia",x14="Reflux-like Symptoms/Atypical Chest Pain",x15="chronic abdo pain and constipaton",x16="Oesophagus- Dysplasia",x17="Therapeutic- RFA")

Diagnosis<-list(x1="Ulcer- Oesophageal. ",x2="Post chemo-radiotherapy stricture ",x3="Possible achalasia.",x4="Oesophagitis. ",x5="Food bolus obstructing the oesophagus.",x6="Hiatus Hernia. ",x7="Extensive neoplastic looking esophageal lesion. ",x8="Esophageal candidiasis ",x9="Barretts oesophagus. ",x10="Gastritis")

Endodat<-sample(seq(as.Date('2013/01/01'), as.Date('2017/05/01'), by="day"), replace=T,rpts)
EndoHospNum<-sample(c("P433224","P633443","K522332","G244224","S553322","D0739033","U873352","P223333","Y763634","I927282","P223311","P029834","U22415","U234252","S141141","O349253","T622722","J322909","F630230","T432452"), rpts, replace = TRUE)
#Yes I know... This was just easier..
BarrettsLength<-c("C0M1","C0M2","C0M3","C0M4","C0M5","C0M6","C0M7","C0M8","C0M9","C0M10","C1M2","C1M3","C1M4","C1M5","C1M6","C1M7","C1M8","C1M9","C1M10","C2M3","C2M4","C2M5","C2M6","C2M7","C2M8","C2M9","C2M10","C3M4","C3M5","C3M6","C3M7","C3M8","C3M9","C3M10","C4M5","C4M6","C4M7","C4M8","C4M9","C4M10","C5M6","C5M7","C5M8","C5M9","C5M10","C6M7","C6M8","C6M9")

#Merge them all together into a dataframe
Endoscopies<-data.frame(EndoHospNum,replicate(rpts,paste("Date of Procedure",sample(Endodat,1,replace=F)," Endoscopist: ",sample(Endoscopist,1,replace=F),"Midazolam: ",sample(Midazolam,1,replace=F),"Fentanyl: ",sample(Fentanyl,1,replace=F),"Indication:",sample(Indication,1,replace=F),"Diagnosis:",str_c(sample(Diagnosis,sample(1:10,1),replace=F),collapse='.'),sample(c("",paste("Barrett's oesophagus length:",sample(BarrettsLength,1))),1))))
#Lets rename the one column to something more intelligent
names(Endoscopies)<-c("HospNum_Id","EndoReports")
######### Data accordionisation
# Convert into paragraphs so can be more easily separated


Endoscopies$Date<-str_extract(Endoscopies$EndoReports, 'Date of Procedure.*Endoscopist')
Endoscopies$Endoscopist<-str_extract(Endoscopies$EndoReports, 'Endoscopist:.*Midazolam')
Endoscopies$Midazolam<-str_extract(Endoscopies$EndoReports, 'Midazolam:.*Fentanyl')
Endoscopies$Fentanyl<-str_extract(Endoscopies$EndoReports, 'Fentanyl:.*Indication')
Endoscopies$Indication<-str_extract(Endoscopies$EndoReports, 'Indication:.*Diagnosis')
Endoscopies$Diagnosis<-str_extract(Endoscopies$EndoReports, 'Diagnosis:.*')
Endoscopies$BarrC<-str_extract(Endoscopies$EndoReports, " oesophagus length: C.*M.*")
Endoscopies$BarrM<-str_extract(Endoscopies$BarrC, 'M.*')


######### Data cleaning Endoscopy dataset and formatting the columns
Endoscopies$Date<-gsub("Date of Procedure","",Endoscopies$Date)
#Note we are using the date conversion function here
Endoscopies$Date<-as.Date(gsub(" Endoscopist","",Endoscopies$Date),format="%Y-%m-%d")

Endoscopies$Endoscopist<-gsub("Endoscopist:  Dr ","",Endoscopies$Endoscopist)
Endoscopies$Endoscopist<-gsub("Midazolam","",Endoscopies$Endoscopist)

Endoscopies$Midazolam<-gsub("Midazolam: ","",Endoscopies$Midazolam)
#Also reformatting this column into a nueric column at the same time
Endoscopies$Midazolam<-as.numeric(gsub("mg Fentanyl","",Endoscopies$Midazolam))

Endoscopies$Fentanyl<-gsub("Fentanyl: ","",Endoscopies$Fentanyl)
#Also reformatting this column into a nueric column at the same time
Endoscopies$Fentanyl<-as.numeric(gsub("mcg Indication","",Endoscopies$Fentanyl))

Endoscopies$Indication<-gsub("Indication: ","",Endoscopies$Indication)
Endoscopies$Indication<-gsub(" Diagnosis","",Endoscopies$Indication)

Endoscopies$Diagnosis<-gsub("Indication: ","",Endoscopies$Diagnosis)
Endoscopies$Diagnosis<-gsub(" Diagnosis","",Endoscopies$Diagnosis)

Endoscopies$BarrC<-gsub("oesophagus length: ","",Endoscopies$BarrC)
#Also reformatting this column into a nueric column at the same time
Endoscopies$BarrC<-gsub("M.*","",Endoscopies$BarrC)
Endoscopies$BarrC<-as.numeric(gsub("C","",Endoscopies$BarrC))
#Also reformatting this column into a nueric column at the same time
Endoscopies$BarrM<-as.numeric(gsub("M","",Endoscopies$BarrM))




######### Data acquisition
#For the purposes of this example we will generate the data for both the Endoscopy and the histopathology

#Generate a load of strings
line <- list(x1 = "Intestinal metaplasia is present.", x2 = "Basal hyperplasia is prominent",x3="There is no dysplasia or malignancy.",x4="No Helicobacter are seen.",x5="There is some ulceration.",x6="There is no intercellular oedema in the surface epithelium.",x7=" PAS staining shows occasional spores, consistent with candida.",x8=" No herpetic viral inclusions are seen.",x9=" There is no dysplasia and no invasive carcinoma.",x10=" There is mild regenerative epithelial change, but neither dysplasia nor malignancy is seen.",x11="The appearances are consistent with the endoscopic diagnosis of Barrett's oesophagus with active chronic inflammation.",x12="The biopsies of oesophageal squamous mucosa show surface erosion and active chronic inflammation.",x13="Numerous Candida spores and hyphae are present admixed with ulcer slough.",x14="There is reactive basal cell hyperplasia and mild inflammatory epithelial atypia.",x15="There is no significant increase in intraepithelial eosinophils.",x16="No granulomas or viral inclusions are seen.",x17="The appearances are those of Candida oesophagitis.",x18="Neither dysplasia nor malignancy is seen.",x19="The appearances are consistent with, but not specific for Barrett's (columnar lined) oesophagus.",x20="High grade dysplasia is present throughout this sample",x21="There is low grade dysplasia",x22="This is a dysplastic sample")

list.of.samples <- replicate(rpts, paste("Macrosopic description:",sample(1:10,1), "specimens collected the largest measuring", sample(1:5,1) ,"x", sample(1:5,1) ,"x", sample(1:5,1), "mm and the smallest", sample(1:5,1) ,"x", sample(1:5,1) ,"x", sample(1:5,1), "mm"), simplify=FALSE)

library('stringr')
#Merge the strings together randomly
histop<-replicate(rpts,paste (sample(list.of.samples,1,replace=F),paste("Diagnoses",str_c(sample(line,sample(3:10,1),replace=F),collapse='.'))))   

#Because we eventually will merge histopath and endoscopy together we are going to be crafty and generate the histopath dates from the endoscopy dates with 0-2 days difference
dat<-Endoscopies$Date+sample(0:2,1)
dat<-sample(seq(as.Date('2013/01/01'), as.Date('2017/05/01'), by="day"), rpts,replace=T)
#Generate hospital numbers from the Endoscopies report
HospNum_Id<-Endoscopies$HospNum_Id

Histop_df<-data.frame(HospNum_Id,dat,paste("Date received:",dat,histop))
names(Histop_df)<-c("HospNum_Id","dat","HistoReport")







######### Data accordionisation
# Convert into paragraphs so can be more easily separated
Histop_df$Date<-str_extract(Histop_df$HistoReport, 'Date received:.*Macrosopic description:')
Histop_df$Macro<-str_extract(Histop_df$HistoReport, 'Macrosopic description:.*Diagnoses')
Histop_df$Diagnoses<-str_extract(Histop_df$HistoReport, 'Diagnoses.*')
######### Data cleaning Histopathology dataset and formatting the columns
Histop_df$Date<-gsub("Date received: ","",Histop_df$Date)
Histop_df$Date<-as.Date(gsub("Macrosopic description:","",Histop_df$Date),format="%Y-%m-%d")

Histop_df$Macro<-gsub("Macrosopic description: ","",Histop_df$Macro)
Histop_df$Macro<-gsub("Diagnoses","",Histop_df$Macro)

Histop_df$Diagnoses<-gsub("Diagnoses","",Histop_df$Diagnoses)
#Lets get rid of a column we don't need
Histop_df$dat<-NULL






######### Data merging
#We can merge straight away as we have the same names for the columns date and HospNum_Id so no need to mess around. We will use the fuzzyjoin method as there is sometimes a gap between the endoscopy date and the date that the histopathology was received:


#Rename the columns so can do the join
colnames(Endoscopies)[grepl("HospNum_Id", colnames(Endoscopies))] <- "EndoHospNumId"
colnames(Histop_df)[grepl("HospNum_Id", colnames(Histop_df))] <- "Histop_dfHospNumId"
EndoHistoMerge <- fuzzyjoin::difference_full_join(Endoscopies, Histop_df, by = 'Date', max_dist = 2, distance_col = 'Days') %>%
  filter(EndoHospNumId == Histop_dfHospNumId) 



