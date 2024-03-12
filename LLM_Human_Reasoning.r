##Plot & Analysis script for the "Improving reasoning in Humans and Machines" process
##By subcomarc

#Make sure you set up your directories if you want to run this script!!!
#Also make sure that you have the raw data needed for this script to work. It is available in the repository (see the paper for URL).
#We are talking three files here: logscores_ALL.csv, phylo3m.csv and GPTReasoning.csv.

############### ################################
#Preloading settings, directories and libraries#
############### ################################

#housekeeping
rm(list = ls())

# setwd() # set your working directory if needed

#load libraries (DONT RUN library("httpgd") IN RSTUDIO)

library(lme4)
library('car')
library("ggplot2")
library("dplyr")
library("tidyverse")
library(stringr)
library("httpgd") # (DONT RUN IN RSTUDIO)

##Open plot interphase (DONT RUN IN RSTUDIO)
hgd()
hgd_browse()

#Before anything else, log and relabel data about logscore probabilities for all experiments
#This is a very important step, as it will allow us to compare the performance of the models in a more meaningful way
#load the data and clean it up!

dataLogSCORES <- read.csv(file="/data/logscores_ALL.csv") # CHANGE DIRECTORY TO YOUR DIRECTORY!!!

dataLogSCORES$logx <- log(dataLogSCORES$x) #so these are actually raw scores, we need to log them to have the log score
dataLogSCORES$logy <- log(dataLogSCORES$y)
dataLogSCORES$exp <- 1
dataLogSCORES[dataLogSCORES$label %in% c("crt1","crt2","crt3","crt4","crt5","crt6","crt7","new crt1","new crt2","new crt3","new crt4","new crt5","new crt6","new crt7" ),]$exp <- "CRT"
dataLogSCORES[dataLogSCORES$label %in% c("linda","bill","new linda/bill"),]$exp <- "CF"
dataLogSCORES$gen <- 1
dataLogSCORES[dataLogSCORES$label %in% c("crt1","crt2","crt3","crt4","crt5","crt6","crt7","linda","bill"),]$gen <- "Canonic"
dataLogSCORES[dataLogSCORES$label %in% c("new linda/bill","new crt1","new crt2","new crt3","new crt4","new crt5","new crt6","new crt7"),]$gen <- "New"
dataLogSCORES$NewLabels <- 1
dataLogSCORES[dataLogSCORES$label %in% c("crt1"),]$NewLabels <- "1"
dataLogSCORES[dataLogSCORES$label %in% c("new crt1"),]$NewLabels <- "1"
dataLogSCORES[dataLogSCORES$label %in% c("crt2"),]$NewLabels <- "2"
dataLogSCORES[dataLogSCORES$label %in% c("new crt2"),]$NewLabels <- "2"
dataLogSCORES[dataLogSCORES$label %in% c("crt3"),]$NewLabels <- "3"
dataLogSCORES[dataLogSCORES$label %in% c("new crt3"),]$NewLabels <- "3"
dataLogSCORES[dataLogSCORES$label %in% c("crt4"),]$NewLabels <- "4"
dataLogSCORES[dataLogSCORES$label %in% c("new crt4"),]$NewLabels <- "4"
dataLogSCORES[dataLogSCORES$label %in% c("crt5"),]$NewLabels <- "5"
dataLogSCORES[dataLogSCORES$label %in% c("new crt5"),]$NewLabels <- "5"
dataLogSCORES[dataLogSCORES$label %in% c("crt6"),]$NewLabels <- "6"
dataLogSCORES[dataLogSCORES$label %in% c("new crt6"),]$NewLabels <- "6"
dataLogSCORES[dataLogSCORES$label %in% c("crt7"),]$NewLabels <- "7"
dataLogSCORES[dataLogSCORES$label %in% c("new crt7"),]$NewLabels <- "7"
dataLogSCORES[dataLogSCORES$label %in% c("new linda/bill"),]$NewLabels <- ""
dataLogSCORES[dataLogSCORES$label %in% c("bill"),]$NewLabels <- "Bill"
dataLogSCORES[dataLogSCORES$label %in% c("linda"),]$NewLabels <- "Linda"
dataLogSCORES[dataLogSCORES$label %in% c("new linda/bill","new crt1","new crt2","new crt3","new crt4","new crt5","new crt6","new crt7"),]$gen <- "New"
dataLogSCORES$Model <- 1
dataLogSCORES[dataLogSCORES$model %in% c("dv"),]$Model <- "DV"
dataLogSCORES[dataLogSCORES$model %in% c("dvb"),]$Model <- "DVB"
dataLogSCORES[dataLogSCORES$model %in% c("dv1"),]$Model <- "DV1"
dataLogSCORES[dataLogSCORES$model %in% c("dv2"),]$Model <- "DV2"
dataLogSCORES[dataLogSCORES$model %in% c("dv3"),]$Model <- "DV3"

#Now, to load and clean up the second auxiliary dataset, with the phylogeny data from the PCA analysis of the models
#PCA analysis not included! Only results here. See github for the analysis if you really want to go that deep.
PhyloData <- read.csv(file="/data/phylo3m.csv") #new
PhyloData$model <- factor(PhyloData$model)
PhyloData$Xaxis <- PhyloData$x
PhyloData$Yaxis <- PhyloData$y
PhyloData$Zaxis <- PhyloData$z
PhyloData[PhyloData$color %in% "red",]$color <- "red3"
PhyloData[PhyloData$color %in% "grey",]$color <- "grey40"
PhyloData$size <- PhyloData$size  * 100 #play with dot size for convenient visualization


##Moving on, load data from the main dataset with the actual experiment & do some cleanup/relabelling
Data <- read.csv(file="/data/GPTReasoning.csv") # file containing the data, it is available in the repository (see the paper for URL) 
# Data <- read.csv(file="GPTReasoning20-02-2024.csv") # file containing the data, it is available in the repository (see the paper for URL) 
Data[!Data$Model %in% "human",]$ID <- as.numeric(Data[!Data$Model %in% "human",]$ID) + 1 #no participant id = 0
#remove models whose data we have but didn't use in the experiment
Data <- Data[!Data$Model %in% c("babbage-002","davinci-002","gpt-3.5-instruct-0914","chat-bison-001","CHGPT","text-bison-001"),]
#rename and crop gpt-4-0314
# Data <- Data[!Data$Model %in% c("gpt-4-turbo-0314"),]
# Data[Data$Model %in% "gpt-4-0314",]$Model <- "gpt-4-turbo-0314"
Data <- Data[!(Data$Model %in% "gpt-4-turbo-0314" & Data$Experiment %in% "old-crt"),]
#relabel the models and human participants with frendlier names
Data$Model <- factor(Data$Model, 
levels = c("bloom-7b", "code-davinci-002", "davinci", "davinci-instruct-beta", "gpt-3.5-turbo-0301", 
            "gpt-3.5-turbo-0613", "gpt-4-turbo-0314", "gpt-4-turbo-0613", "human", "llama-2-7b", 
            "opt-13b", "text-davinci-001", "text-davinci-002", "text-davinci-003", "vicuna-13b-v1.5"),
labels = c("BLOOM", "CDV2", "DV", "DVB", "ChatGPT_0301", 
            "ChatGPT_0613", "GPT4_0314", "GPT4_0613", "Homo sapiens", "LLAMA", 
            "OPT", "DV1", "DV2", "DV3", "VICUNA"))

Data$ID <- paste(Data$ID, Data$Model, sep = "")

##DATA LOADED!! NOW PLOT EACH FIGURE AND RUN ITS CORRESPONDING STATS

##FIGURE 1 - NA

##FIGURE 2 - A: scatterplot with model PCA analysis (choose dimensions manually)

ggplot()+
  geom_point(data=PhyloData, aes(y, z, fill=color, size = size), color="black", shape = 21, stroke=2, alpha = 1) +
  guides(fill = guide_legend(title = ""), size="none") +
  scale_fill_manual(values = c("grey40", "red3"), aesthetics = c("fill"), labels=c("Not included","Included")) +
  geom_text(data=PhyloData, aes(y, z, label=model), hjust=0, vjust=-1.5, size=5) +  
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="plain", colour="black", size=30, family="arial"),
        legend.position = c(0.9,0.9),
        legend.key.size = unit(4, "line"),
        strip.text.x =  element_text(angle=0, face="plain", colour="black", size="38", family="arial"),
        strip.background = element_blank(),
        panel.spacing = unit(4, "lines"), #space between plots
        # legend.title = element_text(angle=0, face="plain", colour="black", size=30, family="arial"),
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=28, family="arial"), axis.title.x=element_text(size=38, family="arial"),
        axis.text.y=element_text(size=28, family="arial"), axis.title.y=element_text(size=38, family="arial"),
        plot.title=element_text(size=22, family="arial", face="plain", color="black")) +
  labs(face="plain", family="arial",
       title = "Figure 2 A",
       x = "Principal component 2", 
       y = "Principal component 3") 


##FIGURE 2 - B: Curvature and asymptote for the Log Score models (to determine models' "level of surprise")

#Main panel (Linda/Bill)
CFLS <- dataLogSCORES %>% filter(exp %in% "CF", Model %in% "DV3")
ggplot()+
  geom_point(data=CFLS, aes(logx, logy, fill=gen), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  scale_fill_manual(values = c("#FF8C94","#A7226E"), aesthetics = c("fill")) +
  geom_text(data=CFLS, aes(logx, logy, label=NewLabels), hjust=0, vjust=-1.5, size=5) +  
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="plain", colour="black", size=22),
        legend.position = c(0.1,0.08),
        strip.text.x =  element_text(angle=0, face="plain", colour="black", size="30"),
        strip.background = element_blank(),
        panel.spacing = unit(4, "lines"), #space between plots
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=28), axis.title.x=element_text(size=30),
        axis.text.y=element_text(size=28), axis.title.y=element_text(size=30),
        plot.title=element_text(size=22, face="plain", color="black")) +
  labs(face="plain",
       title = "Figure 2 B [Large panel Linda/Bill]",
       x = "Asymptote", 
       y = "Curvature") 

#PiP panel (CRT)

CRTLS <- dataLogSCORES %>% filter(exp %in% "CRT", Model %in% "DV3")
ggplot()+
  geom_point(data=CRTLS, aes(logx, logy, fill=gen), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  guides(fill = guide_legend(title = "CRT Items")) +
  scale_x_continuous(limits = c(-2, 2), breaks = seq(-2, 2, by = 0.5)) + #order
  scale_fill_manual(values = c("deepskyblue1", "deepskyblue4"), aesthetics = c("fill")) +
  geom_text(data=CRTLS, aes(logx, logy, label=NewLabels), hjust=0, vjust=-1, size=5) +  
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="plain", colour="black", size=22),
        legend.position = c(0.1,0.08),
        strip.text.x =  element_text(angle=0, face="plain", colour="black", size="30"),
        strip.background = element_blank(),
        panel.spacing = unit(4, "lines"), #space between plots
        # legend.title = element_text(angle=0, face="plain", colour="black", size=30),
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=28), axis.title.x=element_text(size=30),
        axis.text.y=element_text(size=28), axis.title.y=element_text(size=30),
        plot.title=element_text(size=22, face="plain", color="black")) +
  labs(face="plain",
       title = "Figure 2 B [Small panel CRT per item]",
       x = "Asymptote", 
       y = "Curvature") 

##FIGURE 3 - PANEL A: CRT (new items) choice rate by model
#Get the model subset for the CRT figures that concern models up to DV3 (included) and humans
CRT <- Data %>% filter(Experiment %in% "new-crt", Model %in% c("DV","DVB","DV1","CDV2","DV2","DV3","Homo sapiens"))
#Code accuracy
CRT$Accuracy <- 0
CRT[CRT$Label %in% "correct",]$Accuracy <- 1 
CRT$Intui <- 0
CRT[CRT$Label %in% "intuitive",]$Intui <- 1 
CRT$Label <- factor(CRT$Label, labels = c("Correct","Intuitive","Other")) 
CRT$Label <- fct_relevel(CRT$Label, "Other","Intuitive","Correct") 
CRT$Model <- fct_relevel(CRT$Model, c("DV","DVB","DV1","CDV2","DV2","DV3","Homo sapiens"))
CRT$Condition<-factor(CRT$Condition, levels=c("vanilla", "example", "reasoning"),
                    labels=c("Baseline", "Example", "Reasoning"))
CRT$Condition<-fct_relevel(CRT$Condition, "Baseline", "Example", "Reasoning")
CRT$AnswLength <- nchar(CRT$Answer)
# CRT[CRT$Model %in% "Homo sapiens",]$Model <- "HS" 
CRT <- CRT %>% filter(Check %in% "main")

#Plot panel
#Prepare dataset for plot with descriptive stats (mean and bootstrapped SEM)
CRT4plot <- CRT %>%  group_by(Model, Condition) %>% summarise(MeanAccuracy=mean(Accuracy),  #GPT mathematically speaking
            AccuracySE=mean(replicate(1000, sd(sample(
            Accuracy, replace=T))/sqrt(length(Accuracy)))),
            MeanIntui=mean(Intui),
            MeanSE=mean(replicate(1000, sd(sample(
            Intui, replace=T))/sqrt(length(Intui))))
            )
CRT4plot <- CRT4plot %>% filter(Condition %in% "Baseline")
#Actually plot it
ggplot()+
  geom_errorbar(data=CRT4plot, aes(x = Model, y = MeanAccuracy, ymax= MeanAccuracy + AccuracySE, ymin= MeanAccuracy - AccuracySE), color="black", width=0.1, size=1.5)+
  geom_point(data=CRT4plot, aes(x = Model, y = MeanAccuracy, fill="Correct"), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  geom_errorbar(data=CRT4plot, aes(x = Model, y = MeanIntui, ymax= MeanIntui + MeanSE, ymin= MeanIntui - MeanSE), color="black", width=0.1, size=1.5)+
  geom_point(data=CRT4plot, aes(x = Model, y = MeanIntui, fill="Intuitive"), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  geom_line(data=CRT4plot[!CRT4plot$Model %in% "Homo sapiens",], aes(x=Model, y=MeanAccuracy, group=Condition), color="#F26B38", linewidth=1.5) +
  geom_line(data=CRT4plot[!CRT4plot$Model %in% "Homo sapiens",], aes(x=Model, y=MeanIntui, group=Condition), color="#A7226E", linewidth=1.5) + 
   scale_fill_manual(values = c("#F26B38","#A7226E"), aesthetics = c("color","fill")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + #order
  guides(fill = guide_legend(title = "Response")) +
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="plain", colour="black", size=46, family="arial"),
        legend.position = c(0.1,0.9),
        strip.text.x =  element_text(angle=90, face="plain", colour="black", size="38", family="arial"),
        strip.background = element_blank(),
        panel.spacing = unit(4, "lines"), #space between plots
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=46, family="arial", angle=0), axis.title.x=element_text(size=38, family="arial"),
        axis.text.y=element_text(size=46, family="arial"), axis.title.y=element_text(size=38, family="arial"),
        plot.title=element_text(size=22, family="arial", face="plain", color="black")) +
  labs(face="plain", family="arial",
        title = "Figure 3 Panel A",
       x = "", 
       y = "") 

#And the stats for this particular figure:
#Prepare dataset to run a glmer
CRT_GLMER <- CRT %>% pivot_longer(cols=c("Accuracy","Intui"), names_to=c("ResponseType"), values_to="Accuracy")
CRT_GLMER$ResponseType <- factor(CRT_GLMER$ResponseType, labels = c("Correct","Intuitive"))   
CRT_GLMER$Model <- factor(CRT_GLMER$Model)
CRT_GLMER$IdxQuestion <- CRT_GLMER$IdxQuestion+1
CRT_GLMER$IdxQuestion <- factor(CRT_GLMER$IdxQuestion)

#run it
CRT_GLMER_Analysis <- glmer(Accuracy ~ ResponseType*IdxQuestion + (1|ID), 
             family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)), CRT_GLMER)
#Check results
Anova(CRT_GLMER_Analysis)

##FIGURE 3 - PANEL B: CRT (new items) in purely mathematical form

#Get the model subset for the CRT figures that concern models up to DV3 (included) and humans
CRT <- Data %>% filter(Experiment %in% "crt-math", Model %in% c("DV","DVB","DV1","CDV2","DV2","DV3","Homo sapiens"))
#Code accuracy
CRT$Accuracy <- 0
CRT[CRT$Label %in% "correct",]$Accuracy <- 1 
CRT$Intui <- 0
CRT[CRT$Label %in% "intuitive",]$Intui <- 1 
CRT$Label <- factor(CRT$Label, labels = c("Correct","Intuitive","Other")) 
CRT$Label <- fct_relevel(CRT$Label, "Other","Intuitive","Correct") 
CRT$Model <- fct_relevel(CRT$Model, c("DV","DVB","DV1","CDV2","DV2","DV3","Homo sapiens"))
CRT$Condition<-factor(CRT$Condition, levels=c("vanilla", "example", "reasoning"),
                    labels=c("Baseline", "Example", "Reasoning"))
CRT$Condition<-fct_relevel(CRT$Condition, "Baseline", "Example", "Reasoning")
CRT$AnswLength <- nchar(CRT$Answer)
# CRT[CRT$Model %in% "Homo sapiens",]$Model <- "HS" 
CRT <- CRT %>% filter(Check %in% "main")

#Plot panel
#Prepare dataset for plot with descriptive stats (mean and bootstrapped SEM)
CRT4plot <- CRT %>%  group_by(Model, Condition) %>% summarise(MeanAccuracy=mean(Accuracy),  #GPT mathematically speaking
            AccuracySE=mean(replicate(1000, sd(sample(
            Accuracy, replace=T))/sqrt(length(Accuracy)))),
            MeanIntui=mean(Intui),
            MeanSE=mean(replicate(1000, sd(sample(
            Intui, replace=T))/sqrt(length(Intui))))
            )
CRT4plot <- CRT4plot %>% filter(Condition %in% "Baseline")
#Actually plot it
ggplot()+
  geom_errorbar(data=CRT4plot, aes(x = Model, y = MeanAccuracy, ymax= MeanAccuracy + AccuracySE, ymin= MeanAccuracy - AccuracySE), color="black", width=0.1, size=1.5)+
  geom_point(data=CRT4plot, aes(x = Model, y = MeanAccuracy, fill="Correct"), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  geom_errorbar(data=CRT4plot, aes(x = Model, y = MeanIntui, ymax= MeanIntui + MeanSE, ymin= MeanIntui - MeanSE), color="black", width=0.1, size=1.5)+
  geom_point(data=CRT4plot, aes(x = Model, y = MeanIntui, fill="Intuitive"), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  geom_line(data=CRT4plot[!CRT4plot$Model %in% "Homo sapiens",], aes(x=Model, y=MeanAccuracy, group=Condition), color="#F26B38", linewidth=1.5) +
  geom_line(data=CRT4plot[!CRT4plot$Model %in% "Homo sapiens",], aes(x=Model, y=MeanIntui, group=Condition), color="#A7226E", linewidth=1.5) + #for CF math
   scale_fill_manual(values = c("#F26B38","#A7226E"), aesthetics = c("color","fill")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + #order
  guides(fill = guide_legend(title = "Response")) +
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="plain", colour="black", size=46, family="arial"),
        legend.position = c(0.1,0.9),
        strip.text.x =  element_text(angle=90, face="plain", colour="black", size="38", family="arial"),
        strip.background = element_blank(),
        panel.spacing = unit(4, "lines"), #space between plots
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=46, family="arial", angle=0), axis.title.x=element_text(size=38, family="arial"),
        axis.text.y=element_text(size=46, family="arial"), axis.title.y=element_text(size=38, family="arial"),
        plot.title=element_text(size=22, family="arial", face="plain", color="black")) +
  labs(face="plain", family="arial",
        title = "Figure 3 Panel B",
       x = "", 
       y = "") 

#And the stats for this particular figure:
#Prepare dataset to run a glmer
CRT_GLMER <- CRT %>% pivot_longer(cols=c("Accuracy","Intui"), names_to=c("ResponseType"), values_to="Accuracy")
CRT_GLMER$ResponseType <- factor(CRT_GLMER$ResponseType, labels = c("Correct","Intuitive"))   
CRT_GLMER$Model <- factor(CRT_GLMER$Model)
CRT_GLMER$IdxQuestion <- CRT_GLMER$IdxQuestion+1
CRT_GLMER$IdxQuestion <- factor(CRT_GLMER$IdxQuestion)

#run it
CRT_GLMER_Analysis <- glmer(Accuracy ~ ResponseType*IdxQuestion + (1|ID), 
             family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)), CRT_GLMER)
#Check results
Anova(CRT_GLMER_Analysis)



##FIGURE 3 - PANEL C: Linda/Bill (new items) choice rate by model

#Get the model subset for the CRT figures that concern models up to DV3 (included) and humans
CF <- Data %>% filter(Experiment %in% "cf", Model %in% c("DV","DVB","DV1","CDV2","DV2","DV3","Homo sapiens"))
#Code accuracy
CF$StyleCat <- paste(str_sub(CF$Training1,3,3),
                     str_sub(CF$Hobby1,3,3), 
                     str_sub(CF$Work1,3,3), 
                     str_sub(CF$Hobby2,3,3), sep="")

CF$Accuracy <- 0
CF$Order <- ifelse(CF$Order==1, "Standard_Conjunction", "Conjunction_Standard")
CF[CF$Order %in% "Standard_Conjunction",]$Accuracy <- ifelse(CF[CF$Order %in% "Standard_Conjunction",]$Answer %in% c("a","A"), 1,0)
CF[CF$Order %in% "Conjunction_Standard",]$Accuracy <- ifelse(CF[CF$Order %in% "Conjunction_Standard",]$Answer %in% c("b","B"), 1,0)
 
CF$Model <- fct_relevel(CF$Model, c("DV","DVB","DV1","CDV2","DV2","DV3","Homo sapiens"))
CF$Condition<-factor(CF$Condition, levels=c("vanilla", "example", "reasoning"),
                    labels=c("Baseline", "Example", "Reasoning"))
CF$Type<-factor(CF$Type, levels=c("bill", "linda"),
                    labels=c("Bill", "Linda"))
CF$Condition<-fct_relevel(CF$Condition, "Baseline", "Example", "Reasoning")
CF$AnswLength <- nchar(CF$Answer)
# CRT[CRT$Model %in% "Homo sapiens",]$Model <- "HS" 
CF <- CF %>% filter(Check %in% "main")

#Plot panel
#Prepare dataset for plot with descriptive stats (mean and bootstrapped SEM)
CF4plot <- CF %>%  group_by(Model, Condition, Type) %>% summarise(MeanAccuracy=mean(Accuracy),  #GPT mathematically speaking
            AccuracySE=mean(replicate(1000, sd(sample(
            Accuracy, replace=T))/sqrt(length(Accuracy))))
            )
CF4plot <- CF4plot %>% filter(Condition %in% "Baseline")
#Actually plot it
ggplot()+
  geom_errorbar(data=CF4plot, aes(x = Model, y = MeanAccuracy, ymax= MeanAccuracy + AccuracySE, ymin= MeanAccuracy - AccuracySE, color=Type), width=0.1, size=1.5)+
  geom_point(data=CF4plot, aes(x = Model, y = MeanAccuracy, fill=Type), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  geom_line(data=CF4plot[CF4plot$Type %in% "Bill" & !CF4plot$Model %in% "Homo sapiens",], aes(x=Model, y=MeanAccuracy, group=Type), color="green4", size=1.5) +
  geom_line(data=CF4plot[CF4plot$Type %in% "Linda" & !CF4plot$Model %in% "Homo sapiens",], aes(x=Model, y=MeanAccuracy, group=Type), color="deepskyblue4", size=1.5) +
  scale_fill_manual(values = c("green4","deepskyblue4"), aesthetics = c("fill")) +
  scale_color_manual(values = c("black","black"), aesthetics = c("color")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + #order
  geom_hline(yintercept=0.5, linetype=3, size=3) +
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="plain", colour="black", size=46, family="arial"),
        legend.position = c(0.1,0.9),
        strip.text.x =  element_text(angle=90, face="plain", colour="black", size="38", family="arial"),
        strip.background = element_blank(),
        panel.spacing = unit(4, "lines"), #space between plots
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=46, family="arial", angle=0), axis.title.x=element_text(size=38, family="arial"),
        axis.text.y=element_text(size=46, family="arial"), axis.title.y=element_text(size=38, family="arial"),
        plot.title=element_text(size=22, family="arial", face="plain", color="black")) +
  labs(face="plain", family="arial",
        title = "Figure 3 Panel B",
       x = "", 
       y = "") 

#And the stats for this particular figure:
#Prepare dataset to run a glmer
CF_GLMER <- CF
CF_GLMER$Model <- factor(CF_GLMER$Model)
CF_GLMER$Type <- factor(CF_GLMER$Type)

#run it
CF_GLMER_Analysis <- glmer(Accuracy ~ Model*Type + (1|ID),
             family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)), CF_GLMER)
#Check results
Anova(CF_GLMER_Analysis)


##FIGURE 3 - PANEL D: Linda/Bill (new items) in purely mathematical form

CF <- Data %>% filter(Experiment %in% "cf-math", Model %in% c("DV","DVB","DV1","CDV2","DV2","DV3","Homo sapiens"))
#Code accuracy
CF$Accuracy <- 0
CF[CF$Label %in% "correct",]$Accuracy <- 1 
CF$Label <- factor(CF$Label, labels = c("Correct","Other")) 
CF$Label <- fct_relevel(CF$Label, "Other","Correct") 
CF$Model <- fct_relevel(CF$Model, c("DV","DVB","DV1","CDV2","DV2","DV3","Homo sapiens"))
CF$Condition<-factor(CF$Condition, levels=c("vanilla"),
                    labels=c("Baseline"))
CF <- CF %>% filter(Check %in% "main")

#Plot panel
#Prepare dataset for plot with descriptive stats (mean and bootstrapped SEM)
CF4plot <- CF %>%  group_by(Model, Condition) %>% summarise(MeanAccuracy=mean(Accuracy),  #GPT mathematically speaking
            AccuracySE=mean(replicate(1000, sd(sample(
            Accuracy, replace=T))/sqrt(length(Accuracy))))
            )
CF4plot <- CF4plot %>% filter(Condition %in% "Baseline")
#Actually plot it
ggplot()+
  geom_errorbar(data=CF4plot, aes(x = Model, y = MeanAccuracy, ymax= MeanAccuracy + AccuracySE, ymin= MeanAccuracy - AccuracySE), color="black", width=0.1, size=1.5)+
  geom_point(data=CF4plot, aes(x = Model, y = MeanAccuracy, fill="Correct"), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  geom_line(data=CF4plot[!CF4plot$Model %in% "Homo sapiens",], aes(x=Model, y=MeanAccuracy, group=Condition), color="deepskyblue4", linewidth=1.5) +
  scale_fill_manual(values = c("deepskyblue4"), aesthetics = c("color","fill")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + #order
  guides(fill = guide_legend(title = "Response")) +
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="plain", colour="black", size=46, family="arial"),
        legend.position = c(0.1,0.9),
        strip.text.x =  element_text(angle=90, face="plain", colour="black", size="38", family="arial"),
        strip.background = element_blank(),
        panel.spacing = unit(4, "lines"), #space between plots
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=46, family="arial", angle=0), axis.title.x=element_text(size=38, family="arial"),
        axis.text.y=element_text(size=46, family="arial"), axis.title.y=element_text(size=38, family="arial"),
        plot.title=element_text(size=22, family="arial", face="plain", color="black")) +
  labs(face="plain", family="arial",
        title = "Figure 3 Panel D",
       x = "", 
       y = "") 

#And the stats for this particular figure:
#Prepare dataset to run a glmer

CF[!CF$Model %in% "Homo sapiens",]$ID <- rep(c(1:(nrow(CF[!CF$Model %in% "Homo sapiens",])/3)), 
                                                times = 1, each = 3)
CF_GLMER <- CF
CF_GLMER$Model <- factor(CF_GLMER$Model)

#run it
CF_GLMER_Analysis <- glmer(Accuracy ~ Model + (1|ID),
             family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)), CF_GLMER)
#Check results
Anova(CF_GLMER_Analysis)


#FIGURE 4: PANEL A & B (DV3 & Humans, prompt-hacking, CRT)

#Get the model subset for the CRT figures that concern models up to DV3 (included) and humans
CRT <- Data %>% filter(Experiment %in% "new-crt", Model %in% c("DV3", "Homo sapiens"))
#Code accuracy
CRT$Accuracy <- 0
CRT[CRT$Label %in% "correct",]$Accuracy <- 1 
CRT$Intui <- 0
CRT[CRT$Label %in% "intuitive",]$Intui <- 1 
CRT$Label <- factor(CRT$Label, labels = c("Correct","Intuitive","Other")) 
CRT$Label <- fct_relevel(CRT$Label, "Other","Intuitive","Correct") 
CRT$Condition<-factor(CRT$Condition, levels=c("vanilla", "example", "reasoning"),
                    labels=c("Baseline", "Example", "Reasoning"))
CRT$Condition<-fct_relevel(CRT$Condition, "Baseline", "Example", "Reasoning")
CRT$AnswLength <- nchar(CRT$Answer)
# CRT[CRT$Model %in% "Homo sapiens",]$Model <- "HS" 
CRT <- CRT %>% filter(Check %in% "main")

#Plot panel
#Prepare dataset for plot with descriptive stats (mean and bootstrapped SEM)
CRT4plot <- CRT %>%  group_by(Condition, Model) %>% summarise(MeanAccuracy=mean(Accuracy),  #GPT mathematically speaking
            AccuracySE=mean(replicate(1000, sd(sample(
            Accuracy, replace=T))/sqrt(length(Accuracy)))),
            MeanIntui=mean(Intui),
            MeanSE=mean(replicate(1000, sd(sample(
            Intui, replace=T))/sqrt(length(Intui))))
            )

CRT4plot <- CRT4plot %>% pivot_longer(cols=c("MeanAccuracy","MeanIntui"), names_to=c("ResponseType"), values_to="Accuracy")  %>% pivot_longer(cols=c("AccuracySE","MeanSE"), names_to=c("SEType"), values_to="AccSE") %>% 
  filter(!(ResponseType %in% "MeanAccuracy" & SEType %in% "MeanSE")) %>% filter(!(ResponseType %in% "MeanIntui" & SEType %in% "AccuracySE"))
CRT4plot$ResponseType <- factor(CRT4plot$ResponseType, labels = c("Correct","Intuitive"))   

#Actually plot it

ggplot()+
  geom_line(data=CRT4plot[CRT4plot$Condition %in% "Example",], aes(x=ResponseType, y=Accuracy, group=Condition), color="purple3", size=1.5) +
  geom_line(data=CRT4plot[CRT4plot$Condition %in% "Reasoning",], aes(x=ResponseType, y=Accuracy, group=Condition), color="red3", size=1.5) +
  geom_errorbar(data=CRT4plot[CRT4plot$Condition %in% c("Reasoning","Example"),], aes(x = ResponseType, y = Accuracy, ymax= Accuracy + AccSE, ymin= Accuracy - AccSE), color="black", width=0.1, size=1.5)+
  geom_point(data=CRT4plot[CRT4plot$Condition %in% c("Reasoning","Example"),], aes(x = ResponseType, y = Accuracy, fill=Condition), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +  
  geom_errorbar(data=CRT4plot[CRT4plot$Condition %in% c("Baseline"),], aes(x = ResponseType, y = Accuracy, ymax= Accuracy + AccSE, ymin= Accuracy - AccSE), color="black", width=0.1, size=1.5, alpha=0.3, position=position_nudge(x=-0.15))+
  geom_point(data=CRT4plot[CRT4plot$Condition %in% c("Baseline"),], aes(x = ResponseType, y = Accuracy, fill=Condition), color="grey60", size = 6, shape = 21, stroke=2, alpha = 1, position=position_nudge(x=-0.15)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + #order
  scale_fill_manual(values = c("grey70","purple3","red3" ), aesthetics = c("fill","color")) +
  guides(fill = guide_legend(title = "Response")) +
  facet_wrap(vars(Model)) + #control for order
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="plain", colour="black", size=30, family="arial"),
        legend.position = c(0.06,0.9),
        strip.text.x =  element_text(angle=0, face="plain", colour="black", size="38", family="arial"),
        strip.background = element_blank(),
        panel.spacing = unit(4, "lines"), #space between plots
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=38, family="arial"), axis.title.x=element_text(size=38, family="arial"),
        axis.text.y=element_text(size=38, family="arial"), axis.title.y=element_text(size=38, family="arial"),
        plot.title=element_text(size=22, family="arial", face="plain", color="black")) +
  labs(face="plain", family="arial",
        title = "Figure 4: Panels A & B",
       x = "", 
       y = "") 

#And the stats for this particular figure:
#Prepare dataset to run a glmer
CRT_GLMER <- CRT %>% pivot_longer(cols=c("Accuracy","Intui"), names_to=c("ResponseType"), values_to="Accuracy")
CRT_GLMER$ResponseType <- factor(CRT_GLMER$ResponseType, labels = c("Correct","Intuitive"))   
CRT_GLMER$Model <- factor(CRT_GLMER$Model)
CRT_GLMER$IdxQuestion <- CRT_GLMER$IdxQuestion+1
CRT_GLMER$IdxQuestion <- factor(CRT_GLMER$IdxQuestion)
CRT_GLMER$Condition <- factor(CRT_GLMER$Condition)

CRT_GLMER <- CRT_GLMER %>% filter(Model %in% c("DV3","Homo sapiens"))

#run it
CRT_GLMER_Analysis <- glmer(Accuracy ~ ResponseType*Condition*Model + (1|ID), 
             family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)), CRT_GLMER)
#Check results
Anova(CRT_GLMER_Analysis)


#FIGURE 4: PANEL C & D (DV3 & Humans, prompt-hacking, CF)

#Get the model subset for the CRT figures that concern models up to DV3 (included) and humans

CF <- Data %>% filter(Experiment %in% "cf", Model %in% c("DV3","Homo sapiens"))
#Code accuracy
CF$StyleCat <-paste(str_sub(CF$Training1,3,3),
                     str_sub(CF$Hobby1,3,3), 
                     str_sub(CF$Work1,3,3), 
                     str_sub(CF$Hobby2,3,3), sep="")

CF$Accuracy <- 0
CF$Order <- ifelse(CF$Order==1, "Standard_Conjunction", "Conjunction_Standard")
CF[CF$Order %in% "Standard_Conjunction",]$Accuracy <- ifelse(CF[CF$Order %in% "Standard_Conjunction",]$Answer %in% c("a","A"), 1,0)
CF[CF$Order %in% "Conjunction_Standard",]$Accuracy <- ifelse(CF[CF$Order %in% "Conjunction_Standard",]$Answer %in% c("b","B"), 1,0)
CF$Condition<-factor(CF$Condition, levels=c("vanilla", "example", "reasoning"),
                    labels=c("Baseline", "Example", "Reasoning"))
CF$Type<-factor(CF$Type, levels=c("bill", "linda"),
                    labels=c("Bill", "Linda"))
CF$Condition<-fct_relevel(CF$Condition, "Baseline", "Example", "Reasoning")
CF$AnswLength <- nchar(CF$Answer)
# CRT[CRT$Model %in% "Homo sapiens",]$Model <- "HS" 
CF <- CF %>% filter(Check %in% "main")

#Plot panel
#Prepare dataset for plot with descriptive stats (mean and bootstrapped SEM)
CF4plot <- CF %>%  group_by(Model, Condition, Type) %>% summarise(MeanAccuracy=mean(Accuracy),  #GPT mathematically speaking
            AccuracySE=mean(replicate(1000, sd(sample(
            Accuracy, replace=T))/sqrt(length(Accuracy))))
            )

ggplot()+
  geom_line(data=CF4plot[CF4plot$Condition %in% "Example",], aes(x=Type, y=MeanAccuracy, group=Condition), color="purple3", size=1.5) +
  geom_line(data=CF4plot[CF4plot$Condition %in% "Reasoning",], aes(x=Type, y=MeanAccuracy, group=Condition), color="red3", size=1.5) +
  geom_errorbar(data=CF4plot[CF4plot$Condition %in% c("Reasoning","Example"),], aes(x = Type, y = MeanAccuracy, ymax= MeanAccuracy + AccuracySE, ymin= MeanAccuracy - AccuracySE), color="black", width=0.1, size=1.5)+
  geom_point(data=CF4plot[CF4plot$Condition %in% c("Reasoning","Example"),], aes(x = Type, y = MeanAccuracy, fill=Condition), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  geom_errorbar(data=CF4plot[CF4plot$Condition %in% c("Baseline"),], aes(x = Type, y = MeanAccuracy, ymax= MeanAccuracy + AccuracySE, ymin= MeanAccuracy - AccuracySE), color="black", width=0.1, size=1.5, alpha = 0.3, position=position_nudge(x=-0.15)) +
  geom_point(data=CF4plot[CF4plot$Condition %in% c("Baseline"),], aes(x = Type, y = MeanAccuracy, fill=Condition), color="grey60", size = 6, shape = 21, stroke=2, alpha = 1, position=position_nudge(x=-0.15)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + #order
  scale_fill_manual(values = c("grey70","purple3","red3" ), aesthetics = c("fill","color")) +
  guides(fill = guide_legend(title = "Response")) +
  facet_wrap(vars(Model)) + #control for order
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="plain", colour="black", size=30, family="arial"),
        legend.position = c(0.06,0.1),
        strip.text.x =  element_text(angle=0, face="plain", colour="black", size="38", family="arial"),
        strip.background = element_blank(),
        panel.spacing = unit(4, "lines"), #space between plots
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=38, family="arial"), axis.title.x=element_text(size=38, family="arial"),
        axis.text.y=element_text(size=38, family="arial"), axis.title.y=element_text(size=38, family="arial"),
        plot.title=element_text(size=22, family="arial", face="plain", color="black")) +
  labs(face="plain", family="arial",
       title = "Figure 4: Panels C & D",
       x = "", 
       y = "") 

#And the stats for this particular figure:
#Prepare dataset to run a glmer
CF_GLMER <- CF
CF_GLMER$Model <- factor(CF_GLMER$Model)
CF_GLMER$Type <- factor(CF_GLMER$Type)
CF_GLMER$Condition <- factor(CF_GLMER$Condition)

#run it
CF_GLMER_Analysis <- glmer(Accuracy ~ Model*Type*Condition + (1|ID),
             family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)), CF_GLMER)
#Check results
Anova(CF_GLMER_Analysis)


#FIGURE 5: PANEL A (CRT)

#Get the model subset for the CRT figures that concern from DV3 and above

CRT <- Data %>% filter(Experiment %in% "new-crt", Model %in% c("DV3","Homo sapiens", "ChatGPT_0301", "GPT4_0314"))
#Code accuracy
CRT$Accuracy <- 0
CRT[CRT$Label %in% "correct",]$Accuracy <- 1 
CRT$Intui <- 0
CRT[CRT$Label %in% "intuitive",]$Intui <- 1 
CRT$Label <- factor(CRT$Label, labels = c("Correct","Intuitive","Other")) 
CRT$Label <- fct_relevel(CRT$Label, "Other","Intuitive","Correct") 
CRT$Model <- fct_relevel(CRT$Model, c("DV3","Homo sapiens", "ChatGPT_0301", "GPT4_0314"))
CRT$Condition<-factor(CRT$Condition, levels=c("vanilla", "example", "reasoning"),
                    labels=c("Baseline", "Example", "Reasoning"))
CRT$Condition<-fct_relevel(CRT$Condition, "Baseline", "Example", "Reasoning")
CRT$AnswLength <- nchar(CRT$Answer)
# CRT[CRT$Model %in% "Homo sapiens",]$Model <- "HS" 
CRT <- CRT %>% filter(Check %in% "main")

#Plot panel
#Prepare dataset for plot with descriptive stats (mean and bootstrapped SEM)
CRT4plot <- CRT %>%  group_by(Model, Condition) %>% summarise(MeanAccuracy=mean(Accuracy),  #GPT mathematically speaking
            AccuracySE=mean(replicate(1000, sd(sample(
            Accuracy, replace=T))/sqrt(length(Accuracy)))),
            MeanIntui=mean(Intui),
            MeanSE=mean(replicate(1000, sd(sample(
            Intui, replace=T))/sqrt(length(Intui))))
            )
CRT4plot <- CRT4plot %>% filter(Condition %in% "Baseline")

ggplot()+
  geom_errorbar(data=CRT4plot, aes(x = Model, y = MeanAccuracy, ymax= MeanAccuracy + AccuracySE, ymin= MeanAccuracy - AccuracySE), color="black", width=0.1, size=1.5)+
  geom_point(data=CRT4plot, aes(x = Model, y = MeanAccuracy, fill="Correct"), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  geom_errorbar(data=CRT4plot, aes(x = Model, y = MeanIntui, ymax= MeanIntui + MeanSE, ymin= MeanIntui - MeanSE), color="black", width=0.1, size=1.5)+
  geom_point(data=CRT4plot, aes(x = Model, y = MeanIntui, fill="Intuitive"), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  scale_fill_manual(values = c("#F26B38","#A7226E"), aesthetics = c("color","fill")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + #order
  guides(fill = guide_legend(title = "Response")) +
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="plain", colour="black", size=46, family="arial"),
        legend.position = c(0.1,0.9),
        strip.text.x =  element_text(angle=90, face="plain", colour="black", size="38", family="arial"),
        strip.background = element_blank(),
        panel.spacing = unit(4, "lines"), #space between plots
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=46, family="arial", angle=0), axis.title.x=element_text(size=38, family="arial"),
        axis.text.y=element_text(size=46, family="arial"), axis.title.y=element_text(size=38, family="arial"),
        plot.title=element_text(size=22, family="arial", face="plain", color="black")) +
  labs(face="plain", family="arial",
        title = "Figure 5 Panel A",
       x = "", 
       y = "") 

#And the stats for this particular figure:
CRT_GLMER <- CRT %>% pivot_longer(cols=c("Accuracy","Intui"), names_to=c("ResponseType"), values_to="Accuracy")
CRT_GLMER$ResponseType <- factor(CRT_GLMER$ResponseType, labels = c("Correct","Intuitive"))   
CRT_GLMER$Model <- factor(CRT_GLMER$Model)
CRT_GLMER$IdxQuestion <- CRT_GLMER$IdxQuestion+1
CRT_GLMER$IdxQuestion <- factor(CRT_GLMER$IdxQuestion)

#run it
CRT_GLMER_Analysis <- glmer(Accuracy ~ ResponseType*Model + (1|ID), 
             family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)), CRT_GLMER)
#Check results
Anova(CRT_GLMER_Analysis)



#FIGURE 5: PANEL B (CF)

CF <- Data %>% filter(Experiment %in% "cf", Model %in% c("DV3","Homo sapiens", "ChatGPT_0301", "GPT4_0314"))
#Code accuracy
CF$StyleCat <- paste(str_sub(CF$Training1,3,3),
                     str_sub(CF$Hobby1,3,3), 
                     str_sub(CF$Work1,3,3), 
                     str_sub(CF$Hobby2,3,3), sep="")
CF$Accuracy <- 0
CF$Order <- ifelse(CF$Order==1, "Standard_Conjunction", "Conjunction_Standard")
CF[CF$Order %in% "Standard_Conjunction",]$Accuracy <- ifelse(CF[CF$Order %in% "Standard_Conjunction",]$Answer %in% c("a","A"), 1,0)
CF[CF$Order %in% "Conjunction_Standard",]$Accuracy <- ifelse(CF[CF$Order %in% "Conjunction_Standard",]$Answer %in% c("b","B"), 1,0)
 
CF$Model <- fct_relevel(CF$Model, c("DV3","Homo sapiens", "ChatGPT_0301", "GPT4_0314"))
CF$Condition<-factor(CF$Condition, levels=c("vanilla", "example", "reasoning"),
                    labels=c("Baseline", "Example", "Reasoning"))
CF$Type<-factor(CF$Type, levels=c("bill", "linda"),
                    labels=c("Bill", "Linda"))
CF$Condition<-fct_relevel(CF$Condition, "Baseline", "Example", "Reasoning")
CF$AnswLength <- nchar(CF$Answer)
# CRT[CRT$Model %in% "Homo sapiens",]$Model <- "HS" 
CF <- CF %>% filter(Check %in% "main")

#Plot panel
#Prepare dataset for plot with descriptive stats (mean and bootstrapped SEM)
CF4plot <- CF %>%  group_by(Model, Condition, Type) %>% summarise(MeanAccuracy=mean(Accuracy),  #GPT mathematically speaking
            AccuracySE=mean(replicate(1000, sd(sample(
            Accuracy, replace=T))/sqrt(length(Accuracy))))
            )
CF4plot <- CF4plot %>% filter(Condition %in% "Baseline")
#Actually plot it
ggplot()+
  geom_errorbar(data=CF4plot, aes(x = Model, y = MeanAccuracy, ymax= MeanAccuracy + AccuracySE, ymin= MeanAccuracy - AccuracySE, color=Type), width=0.1, size=1.5)+
  geom_point(data=CF4plot, aes(x = Model, y = MeanAccuracy, fill=Type), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  scale_fill_manual(values = c("green4","deepskyblue4"), aesthetics = c("fill")) +
  scale_color_manual(values = c("black","black"), aesthetics = c("color")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + #order
  geom_hline(yintercept=0.5, linetype=3, size=3) +
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="plain", colour="black", size=46, family="arial"),
        legend.position = c(0.1,0.9),
        strip.text.x =  element_text(angle=90, face="plain", colour="black", size="38", family="arial"),
        strip.background = element_blank(),
        panel.spacing = unit(4, "lines"), #space between plots
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=46, family="arial", angle=0), axis.title.x=element_text(size=38, family="arial"),
        axis.text.y=element_text(size=46, family="arial"), axis.title.y=element_text(size=38, family="arial"),
        plot.title=element_text(size=22, family="arial", face="plain", color="black")) +
  labs(face="plain", family="arial",
        title = "Figure 5 Panel B",
       x = "", 
       y = "") 

#And the stats for this particular figure:
#Prepare dataset to run a glmer
CF_GLMER <- CF
CF_GLMER$Model <- factor(CF_GLMER$Model)
CF_GLMER$Type <- factor(CF_GLMER$Type)

#run it
CF_GLMER_Analysis <- glmer(Accuracy ~ Model*Type + (1|ID),
             family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)), CF_GLMER)
#Check results
Anova(CF_GLMER_Analysis)


#FIGURE 6: PANEL A (CRT per item, humans and more recent models)


CRT <- Data %>% filter(Experiment %in% "new-crt", Model %in% c("DV3","Homo sapiens", "ChatGPT_0301", "GPT4_0314"))
#Code accuracy
CRT$Accuracy <- 0
CRT[CRT$Label %in% "correct",]$Accuracy <- 1 
CRT$Intui <- 0
CRT[CRT$Label %in% "intuitive",]$Intui <- 1 
CRT$Label <- factor(CRT$Label, labels = c("Correct","Intuitive","Other")) 
CRT$Label <- fct_relevel(CRT$Label, "Other","Intuitive","Correct") 
CRT$Model <- fct_relevel(CRT$Model, c("DV3","Homo sapiens", "ChatGPT_0301", "GPT4_0314"))
CRT$Condition<-factor(CRT$Condition, levels=c("vanilla", "example", "reasoning"),
                    labels=c("Baseline", "Example", "Reasoning"))
CRT$Condition<-fct_relevel(CRT$Condition, "Baseline", "Example", "Reasoning")
CRT$AnswLength <- nchar(CRT$Answer)
# CRT[CRT$Model %in% "Homo sapiens",]$Model <- "HS" 
CRT <- CRT %>% filter(Check %in% "main")

#Plot panel
#Prepare dataset for plot with descriptive stats (mean and bootstrapped SEM)
CRT4plot <- CRT %>%  group_by(Model, Condition, IdxQuestion) %>% summarise(MeanAccuracy=mean(Accuracy),  #GPT mathematically speaking
            AccuracySE=mean(replicate(1000, sd(sample(
            Accuracy, replace=T))/sqrt(length(Accuracy)))),
            MeanIntui=mean(Intui),
            MeanSE=mean(replicate(1000, sd(sample(
            Intui, replace=T))/sqrt(length(Intui))))
            )
CRT4plot <- CRT4plot %>% filter(Condition %in% "Baseline")

ggplot()+
  geom_line(data=CRT4plot, aes(x=IdxQuestion, y=MeanAccuracy, group=Condition), color="#F26B38", linewidth=1.5) +
  geom_line(data=CRT4plot, aes(x=IdxQuestion, y=MeanIntui, group=Condition), color="#A7226E", linewidth=1.5) + 
  geom_errorbar(data=CRT4plot, aes(x = IdxQuestion, y = MeanAccuracy, ymax= MeanAccuracy + AccuracySE, ymin= MeanAccuracy - AccuracySE), color="black", width=0.1, size=1.5)+
  geom_point(data=CRT4plot, aes(x = IdxQuestion, y = MeanAccuracy, fill="Correct"), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  geom_errorbar(data=CRT4plot, aes(x = IdxQuestion, y = MeanIntui, ymax= MeanIntui + MeanSE, ymin= MeanIntui - MeanSE), color="black", width=0.1, size=1.5)+
  geom_point(data=CRT4plot, aes(x = IdxQuestion, y = MeanIntui, fill="Intuitive"), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  scale_fill_manual(values = c("#F26B38","#A7226E"), aesthetics = c("color","fill")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + #order
  facet_wrap(vars(Model)) +
  guides(fill = guide_legend(title = "Response")) +
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="plain", colour="black", size=46, family="arial"),
        legend.position = c(0.1,0.9),
        strip.text.x =  element_text(angle=0, face="plain", colour="black", size="38", family="arial"),
        strip.background = element_blank(),
        panel.spacing = unit(4, "lines"), #space between plots
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=46, family="arial", angle=0), axis.title.x=element_text(size=38, family="arial"),
        axis.text.y=element_text(size=46, family="arial"), axis.title.y=element_text(size=38, family="arial"),
        plot.title=element_text(size=22, family="arial", face="plain", color="black")) +
  labs(face="plain", family="arial",
        title = "Figure 6 Panel A",
       x = "", 
       y = "") 

#And the stats for this particular figure:
CRT_GLMER <- CRT %>% pivot_longer(cols=c("Accuracy","Intui"), names_to=c("ResponseType"), values_to="Accuracy")
CRT_GLMER$ResponseType <- factor(CRT_GLMER$ResponseType, labels = c("Correct","Intuitive"))   
CRT_GLMER$Model <- factor(CRT_GLMER$Model)
CRT_GLMER$IdxQuestion <- CRT_GLMER$IdxQuestion+1
CRT_GLMER$IdxQuestion <- factor(CRT_GLMER$IdxQuestion)

#run it
CRT_GLMER_Analysis <- glmer(Accuracy ~ ResponseType*Model*IdxQuestion + (1|ID), 
             family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)), CRT_GLMER)
#Check results
Anova(CRT_GLMER_Analysis)


#FIGURE 6: PANEL B (CF per problem and sciency/artsy categories, humans and more recent models)

CF <- Data %>% filter(Experiment %in% "cf", Model %in% c("DV3","Homo sapiens", "ChatGPT_0301", "GPT4_0314"))
#Code accuracy
CF$StyleCat <- paste(str_sub(CF$Training1,3,3),
                     str_sub(CF$Hobby1,3,3), 
                     str_sub(CF$Work1,3,3), 
                     str_sub(CF$Hobby2,3,3), sep="")

CF$Accuracy <- 0
CF$Order <- ifelse(CF$Order==1, "Standard_Conjunction", "Conjunction_Standard")
CF[CF$Order %in% "Standard_Conjunction",]$Accuracy <- ifelse(CF[CF$Order %in% "Standard_Conjunction",]$Answer %in% c("a","A"), 1,0)
CF[CF$Order %in% "Conjunction_Standard",]$Accuracy <- ifelse(CF[CF$Order %in% "Conjunction_Standard",]$Answer %in% c("b","B"), 1,0)
 
CF$Model <- fct_relevel(CF$Model, c("DV3","Homo sapiens", "ChatGPT_0301", "GPT4_0314"))
CF$Condition<-factor(CF$Condition, levels=c("vanilla", "example", "reasoning"),
                    labels=c("Baseline", "Example", "Reasoning"))
CF$Type<-factor(CF$Type, levels=c("bill", "linda"),
                    labels=c("Bill", "Linda"))
CF$Condition<-fct_relevel(CF$Condition, "Baseline", "Example", "Reasoning")
CF$StyleCat<-fct_relevel(CF$StyleCat, "AAAS", "SSSA", "SSAS","AASA")
CF$AnswLength <- nchar(CF$Answer)
# CRT[CRT$Model %in% "Homo sapiens",]$Model <- "HS" 
CF <- CF %>% filter(Check %in% "main")

#Plot panel
#Prepare dataset for plot with descriptive stats (mean and bootstrapped SEM)
CF4plot <- CF %>%  group_by(Model, Type, Condition, StyleCat) %>% summarise(MeanAccuracy=mean(Accuracy),  #GPT mathematically speaking
            AccuracySE=mean(replicate(1000, sd(sample(
            Accuracy, replace=T))/sqrt(length(Accuracy))))
            )
CF4plot <- CF4plot %>% filter(Condition %in% "Baseline")
#Actually plot it
ggplot()+
  geom_errorbar(data=CF4plot, aes(x = StyleCat, y = MeanAccuracy, ymax= MeanAccuracy + AccuracySE, ymin= MeanAccuracy - AccuracySE, color=Type), width=0.1, size=1.5)+
  geom_point(data=CF4plot, aes(x = StyleCat, y = MeanAccuracy, fill=Type), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  scale_fill_manual(values = c("green4","deepskyblue4"), aesthetics = c("fill")) +
  scale_color_manual(values = c("black","black"), aesthetics = c("color")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + #order
  geom_hline(yintercept=0.5, linetype=3, size=3) +
  facet_wrap(vars(Model)) + #control for order
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="plain", colour="black", size=46, family="arial"),
        legend.position = c(0.1,0.9),
        strip.text.x =  element_text(angle=0, face="plain", colour="black", size="38", family="arial"),
        strip.background = element_blank(),
        panel.spacing = unit(4, "lines"), #space between plots
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=46, family="arial", angle=0), axis.title.x=element_text(size=38, family="arial"),
        axis.text.y=element_text(size=46, family="arial"), axis.title.y=element_text(size=38, family="arial"),
        plot.title=element_text(size=22, family="arial", face="plain", color="black")) +
  labs(face="plain", family="arial",
        title = "Figure 6 Panel B",
       x = "", 
       y = "") 

#And the stats for this particular figure:
#Prepare dataset to run a glmer
CF_GLMER <- CF
CF_GLMER$Model <- factor(CF_GLMER$Model)
CF_GLMER$StyleCat <- factor(CF_GLMER$StyleCat)

#run it
CF_GLMER_Analysis <- glmer(Accuracy ~ Model*StyleCat + (1|ID),
             family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)), CF_GLMER)
#Check results
Anova(CF_GLMER_Analysis)


#FIGURE 7: PANEL A (CRT same models, different dates)


CRT <- Data %>% filter(Experiment %in% "new-crt", Model %in% c("ChatGPT_0301", "GPT4_0314", "ChatGPT_0613", "GPT4_0613"))
#Code accuracy
CRT$Accuracy <- 0
CRT[CRT$Label %in% "correct",]$Accuracy <- 1 
CRT$Intui <- 0
CRT[CRT$Label %in% "intuitive",]$Intui <- 1 
CRT$Label <- factor(CRT$Label, labels = c("Correct","Intuitive","Other")) 
CRT$Label <- fct_relevel(CRT$Label, "Other","Intuitive","Correct") 
CRT$Model <- fct_relevel(CRT$Model, c("DV3","Homo sapiens", "ChatGPT_0301", "GPT4_0314"))
CRT$Condition<-factor(CRT$Condition, levels=c("vanilla", "example", "reasoning"),
                    labels=c("Baseline", "Example", "Reasoning"))
CRT$Condition<-fct_relevel(CRT$Condition, "Baseline", "Example", "Reasoning")
CRT$AnswLength <- nchar(CRT$Answer)
# CRT[CRT$Model %in% "Homo sapiens",]$Model <- "HS" 
CRT <- CRT %>% filter(Check %in% "main")
CRT$Family <- factor(CRT$Family)
CRT$Model <- fct_relevel(CRT$Model, c("ChatGPT_0301","ChatGPT_0613", "GPT4_0314", "GPT4_0613"))


#Plot panel
#Prepare dataset for plot with descriptive stats (mean and bootstrapped SEM)
CRT4plot <- CRT %>%  group_by(Model, Condition, Family) %>% summarise(MeanAccuracy=mean(Accuracy),  #GPT mathematically speaking
            AccuracySE=mean(replicate(1000, sd(sample(
            Accuracy, replace=T))/sqrt(length(Accuracy)))),
            MeanIntui=mean(Intui),
            MeanSE=mean(replicate(1000, sd(sample(
            Intui, replace=T))/sqrt(length(Intui))))
            )
CRT4plot <- CRT4plot %>% filter(Condition %in% "Baseline")

ggplot()+
  geom_line(data=CRT4plot, aes(x=Model, y=MeanAccuracy, group=Family), color="#F26B38", linewidth=1.5) +
  geom_line(data=CRT4plot, aes(x=Model, y=MeanIntui, group=Family), color="#A7226E", linewidth=1.5) + 
  geom_errorbar(data=CRT4plot, aes(x = Model, y = MeanAccuracy, ymax= MeanAccuracy + AccuracySE, ymin= MeanAccuracy - AccuracySE), color="black", width=0.1, size=1.5)+
  geom_point(data=CRT4plot, aes(x = Model, y = MeanAccuracy, fill="Correct"), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  geom_errorbar(data=CRT4plot, aes(x = Model, y = MeanIntui, ymax= MeanIntui + MeanSE, ymin= MeanIntui - MeanSE), color="black", width=0.1, size=1.5)+
  geom_point(data=CRT4plot, aes(x = Model, y = MeanIntui, fill="Intuitive"), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  scale_fill_manual(values = c("#F26B38","#A7226E"), aesthetics = c("color","fill")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + #order
#   facet_wrap(vars(Model)) +
  guides(fill = guide_legend(title = "Response")) +
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="plain", colour="black", size=46, family="arial"),
        legend.position = c(0.1,0.9),
        strip.text.x =  element_text(angle=0, face="plain", colour="black", size="38", family="arial"),
        strip.background = element_blank(),
        panel.spacing = unit(4, "lines"), #space between plots
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=46, family="arial", angle=0), axis.title.x=element_text(size=38, family="arial"),
        axis.text.y=element_text(size=46, family="arial"), axis.title.y=element_text(size=38, family="arial"),
        plot.title=element_text(size=22, family="arial", face="plain", color="black")) +
  labs(face="plain", family="arial",
        title = "Figure 7 Panel A",
       x = "", 
       y = "") 

#And the stats for this particular figure:
CRT_GLMER <- CRT %>% pivot_longer(cols=c("Accuracy","Intui"), names_to=c("ResponseType"), values_to="Accuracy")
CRT_GLMER$ResponseType <- factor(CRT_GLMER$ResponseType, labels = c("Correct","Intuitive"))   
CRT_GLMER$Model <- factor(CRT_GLMER$Model)
CRT_GLMER$IdxQuestion <- CRT_GLMER$IdxQuestion+1
CRT_GLMER$IdxQuestion <- factor(CRT_GLMER$IdxQuestion)

#run it
CRT_GLMER_Analysis <- glmer(Accuracy ~ ResponseType*Model + (1|ID), 
             family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)), CRT_GLMER)
#Check results
Anova(CRT_GLMER_Analysis)

#FIGURE 7: PANEL B (CF same models, different dates)

CF <- Data %>% filter(Experiment %in% "cf", Model %in% c("ChatGPT_0301","ChatGPT_0613", "GPT4_0314", "GPT4_0613"))
#Code accuracy
CF$StyleCat <- paste(str_sub(CF$Training1,3,3),
                     str_sub(CF$Hobby1,3,3), 
                     str_sub(CF$Work1,3,3), 
                     str_sub(CF$Hobby2,3,3), sep="")

CF$Accuracy <- 0
CF$Order <- ifelse(CF$Order==1, "Standard_Conjunction", "Conjunction_Standard")
CF[CF$Order %in% "Standard_Conjunction",]$Accuracy <- ifelse(CF[CF$Order %in% "Standard_Conjunction",]$Label %in% c("a","A"), 1,0)
CF[CF$Order %in% "Conjunction_Standard",]$Accuracy <- ifelse(CF[CF$Order %in% "Conjunction_Standard",]$Label %in% c("b","B"), 1,0)
 
CF$Model <- fct_relevel(CF$Model, c("DV3","Homo sapiens", "ChatGPT_0301", "GPT4_0314"))
CF$Condition<-factor(CF$Condition, levels=c("vanilla", "example", "reasoning"),
                    labels=c("Baseline", "Example", "Reasoning"))
CF$Type<-factor(CF$Type, levels=c("bill", "linda"),
                    labels=c("Bill", "Linda"))
CF$Condition<-fct_relevel(CF$Condition, "Baseline", "Example", "Reasoning")
CF$StyleCat<-fct_relevel(CF$StyleCat, "AAAS", "SSSA", "SSAS","AASA")
CF$AnswLength <- nchar(CF$Answer)
# CRT[CRT$Model %in% "Homo sapiens",]$Model <- "HS" 
CF <- CF %>% filter(Check %in% "main")
CF$Family <- factor(CF$Family)
CF$Model <- fct_relevel(CF$Model, c("ChatGPT_0301","ChatGPT_0613", "GPT4_0314", "GPT4_0613"))


#Plot panel
#Prepare dataset for plot with descriptive stats (mean and bootstrapped SEM)
CF4plot <- CF %>%  group_by(Model, Type, Condition) %>% summarise(MeanAccuracy=mean(Accuracy),  #GPT mathematically speaking
            Family=Family,
            AccuracySE=mean(replicate(1000, sd(sample(
            Accuracy, replace=T))/sqrt(length(Accuracy))))
            ) %>% distinct()
CF4plot <- CF4plot %>% filter(Condition %in% "Baseline")
#Actually plot it
ggplot()+
  geom_line(data=CF4plot[CF4plot$Type %in% "Bill",], aes(x=Model, y=MeanAccuracy, group=Family), color="green4", size=1.5) +
  geom_line(data=CF4plot[CF4plot$Type %in% "Linda",], aes(x=Model, y=MeanAccuracy, group=Family), color="deepskyblue4", size=1.5) +
  geom_errorbar(data=CF4plot, aes(x = Model, y = MeanAccuracy, ymax= MeanAccuracy + AccuracySE, ymin= MeanAccuracy - AccuracySE, color=Type), width=0.1, size=1.5)+
  geom_point(data=CF4plot, aes(x = Model, y = MeanAccuracy, fill=Type), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  scale_fill_manual(values = c("green4","deepskyblue4"), aesthetics = c("fill")) +
  scale_color_manual(values = c("black","black"), aesthetics = c("color")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + #order
  geom_hline(yintercept=0.5, linetype=3, size=3) +
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="plain", colour="black", size=46, family="arial"),
        legend.position = c(0.1,0.9),
        strip.text.x =  element_text(angle=0, face="plain", colour="black", size="38", family="arial"),
        strip.background = element_blank(),
        panel.spacing = unit(4, "lines"), #space between plots
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=46, family="arial", angle=0), axis.title.x=element_text(size=38, family="arial"),
        axis.text.y=element_text(size=46, family="arial"), axis.title.y=element_text(size=38, family="arial"),
        plot.title=element_text(size=22, family="arial", face="plain", color="black")) +
  labs(face="plain", family="arial",
        title = "Figure 7 Panel B",
       x = "", 
       y = "") 

#And the stats for this particular figure:
#Prepare dataset to run a glmer
CF_GLMER <- CF
CF_GLMER$Model <- factor(CF_GLMER$Model)

#run it
CF_GLMER_Analysis <- glmer(Accuracy ~ Model + (1|ID),
             family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)), CF_GLMER)
#Check results
Anova(CF_GLMER_Analysis)

#FIGURE 7: PANEL C (CRT models outside OpenAI)


CRT <- Data %>% filter(Experiment %in% "new-crt", Model %in% c("OPT", "BLOOM", "LLAMA", "VICUNA"))
#Code accuracy
CRT$Accuracy <- 0
CRT[CRT$Label %in% "correct",]$Accuracy <- 1 
CRT$Intui <- 0
CRT[CRT$Label %in% "intuitive",]$Intui <- 1 
CRT$Label <- factor(CRT$Label, labels = c("Correct","Intuitive","Other")) 
CRT$Label <- fct_relevel(CRT$Label, "Other","Intuitive","Correct") 
CRT$Model <- fct_relevel(CRT$Model, c("DV3","Homo sapiens", "ChatGPT_0301", "GPT4_0314"))
CRT$Condition<-factor(CRT$Condition, levels=c("vanilla", "example", "reasoning"),
                    labels=c("Baseline", "Example", "Reasoning"))
CRT$Condition<-fct_relevel(CRT$Condition, "Baseline", "Example", "Reasoning")
CRT$AnswLength <- nchar(CRT$Answer)
# CRT[CRT$Model %in% "Homo sapiens",]$Model <- "HS" 
CRT <- CRT %>% filter(Check %in% "main")
CRT$Family <- factor(CRT$Family)
CRT$Model <- fct_relevel(CRT$Model, c("OPT", "BLOOM", "LLAMA", "VICUNA"))


#Plot panel
#Prepare dataset for plot with descriptive stats (mean and bootstrapped SEM)
CRT4plot <- CRT %>%  group_by(Model, Condition) %>% summarise(MeanAccuracy=mean(Accuracy),  #GPT mathematically speaking
            AccuracySE=mean(replicate(1000, sd(sample(
            Accuracy, replace=T))/sqrt(length(Accuracy)))),
            MeanIntui=mean(Intui),
            MeanSE=mean(replicate(1000, sd(sample(
            Intui, replace=T))/sqrt(length(Intui))))
            )
CRT4plot <- CRT4plot %>% filter(Condition %in% "Baseline")

ggplot()+

  geom_errorbar(data=CRT4plot, aes(x = Model, y = MeanAccuracy, ymax= MeanAccuracy + AccuracySE, ymin= MeanAccuracy - AccuracySE), color="black", width=0.1, size=1.5)+
  geom_point(data=CRT4plot, aes(x = Model, y = MeanAccuracy, fill="Correct"), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  geom_errorbar(data=CRT4plot, aes(x = Model, y = MeanIntui, ymax= MeanIntui + MeanSE, ymin= MeanIntui - MeanSE), color="black", width=0.1, size=1.5)+
  geom_point(data=CRT4plot, aes(x = Model, y = MeanIntui, fill="Intuitive"), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  scale_fill_manual(values = c("#F26B38","#A7226E"), aesthetics = c("color","fill")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + #order
#   facet_wrap(vars(Model)) +
  guides(fill = guide_legend(title = "Response")) +
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="plain", colour="black", size=46, family="arial"),
        legend.position = c(0.1,0.9),
        strip.text.x =  element_text(angle=0, face="plain", colour="black", size="38", family="arial"),
        strip.background = element_blank(),
        panel.spacing = unit(4, "lines"), #space between plots
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=46, family="arial", angle=0), axis.title.x=element_text(size=38, family="arial"),
        axis.text.y=element_text(size=46, family="arial"), axis.title.y=element_text(size=38, family="arial"),
        plot.title=element_text(size=22, family="arial", face="plain", color="black")) +
  labs(face="plain", family="arial",
        title = "Figure 7 Panel C",
       x = "", 
       y = "") 

#And the stats for this particular figure:
CRT_GLMER <- CRT %>% pivot_longer(cols=c("Accuracy","Intui"), names_to=c("ResponseType"), values_to="Accuracy")
CRT_GLMER$ResponseType <- factor(CRT_GLMER$ResponseType, labels = c("Correct","Intuitive"))   
CRT_GLMER$Model <- factor(CRT_GLMER$Model)
CRT_GLMER$IdxQuestion <- CRT_GLMER$IdxQuestion+1
CRT_GLMER$IdxQuestion <- factor(CRT_GLMER$IdxQuestion)

#run it
CRT_GLMER_Analysis <- glmer(Accuracy ~ ResponseType*Model + (1|ID), 
             family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)), CRT_GLMER)
#Check results
Anova(CRT_GLMER_Analysis)

#FIGURE 7: PANEL D (CF models outside OpenAI)

CF <- Data %>% filter(Experiment %in% "cf", Model %in% c("OPT", "BLOOM", "LLAMA", "VICUNA"))
#Code accuracy
CF$StyleCat <- paste(str_sub(CF$Training1,3,3),
                     str_sub(CF$Hobby1,3,3), 
                     str_sub(CF$Work1,3,3), 
                     str_sub(CF$Hobby2,3,3), sep="")

CF$Accuracy <- 0
CF$Order <- ifelse(CF$Order==1, "Standard_Conjunction", "Conjunction_Standard")
CF[CF$Order %in% "Standard_Conjunction",]$Accuracy <- ifelse(CF[CF$Order %in% "Standard_Conjunction",]$Label %in% c("a","A"), 1,0)
CF[CF$Order %in% "Conjunction_Standard",]$Accuracy <- ifelse(CF[CF$Order %in% "Conjunction_Standard",]$Label %in% c("b","B"), 1,0)
 
CF$Model <- fct_relevel(CF$Model, c("DV3","Homo sapiens", "ChatGPT_0301", "GPT4_0314"))
CF$Condition<-factor(CF$Condition, levels=c("vanilla", "example", "reasoning"),
                    labels=c("Baseline", "Example", "Reasoning"))
CF$Type<-factor(CF$Type, levels=c("bill", "linda"),
                    labels=c("Bill", "Linda"))
CF$Condition<-fct_relevel(CF$Condition, "Baseline", "Example", "Reasoning")
CF$StyleCat<-fct_relevel(CF$StyleCat, "AAAS", "SSSA", "SSAS","AASA")
CF$AnswLength <- nchar(CF$Answer)
# CRT[CRT$Model %in% "Homo sapiens",]$Model <- "HS" 
CF <- CF %>% filter(Check %in% "main")
CF$Family <- factor(CF$Family)
CF$Model <- fct_relevel(CF$Model, c("OPT", "BLOOM", "LLAMA", "VICUNA"))


#Plot panel
#Prepare dataset for plot with descriptive stats (mean and bootstrapped SEM)
CF4plot <- CF %>%  group_by(Model, Type, Condition) %>% summarise(MeanAccuracy=mean(Accuracy), 
            Family=Family,
            AccuracySE=mean(replicate(1000, sd(sample(
            Accuracy, replace=T))/sqrt(length(Accuracy))))
            ) %>% distinct()
CF4plot <- CF4plot %>% filter(Condition %in% "Baseline")
#Actually plot it
ggplot()+
  geom_errorbar(data=CF4plot, aes(x = Model, y = MeanAccuracy, ymax= MeanAccuracy + AccuracySE, ymin= MeanAccuracy - AccuracySE, color=Type), width=0.1, size=1.5)+
  geom_point(data=CF4plot, aes(x = Model, y = MeanAccuracy, fill=Type), color="black", size = 6, shape = 21, stroke=2, alpha = 1) +
  scale_fill_manual(values = c("green4","deepskyblue4"), aesthetics = c("fill")) +
  scale_color_manual(values = c("black","black"), aesthetics = c("color")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) + #order
  geom_hline(yintercept=0.5, linetype=3, size=3) +
  theme_bw() +
  theme(legend.text = element_text(angle=0, face="plain", colour="black", size=46, family="arial"),
        legend.position = c(0.1,0.9),
        strip.text.x =  element_text(angle=0, face="plain", colour="black", size="38", family="arial"),
        strip.background = element_blank(),
        panel.spacing = unit(4, "lines"), #space between plots
        legend.title = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line=element_line(size = 1), 
        panel.border=element_blank(), 
        axis.text.x=element_text(size=46, family="arial", angle=0), axis.title.x=element_text(size=38, family="arial"),
        axis.text.y=element_text(size=46, family="arial"), axis.title.y=element_text(size=38, family="arial"),
        plot.title=element_text(size=22, family="arial", face="plain", color="black")) +
  labs(face="plain", family="arial",
        title = "Figure 7 Panel D",
       x = "", 
       y = "") 


#And the stats for this particular figure:
#Prepare dataset to run a glmer
CF_GLMER <- CF
CF_GLMER$Model <- factor(CF_GLMER$Model)

#run it
CF_GLMER_Analysis <- glmer(Accuracy ~ Model + (1|ID),
             family=binomial, control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 1000000)), CF_GLMER)
#Check results
Anova(CF_GLMER_Analysis)