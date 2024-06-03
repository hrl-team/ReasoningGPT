# ReasoningGPT

This repository contains relevant information for replicating figures in the paper "Improving reasoning in Humans and Machines".
https://www.nature.com/articles/s44271-024-00091-8

## Plot & Analysis script (in R)
##By subcomarc

#Make sure you set up your directories if you want to run this script!!!

#Also make sure that you have the raw data needed for this script to work. It is available in the repository (see the paper for URL).

#We are talking two files here: logscores_ALL.csv, phylo3m.csv and GPTReasoning-12-03-2024-v3.csv.

The analysis script includes some cleaning and tidying up features. After these are run, the result is a dataframe suitable for analysis.
This dictionary includes information of the CLEANED dataframes.

###VERY IMPORTANT#####

Make sure that you have updated versions of the following libraries installed

- lme4
- car
- ggplot2
- dplyr
- tidyverse
- stringr
- "httpgd" # (DONT INSTALL IF YOU INTEND TO RUN THIS CODE IN RSTUDIO)

DATAFRAME 1: dataLogSCORES

'data.frame':   5200 obs. of  12 variables:
- X        : int  0 1 2 3 4 5 6 7 8 9 ... ---> row numeration
- x        : num  1.13 6.27 13.56 19.35 14.69 ... ---> asymptote (model fitted on token probability)
- y        : num  6.536 0.877 0.348 0.219 0.305 ...  ---> curvature (model fitted on token probability)
- color    : chr  "blue" "blue" "red" "red" ...  ---> color code for figure
- label    : chr  "linda" "bill" "new linda/bill" "new linda/bill" ...  ---> identifier for the old (classic) version of the problem and the new version produced by us
- model    : chr  "dv" "dv" "dv" "dv" ... ---> model identifyier
- logx     : num  0.119 1.835 2.607 2.963 2.687 ...  ---> log of asymptote (model fitted on token probability)
- logy     : num  1.877 -0.131 -1.056 -1.52 -1.186 ... ---> log of curvature (model fitted on token probability)
- exp      : chr  "CF" "CF" "CF" "CF" ... ---> problem identifier (CF conjunction fallacy, CRT cognitive reflection test)
- gen      : chr  "Canonic" "Canonic" "New" "New" ...   ---> identifier for the old (classic) version of the problem and the new version produced by us
- NewLabels: chr  "Linda" "Bill" "" "" ...   ---> Type of problem identifier (Linda, Bill, each item of the CRT)
- Model    : chr  "DV" "DV" "DV" "DV" ...   ---> model identifyier
 
 
 DATAFRAME 2: PhyloData
 
 'data.frame':   12 obs. of  10 variables:
- X    : int  0 1 2 3 4 5 6 7 8 9 ... ---> row numeration
- x    : num  23 -29.6 -66.6 -84.1 -76.6 ...  ---> PCA analysis on phylogeny data, value for PCA1
- y    : num  -72.5 -70.6 -52.8 -22.2 -32 ...  ---> PCA analysis on phylogeny data, value for PCA2
- z    : num  -53 -27.7 -11.9 -14.9 -14.1 ...  ---> PCA analysis on phylogeny data, value for PCA3
- color: chr  "grey40" "grey40" "grey40" "red3" ...  ---> color code for figure
- size : num  6734 17233 36254 68881 68881 ...  ---> size code for figure (based on model complexity)
- model: Factor w/ 12 levels "AD","AD1","BB",..: 1 3 6 8 12 2 4 7 9 5 ...  ---> model identifyier
- Xaxis: num  23 -29.6 -66.6 -84.1 -76.6 ...  ---> same as above
- Yaxis: num  -72.5 -70.6 -52.8 -22.2 -32 ...  ---> same as above
- Zaxis: num  -53 -27.7 -11.9 -14.9 -14.1 ...  ---> same as above
 
 DATAFRAME 3: Data
 
 'data.frame':   100092 obs. of  25 variables:
- ID         : chr  "1DV" "1DV" "1DV" "1DV" ... ---> Participant ID numeration
- Model      : Factor w/ 15 levels "BLOOM","CDV2",..: 3 3 3 3 3 3 3 3 3 3 ...  ---> Model identifier
- Condition  : chr  "Baseline" "Baseline" "Baseline" "Baseline" ... ---> Condition identifier
- Question   : chr  "A scarf costs 210€ more than a hat. The scarf and the hat cost 220€ in total. How much does the hat cost?" "How long would it take 80 carpenters to repair 80 tables, if it takes 8 carpenters 8 hours to repair 8 tables?" "An entire forest was consumed by a
wildfire in 40 hours, with its size doubling every hour. How long did it take to burn 50% of the forest?" "If Andrea can clean a house in 3 hours, and Alex can clean a house in 6 hours, how many hours would it take for them to clean a house together?" ... ---> Content of item
- IdxQuestion: num  0 1 2 3 4 5 6 0 1 2 ... ---> Index of CRT questions.
- Answer     : chr  "110€\n\n" " 8 hours.\n\n" "The fire burned half the forest in 82 hours.\n\n\n\n" "4 hours\n\n" ...  ---> Content of answer
- Label      : chr  "other" "correct" "other" "other" ... ---> Category of answer
- IdxExample : num  NA NA NA NA NA NA NA NA NA NA ... ---> Index of example
- Order      : num  NA NA NA NA NA NA NA NA NA NA ... ---> Prompt order
- Style      : chr  "" "" "" "" ... ---> Artsy, Sciency, per clause
- Hobby1     : chr  "" "" "" "" ...  ---> Clause: Hobby 1
- Hobby2     : chr  "" "" "" "" ...  ---> Clause: Hobby 2
- Work1      : chr  "" "" "" "" ...  ---> Clause: Work 1
- Training1  : chr  "" "" "" "" ...  ---> Clause: Training 1
- ProbA      : logi  NA NA NA NA NA NA ...  ---> Probability A
- ProbB      : logi  NA NA NA NA NA NA ...  ---> Probability B
- Name       : chr  "" "" "" "" ...  ---> Name of actor in prompt/problem
- Question2  : chr  "" "" "" "" ...  ---> Clause: Hobby 1
- Context    : chr  "" "" "" "" ...  ---> Context
- Type       : chr  "" "" "" "" ...  ---> Type of problem
- Answer2    : chr  "" "" "" "" ...  ---> Auxiliary answer column
- Label2     : num  NA NA NA NA NA NA NA NA NA NA ...  ---> Auxiliary label column
- Family     : chr  "gpt-3" "gpt-3" "gpt-3" "gpt-3" ...  ---> Model family
- Experiment : chr  "new-crt" "new-crt" "new-crt" "new-crt" ...  ---> Experiment type
- Check      : chr  "main" "main" "main" "main" ...  ---> Main trials or catch trials?

## Data acquisition pipeline (in Python)

Coming soon !
