######################################################################################
####################### AnthroTools: Some Custom Tools for Anthropology ##############
######################################################################################

### Benjamin Grant Purzycki and Alastair Jamieson-Lane prepared this script for use 
### with the accompanying article:

### Purzycki, B. G., and Jamieson-Lane, A. (2017). AnthroTools: A package for cross-cultural
### ethnographic data analysis. Cross-Cultural Research 51(1):51-74.

### Please feel free to contact us with any questions, comments, or concerns at 
### bgpurzycki@au.cas.dk or aja107@math.ubc.ca. We are happy to help develop this 
### package, so if you have any recommendations, please let us know!

### If you wish to cite this, please cite the package using the following:

### Alastair Jamieson Lane and Benjamin Grant Purzycki. (2016). AnthroTools: Some custom 
### for anthropology. R package version 0.8.

### If at any point you wish to call up the help menu, run this:

help("AnthroTools")

##########################################################################################
#################################### Installation ########################################
##########################################################################################

### To install the package, highlight the following and use CTRL+R
### if you're using a PC, or command+return if using a Mac:

install.packages("devtools")
library("devtools")
install_github('alastair-JL/AnthroTools')
library(AnthroTools)

### You might consider doing this occasionally as this will install any updates.  

##########################################################################################
######################## Analyzing Free-List Data ########################################
##########################################################################################

### Now you're ready. We'll now walk through a free-list example in the main text
### step-by-step.  We write this primarily for those new to R. 

### First, run the following:

data(FruitList)
View(FruitList)

### You just loaded and pulled up the FruitList data set. Now, you'll create a new 
### object. Let's call it "FL".

FL <- CalculateSalience(FruitList)
View(FL)

### You just ran the function "CalculateSalience". It creates a new
### data set for you with the per-item salience calculations. Here's 
### the full script for doing the same thing with the options.

FL <- CalculateSalience(FruitList, Order = "Order", Subj = "Subj", 
     CODE = "CODE", GROUPING = NA, Rescale = FALSE, Salience = "Salience")

### One helpful tool is built into the R language. It will call up only the
### first-listed items per person. Given the assumption that earlier-listed
### items are more salient (Romney & D'Andrade, 1964), this will allow for 
### a quick look at the most salient items.

FruitList[which(FruitList$Order==1), ]

### Now, for salience calculations for items, you can run the following code.
### You'll see that per item salience mean, sum, and Smith's S is your output.

FL.S <- SalienceByCode(FL, dealWithDoubles = "MAX")
View(FL.S)

### Here is the full code, with examples already plugged in. Note that if you call
### salience something else, you'll have to appropriately alter the Salience = 
### Fruist_S" part.

FL.S <- SalienceByCode(FL, Subj = "ID", CODE = "CODE", GROUPING = "Culture", 
                         Salience = "Fruits_S", dealWithDoubles = "MAX")

### What's nice about the "deadWithDoubles" argument is that you can deal with
### a common problem in free-list data. When someone lists multiples of the same
### items, you can deal with it in a variety of ways. Run help(SalienceByCode) for
### options and their description.

### The following code highlights the "GROUPING" option. This option allows you
### to run the analyses detailed above, but split by some factor of your choosing.
### It is especially ideal for cross-cultural research. In this example, say we
### collected "kinds of fruits" data from three different samples (in this case,
### we have people from "MAINLAND", an "ISLAND", and the "MOON").

data(WorldList )
View(WorldList)
WL <- CalculateSalience(WorldList , GROUPING = "GROUPING")
View(WL)
WL.S <- SalienceByCode(WL, dealWithDoubles = "MAX", GROUPING = "GROUPING")
View(WL.S)

### We also include some table functions that make data integration easier. There
### are a few options for the tableType argument:  PRESENCE, SUM_SALIENCE, MAX_SALIENCE,
### and FREQUENCY. PRESENCE reports a 1 if participants mentioned specific codes or a 0
### otherwise. FREQUENCY reports by-code frequencies, MAX_SALIENCE reports the salience of 
### the code the first time it was mentioned, while SUM_SALIENCE reports the sum salience
### each person has for each code.

FLT <- FreeListTable(dataset, CODE = "CODE", Salience = "Salience", Subj = "Subj", 
    tableType = "DEFAULT")
View(FLT)
colSums(FLT)

### Let's try two with the Fruit List data.

### The first one (FL.BIN) will be a dichotomous dataset of whether or not someone mentioned a
### certain item. The second one (FL.FREQ) is a frequency matrix of the number of times each 
### participant listed a specific item.

FL.BIN <- FreeListTable(FL, CODE = "CODE", Salience = "Salience", Subj = "Subj", 
    tableType = "PRESENCE")
View(FL.BIN)

FL.FREQ <- FreeListTable(FL, CODE = "CODE", Salience = "Salience", Subj = "Subj", 
    tableType = "FREQUENCY")
View(FL.FREQ)

##########################################################################################
###################### Cultural Consensus Analysis #######################################
##########################################################################################

### The following: 1) Calls up a mock data set, 2) Pops up the set viewer, 
### 3) Runs the Cultural Consensus Analysis on this data set, and 4) Reports
### the analysis.

data(ConsensusTestData)
View(ConsensusTestData)
Results <- ConsensusPipeline(ConsensusTestData,3)
Results

### The sections of the report are as follows. If you wish to take them segment-by
### segment, you may use these:

Results$Answers  ##This provides the culturally "correct" answers provided by the function.

Results$Competence  ##This is the estimated cultural competence for each individual.

Results$origCompetence	##This is the competence score before transformation into the [0,1] range.

Results$TestScore  ##Assuming the answer key is correct, this is the test score for each individual.

Results$Probs  ##The probability that each answer is correct for a given question.

Results$reportback ##Important feedback from the analyses

Results$reportNumbers ##	A vector containing all numerical values contained in reportback.

#########################################################################################
############################# Stress Tests ##############################################
#########################################################################################

### This is a more technical assessment of one's analysis. We've left the discussion
### for the help page and the main document. Still, these will quickly perform all
### described analyses in the main paper.

StressSummary<- ConsensusStressTest(15,15,4,5000,lockCompetance=0.6) 

### This means:  15 individuals, 15 questions, 4 possible answers A,B,C,D, 5000 surveys simulated, 
### all individuals have competence 0.6.

StressSummary[[1]] ##True and expected error rate.
mean(StressSummary[[2]]) ##The mean of the mean of calculated competence (should be near 0.6).
sum((StressSummary[[3]])>0.1)/length(StressSummary[[3]]) 

### The proportion of simulations with Competence variance calculated above 0.1.
### Note that the true value for this is 0, so all variance found is noise.

quantile(StressSummary[[3]],0.95)  

### 95% of simulations detected variance below this value- even when true variance is 0. 
### If your variance is below this level, there probably isn't much evidence for competence variability.

quantile(StressSummary[[3]],c(0.5,0.95,0.99,0.999) )     
sum(StressSummary[[4]]<3.0)

### This last number is the number of surveys with Comrey ratio less than 3- these are datasets
### that the function would refuse to analyse unless the safety override was used.
### Please understand that this is the number of "Good" datasets that the function believes are bad.
### This value tells you nothing about what the Comrey ratio is likely to look like on "bad" datasets where
### important assumptions are violated.
