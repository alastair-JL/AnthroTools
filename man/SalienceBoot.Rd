\name{SalienceBoot}
\alias{SalienceBoot}
\title{SalienceBoot}
\usage{
SalienceBoot(data, var_sel, variables, top, threshold, iterations = 1000, seed, IDs_first = TRUE)
}
\arguments{
\item{data}{This is your data frame of item saliences (i.e., the output of 'FreeListTable(... , tableType = "MAX_SALIENCE")' function). Make sure there are no missing values.}

\item{var_sel}{Method to select variables to perform bootstrapping on. Options are: i) Manually select single or multiple variables ("MANUAL"); ii) select items on the top x (default = 8) variables with the highest Smith's S values ("TOP"); and iii) any items with Smith's S values above the specified threshold (default = 0.1; "THRESH").}

\item{variables}{The manually-specified variables to perform boot-strapping on. Either a single variable name or a vector of variable names. Only to be used when var_sel = "MANUAL".}

\item{top}{The number of variables to perform boot-strapping on (i.e., the x variables with the highest Smith's S values; default = 8). Only to be used when var_sel = "TOP".}

\item{threshold}{The minimum Smith's S threshold for inclusion in the boot-strapping process (default = 0.1). Only to be used when var_sel = "THRESH".}

\item{iterations}{The number of iterations to perform bootstrapping (default = 1,000).}

\item{seed}{Specify a seed so that results are reproducible.}

\item{IDs_first}{Specifies whether the first row of the dataset is a list of IDs (default = TRUE).}
}
\value{
The value returned is a list of vectors of these boot-strapped Smith's S estimates from each sample, for each variable selected.
}
\description{
Given a data frame of item saliences, including 0s and with no missing values, for each item listed in the sample (i.e., the output of 'FreeListTable(... , tableType = "MAX_SALIENCE")' function), performing bootstrapping on a variable/range of variables. This will randomly sample, with replacement, the observed item saliences x (default = 1,000) times, and calculate Smith's S cultural salience in each boot-strapped sample for each item. Methods to select variables are: i) Manually select single or multiple variables; ii) select items on the top x (default = 8) variables with the highest Smith's S values; and iii) any items with Smith's S values above the specified threshold (default = 0.1). This function returns a list of vectors of these boot-strapped Smith's S estimates from each sample, for each variable selected.
}
\examples{
## Generate fake free-list data about fruits
set.seed(41)
fakeData <- GenerateFakeFreeListData() 

## Calculate item salience
fakeData.s <- CalculateSalience(fakeData, Subj = "Subj", Order = "Order", CODE = "CODE", Salience = "CODE.S")

## Convert to data frame with maximum item saliences for each item as separate rows, and including 0s
fakeData.sal0 <- FreeListTable(fakeData.s, Subj = "Subj", Order = "Order", CODE = "CODE", Salience = "CODE.S", tableType = "MAX_SALIENCE")
head(fakeData.sal0)


### Example of different combinations of options

## First, calculate uncertainty in Smith's S via boot-strapping for item 'pear' using 1,000 iterations
S_boot <- SalienceBoot(fakeData.sal0, var_sel = "MANUAL", variables = "pear", iterations = 1000, seed = 182, IDs_first = TRUE)

## Summarise results
hist(S_boot$pear)
summary(S_boot$pear)
quantile(S_boot$pear, c(0.025, 0.5, 0.975))


## Next, calculate uncertainty in Smith's S via boot-strapping for items 'pear', 'peach' and 'lemon' using 1,000 iterations for each item
S_boot <- SalienceBoot(fakeData.sal0, var_sel = "MANUAL", variables = c("pear", "peach", "lemon"), iterations = 1000, seed = 182, IDs_first = TRUE)

## Summarise results
quantile(S_boot$pear, c(0.025, 0.5, 0.975))
quantile(S_boot$peach, c(0.025, 0.5, 0.975))
quantile(S_boot$lemon, c(0.025, 0.5, 0.975))


## Next, calculate uncertainty in Smith's S via boot-strapping for top 3 items in terms of Smith's S, using 1,000 iterations for each item
S_boot <- SalienceBoot(fakeData.sal0, var_sel = "TOP", top = 3, iterations = 1000, seed = 182, IDs_first = TRUE)

## Summarise results
names(S_boot)
quantile(S_boot$apple, c(0.025, 0.5, 0.975))
quantile(S_boot$banana, c(0.025, 0.5, 0.975))
quantile(S_boot$peach, c(0.025, 0.5, 0.975))


## Finally, calculate uncertainty in Smith's S via boot-strapping for any items with a Smith's S value above 0.2, using 1,000 iterations for each item
S_boot <- SalienceBoot(fakeData.sal0, var_sel = "THRESH", threshold = 0.2, iterations = 1000, seed = 182, IDs_first = TRUE)

## Summarise results
names(S_boot)
quantile(S_boot$apple, c(0.025, 0.5, 0.975))
quantile(S_boot$banana, c(0.025, 0.5, 0.975))
quantile(S_boot$peach, c(0.025, 0.5, 0.975))
quantile(S_boot$plum, c(0.025, 0.5, 0.975))

}
\author{
Daniel Major-Smith. <dan.major-smith@cas.au.dk>

Benjamin Grant Purzycki. <bgpurzycki@cas.au.dk>
}
\keyword{FreeList, Smith's S, Cultural salience, Bootstrapping}