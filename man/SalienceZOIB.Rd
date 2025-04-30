\name{SalienceZOIB}
\alias{SalienceZOIB}
\title{SalienceZOIB}
\usage{
SalienceZOIB(data, var_sel, variables, top, threshold, print_model = TRUE, seed, 
		chains = 4, iterations = 2000, warmup = 1000, cores = 4, priors, IDs_first = TRUE)
}
\arguments{
\item{data}{This is your data frame of item saliences (i.e., the output of 'FreeListTable(... , tableType = "MAX_SALIENCE")' function). Make sure there are no missing values.}

\item{var_sel}{Method to select variables to perform ZOIB model on. Options are: i) Manually select single or multiple variables ("MANUAL"); ii) select items on the top x variables with the highest Smith's S values ("TOP"); and iii) any items with Smith's S values above the specified threshold ("THRESH").}

\item{variables}{The manually-specified variables to perform ZOIB model on. Either a single variable name or a vector of variable names. Only to be used when var_sel = "MANUAL".}

\item{top}{The number of variables to perform ZOIB model on (i.e., the x variables with the highest Smith's S values). Only to be used when var_sel = "TOP".}

\item{threshold}{The minimum Smith's S threshold for inclusion in the ZOIB model process. Only to be used when var_sel = "THRESH".}

\item{print_model}{Whether to print the model output or not (default = TRUE).}

\item{seed}{Specify a seed so that results are reproducible.}

\item{chains}{The number of chains for the model to run on (default = 4).}

\item{iterations}{The total number of iterations per chain (default = 2,000).}

\item{warmup}{The total number of warmup iterations per chain (default = 1,000).}

\item{cores}{The number of cores to run analyses on in parallel (default = 4).}

\item{priors}{Specification of priors. If no priors are specified, the default non/weakly-informative priors are used.}

\item{IDs_first}{Specifies whether the first row of the dataset is a list of IDs (default = TRUE).}
}
\value{
The value returned is a list of vectors of Smith's S estimates from each of the posterior samples, for each variable selected.
}
\description{
Given a data frame of item saliences, including 0s and no missing values, for each item listed in the sample (i.e., the output of 'FreeListTable(... , tableType = "MAX_SALIENCE")' function), run Bayesian zero-one inflated beta (ZOIB) model on a variable/range of variables. This will estimate the observed item saliences using a ZOIB model, then calculate Smith's S cultural salience in each of the posterior samples. 

Methods to select variables are: i) Manually select single or multiple variables; ii) select items on the top x variables with the highest Smith's S values; and iii) any items with Smith's S values above the specified threshold. 

Note that if there are no zeroes and/or no ones in the data, the function will adapt the ZOIB model accordingly (e.g., if there are no zeroes, then hard-coding the one-inflated term to '0', meaning "of all the zeroes and ones in the dataset, there are no ones"). 

This function returns a list of vectors of these boot-strapped Smith's S estimates from each sample, for each variable selected. This command uses the 'brms' package, and so requires both 'brms' and 'stan' to be installed. Note also that by default this function simply uses the default non-/weakly-informative priors associated with the 'brms' package.
}
\examples{
## Generate fake free-list data about fruits
set.seed(41)
fakeData <- GenerateFakeFreeListData() 

## Calculate item salience
fakeData.s <- CalculateSalience(fakeData, Subj = "Subj", Order = "Order", 
		CODE = "CODE", Salience = "CODE.S")

## Convert to data frame with maximum item saliences for each 
## item as separate rows, and including 0s
fakeData.sal0 <- FreeListTable(fakeData.s, Subj = "Subj", Order = "Order", 
		CODE = "CODE", Salience = "CODE.S", tableType = "MAX_SALIENCE")
head(fakeData.sal0)


### Example of different combinations of options

## First, calculate uncertainty in Smith's S using ZOIB model for 
## item 'apple' (using default options)
S_ZOIB <- SalienceZOIB(fakeData.sal0, var_sel = "MANUAL", 
		variables = "apple", seed = 182, IDs_first = TRUE)

## Summarise results
hist(S_ZOIB$apple)
summary(S_ZOIB$apple)
quantile(S_ZOIB$apple, c(0.025, 0.5, 0.975))


## Same example as above, but specifying more informative priors
S_ZOIB_priors <- SalienceZOIB(fakeData.sal0, var_sel = "MANUAL", 
		variables = "apple", seed = 182, IDs_first = TRUE, 
		priors = c(prior(normal(0, 1.5), class = Intercept), 
				prior(normal(0, 1), class = Intercept, dpar = phi), 
				prior(normal(0, 1.5), class = Intercept, dpar = zoi), 
				prior(normal(0, 1.5), class = Intercept, dpar = coi)))

## Summarise results
hist(S_ZOIB_priors$apple)
summary(S_ZOIB_priors$apple)
quantile(S_ZOIB_priors$apple, c(0.025, 0.5, 0.975))


## Note that care needs to be taken when specifying priors, as if 
## there are no 1s and/or 0s, and the model specification changes 
## (i.e., some elements of the ZOIB model are fixed), specifying 
## priors will result in the model not working - Will demonstrate 
## using example of 'pear', where there are no item saliences of '1'.
S_ZOIB_pear <- SalienceZOIB(fakeData.sal0, var_sel = "MANUAL", 
		variables = "pear", seed = 182, IDs_first = TRUE, 
		priors = c(prior(normal(0, 1.5), class = Intercept), 
				prior(normal(0, 1), class = Intercept, dpar = phi), 
				prior(normal(0, 1.5), class = Intercept, dpar = zoi), 
				prior(normal(0, 1.5), class = Intercept, dpar = coi)))

# But the following would work, if the prior for the COI term was removed
S_ZOIB_pear <- SalienceZOIB(fakeData.sal0, var_sel = "MANUAL", 
		variables = "pear", seed = 182, IDs_first = TRUE, 
		priors = c(prior(normal(0, 1.5), class = Intercept), 
				prior(normal(0, 1), class = Intercept, dpar = phi), 
				prior(normal(0, 1.5), class = Intercept, dpar = zoi)))

# Care therefore needs to be taken when manually specifying priors, 
## particularly when looping over multiple items as different priors 
## may be needed for different models if the parameters differ.


## Next, calculate uncertainty in Smith's S using ZOIB model for 
## items 'apple', 'peach' and 'lemon' (using default options)
S_ZOIB <- SalienceZOIB(fakeData.sal0, var_sel = "MANUAL", 
		variables = c("apple", "peach", "lemon"), 
		seed = 182, IDs_first = TRUE)

## Summarise results
quantile(S_ZOIB$apple, c(0.025, 0.5, 0.975))
quantile(S_ZOIB$peach, c(0.025, 0.5, 0.975))
quantile(S_ZOIB$lemon, c(0.025, 0.5, 0.975))


## Next, calculate uncertainty in Smith's S using ZOIB model for top 3 
## items in terms of Smith's S (using default options)
S_ZOIB <- SalienceZOIB(fakeData.sal0, var_sel = "TOP", 
		top = 3, seed = 182, IDs_first = TRUE)

## Summarise results
names(S_ZOIB)
quantile(S_ZOIB$apple, c(0.025, 0.5, 0.975))
quantile(S_ZOIB$banana, c(0.025, 0.5, 0.975))
quantile(S_ZOIB$peach, c(0.025, 0.5, 0.975))


## Finally, calculate uncertainty in Smith's S using ZOIB model for any 
## items with a Smith's S value above 0.2 (using default options)
S_ZOIB <- SalienceZOIB(fakeData.sal0, var_sel = "THRESH", 
		threshold = 0.2, seed = 182, IDs_first = TRUE)

## Summarise results
names(S_ZOIB)
quantile(S_ZOIB$apple, c(0.025, 0.5, 0.975))
quantile(S_ZOIB$banana, c(0.025, 0.5, 0.975))
quantile(S_ZOIB$peach, c(0.025, 0.5, 0.975))
quantile(S_ZOIB$plum, c(0.025, 0.5, 0.975))

}
\author{
Daniel Major-Smith. <dan.major-smith@cas.au.dk>

Benjamin Grant Purzycki. <bgpurzycki@cas.au.dk>
}
\keyword{FreeList, Smith's S, Cultural salience, Bayesian, brms, Zero-one inflated Beta}