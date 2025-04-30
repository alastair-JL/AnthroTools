\name{SalienceEstimateSummary}
\alias{SalienceEstimateSummary}
\title{SalienceEstimateSummary}
\usage{
SalienceEstimateSummary(data, quantiles = c(0, 0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975, 1))
}
\arguments{
\item{data}{This is a list of vectors of Smith's S estimates (i.e., the output of commands 'SalienceBoot()', 'SalienceZOIB()' or 'SalienceOrdBeta()').}

\item{quantiles}{The specific quantiles/uncertainty intervals to estimate (default = c(0, 0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975, 1)).}
}
\value{
A data frame of Smith's S estimates and uncertainty intervals for each item.
}
\description{
Given a list of vectors of Smith's S estimates, summarise each vector in the list to calculate uncertainty intervals (e.g., percentile intervals from boot-strapping, or credible intervals from Bayesian models). The input to this function is the output to commands 'FLboot_sal()', 'FLZOIB_sal()' or 'FLordBeta_sal()'.
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

## Calculate uncertainty in Smith's S via boot-strapping for top 8 items in terms of Smith's S, using 1,000 iterations for each item
S_boot <- FLboot_sal(fakeData.sal0, var_sel = "TOP", top = 8, iterations = 1000, seed = 182, IDs_first = TRUE)

## Summarise results, displaying median Smith's S estimate and 95% percentile intervals
res <- SalienceEstimateSummary(S_boot, quantiles = c(0.025, 0.5, 0.975))

}
\author{
Daniel Major-Smith. <dan.major-smith@cas.au.dk>

Benjamin Grant Purzycki. <bgpurzycki@cas.au.dk>
}
\keyword{FreeList, Smith's S, Cultural salience}