\name{FlowerPlotIntervals}
\alias{FlowerPlotIntervals}
\title{FlowerPlotIntervals}
\usage{
FlowerPlotIntervals(SmithsS, S_uncert, label, lower_int = "2.5\%", upper_int = "97.5\%")
}
\arguments{
\item{SmithsS}{This is a dataset of Smith's S values for all items in the data set, normally used as input to the 'FlowerPlot()' command (i.e., the output of the 'SalienceByCode()' command). Note that the first column of this dataset must be the item codes.}

\item{S_uncert}{This is a dataset summarising the uncertainty estimates across a range of items in the dataset (e.g., the output of the 'SalienceEstimateSummary()' command on the top 8 items with highest Smith's S values). Note that a minimum of 8 items is necessary for the flower-plot to display correctly.}

\item{label}{The label for the centre of the flower-plot.}

\item{lower_int}{The column name for the lower uncertainty interval in the 'S_uncert' object (default = "2.5\%").}

\item{upper_int}{The column name for the upper uncertainty interval in the 'S_uncert' object (default = "97.5\%").}
}
\value{
A flowerplot with uncertainty estimates in the 'petals'.
}
\description{
Plotting function to generate flower-plots of Smith's S estimates to top 8 items with highest cultural salience, adding uncertainty intervals into the 'petals'.
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
S_boot <- SalienceBoot(fakeData.sal0, var_sel = "TOP", top = 8, iterations = 1000, seed = 182, IDs_first = TRUE)

## Summarise results, displaying median Smith's S estimate and 95% percentile intervals
res <- SalienceEstimateSummary(S_boot, quantiles = c(0.025, 0.5, 0.975))

## Calculate Smith's S using the 'fakeData.s' dataset (this is the output used for standard flower-plots in AnthroTools)
fake.S <- SalienceByCode(fakeData.s, Subj = "Subj", CODE = "CODE", Salience = "CODE.S", dealWithDoubles = "MAX")

## Create flowerplot with uncertainty estimates - First, make margins wider so looks nicer
par(mar = c(0, 0, 0, 0))
FlowerPlotIntervals(SmithsS = fake.S, S_uncert = res, label = "Fruits")

## Or, if wanted to display 89% uncertainty intervals
res <- SalienceEstimateSummary(S_boot, quantiles = c(0.055, 0.5, 0.945))
par(mar = c(0, 0, 0, 0))
FlowerPlotIntervals(SmithsS = fake.S, S_uncert = res, label = "Fruits", lower_int = "5.5\%", upper_int = "94.5\%")

}
\author{
Daniel Major-Smith. <dan.major-smith@cas.au.dk>

Benjamin Grant Purzycki. <bgpurzycki@cas.au.dk>
}
\keyword{FreeList, Smith's S, Cultural salience}
