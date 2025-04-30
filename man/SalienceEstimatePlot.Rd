\name{SalienceEstimatePlot}
\alias{SalienceEstimatePlot}
\title{SalienceEstimatePlot}
\usage{
SalienceEstimatePlot(data, order = "high-low", manual_order)
}
\arguments{
\item{data}{This is a list of vectors of Smith's S estimates (i.e., commands 'SalienceBoot()', 'SalienceZOIB()' or 'SalienceOrdBeta()').}

\item{order}{String to denote the order in which to display items on the plot. Options are: "high-low" (from highest to lowest; the default), "low-high" (from lowest to highest), "alpha" (alphabetically), and "manual" (manually specify order - see 'manual_order' option).}

\item{manual_order}{A vector of variable names specifying the order in which to display items on the plot. Only to be used if 'order = "manual"' (else this option is ignored).}
}
\value{
A series of distributions of Smith's S estimates for each item, along with 80% and 95% uncertainty intervals.
}
\description{
Plotting function to generate density plots of Smith's S estimates over a range of items. The input to this function is the output to commands 'SalienceBoot()', 'SalienceZOIB()' or 'SalienceOrdBeta()'; i.e., a list of vectors of Smith's S estimates. Note that this command relies on the 'tidyverse', 'ggplot2' and 'ggdist' packages - Make sure that these are installed and loaded.
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

## Calculate uncertainty in Smith's S via boot-strapping for top 6 items in terms of Smith's S, using 1,000 iterations for each item
S_boot <- SalienceBoot(fakeData.sal0, var_sel = "TOP", top = 6, iterations = 1000, seed = 182, IDs_first = TRUE)

## Plot of these results
# Ranked from highest to lowest
SalienceEstimatePlot(S_boot, order = "high-low")

# And using manual specification
SalienceEstimatePlot(S_boot, order = "manual", manual_order = c("apple", "banana", "pear", "orange", "peach", "plum", "lemon"))

}
\author{
Daniel Major-Smith. <dan.major-smith@cas.au.dk>

Benjamin Grant Purzycki. <bgpurzycki@cas.au.dk>
}
\keyword{FreeList, Smith's S, Cultural salience}