\name{SalienceContrastGen}
\alias{SalienceContrastGen}
\title{SalienceContrastGen}
\usage{
SalienceContrastGen(data, contrast = "absolute_diff")
}
\arguments{
\item{data}{This is a list of vectors of Smith's S estimates for a range of items items (i.e., commands 'SalienceBoot()', 'SalienceZOIB()' or 'SalienceOrdBeta()').}

\item{contrast}{String to denote whether to calculate contrasts on the absolute difference scale ("absolute_diff"; e.g., a 0.2 unit difference in Smith's S estimates) or on the ratio scale ("ratio_diff"; e.g., item x has double the Smith's S cultural salience of item y). Absolute difference is the default.}
}
\value{
A list of vectors for the contrasts in Smith's S between each item.
}
\description{
Given a list of vectors of Smith's S estimates, calculate contrasts between each item. Contrasts are either on the absolute difference scale (e.g., a 0.2 unit difference in Smith's S estimates) or on the ratio scale (e.g., item x has double the Smith's S cultural salience of item y). The input to this function is the output to commands 'SalienceBoot()', 'SalienceZOIB()' or 'SalienceOrdBeta()'.
}
\examples{
## Generate fake free-list data about fruits
set.seed(41)
fakeData <- GenerateFakeFreeListData() 

## Calculate item salience
fakeData.s <- CalculateSalience(fakeData, Subj = "Subj", Order = "Order", 
		CODE = "CODE", Salience = "CODE.S")

## Convert to data frame with maximum item saliences for each item as 
## separate rows, and including 0s
fakeData.sal0 <- FreeListTable(fakeData.s, Subj = "Subj", Order = "Order", 
		CODE = "CODE", Salience = "CODE.S", tableType = "MAX_SALIENCE")
head(fakeData.sal0)

## Calculate uncertainty in Smith's S via boot-strapping for top 6 items 
## in terms of Smith's S, using 1,000 iterations for each item
S_boot <- SalienceBoot(fakeData.sal0, var_sel = "TOP", top = 6, 
		iterations = 1000, seed = 182, IDs_first = TRUE)

## Contrasts between each item (on absolute difference scale)
S_contrasts <- SalienceContrastGen(S_boot, contrast = "absolute_diff")

}
\author{
Daniel Major-Smith. <dan.major-smith@cas.au.dk>

Benjamin Grant Purzycki. <bgpurzycki@cas.au.dk>
}
\keyword{FreeList, Smith's S, Cultural salience}