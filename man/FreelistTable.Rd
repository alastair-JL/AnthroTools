\name{FreelistTable}
\alias{FreelistTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
FreelistTable
}
\description{
Given a Freelist data frame (with columns for subject number, order of responses and response code), produce a table comparing each subjects responses to each code. This table can be a simple "presence/absence" table for determining if a subject mentioned a particular code, or something more complex.
}
\usage{
FreelistTable(mydata, CODE = "CODE", Salience = "Salience", Subj = "Subj", tableType = "DEFAULT")
}
\arguments{
  \item{mydata}{
This is your freelist data, stores as a data frame. 
}
  \item{CODE}{
The name of the column in which your "CODE" (eg, the subjects' individual responses) is stored.
}
  \item{Salience}{
The name of the column in which each responses Salience is stored. If you wish to ask questions of salience, it is important you calculate salience before using this function. (using \code{\link{CalculateSalience}}).
}
  \item{Subj}{
The name of the column where your subjects names/subject numbers are stored.
}
  \item{tableType}{
What type of data do you want in this table. Currently there are four types: "PRSENCE", "SUM_SALIENCE","MAX_SALIANCE" and "FREQUENCY". An entry in the presence/absence table will be one if the specified subject mentioned the specified code. It will be zero otherwise.
If you specify Frequency, then you will get a count of how often each code was mentioned by each person.
If you use "SUM_SALIENCE" then you will get the total salience each person has associated with each code. If you use "MAX_SALIANCE" then you will get the maximum salience, I.E. the salience of the code the first time it was mentioned.
}
%- maybe also 'usage' for other objects documented here.

}
\value{
The value returned is a data frame, where each row represents a subject, and each column represents one of your freelist Codes. 
Depending on "tabeType" the entries of the dataframe will either represent a frequency/ absence of salience table.
}
\author{
Alastair Jamieson-Lane.
}
\note{
What of types of tables could be useful here? 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

fakeData<- GenerateFakeFreelistData()
table<- FreelistTable(fakeData, tableType="PRESENCE")
View(table)

## The function is currently defined as
#function (mydata, CODE = "CODE", Salience = "Salience", Subj = "Subj", 
#    tableType = "DEFAULT") 
#{
#    switch(tableType, PRESENCE = {
#        doThing <- FreelistTable.Present
#    }, MAX_SALIENCE = {
#        doThing <- FreelistTable.MaxSal
#    }, SUM_SALIENCE = {
#        doThing <- FreelistTable.SumSal
#    }, FREQUENCY = {
#        doThing <- FreelistTable.freq
#    }, stop(" tableType must take a value of \"PRESENCE\",\"SUM_SALIENCE\",\"FREQUENCY\" #or \"MAX_SALIENCE\". "))
#    subjList <- unique(mydata[, Subj])
#    CODEList <- unique(mydata[, CODE])
#    df <- data.frame(matrix(ncol = length(CODEList), nrow = length(subjList)))
#    colnames(df) <- CODEList
#    rownames(df) <- subjList
#    for (iii in subjList) {
#        for (jjj in CODEList) {
#            df[iii, jjj] <- doThing(mydata, CODE = CODE, Salience = Salience, 
#                Subj = Subj, subjNum = iii, CODEnum = jjj)
#        }
#    }
#    return(df)
#  }
#}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
