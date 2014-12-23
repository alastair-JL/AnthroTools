\name{SalienceByCode}
\alias{SalienceByCode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SalienceByCode
}
\description{
Generate a table containing saliance details for each code, including mean Salience, and total salience over the entire survey set. This function should be preceeded by "Calculate Salience"
}
\usage{
SalienceByCode(mydata, CODE = "CODE", Salience = "Salience", Subj = "Subj", dealWithDoubles = "DEFAULT")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{
A table containing a list of freelist responses. Each response gets one row, containing the subject number, CODE, and calculated Salience of their response.
}
  \item{CODE}{
The name of the column containing the "CODE" of the response.
NOTE TO SELF/BEN: I suspect I need an explaination of what "CODE" is somewhere. Or is it just assumed knowledge?
}
  \item{Salience}{
The name of the Column containing the "Salience" of a response.
}
  \item{Subj}{
The name of the column containing the subject number. 
}
  \item{dealWithDoubles}{
How you would like the function to deal with cases where a single survey respondant gave the same answer multiple times. On the default setting, the function will assume that no such cases arise, and will throw an error if it encounters such a case. Aside from DEFAULT, you also have the options MAX, SUM and IGNORE. MAX indicates that for each code, you want the computer to pay note the first time a respondant lists a particular CODE, and ignore subsequent mentions. For SUM, you are asking the computer to determine each responants TOTAL salience with respect to a given code. IGNORE tells the computer not to think about it, and is merely a way of supressing errors. IGNORE is not recommended.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Upon running this function, you will recieve a data frame, where each row is one of you possible response codes, and each column contains a different statistic about the code. 
MeanSalience is the average salience of a code, averaged over the number of responses that referenced that code. Sum Salience just gives the total amount of salience of a given code, and SmithS gives this sum, divide by the total number of respondents.

}
\references{
NOTE TO BEN: would it be good to have a reference here to give a concrete example of what we are doing?
}
\author{
Alastair Jamieson-Lane
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

test<- data(FruitList)
test<- CalculateSalience(test)
SalienceByCode(test,dealWithDoubles="MAX")

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
