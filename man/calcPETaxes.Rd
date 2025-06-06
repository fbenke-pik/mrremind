% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcPETaxes.R
\name{calcPETaxes}
\alias{calcPETaxes}
\title{Calculate SubsStationary}
\usage{
calcPETaxes(subtype = "subsidies")
}
\arguments{
\item{subtype}{subsidies rate ("subsidies") output}
}
\value{
MAgPIE object
}
\description{
Reads in the data of the source IIASA_subs_taxes, by country. and
calculate taxes at primary energy level. Regional aggregation is done via the
respective energy quantities as weights.
}
\examples{
\dontrun{
calcOutput("PETaxes")
}

}
\seealso{
\code{\link{readIIASA_subs_taxes}},
\code{\link{convertIIASA_subs_taxes}}
}
\author{
Christoph Bertram and Renato Rodrigues
}
