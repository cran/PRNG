% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PRNG.R
\name{linear_con}
\alias{linear_con}
\title{Linear congruence map}
\usage{
linear_con(x0)
}
\arguments{
\item{x0}{seed value}
}
\value{
the map gives an integer ax+b mod(n)
}
\description{
the map is a member  of the family of the maps f(x)=(ax+b) mod(n)
}
\examples{
linear_con(5)


}
