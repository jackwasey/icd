% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/real.R
\name{is_leaf}
\alias{is_leaf}
\alias{is_leaf.icd9}
\alias{is_leaf.icd10cm}
\alias{is_leaf.icd10}
\alias{is_leaf.icd9cm}
\alias{is_leaf.default}
\title{Determine whether codes are billable leaf-nodes}
\usage{
is_leaf(x, short_code = guess_short(x), ...)

\method{is_leaf}{icd9}(x, short_code = guess_short(x), ...)

\method{is_leaf}{icd10cm}(x, short_code = guess_short(x), ...)

\method{is_leaf}{icd10}(x, short_code = guess_short(x), ...)

\method{is_leaf}{icd9cm}(x, short_code = guess_short(x), ...)

\method{is_leaf}{default}(x, short_code = guess_short(x), ...)
}
\arguments{
\item{x}{input vector to test}

\item{short_code}{single logical value which determines whether the ICD-9
code provided is in short (\code{TRUE}) or decimal (\code{FALSE}) form.
Where reasonable, this is guessed from the input data.}

\item{...}{arguments passed on to other functions}
}
\value{
logical vector of same length as input
}
\description{
Codes provided are compared to the most recent edition of the CMS list of
billable codes, or another edition if specified.
}
\section{Methods (by class)}{
\itemize{
\item \code{icd9}: Which of the given ICD-9 codes are leaf nodes in
ICD-9-CM. Currently assumes ICD-9 codes are ICD-9-CM

\item \code{icd10cm}: Which of the given ICD-10 codes are leaf nodes in
ICD-10-CM. Currently assumes ICD-10 codes are ICD-10-CM

\item \code{icd10}: Which of the given ICD-10 codes are leaf nodes in
ICD-10-CM. Currently assumes ICD-10 codes are ICD-10-CM

\item \code{icd9cm}: Which of the given ICD-9 codes are leaf nodes in
ICD-9-CM

\item \code{default}: Which of the given ICD codes are leaf nodes in
ICD version (9 vs 10) guessed from the codes themselves.
}}

\keyword{internal}
