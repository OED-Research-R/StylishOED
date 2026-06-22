% OED_CES_Table documentation

\name{OED_CES_Table}

\alias{OED_CES_Table}
\title{Download Oregon CES Tables From QualityInfo}

\usage{

OED_CES_Table(Geographies = NULL, Type = c("Annual", "SA", "NSA"))

}

\arguments{

\item{Geographies}{
Optional character vector of geography name(s) or CES series number. If \code{NULL}, the
function attempts to pull all geographies in the internal lookup table, including
Oregon, Oregon counties, and Oregon MSAs.
}

\item{Type}{
(Required) Table type to return. Requires single type to determine which tab to import into the dataframe.

\itemize{
  \item \code{"Annual"} - Returns the Annual tab.
  \item \code{"SA"} - Returns the Seasonally Adjusted monthly tab.
  \item \code{"NSA"} - Returns the Not Seasonally Adjusted monthly tab.

}
}

}

\value{

A tidy data frame with one row per geography, series, and date. The returned data
frame includes geography metadata, CES area code, table type, source sheet,
series name, date, and employment value.

Failed or empty requests are attached as an attribute named \code{"failed_requests"}.

}

\description{

Download Current Employment Statistics tables from QualityInfo.org and return the
selected table as a tidy data frame.

}

\details{

\strong{Key features:}

\itemize{

 \item Supports Oregon statewide, county, and MSA CES geographies.

 \item Allows users to filter geographies by name or CES area code.

 \item Supports Annual, Seasonally Adjusted monthly, and Not Seasonally Adjusted
 monthly tables.

 \item Converts wide CES tables into a long format with \code{Date} and
 \code{Employment} fields.

 \item Tracks failed or no-data requests in the \code{"failed_requests"} attribute.

}

}

\section{Requirements}{

\itemize{

 \item Requires the \code{readxl}, \code{dplyr}, \code{tidyr}, \code{httr2}, and
 \code{tibble} packages.

 \item Requires access to QualityInfo.org.

}

}

\examples{

\dontrun{

library(StylishOED)

# Pull annual data for all available geographies
annual_df <- OED_CES_Table(Type = "Annual")

# Pull annual data for one geography by name
corvallis_df <- OED_CES_Table(
  Geographies = c("Corvallis MSA"),
  Type = "Annual"
)

# Pull seasonally adjusted monthly data
msa_df <- OED_CES_Table(
  Geographies = c("Oregon", "Salem MSA"),
  Type = "SA"
)

# Pull not seasonally adjusted monthly data
nmsa_df <- OED_CES_Table(
  Geographies = c("4121018700"),
  Type = "NSA"
)

# View failed requests
attr(annual_df, "failed_requests")

}

}
