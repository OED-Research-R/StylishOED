% OED_Export_HiChart documentation
\name{OED_Export_HiChart}
\alias{OED_Export_HiChart}
\title{Export Highcharts HTML From R}
\usage{
OED_Export_HiChart(hc_chart, .file, .title, .alt_text, .chart_num = "1", .custom_map = FALSE)
}
\value{
A formatted HTML file is generated and saved in the same folder as the article file.
}
\description{
Export a Highcharts (highcharter) object to an HTML file for use on QualityInfo.org.
This helper function is designed to recognize different chart types and extract the
minimum script needed to generate a dynamic chart, using CSS classes and structure
aligned with those used on QualityInfo.org.
}
\details{
\strong{Key features:}
\itemize{
  \item Accepts a Highcharts object and pulls all options stored in JSON.
  \item Assumes QualityInfo.org already has \code{highcharts.js} loaded, so no script
        tags are needed in the document header.
  \item Prompts for naming the output file (no need to type \code{.html}).
  \item Set \code{.custom_map = TRUE} if the chart will use a custom QualityInfo map. 
  \item Saves an HTML file styled for QualityInfo.org, with CSS classes integrated for
        OED management.
  \item Can include skip logic or accessibility attributes to improve screen reader behavior.
}
}
\section{Requirements}{
\itemize{
  \item Requires the \code{rstudioapi} and \code{htmltools} packages.
  \item Users should review the Division style guide to ensure all colors and contrasts
        meet accessibility requirements.
}
}
\examples{
\dontrun{
# Example: export a Highcharts object to HTML
library(StylishOED)

# Assuming hc_chart is a highcharter object created elsewhere
OED_Export_HiChart(hc_chart)

# You may also skip the prompts by setting the options manually.
OED_Export_HiChart(
  hc_chart = hc_map,
  .file = "file1",
  .title = "Title of Chart or Map",
  .alt_text = "Description of what is being visualized.",
  .chart_num = "1",
  .custom_map = TRUE
)

}
}
