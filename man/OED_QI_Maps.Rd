\name{OED_QI_Maps}
\alias{OED_QI_Maps}
\title{Load a QualityInfo Custom Map for Highcharts}
\usage{
OED_QI_Maps(custom_map_number, join_by)
}
\arguments{
  \item{custom_map_number}{
    Integer identifying the QualityInfo custom GeoJSON map to use. Valid
    values are 1 through 6. See \emph{Custom maps} below.
  }

  \item{join_by}{
    Character string giving the field name in the Highcharter series data
    that should be matched to the custom map's join field. The map-side join
    field is selected automatically from \code{custom_map_number}.
  }
}
\description{
Creates a JavaScript load event for a Highcharter map that retrieves a
QualityInfo GeoJSON shape file and joins the map geometry to the chart series
data.

The function is designed for use inside the \code{events} argument of
\code{highcharter::hc_chart()}. It also stores custom-map metadata used by
\code{OED_Export_HiChart()} when exporting a chart with
\code{custom_map = TRUE}.
}
\details{
\code{OED_QI_Maps()} builds the QualityInfo GeoJSON URL from
\code{custom_map_number}, determines the map-side Highcharts \code{joinBy}
field, and returns a \code{highcharter::JS()} load event.

The resulting Highcharts \code{joinBy} pair is:

\preformatted{
c(<map property>, join_by)
}

For example, if \code{custom_map_number = 3} and
\code{join_by = "Area"}, the function uses the custom map's \code{Area}
property and matches it to the \code{Area} field in the chart data.

The data supplied to \code{hc_add_series()} must contain the field named by
\code{join_by}, and its values must match the corresponding property values in
the selected GeoJSON file. Users do not need to repeat the \code{joinBy}
setting inside \code{hc_add_series()} when using \code{OED_QI_Maps()}.

For custom QualityInfo map files, set \code{allAreas = FALSE} in
\code{hc_add_series()} so Highcharts does not attempt to render unmatched map
areas. In addition, set \code{hc_map$x$hc_opts$mapView} after building the chart
to preserve the intended Oregon projection and appearance of the custom shape
file.

When the chart will be exported with \code{OED_Export_HiChart()}, use
\code{custom_map = TRUE}. The exporter reads the metadata attached by
\code{OED_QI_Maps()}, fetches the custom GeoJSON before the map is created, and
avoids the default Highcharts map fetch.

Each chart on the same HTML page must use a unique chart number in
\code{OED_Export_HiChart()} so that each chart has a unique container ID.
}
\section{Custom maps}{
The following custom maps are available:

\tabular{rlll}{
\strong{Number} \tab \strong{Map} \tab \strong{GeoJSON file} \tab \strong{Map join field} \cr
1 \tab Workforce Areas \tab oregon_workforce_areas_2026.geojson \tab Area \cr
2 \tab Workforce Sub-Areas \tab oregon_workforce_sub_areas_2026.geojson \tab Area \cr
3 \tab Projections Workforce Areas \tab oregon_projections_workforce_areas_2026.geojson \tab Area \cr
4 \tab Projections Sub-Areas \tab oregon_projections_sub_areas_2026.geojson \tab Area \cr
5 \tab Oregon House Districts \tab oregon_leg_house_2026.geojson \tab HOUSE \cr
6 \tab Oregon Senate Districts \tab oregon_leg_senate_2026.geojson \tab SENATE \cr
7 \tab Oregon Minimum Wage Areas \tab MinWagePortlandMetro.geojson \tab MinWage \cr
}

For maps 1 through 4, the corresponding series-data field will commonly be an
area code field such as \code{Area}. For legislative maps, the series-data
field supplied in \code{join_by} must contain values matching the GeoJSON
\code{HOUSE} or \code{SENATE} property, respectively.
}
\section{Map examples}{
The images below illustrate the six custom map geometries.

\subsection{1. Workforce Areas}{
\figure{custom_map_1_workforce_areas.png}{options: width=500}
}

\subsection{2. Workforce Sub-Areas}{
\figure{custom_map_2_workforce_sub_areas.png}{options: width=500}
}

\subsection{3. Projections Workforce Areas}{
\figure{custom_map_3_projections_workforce_areas.png}{options: width=500}
}

\subsection{4. Projections Sub-Areas}{
\figure{custom_map_4_projections_sub_areas.png}{options: width=500}
}

\subsection{5. Oregon House Districts}{
\figure{custom_map_5_house_districts.png}{options: width=500}
}

\subsection{6. Oregon Senate Districts}{
\figure{custom_map_6_senate_districts.png}{options: width=500}
}

\subsection{7. Oregon Minimum Wage Areas}{
\figure{custom_map_7_Minimum_Wage_Areas.png}{options: width=500}
}
}
\value{
A \code{highcharter::JS()} object containing the chart load event. The object
also contains metadata attributes used by \code{OED_Export_HiChart()} for
custom-map exports.
}
\examples{
\dontrun{
library(highcharter)

# Example: projections workforce areas.
# The series data must contain an Area field whose values match the
# Area property in the GeoJSON.
hc_map <- highchart(type = "map") |>
  hc_chart(
    events = list(
      load = OED_QI_Maps(
        custom_map_number = 3,
        join_by = "Area"
      )
    )
  ) |>
  hc_add_series(
    data = hc_df,
    type = "map",
    allAreas = FALSE  # needed for custom map
  )

# If you use a QI shape file, you will need to cancel
# Highcharts changing the appearance of the custom geography.
hc_map$x$hc_opts$mapView <- list(
  projection = list(
    name = "LambertConformalConic",
    parallels = c(43, 45.5),
    rotation = c(120, 0, 0)
  ),
  padding = "3%"
)

# Export the custom map.
OED_Export_HiChart(
  hc_chart = hc_map,
  .file = "Map1",
  .title = "Oregon Average Hourly Wages by Area",
  .alt_text = paste(
    "Map showing average hourly wages across Oregon",
    "by workforce area."
  ),
  .chart_num = "1",
  custom_map = TRUE
)
}
}
\seealso{
\code{\link{OED_Export_HiChart}}
}
