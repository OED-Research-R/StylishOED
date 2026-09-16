OED_QI_Maps <- function(custom_map_number, join_by) {
  
geojson_url <- paste0(
  "https://qualityinfotest.emp.state.or.us/",
  "lmiservice/service/visualizations/geojson/"
) 

geojson_url <- paste0(geojson_url, 
                      switch(
                        custom_map_number,
                        "oregon_workforce_areas_2026.geojson",
                        "oregon_workforce_sub_areas_2026.geojson",
                        "oregon_projections_workforce_areas_2026.geojson",
                        "oregon_projections_sub_areas_2026.geojson",
                        "oregon_leg_house_2026.geojson",
                        "oregon_leg_senate_2026.geojson"
                            )
                      )

  
  
  url_json <- jsonlite::toJSON(
    geojson_url,
    auto_unbox = TRUE
  )

join_by_value <- c(
  switch(custom_map_number,
        "Area",
        "Area",
        "Area",
        "Area",
        "HOUSE",
        "SENATE",
        ), 
  join_by 
)
  
  join_json <- jsonlite::toJSON(
    join_by_value,
    auto_unbox = TRUE
  )
  
  highcharter::JS(sprintf(
    "
    function () {
      const chart = this;

      fetch(%s)
        .then(function (response) {
          if (!response.ok) {
            throw new Error(
              'GeoJSON request failed: ' + response.status
            );
          }

          return response.json();
        })
        .then(function (geojson) {

          chart.series[0].update({
            type: 'map',

            // Pass the original GeoJSON to Highcharts.
            mapData: geojson,

            joinBy: %s
          }, false);

          chart.redraw();

          if (chart.mapView) {
            chart.mapView.fitToBounds();
          }
        })
        .catch(function (error) {
          console.error('Unable to load map:', error);
        });
    }
    ",
    url_json,
    join_json
  ))
}
