OED_QI_Maps <- function(custom_map_number, join_by) {
  
  geojson_url <- paste0(
    "/lmiservice/service/visualizations/geojson/"
  ) 
  
  geojson_url <- paste0(
    geojson_url, 
    switch(
      custom_map_number,
      "oregon_workforce_areas_2026.geojson",
      "oregon_workforce_sub_areas_2026.geojson",
      "oregon_projections_workforce_areas_2026.geojson",
      "oregon_projections_sub_areas_2026.geojson",
      "oregon_leg_house_2026.geojson",
      "oregon_leg_senate_2026.geojson",
      "MinWagePortlandMetro.geojson"
    )
  )
  
  
  url_json <- jsonlite::toJSON(
    geojson_url,
    auto_unbox = TRUE
  )
  
  
  join_by_value <- c(
    switch(
      custom_map_number,
      "Area",
      "Area",
      "Area",
      "Area",
      "HOUSE",
      "SENATE",
      "MinWage"
    ), 
    join_by
  )
  
  
  join_json <- jsonlite::toJSON(
    join_by_value,
    auto_unbox = TRUE
  )
  
  
  js_map <- highcharter::JS(sprintf(
  "
  function () {

    const chart = this;
    const container = chart.renderTo;
    const containerId = container?.id || 'unknown Highcharts container';
    const joinBy = %s;

    function failMap(reason) {

      console.error(
        'Unable to load map in ' + containerId + ':',
        reason
      );

      try {
        chart.destroy();
      } catch (e) {}

      if (container) {
        container.innerHTML = '';
      }
    }

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

        const series = chart.series?.[0];
        const data = series?.options?.data || [];

        const mapField = joinBy?.[0];
        const dataField = joinBy?.[1];

        const mapKeys = new Set(
          (geojson?.features || [])
            .map(f => f?.properties?.[mapField])
            .filter(v => v != null)
            .map(String)
        );

        const validMap =
          Array.isArray(geojson?.features) &&
          geojson.features.length > 0 &&
          Array.isArray(joinBy) &&
          joinBy.length === 2 &&
          series &&
          data.length > 0 &&
          mapKeys.size > 0 &&
          data.some(
            p =>
              p?.[dataField] != null &&
              mapKeys.has(String(p[dataField]))
          );

        if (!validMap) {
          failMap(
            'GeoJSON, series data, or join fields are invalid.'
          );
          return;
        }

        series.update({
          type: 'map',
          mapData: geojson,
          joinBy: joinBy
        }, false);

        chart.redraw();

        if (chart.mapView) {
          chart.mapView.fitToBounds();
        }

        requestAnimationFrame(function () {

          const rendered =
            chart.series?.[0]?.points?.some(
              p => p?.graphic?.element
            );

          if (!rendered) {
            failMap(
              'Map data loaded but no map shapes rendered.'
            );
          }

        });

      })

      .catch(failMap);
  }
  ",
  join_json,
  url_json
))
  
  
  # ----------------------------------------------------------
  # Store metadata for OED_Export_HiChart()
  # ----------------------------------------------------------
  
  attr(js_map, "oed_custom_map") <- TRUE
  
  attr(js_map, "geojson_url") <- geojson_url
  
  attr(js_map, "join_by") <- join_by_value
  
  attr(js_map, "custom_map_number") <- custom_map_number
  
  
  js_map
}
