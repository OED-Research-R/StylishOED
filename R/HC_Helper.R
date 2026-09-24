OED_Export_HiChart <- function(
    hc_chart,
    container_id = "hc_container",
    .file = showPrompt(
      "Output filename",
      "Save As",
      default = "chart_output"
    ),
    .title = showPrompt(
      "Chart title",
      "Enter chart title",
      default = "Example Chart"
    ),
    .alt_text = showPrompt(
      "Alt text",
      "Describe this chart",
      default = "Highcharts visualization."
    ),
    .chart_num = showPrompt(
      "Chart number",
      "Enter chart number",
      default = "1"
    ),
    .custom_map = showPrompt(
      "Is this a custom map?",
      "Enter TRUE or FALSE",
      default = "FALSE"
    )
) {

  # ============================================================
  # 1. Store user inputs and create the chart container ID
  # ============================================================

  file <- .file
  title <- .title
  alt_text <- .alt_text
  chart_num <- .chart_num
  custom_map <- as.logical(.custom_map)

  container_id_full <- paste0(
    container_id,
    chart_num
  )


  # ============================================================
  # 2. Identify the Highcharts chart type
  # ============================================================

  chart_type <- hc_chart$x$type

  if (is.null(chart_type)) {
    chart_type <- "chart"
  }


  # ============================================================
  # 3. Prepare map information
  #
  # Standard maps retain the existing Highcharts map behavior.
  #
  # Custom maps read the URL and join fields stored by
  # OED_QI_Maps().
  # ============================================================

  geourl <- NULL


  if (chart_type == "map") {

    # ----------------------------------------------------------
    # 3A. Custom QualityInfo map
    # ----------------------------------------------------------

    if (isTRUE(custom_map)) {

      load_event <-
        hc_chart$x$hc_opts$chart$events$load


      if (is.null(load_event)) {

        stop(
          "custom_map = TRUE requires ",
          "OED_QI_Maps() in chart.events.load."
        )
      }


      # Confirm the event came from OED_QI_Maps()

      if (!isTRUE(
        attr(
          load_event,
          "oed_custom_map",
          exact = TRUE
        )
      )) {

        stop(
          "custom_map = TRUE requires a load event ",
          "created by OED_QI_Maps()."
        )
      }


      # Retrieve metadata stored by OED_QI_Maps()

      geojson_url <- attr(
        load_event,
        "geojson_url",
        exact = TRUE
      )

      join_by <- attr(
        load_event,
        "join_by",
        exact = TRUE
      )


      if (is.null(geojson_url)) {

        stop(
          "Could not retrieve geojson_url ",
          "from OED_QI_Maps()."
        )
      }


      if (
        is.null(join_by) ||
        length(join_by) != 2L
      ) {

        stop(
          "Could not retrieve a valid join_by ",
          "from OED_QI_Maps()."
        )
      }


      # Use custom GeoJSON URL for the export fetch

      geourl <- geojson_url


      # --------------------------------------------------------
      # Remove the OED_QI_Maps() load event.
      #
      # The exported HTML fetches the GeoJSON before creating
      # the map, so retaining the load event would duplicate
      # the map request.
      # --------------------------------------------------------

      hc_chart$x$hc_opts$chart$events$load <- NULL


      if (
        !is.null(hc_chart$x$hc_opts$chart$events) &&
        length(
          hc_chart$x$hc_opts$chart$events
        ) == 0L
      ) {

        hc_chart$x$hc_opts$chart$events <- NULL
      }


      # Remove any existing/default map reference

      hc_chart$x$hc_opts$chart$map <- NULL


      if (
        is.null(hc_chart$x$hc_opts$series) ||
        length(
          hc_chart$x$hc_opts$series
        ) == 0L
      ) {

        stop(
          "custom_map = TRUE requires ",
          "at least one map series."
        )
      }


      hc_chart$x$hc_opts$series[[1]]$mapData <- NULL


      # Explicitly define the custom map series

      hc_chart$x$hc_opts$series[[1]]$type <- "map"

      hc_chart$x$hc_opts$series[[1]]$joinBy <-
        unname(join_by)


    # ----------------------------------------------------------
    # 3B. Standard Highcharts map
    # ----------------------------------------------------------

    } else {

      # Save mapData before clearing it so the Highcharts
      # map URL can be determined.

      map_data <- NULL


      if (
        !is.null(hc_chart$x$hc_opts$series) &&
        length(
          hc_chart$x$hc_opts$series
        ) > 0L
      ) {

        map_data <-
          hc_chart$x$hc_opts$series[[1]]$mapData
      }


      # Clear embedded map references

      hc_chart$x$hc_opts$chart$map <- NULL


      if (!is.null(
        hc_chart$x$hc_opts$series
      )) {

        hc_chart$x$hc_opts$series <- lapply(
          hc_chart$x$hc_opts$series,
          function(s) {

            s$mapData <- NULL

            s
          }
        )
      }


      # Try to determine the Highcharts map ID

      map_id <- NULL


      if (is.character(map_data)) {

        map_id <- map_data

      } else if (
        inherits(
          map_data,
          "JS_EVAL"
        )
      ) {

        map_code <- map_data[[1]]

        map_match <- regmatches(
          map_code,
          regexec(
            "Highcharts\\.maps\\[['\"]([^'\"]+)['\"]\\]",
            map_code
          )
        )[[1]]


        if (length(map_match) >= 2L) {

          map_id <- map_match[2]
        }
      }


      # Use requested map if available.
      # Otherwise retain Oregon fallback behavior.

      if (!is.null(map_id)) {

        geourl <- sprintf(
          paste0(
            "https://code.highcharts.com/",
            "mapdata/%s.topo.json"
          ),
          map_id
        )

      } else {

        geourl <- paste0(
          "https://code.highcharts.com/",
          "mapdata/countries/us/",
          "us-or-all.topo.json"
        )
      }
    }
  }


  # ============================================================
  # 4. Convert JavaScript functions to temporary markers
  #
  # jsonlite cannot directly serialize Highcharter JS_EVAL
  # objects as executable JavaScript.
  # ============================================================

  clean_opts_for_js <- function(obj) {

    if (inherits(obj, "JS_EVAL")) {

      paste0(
        "JS_MARKER(",
        obj[[1]],
        ")"
      )

    } else if (is.list(obj)) {

      lapply(
        obj,
        clean_opts_for_js
      )

    } else {

      obj
    }
  }


  opts_clean <- clean_opts_for_js(
    hc_chart$x$hc_opts
  )


  # ============================================================
  # 5. Serialize the Highcharts options to JSON
  # ============================================================

  json <- jsonlite::toJSON(
    opts_clean,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )


  # ============================================================
  # 6. Convert JS_MARKER values back to executable JavaScript
  # ============================================================

  inject_js_literals <- function(json_txt) {

    out <- gsub(
      paste0(
        '"JS_MARKER\\(',
        '(function\\s*\\(.*?\\{[\\s\\S]*?\\})',
        '\\)"'
      ),
      "\\1",
      json_txt,
      perl = TRUE
    )

    out <- gsub(
      "\\\\n",
      "\n",
      out,
      perl = TRUE
    )

    out
  }


  json <- inject_js_literals(
    json
  )


  # ============================================================
  # 7. Create the HTML chart container and responsive styling
  # ============================================================

  style_html <- paste0(
    "<style>\n",
    "#",
    container_id_full,
    " {\n",
    "  font-family: Arial, sans-serif;\n",
    "  width: 100%;\n",
    "  max-width: 880px;\n",
    "  min-height:400px;\n",
    "  aspect-ratio: 4 / 3;\n",
    "  margin: 0 auto;\n",
    "}\n",
    "\n",
    "@media screen and (max-width: 480px) {\n",
    "  #",
    container_id_full,
    " { aspect-ratio: 1 / 1; }\n",
    "}\n",
    "</style>"
  )


  container_html <- paste0(
    '<div id="',
    container_id_full,
    '" role="region" aria-label="',
    alt_text,
    '"></div>'
  )


  # ============================================================
  # 8. Build the JavaScript used to create the chart
  #
  # Custom map:
  #   fetch GeoJSON -> validate -> assign mapData -> create map
  #
  # Standard map:
  #   fetch Highcharts topology -> assign mapData -> create map
  #
  # Other chart:
  #   create chart directly
  # ============================================================


  # ------------------------------------------------------------
  # 8A. Custom QualityInfo map
  # ------------------------------------------------------------

  if (
    chart_type == "map" &&
    isTRUE(custom_map)
  ) {

    geourl_json <- jsonlite::toJSON(
      geourl,
      auto_unbox = TRUE
    )

    join_json <- jsonlite::toJSON(
      unname(join_by),
      auto_unbox = FALSE
    )


    script <- sprintf(
      '
<script>

  var container =
    document.getElementById("%s");


  function failMap(reason) {

    console.error(
      "Unable to load map in %s:",
      reason
    );

    var chart = null;

    for (var i = 0; i < Highcharts.charts.length; i++) {

      if (
        Highcharts.charts[i] &&
        Highcharts.charts[i].renderTo === container
      ) {

        chart = Highcharts.charts[i];
        break;
      }
    }


    if (chart) {
      chart.destroy();
    }


    if (container) {

      while (container.firstChild) {
        container.removeChild(
          container.firstChild
        );
      }
    }
  }


  fetch(%s)

    .then(function(response) {

      if (!response.ok) {

        throw new Error(
          "GeoJSON request failed: " +
          response.status
        );
      }

      return response.json();

    })

    .then(function(geojson) {

      var joinBy = %s;
      var options = %s;

      var data = [];

      if (
        options.series &&
        options.series[0] &&
        options.series[0].data
      ) {

        data = options.series[0].data;
      }


      var mapField = null;
      var dataField = null;

      if (
        Array.isArray(joinBy) &&
        joinBy.length === 2
      ) {

        mapField = joinBy[0];
        dataField = joinBy[1];
      }


      var features = [];

      if (
        geojson &&
        Array.isArray(geojson.features)
      ) {

        features = geojson.features;
      }


      var mapKeys = [];

      for (
        var i = 0;
        i < features.length;
        i++
      ) {

        if (
          features[i] &&
          features[i].properties &&
          mapField !== null &&
          features[i].properties[mapField] != null
        ) {

          mapKeys.push(
            String(
              features[i].properties[mapField]
            )
          );
        }
      }


      var hasMatch = false;

      for (
        var j = 0;
        j < data.length && !hasMatch;
        j++
      ) {

        if (
          data[j] &&
          dataField !== null &&
          data[j][dataField] != null &&
          mapKeys.indexOf(
            String(data[j][dataField])
          ) !== -1
        ) {

          hasMatch = true;
        }
      }


      var validMap =
        features.length > 0 &&
        Array.isArray(joinBy) &&
        joinBy.length === 2 &&
        data.length > 0 &&
        mapKeys.length > 0 &&
        hasMatch;


      if (!validMap) {

        failMap(
          "GeoJSON, series data, or join fields are invalid."
        );

        return;
      }


      if (!Array.isArray(options.series)) {
        options.series = [];
      }


      if (!options.series.length) {
        options.series.push({});
      }


      options.series[0] = Object.assign(
        {},
        options.series[0],
        {
          type: "map",
          mapData: geojson,
          joinBy: joinBy
        }
      );


      var chart = Highcharts.mapChart(
        "%s",
        options
      );


      if (
        !chart ||
        !chart.series ||
        !chart.series.length
      ) {

        failMap(
          "Highcharts did not create the map series."
        );
      }

    })

    .catch(function(error) {

      failMap(error);

    });

</script>
',
      container_id_full,
      container_id_full,
      geourl_json,
      join_json,
      json,
      container_id_full
    )


  # ------------------------------------------------------------
  # 8B. Standard Highcharts map
  # ------------------------------------------------------------

  } else if (chart_type == "map") {

    script <- sprintf(
      '
<script>

  fetch("%s")

    .then(function(response) {

      if (!response.ok) {

        throw new Error(
          "Map request failed: " +
          response.status
        );
      }

      return response.json();

    })

    .then(function(topology) {

      Highcharts.mapChart(
        "%s",

        (function(c) {

          if (!Array.isArray(c.series)) {
            c.series = [];
          }

          if (!c.series.length) {
            c.series.push({});
          }

          c.series[0] = Object.assign(
            {},
            c.series[0],
            {
              mapData: topology
            }
          );

          return c;

        })(%s)

      );

    })

    .catch(function(error) {

      console.error(
        "Unable to load map in %s:",
        error
      );

    });

</script>
',
      geourl,
      container_id_full,
      json,
      container_id_full
    )


  # ------------------------------------------------------------
  # 8C. Standard non-map chart
  # ------------------------------------------------------------

  } else {

    script <- sprintf(
      '
<script>

  Highcharts.chart(
    "%s",
    %s
  );

</script>
',
      container_id_full,
      json
    )
  }


  # ============================================================
  # 9. Assemble the final HTML fragment
  # ============================================================

  html_output <- paste0(
    "<div>\n",
    style_html,
    "\n",
    container_html,
    "\n",
    script,
    "\n",
    "</div>"
  )


  # ============================================================
  # 10. Write the HTML file
  # ============================================================

  output_file <- file


  if (!grepl(
    "\\.html$",
    output_file,
    ignore.case = TRUE
  )) {

    output_file <- paste0(
      output_file,
      ".html"
    )
  }


  writeLines(
    html_output,
    con = output_file,
    useBytes = TRUE
  )


  message(
    "Highcharts HTML written to: ",
    output_file
  )


  invisible(
    output_file
  )
}
