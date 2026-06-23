OED_CES_Table <- function(
  Type = c("Annual", "SA", "NSA"),
  Geographies = NULL
          ){
  
# --- Load or Install Libraries ---
#  check_and_load <- function(pkg) {
#    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
#    library(pkg, character.only = TRUE)
#  }

# check_and_load("readxl")
# check_and_load("dplyr")
# check_and_load("tidyr")
# check_and_load("httr2")
# check_and_load("tibble") 

Type <- match.arg(Type)

sheet_lookup <- c(
  Annual = "Annual",
  SA = "Seasonally Adjusted",
  NSA = "Not Seasonally Adjusted"
)

selected_sheet <- sheet_lookup[[Type]]

  state_lookup <- tibble(
    geo_type = "State",
    area_type = "01",
    Geography = "Oregon",
    geo_code = "000000"
  ) |>
    mutate(ces_area = paste0("41", area_type, geo_code))

  county_lookup <- tibble(
    geo_type = "County",
    area_type = "04",
    Geography = c(
      "Baker", "Benton", "Clackamas", "Clatsop", "Columbia",
      "Coos", "Crook", "Curry", "Deschutes", "Douglas",
      "Gilliam", "Grant", "Harney", "Hood River", "Jackson",
      "Jefferson", "Josephine", "Klamath", "Lake", "Lane",
      "Lincoln", "Linn", "Malheur", "Marion", "Morrow",
      "Multnomah", "Polk", "Sherman", "Tillamook", "Umatilla",
      "Union", "Wallowa", "Wasco", "Washington", "Wheeler", "Yamhill"
    ),
    geo_code = sprintf(
      "%06d",
      c(
        1, 3, 5, 7, 9,
        11, 13, 15, 17, 19,
        21, 23, 25, 27, 29,
        31, 33, 35, 37, 39,
        41, 43, 45, 47, 49,
        51, 53, 55, 57, 59,
        61, 63, 65, 67, 69, 71
      )
    )
  ) |>
    mutate(ces_area = paste0("41", area_type, geo_code))

  msa_lookup <- tibble(
    geo_type = "MSA",
    area_type = "21",
    Geography = c(
      "Albany MSA",
      "Bend MSA",
      "Corvallis MSA",
      "Eugene-Springfield MSA",
      "Grants Pass MSA",
      "Medford MSA",
      "Portland-Vancouver-Hillsboro MSA",
      "Salem MSA"
    ),
    geo_code = c(
      "010540",
      "013460",
      "018700",
      "021660",
      "024420",
      "032780",
      "038900",
      "041420"
    )
  ) |>
    mutate(ces_area = paste0("41", area_type, geo_code))

  geo_lookup <- bind_rows(
    state_lookup,
    county_lookup,
    msa_lookup
  )

  if (is.null(Geographies)) {
    geo_selected <- geo_lookup
  } else {
    geo_selected <- geo_lookup |>
      filter(
        Geography %in% Geographies |
          ces_area %in% Geographies
      )
  }

  if (nrow(geo_selected) == 0) {
    stop("No matching geographies found in geo_lookup.")
  }

  read_ces_sheet <- function(
    path,
    geography,
    geo_type,
    area_type,
    geo_code,
    ces_area,
    sheet,
    table_type
  ) {

    if (!file.exists(path) || file.info(path)$size == 0) {
      return(NULL)
    }

    sheets <- tryCatch(
      readxl::excel_sheets(path),
      error = function(e) NULL
    )

    if (is.null(sheets) || !sheet %in% sheets) {
      return(NULL)
    }

    dat <- suppressMessages(
      readxl::read_excel(
        path,
        sheet = sheet,
        skip = 11,
        col_names = FALSE
      )
    )

    if (nrow(dat) == 0 || ncol(dat) == 0) {
      return(NULL)
    }

    if (table_type == "Annual") {

      col_names <- c(
        "Series",
        as.character(2001:(2000 + ncol(dat) - 1))
      )

    } else {

      monthly_dates <- seq.Date(
        from = as.Date("2001-01-01"),
        by = "month",
        length.out = ncol(dat) - 1
      )

      col_names <- c(
        "Series",
        as.character(monthly_dates)
      )
    }

    names(dat) <- col_names

    dat |>
      mutate(
        across(
          -Series,
          ~ suppressWarnings(as.numeric(.x))
        )
      ) |>
      filter(!is.na(Series)) |>
      mutate(
        Geography = geography,
        geo_type = geo_type,
        area_type = area_type,
        geo_code = geo_code,
        ces_area = ces_area,
        table_type = table_type,
        sheet = sheet,
        .before = 1
      )
  }

  failed_requests <- tibble(
    Geography = character(),
    geo_type = character(),
    ces_area = character(),
    table_type = character(),
    reason = character()
  )

  cookie_file <- tempfile(fileext = ".txt")

  request("https://www.qualityinfo.org/ceest") |>
    req_user_agent("Mozilla/5.0") |>
    req_options(
      cookiejar = cookie_file,
      cookiefile = cookie_file
    ) |>
    req_perform()

  all_data <- list()

  for (i in seq_len(nrow(geo_selected))) {

    geography <- geo_selected$Geography[i]
    geo_type <- geo_selected$geo_type[i]
    area_type <- geo_selected$area_type[i]
    geo_code <- geo_selected$geo_code[i]
    area <- geo_selected$ces_area[i]

    message("Processing: ", geography, " | ", Type)

    tf <- tempfile(fileext = ".xlsx")

    req <- request("https://www.qualityinfo.org/ceest") |>
      req_url_query(
        p_p_id = "QiDatatoolCes_INSTANCE_QC9M3TzTAbOB",
        p_p_lifecycle = 2,
        p_p_state = "normal",
        p_p_mode = "view",
        p_p_resource_id = "getReportXlsx",
        p_p_cacheability = "cacheLevelPage",
        rt = 6,
        cesSeries = "or",
        cesCode = "00000000",
        cesArea = area,
        cesYear = 2019,
        cesSeasAdj = 1
      ) |>
      req_headers(
        Referer = "https://www.qualityinfo.org/ceest",
        Accept = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet,*/*"
      ) |>
      req_user_agent("Mozilla/5.0") |>
      req_options(
        cookiejar = cookie_file,
        cookiefile = cookie_file
      )

    response <- tryCatch(
      req_perform(req, path = tf),
      error = function(e) NULL
    )

    if (is.null(response)) {
      failed_requests <- bind_rows(
        failed_requests,
        tibble(
          Geography = geography,
          geo_type = geo_type,
          ces_area = area,
          table_type = Type,
          reason = "No response"
        )
      )
      next
    }

    result <- read_ces_sheet(
      path = tf,
      geography = geography,
      geo_type = geo_type,
      area_type = area_type,
      geo_code = geo_code,
      ces_area = area,
      sheet = selected_sheet,
      table_type = Type
    )

    if (is.null(result) || nrow(result) == 0) {
      failed_requests <- bind_rows(
        failed_requests,
        tibble(
          Geography = geography,
          geo_type = geo_type,
          ces_area = area,
          table_type = Type,
          reason = paste("No data in", selected_sheet)
        )
      )
      next
    }

    all_data[[length(all_data) + 1]] <- result
  }

final_df <- bind_rows(all_data)

id_cols <- c(
  "Geography",
  "geo_type",
  "area_type",
  "geo_code",
  "ces_area",
  "table_type",
  "sheet",
  "Series"
)

final_df <- final_df |>
  pivot_longer(
    cols = -all_of(id_cols),
    names_to = "Date",
    values_to = "Employment"
  ) |>
  mutate(
    Date = as.Date(
      if_else(
        table_type == "Annual",
        paste0(Date, "-01-01"),
        Date
      )
    )
  ) |>
  arrange(
    Geography,
    Series,
    Date
  )

attr(final_df, "failed_requests") <- failed_requests

final_df 
                   
} #end of function                  
