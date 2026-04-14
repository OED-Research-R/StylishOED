OED_HTML_Table <- function(
	Excel_Table  = NA,
  sheet_name   = NA,
  ada_caption  = NA,
  table_number = NA,
  col_width_1  = NA,
  output_name  = NA   # new argument
) {

  # --- Load or Install Libraries ---
  check_and_load <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }

  check_and_load("readxl")
  check_and_load("dplyr")
  check_and_load("htmltools")
  check_and_load("openxlsx")
  check_and_load("stringr")
  check_and_load("rstudioapi")
  check_and_load("xml2")

  # --- Helper: treat NA / NULL / "" as blank ---
  is_blank_scalar <- function(x) {
    if (is.null(x)) return(TRUE)
    if (length(x) == 0) return(TRUE)
    x1 <- tryCatch(as.character(x)[1], error = function(e) NA_character_)
    if (is.na(x1)) return(TRUE)
    !nzchar(trimws(x1))
  }

  tryCatch({

    # --- Excel file ---
    if (is_blank_scalar(Excel_Table)) {
      Excel_Table <- rstudioapi::selectFile(caption = "Choose Excel file")
      if (is_blank_scalar(Excel_Table)) stop("Canceled at prompt: Choose Excel file.")
      Excel_Table <- gsub("\\\\", "/", Excel_Table)
    }

    # --- Load workbook & sheets ---
    wb <- openxlsx::loadWorkbook(Excel_Table)

    # --- Sheet name ---
    if (is_blank_scalar(sheet_name)) {
      sheet_names <- openxlsx::getSheetNames(Excel_Table)
      if (length(sheet_names) == 0) stop("No sheets found in the selected Excel file.")

      padded_sheet_names <- paste0("[", seq_along(sheet_names), "] ", sheet_names)
      line_lengths <- nchar(padded_sheet_names)
      padded_lines <- mapply(function(x, len) {
        paste0(x, paste(rep("_", max(55 - len, 0)), collapse = ""), " ")
      }, padded_sheet_names, line_lengths, USE.NAMES = FALSE)

      sheet_question <- paste0(
        "Please select a sheet by number:",
        paste(rep("_", 54 - nchar("Please select a sheet by number:")), collapse = "")
      )
      sheet_prompt <- paste(sheet_question, paste(padded_lines, collapse = ""))

      sheet_index_raw <- rstudioapi::showPrompt(title = "Sheet Name", message = sheet_prompt)
      if (is_blank_scalar(sheet_index_raw)) stop("Canceled at prompt: Sheet Name (sheet number).")

      sheet_index <- suppressWarnings(as.integer(as.character(sheet_index_raw)[1]))
      if (is.na(sheet_index) || sheet_index < 1 || sheet_index > length(sheet_names)) {
        stop("Invalid sheet number at prompt: Sheet Name.")
      }
      sheet_name <- sheet_names[sheet_index]
    } else
		{
		sheet_names <- openxlsx::getSheetNames(Excel_Table)
		sheet_index <- which(sheet_names == sheet_name)
		}

    # --- Accessibility Caption ---
    if (is_blank_scalar(ada_caption)) {
      ada_caption <- rstudioapi::showPrompt(
        title   = "Accessibility Caption",
        message = "Please enter the accessibility caption:"
      )
      if (is_blank_scalar(ada_caption)) stop("Canceled at prompt: Accessibility Caption.")
    }

    # --- Table Number ---
    if (is_blank_scalar(table_number)) {
      table_number <- rstudioapi::showPrompt(
        title   = "Table Number",
        message = "If you have multiple tables, please indicate what table number this is:",
        default = "1"
      )
      if (is_blank_scalar(table_number)) stop("Canceled at prompt: Table Number.")
    }

    # --- First Column Width ---
    if (is_blank_scalar(col_width_1)) {
      col_width_1 <- rstudioapi::showPrompt(
        title   = "First Column Width",
        message = "The default width of the first column is 220px. If you need to change it, you may select another width below. Otherwise click OK.",
        default = "220"
      )
      if (is_blank_scalar(col_width_1)) stop("Canceled at prompt: First Column Width.")
    }

    # --- Output path / name ---
    # If output_path is not given, derive it from output_name (prompt if needed)
    
      if (is_blank_scalar(output_name)) {
        output_name <- rstudioapi::showPrompt(
          title   = "Output Filename",
          message = "What do you want to call this table? (ex., output_table):"
        )
        if (is_blank_scalar(output_name)) stop("Canceled or blank at prompt: Output Filename.")
      }
      output_path <- file.path(dirname(Excel_Table), paste0(output_name, ".html"))
    

    # ... your actual table building code goes here ...
    # use Excel_Table, sheet_name, ada_caption, table_number, col_width_1, output_path

  }, error = function(e) {
    message("\n--- Script terminated ---")
    message("Reason: ", conditionMessage(e))
    stop("User input process was canceled or invalid. Exiting gracefully.")
  })




  # ---------------------------------------
  # Read data (NO deletions before mapping formats)
  # ---------------------------------------
  df_raw  <- read.xlsx(Excel_Table, sheet = sheet_name, colNames = FALSE)
  df_tidy <- read_excel(Excel_Table,  sheet = sheet_name, col_names = FALSE)

  page_title  <- df_raw[1, 1, drop = TRUE]
  total_cols  <- ncol(df_raw)
  #source_note <- df_raw[nrow(df_raw), 1, drop = TRUE]   removed for multiple footnotes

  # ---------------------------------------
  # Merged cells & header detection (no filtering)
  # ---------------------------------------
  parse_excel_ref <- function(ref) {
    ref <- gsub("[<>]", "", ref)
    addr <- sub('.*ref="([^"]+)".*', "\\1", ref)
    parts <- strsplit(addr, ":")[[1]]
    cell_to_coords <- function(cell) {
      col_letters <- gsub("[0-9]", "", cell)
      row <- as.integer(gsub("[A-Z]", "", toupper(cell)))
      col_num_dbl <- sum((utf8ToInt(strsplit(col_letters, "")[[1]]) - 64) *
                           26 ^ rev(seq_along(col_letters) - 1))
      col <- as.integer(col_num_dbl)
      list(col = col, row = row)
    }
    list(
      startCol = cell_to_coords(parts[1])$col,
      endCol   = cell_to_coords(parts[2])$col,
      startRow = cell_to_coords(parts[1])$row,
      endRow   = cell_to_coords(parts[2])$row
    )
  }

  merged_refs <- wb$worksheets[[sheet_index]]$mergeCells
  merged_bounds <- if (length(merged_refs)) lapply(merged_refs, parse_excel_ref) else list()
  merged_bounds_row2 <- Filter(function(b) b$startRow == 2, merged_bounds)
  has_group_headers <- length(merged_bounds_row2) > 0

  row2 <- df_raw[2, ]; row3 <- df_raw[3, ]
  header_labels <- if (has_group_headers) row3 else row2
  header_labels[is.na(header_labels)] <- ""
  if (has_group_headers) { row3[is.na(row3)] <- "" } else { row2[is.na(row2)] <- "" }

  # Define body bounds using ORIGINAL Excel indexing
  data_start <- if (has_group_headers) 4 else 3

  # --- UPDATED: derive data_end from last fully-populated row (all columns non-blank)
  is_blank <- function(x) is.na(x) | trimws(as.character(x)) == ""
  rows_to_check <- seq_len(nrow(df_tidy))
  full_row <- apply(df_tidy, 1, function(r) all(!is_blank(r)))
  last_full_row_idx <- max(c(NA_integer_, which(full_row)), na.rm = TRUE)
  if (is.infinite(last_full_row_idx) || is.na(last_full_row_idx) || last_full_row_idx < data_start) {
    data_end <- data_start - 1L
  } else {
    data_end <- last_full_row_idx
  }

  # (Re)build the render mask using the new data_end
  na_rows <- apply(df_tidy, 1, function(x) all(is_blank(x))) # don't delete; just flag
  excel_data_rows <- which(!na_rows &
                             seq_len(nrow(df_tidy)) >= data_start &
                             seq_len(nrow(df_tidy)) <= data_end)

  # Filtered views for rendering only
  data_table <- df_raw [excel_data_rows, , drop = FALSE]
  tidy_table <- df_tidy[excel_data_rows, , drop = FALSE]

  # ---------------------------------------
  # Extract styles – resolve CORRECT sheet xml, then build fmt_df (BEFORE filtering)
  # ---------------------------------------
  unzip_dir <- tempfile()
  unzip(Excel_Table, exdir = unzip_dir)

  # Resolve correct sheetN.xml by name via relationships
  workbook_xml_file <- file.path(unzip_dir, "xl/workbook.xml")
  rels_xml_file     <- file.path(unzip_dir, "xl/_rels/workbook.xml.rels")
  styles_xml_file   <- file.path(unzip_dir, "xl/styles.xml")

  workbook_xml <- read_xml(workbook_xml_file)
  rels_xml     <- read_xml(rels_xml_file)
  styles_xml   <- read_xml(styles_xml_file)

  wb_ns     <- xml_ns(workbook_xml)
  rels_ns   <- xml_ns(rels_xml)
  styles_ns <- xml_ns(styles_xml)

  sheet_nodes <- xml_find_all(workbook_xml, ".//d1:sheets/d1:sheet", ns = wb_ns)
  sheet_node  <- sheet_nodes[xml_attr(sheet_nodes, "name") == sheet_name][[1]]
  rid <- xml_attr(sheet_node, "{http://schemas.openxmlformats.org/officeDocument/2006/relationships}id")
  if (is.na(rid)) rid <- xml_attr(sheet_node, "id")

  rel_nodes <- xml_find_all(rels_xml, ".//d1:Relationship", ns = rels_ns)
  targets <- xml_attr(rel_nodes, "Target"); ids <- xml_attr(rel_nodes, "Id")
  target  <- targets[ids == rid]
  if (length(target) != 1) stop("Could not resolve worksheet target for sheet: ", sheet_name)

  sheet_xml_file <- file.path(unzip_dir, "xl", target)
  sheet_xml      <- read_xml(sheet_xml_file)
  sheet_ns       <- xml_ns(sheet_xml)

  # Cells/styles from the CORRECT worksheet
  cell_nodes <- xml_find_all(sheet_xml, ".//d1:c", ns = sheet_ns)
  cell_refs  <- xml_attr(cell_nodes, "r")
  style_ids  <- suppressWarnings(as.integer(xml_attr(cell_nodes, "s"))) # <- ORIGINAL name

  # From styles.xml (global)
  cellXfs  <- xml_find_all(styles_xml, ".//d1:cellXfs/d1:xf", ns = styles_ns)
  numFmts  <- xml_find_all(styles_xml, ".//d1:numFmts/d1:numFmt", ns = styles_ns)
  fonts    <- xml_find_all(styles_xml, ".//d1:fonts/d1:font", ns = styles_ns)

  # Helper to parse "A1" -> (col, row) with integer coercion
  coord_from_ref <- function(ref) {
    ref <- toupper(trimws(ref))
    m <- regexec("^([A-Z]+)([0-9]+)$", ref)
    parts <- regmatches(ref, m)[[1]]
    if (length(parts) == 0) return(list(col = NA_integer_, row = NA_integer_))
    col_letters <- parts[2]
    row_num <- as.integer(parts[3])
    col_num_dbl <- sum((utf8ToInt(strsplit(col_letters, "")[[1]]) - 64) *
                         26 ^ rev(seq_along(col_letters) - 1))
    col_num <- as.integer(col_num_dbl)
    list(col = col_num, row = row_num)
  }

  # Resolve numFmtId/formatCode/applyNumberFormat from an XF index
  get_fmt <- function(xf_index) {
    if (is.na(xf_index) || length(cellXfs) < xf_index + 1)
      return(list(id = NA_integer_, code = NA_character_, apply = NA_integer_))
    xf <- cellXfs[[xf_index + 1]]
    id_chr <- xml_attr(xf, "numFmtId")
    id <- suppressWarnings(as.integer(id_chr))
    apply_num <- suppressWarnings(as.integer(xml_attr(xf, "applyNumberFormat")))
    if (is.na(apply_num)) apply_num <- NA_integer_
    code <- NA_character_
    if (!is.na(id) && id > 163 && length(numFmts)) {
      idx <- which(as.integer(xml_attr(numFmts, "numFmtId")) == id)
      if (length(idx) == 1) code <- xml_attr(numFmts[idx], "formatCode")
    }
    list(id = id, code = code, apply = apply_num)
  }

  # ---------------------------------------
  # UPDATED classify_format: detects annual/currency, maps plain numbers to 'employment'
  # ---------------------------------------
  classify_format <- function(xf_index) {
    fmt_info <- get_fmt(xf_index)
    id   <- fmt_info$id
    code <- fmt_info$code

    norm_code <- if (!is.na(code)) gsub("\\[.*?\\]", "", code) else NA_character_

    has_dollar               <- function(fmt) grepl("(\\$|\\Q[$-\\E)", fmt)
    has_percent              <- function(fmt) grepl("%", fmt)
    has_decimal_placeholders <- function(fmt) grepl("\\.(?:0|#)+", fmt)
    has_numeric_placeholders <- function(fmt) grepl("[0#]", fmt)

    # ---- Custom formats (id > 163) ----
    if (!is.na(id) && id > 163 && !is.na(norm_code)) {
      # annual = currency with NO decimals
      if (has_dollar(norm_code) && !has_decimal_placeholders(norm_code)) return("annual")
      # currency = currency WITH decimals
      if (has_dollar(norm_code)) return("currency")
      # percentage
      if (has_percent(norm_code)) return("percentage")
      # plain numeric -> employment
      if (has_numeric_placeholders(norm_code) && !has_percent(norm_code)) return("employment")
      # date/time-ish (leave as text for your pipeline)
      if (grepl("[ymdhis]", tolower(norm_code))) return("text")
	  return("text")
    }

    # ---- Built-in formats ----
    if (!is.na(id)) {
      if (id %in% c(5, 6))  return("annual")     # currency, no decimals
      if (id %in% c(7, 8))  return("currency")   # currency, two decimals
      if (id %in% c(9, 10)) return("percentage")
      if (id %in% c(14:22)) return("text")       # dates
      # General / Number / Accounting variants -> employment (plain number)
      if (id %in% c(0,1,2,3,4,11,12,13,23,24,27,28,29,30)) return("employment")
    }

    "text"
  }

  # ---------- Build fmt_df over the WHOLE SHEET (unfiltered) ----------
  coords <- lapply(cell_refs, coord_from_ref)
  rows <- vapply(coords, function(z) if (is.null(z$row)) NA_integer_ else as.integer(z$row), integer(1))
  cols <- vapply(coords, function(z) if (is.null(z$col)) NA_integer_ else as.integer(z$col), integer(1))

  fmt_info <- lapply(style_ids, get_fmt)
  numFmtId <- vapply(fmt_info, function(z) if (is.null(z$id)) NA_integer_ else as.integer(z$id), integer(1))
  fmtCode  <- vapply(fmt_info, function(z) if (is.null(z$code)) NA_character_ else as.character(z$code), character(1))
  applyNumberFormat <- vapply(fmt_info, function(z) if (is.null(z$apply)) NA_integer_ else as.integer(z$apply), integer(1))
  isCustom <- !is.na(numFmtId) & numFmtId > 163
  category <- vapply(style_ids, classify_format, character(1))

  fmt_df <- data.frame(
    ref = cell_refs,
    row = rows,
    col = cols,
    style_index = style_ids,
    numFmtId = numFmtId,
    applyNumberFormat = applyNumberFormat,
    formatCode = fmtCode,
    isCustom = isCustom,
    category = category,
    stringsAsFactors = FALSE
  )

  # ---------------------------------------
  # BOLD + INDENT (original approach, using Column A cells)
  # ---------------------------------------
  is_bold <- function(xf_index) {
    if (is.na(xf_index) || length(cellXfs) < xf_index + 1) return(FALSE)
    xf_node <- cellXfs[[xf_index + 1]]
    font_id <- as.integer(xml_attr(xf_node, "fontId"))
    if (is.na(font_id) || length(fonts) < font_id + 1) return(FALSE)
    font_node <- fonts[[font_id + 1]]
    !is.na(xml_find_first(font_node, ".//d1:b", ns = styles_ns))
  }

  get_indent <- function(xf_index) {
    if (is.na(xf_index) || length(cellXfs) < xf_index + 1) return(NA_integer_)
    xf_node <- cellXfs[[xf_index + 1]]
    alignment_node <- xml_find_first(xf_node, ".//d1:alignment", ns = styles_ns)
    indent_val <- xml_attr(alignment_node, "indent")
    if (is.na(indent_val)) return(NA_integer_)
    as.integer(indent_val)
  }

  bold_rows <- c()
  indent_map <- integer()

  if (length(cellXfs) > 0) {
    for (i in seq_along(cell_refs)) {
      ref <- cell_refs[i]
      st  <- style_ids[i]
      if (grepl("^A[0-9]+$", ref)) {
        row_num <- as.integer(sub("A", "", ref))
        if (is_bold(st)) bold_rows <- c(bold_rows, row_num)
        indent_val <- get_indent(st)
        if (!is.na(indent_val)) indent_map[as.character(row_num)] <- indent_val
      }
    }
  }

  # ---------------------------------------
  # Now decide which rows to render (AFTER mapping formats)
  # ---------------------------------------
  na_rows <- apply(df_tidy, 1, function(x) all(is.na(x) | trimws(x) == "")) # don't delete; just flag
  excel_data_rows <- which(!na_rows &
                             seq_len(nrow(df_tidy)) >= data_start &
                             seq_len(nrow(df_tidy)) <= data_end)

  # --- Build multi-footnote block from any cells after the data table ---
  last_data_row <- if (length(excel_data_rows)) max(excel_data_rows) else data_end

  foot_block <- if (last_data_row < nrow(df_raw)) {
    df_raw[(last_data_row + 1):nrow(df_raw), , drop = FALSE]
  } else {
    df_raw[0, , drop = FALSE]
  }

  # Flatten row-major, trim, and drop blanks
  flat_vals <- as.vector(t(as.matrix(foot_block)))
  flat_vals <- trimws(as.character(flat_vals))
  flat_vals <- flat_vals[!(is.na(flat_vals) | flat_vals == "")]

  # Join with literal <br> and mark as HTML so it isn't escaped
  source_note <- paste(flat_vals, collapse = "<br>")

  # Filtered views for rendering only
  data_table <- df_raw [excel_data_rows, , drop = FALSE]
  tidy_table <- df_tidy[excel_data_rows, , drop = FALSE]

  # ---------------------------------------
  # Header rows HTML
  # ---------------------------------------
  merge_map <- rep(NA_integer_, total_cols)
  for (b in merged_bounds_row2) {
    span <- b$endCol - b$startCol + 1
    merge_map[b$startCol] <- span
    if (span > 1) for (i in (b$startCol + 1):b$endCol) merge_map[i] <- 0
  }

  if (has_group_headers) {
    top_row <- tags$tr()
    col_index <- 1
    while (col_index <= total_cols) {
      span  <- merge_map[col_index]
      label <- as.character(row2[[col_index]]); label <- ifelse(is.na(label), "", label)
      if (!is.na(span) && span > 0) {
        top_row <- tagAppendChild(top_row, tags$th(colspan = span, class = "atc-header1 atc-group-header atc-align-center", label))
        col_index <- col_index + span
      } else if (is.na(span)) {
        top_row <- tagAppendChild(top_row, tags$th(class = "atc-header1 atc-group-header atc-align-center", label))
        col_index <- col_index + 1
      } else {
        col_index <- col_index + 1
      }
    }
  }

  second_row <- tags$tr(
    lapply(seq_along(header_labels), function(i) {
      align_class <- if (i == 1) "atc-align-left" else "atc-align-right"
      tags$th(class = paste("atc-header1 atc-header2", align_class), as.character(header_labels[[i]]))
    })
  )

  # ---------------------------------------
  # BODY ROWS (per-cell formatting driven by fmt_df; lookup by Excel row/col)
  # ---------------------------------------
  fmt_key <- paste(fmt_df$row, fmt_df$col, sep = ":")
  fmt_map <- setNames(fmt_df$category, fmt_key)

  body_rows <- lapply(seq_len(nrow(tidy_table)), function(i) {
    row_num <- excel_data_rows[i]  # real Excel row in the sheet
    tr_class <- if (row_num %in% bold_rows) "atc-bold-row" else NULL  # <- match your CSS

    tags$tr(
      class = tr_class,
      lapply(seq_len(total_cols), function(j) {
        raw_val <- tidy_table[[i, j]]
        val <- as.character(raw_val)

	# For data columns only (j > 1): treat NA / "" / "NA" as a blank cell
     if (j > 1 && (is.na(raw_val) ||
             !nzchar(trimws(val)) ||
            identical(val, "NA"))) {
   # keep numeric alignment class for data columns
     td_tag <- tags$td(class = "atc-numeric-col", "")

     return(td_tag)  # skip all further formatting for this cell
      } 

        key <- paste(row_num, j, sep = ":")
        v <- fmt_map[key]
        catg <- if (is.na(v)) "text" else as.character(v)

	# --- Suppression markers "-" and "-s-" should count as employment ---
    # Do this only for data columns (j > 1), not the label column
    if (j > 1 && is.character(val)) {
    val_trim <- trimws(tolower(val))
    if (val_trim %in% c("-", "-s-", "N/A")) {
    catg <- "employment"
  }
} 
		  
# --- Reclassify BEFORE setting the class (no new categories except "decimal") ---
# If Excel style is text/number but the user typed $, %, or a decimal, promote appropriately
if (j > 1 && catg %in% c("text", "employment") && is.character(val) && nzchar(trimws(val))) {

  val_trim <- trimws(val)

  # Currency (explicit $)
  if (grepl("^\\$", val_trim)) {
    catg <- if (grepl("\\.[0-9]+", val_trim)) "currency" else "annual"

  # Percentage (explicit %)
  } else if (grepl("%$", val_trim)) {
    catg <- "percentage"

  # Decimal number (no $ or %, but contains a decimal point)
  } else if (
    grepl("^[0-9]+\\.[0-9]+$", val_trim) &&
    !grepl("[%$]", val_trim)
  ) {
    catg <- "decimal"
  }
}

        # Choose the class from the (possibly updated) catg
        cell_class <- if (j == 1) {
          "atc-first-col"
        } else if (catg %in% c("currency","annual","percentage","employment","decimal")) {
          "atc-numeric-col"
        } else {
          "atc-text-col"
        }


		  
        # --- Numeric rendering using the (possibly updated) catg ---

decimal_places_from_format <- function(code) {
  if (is.na(code)) return(NA_integer_)
  m <- regexec("\\.(0+|#+)", code)
  hit <- regmatches(code, m)[[1]]
  if (length(hit) == 0) return(0L)
  nchar(hit[2])
}		  
 
		  if (j > 1 && catg %in% c("currency","annual","percentage","employment","decimal")) {

  original_text <- val
  cleaned <- gsub("[,\\s\\$%]", "", val)
  num_val <- suppressWarnings(as.numeric(cleaned))

  if (!is.na(num_val)) {

    # Look up formatCode for this cell
    fmt_row <- fmt_df[fmt_df$row == row_num & fmt_df$col == j, , drop = FALSE]
    fmt_code <- if (nrow(fmt_row) == 1) fmt_row$formatCode else NA_character_

    dec_places <- decimal_places_from_format(fmt_code)

    val <- switch(catg,
          annual =  paste0("$", formatC(num_val, format = "f", digits = 0, big.mark = ",")),
        currency =  paste0("$", formatC(num_val, format = "f",digits = ifelse(is.na(dec_places), 2, dec_places), big.mark = ",")),
      percentage = {
        if (grepl("%\\s*$", original_text)) {
          paste0(formatC(num_val, format = "f",
                          digits = ifelse(is.na(dec_places), 1, dec_places)), "%")
        } else {
          paste0(formatC(num_val * 100, format = "f",
                          digits = ifelse(is.na(dec_places), 1, dec_places)), "%")
        }
      },

      decimal =
        formatC(num_val, format = "f",
                digits = ifelse(is.na(dec_places), 1, dec_places),
                big.mark = ","),

      employment =
        if (!is.na(dec_places) && dec_places > 0) {
          formatC(num_val, format = "f",
                  digits = dec_places, big.mark = ",")
        } else {
          formatC(num_val, format = "f", digits = 0, big.mark = ",")
        },

      val
    )
  }
}


        td_tag <- tags$td(class = cell_class, val)



        # Apply indent padding for first column (original behavior)
        if (j == 1 && as.character(row_num) %in% names(indent_map)) {
          indent_level <- indent_map[as.character(row_num)]
          padding_val <- 12 + indent_level * 12
          td_tag$attribs$style <- paste0("padding-left: ", padding_val, "px;")
        }

        td_tag
      })
    )
  })

  # ---------------------------
  # Compose table
  # ---------------------------
  skip_link <- tags$a(
    href = paste0("#after-table", table_number),
    class = "atc-skip-link",
    "Skip table"
  )

  anchor_after_table <- tags$div(
    id = paste0("after-table", table_number),
    tabindex = "-1",
	  class = "sr-only"
  )

  width_style <- tags$style(
    type = "text/css",
    HTML(paste0(".atc-first-col { width: ", col_width_1, "px !important; }"))
  )

  html_table <- tags$table(
    class = "atc-table",
    `aria-label` = ada_caption,
    tags$thead(
      if (has_group_headers) top_row,
      second_row
    ),
    tags$tbody(body_rows)
  )

  # --- Build multi-footnote block from rows after the data table ---
  foot_start <- min(nrow(df_raw), data_end + 1L)
  if (foot_start > nrow(df_raw)) {
    source_note <- ""
  } else {
    foot_block <- df_raw[seq.int(foot_start, nrow(df_raw)), , drop = FALSE]

    # Flatten row-major, drop blanks, and join with literal <br>
    flat_vals <- as.vector(t(as.matrix(foot_block)))
    flat_vals <- trimws(as.character(flat_vals))
    flat_vals <- flat_vals[!(is.na(flat_vals) | flat_vals == "")]
    source_note <- paste(flat_vals, collapse = "<br>")
  }

  html_body <- tags$div(
    class = "article-table-container",
    tags$div(class = "atc-title", page_title),
    width_style,
    skip_link,
    html_table,
    anchor_after_table,
    tags$div(class = "atc-footer", HTML(source_note))
  )

  # --- CSS (your original classes)
  custom_css <- "
/* Applied to any HTML table content using SCSS*/
			.article-table-container {
			    max-width: 880px;
			    margin: 0 auto;
				margin-bottom: 10px;
				overflow-x: auto;

			    .atc-title {
			        text-align: center;
			        font-size: 16px;
			        font-family: Arial, sans-serif;
			        color: #003764;
			        font-weight: bold;
			        margin-bottom: 10px;
					background-color: #f5ffff;
			    }

			    .atc-table {
			        font-family: Arial, sans-serif;
			        color: #003764;
			        font-size: 12px;
			        max-width: 880px;
			        margin: 0 auto;
			        border-collapse: collapse;
			        width: 100%;

			        .atc-header1 {
			            font-size: 14px;
			            font-weight: bold;
			            color: #003764;
			            background-color: #f5ffff;
			            padding: 6px;
			            vertical-align: bottom;
			        }

			        .atc-header2 {
			            border-bottom: 3px solid #003764;
			        }

			        .atc-group-header:not(:empty) {
			            border-bottom: 1px solid #003764;
			        }

					.atc-first-col {
						min-width: 220px;
					    position: sticky;
					    left: 0;
					    background-color: #f5ffff;
					    z-index: 2;
					    box-shadow: 2px 0 5px rgba(0,0,0,0.1);
					}

					.atc-table th {
					    position: relative;
						z-index: 1;
					}

			        .atc-first-col[data-indent] {
			            padding-left: calc(12px + (attr(data-indent integer) * 12px));
			        }

			        .atc-text-col {
			            text-align: left;
			            font-weight: normal;
			        }

			        .atc-numeric-col {
			            text-align: right;
			            white-space: nowrap;
			        }

			        .atc-align-left   { text-align: left; }
			        .atc-align-right  { text-align: right; }
			        .atc-align-center { text-align: center; }

			        .atc-bold-row td {
			            font-weight: bold;
			        }

			        tr:nth-child(even) td {
			            background-color: #f8f8f8;
			        }

			        td {
			            padding: 6px;
			            border-bottom: 2px solid #dfdfdf;
			            vertical-align: top;
						background-color: #f5ffff;
			        }
			    }

			    .atc-footer {
			        font-size: 11px;
			        font-style: italic;
			        font-family: Arial, sans-serif;
			        color: #003764;
			        margin-top: 10px;
			        text-align: left;
			    }

			    .atc-skip-link {
			        position: absolute;
			        top: -40px;          /* Hide it above the viewport */
			        left: 0;
			        background: transparent;
			        color: transparent;
			        padding: 8px;
			        z-index: 100;
			        border: none;
			        text-decoration: none;
			    }
			    .atc-skip-link:focus {
			        top: 0;                          /* Bring it into view */
			        background: transparent;         /* Visible background */
			        color: transparent;              /* Visible text color */
			        outline: 2px solid transparent;  /* Optional: visible focus outline */
			    }
			}
"


  # include metadata and title
  page_dep <- htmlDependency(
    name = "page-meta",
    version = "1.0",
    src     = c(file = ""),
    head = paste0(
      '<meta charset="utf-8">\n',
      '<title>', output_name, '</title>\n'
    )
  )

  # save without any forced background color
  save_html(
    tagList(html_body, page_dep),
    file = output_path
  )

  # optional: add lang="en" afterward
  txt <- readLines(output_path, encoding = "UTF-8")
  txt[1] <- sub("<html>", '<html lang="en">', txt[1], fixed = TRUE)
  writeLines(txt, output_path, useBytes = TRUE)

  message("✅ HTML saved to: ", output_path)

  }










