library(tidyverse)
library(stringr)
library(purrr)
library(tools)
library(openxlsx)


get_package_function_overview <- function(package_name, exported_only = FALSE) {
  # Get namespace environment
  pkg_env <- asNamespace(package_name)

  # Get function names
  if (exported_only) {
    fn_names <- getNamespaceExports(package_name)
  } else {
    fn_names <- ls(pkg_env, all.names = TRUE)
  }

  # Keep only functions
  fn_names <- fn_names[
    purrr::map_lgl(fn_names, ~ is.function(get(.x, envir = pkg_env)))
  ]

  # Extract structured information
  overview_tbl <- purrr::map_dfr(fn_names, function(fn_name) {
    fn <- get(fn_name, envir = pkg_env)
    fn_formals <- formals(fn)

    # Argument names
    arg_names <- names(fn_formals)

    # Default values (as text)
    defaults <- purrr::map_chr(fn_formals, function(x) {
      if (identical(x, quote(expr = ))) {
        return(NA_character_)
      } else {
        paste(deparse(x), collapse = "")
      }
    })

    tibble(
      package = package_name,
      fn = fn_name,
      argument = arg_names,
      default = defaults,
      has_default = !is.na(defaults),
      exported = fn_name %in% getNamespaceExports(package_name)
    )
  })

  overview_tbl %>%
    arrange(fn, argument)
}


# 1. Build Package Doc Table ---------------------------------------------

build_package_doc_table <- function(package_name, pkg_path = ".") {
  pkg_env <- asNamespace(package_name)

  # --- Helper: get filename of function ---
  get_function_file <- function(fn_name) {
    obj <- get(fn_name, envir = pkg_env)
    srcref <- attr(obj, "srcref")
    if (!is.null(srcref)) {
      return(basename(attr(srcref, "srcfile")$filename))
    } else {
      return(NA_character_)
    }
  }

  # --- Helper: parse Rd documentation ---
  man_path <- file.path(pkg_path, "man")
  rd_files <- list.files(man_path, pattern = "\\.Rd$", full.names = TRUE)

  rd_db <- purrr::map(rd_files, tools::parse_Rd)
  names(rd_db) <- tools::file_path_sans_ext(basename(rd_files))

  extract_rd_info <- function(fn_name) {
    if (!fn_name %in% names(rd_db)) {
      return(list(
        description = NA_character_,
        return_doc = NA_character_,
        param_docs = list()
      ))
    }

    rd <- rd_db[[fn_name]]

    get_section <- function(tag) {
      nodes <- rd[which(sapply(rd, attr, "Rd_tag") == tag)]
      if (length(nodes) == 0) {
        return(NA_character_)
      }
      paste(unlist(nodes), collapse = " ") |> stringr::str_squish()
    }

    description <- get_section("\\description")
    return_doc <- get_section("\\value")

    param_docs <- list()

    arg_section <- rd[which(sapply(rd, attr, "Rd_tag") == "\\arguments")]

    if (length(arg_section) > 0) {
      for (node in arg_section[[1]]) {
        if (attr(node, "Rd_tag") == "\\item") {
          arg_name <- as.character(node[[1]])
          arg_text <- paste(unlist(node[[2]]), collapse = " ")
          param_docs[[arg_name]] <- stringr::str_squish(arg_text)
        }
      }
    }

    list(
      description = description,
      return_doc = return_doc,
      param_docs = param_docs
    )
  }

  # --- Helper: detect validation rules ---
  detect_validation_rules <- function(fn) {
    body_txt <- paste(deparse(body(fn)), collapse = "\n")

    rules <- c(
      str_extract_all(body_txt, "match\\.arg\\([^\\)]*\\)")[[1]],
      str_extract_all(body_txt, "arg_match\\([^\\)]*\\)")[[1]],
      str_extract_all(body_txt, "assert_[a-zA-Z_]+\\([^\\)]*\\)")[[1]],
      str_extract_all(body_txt, "checkmate::[a-zA-Z_]+\\([^\\)]*\\)")[[1]]
    )

    rules <- rules[!is.na(rules)]

    if (length(rules) == 0) {
      return(NA_character_)
    }

    paste(unique(rules), collapse = " | ")
  }

  # --- Function names ---
  fn_names <- ls(pkg_env, all.names = TRUE)
  fn_names <- fn_names[
    map_lgl(fn_names, ~ is.function(get(.x, envir = pkg_env)))
  ]

  exports <- getNamespaceExports(package_name)

  # --- Build table ---
  result <- map_dfr(fn_names, function(fn_name) {
    fn <- get(fn_name, envir = pkg_env)
    formals_list <- formals(fn)
    arg_names <- names(formals_list)

    defaults <- map_chr(formals_list, function(x) {
      if (identical(x, quote(expr = ))) {
        NA_character_
      } else {
        paste(deparse(x), collapse = "")
      }
    })

    rd_info <- extract_rd_info(fn_name)

    validation_rules <- detect_validation_rules(fn)

    tibble(
      filename = get_function_file(fn_name),
      functn = fn_name,
      #exported      = fn_name %in% exports,
      argument = arg_names,
      default = defaults,
      param_doc = map_chr(
        arg_names,
        ~ rd_info$param_docs[[.x]] %||% NA_character_
      ),
      return_doc = rd_info$return_doc,
      description = rd_info$description,
      validation = validation_rules,
      package = package_name,
    )
  })

  result
}


# 3. Excel Export Function -----------------------------------------------

export_package_doc_excel <- function(
  doc_table,
  file = "package_documentation.xlsx"
) {
  wb <- createWorkbook()
  addWorksheet(wb, "API Documentation")

  writeData(wb, 1, doc_table)

  header_style <- createStyle(
    fontSize = 12,
    fontColour = "#FFFFFF",
    fgFill = "#4F81BD",
    halign = "center",
    textDecoration = "bold"
  )

  addStyle(
    wb,
    sheet = 1,
    style = header_style,
    rows = 1,
    cols = seq_len(ncol(doc_table)),
    gridExpand = TRUE
  )

  setColWidths(wb, 1, cols = seq_len(ncol(doc_table)), widths = "auto")

  freezePane(wb, 1, firstRow = TRUE)

  saveWorkbook(wb, file, overwrite = TRUE)
}


# 2. Get Arguments -------------------------------------------------------

devtools::load_all()

doc_table <- build_package_doc_table("fearbase")

export_package_doc_excel(doc_table, "fearbase_documentation.xlsx")
