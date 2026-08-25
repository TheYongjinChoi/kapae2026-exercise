# KAPAE 2026 — post-render HTML uploader
# Runs automatically after Quarto finishes rendering a student research-design HTML.

suppressPackageStartupMessages({
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite 패키지가 필요합니다: install.packages('jsonlite')")
  }
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("httr2 패키지가 필요합니다: install.packages('httr2')")
  }
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) y else x
}

read_frontmatter_value <- function(lines, key) {
  hit <- grep(
    paste0("^\\s*", gsub("-", "\\\\-", key), "\\s*:"),
    lines,
    value = TRUE
  )
  if (!length(hit)) return(NA_character_)
  value <- sub("^[^:]+:\\s*", "", hit[[1]])
  value <- trimws(value)
  gsub("^['\"]|['\"]$", "", value)
}

read_research_title <- function(lines) {
  hit <- grep("^\\s*research_title\\s*<-\\s*", lines, value = TRUE)
  if (!length(hit)) return("연구 제목 미입력")

  value <- sub("^\\s*research_title\\s*<-\\s*", "", hit[[1]])
  value <- trimws(value)
  value <- gsub("^['\"]|['\"]\\s*$", "", value)

  if (!nzchar(value) || grepl("연구 제목을 입력하세요", value, fixed = TRUE)) {
    return("연구 제목 미입력")
  }
  value
}

find_config <- function(project_root) {
  candidates <- c(
    file.path(project_root, "design_lab", "config.json"),
    file.path(project_root, "projects", "design_lab", "config.json")
  )
  candidates[file.exists(candidates)][1] %||% NA_character_
}

project_root <- Sys.getenv("QUARTO_PROJECT_ROOT")
if (!nzchar(project_root)) project_root <- getwd()
project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)

output_env <- Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES")
if (!nzchar(output_env)) {
  quit(save = "no", status = 0)
}

outputs <- strsplit(output_env, "\n", fixed = TRUE)[[1]]
outputs <- outputs[nzchar(outputs)]
outputs <- outputs[grepl("\\.html$", outputs, ignore.case = TRUE)]

# Only student research-design pages are submitted.
outputs <- outputs[
  grepl(
    "^02_.+_(prediction|dml|did|iv|matching|causal_forest|rd)_research_design\\.html$",
    basename(outputs),
    ignore.case = TRUE
  )
]

if (!length(outputs)) {
  quit(save = "no", status = 0)
}

config_path <- find_config(project_root)
if (is.na(config_path)) {
  message("HTML 제출 건너뜀: design_lab/config.json을 찾지 못했습니다.")
  quit(save = "no", status = 0)
}

config <- jsonlite::fromJSON(config_path)
endpoint <- config$submission_endpoint %||% ""
course_key <- config$course_key %||% ""

if (!nzchar(endpoint)) {
  message("HTML 제출 건너뜀: submission_endpoint가 비어 있습니다.")
  quit(save = "no", status = 0)
}

for (output_rel in outputs) {
  html_path <- file.path(project_root, output_rel)
  if (!file.exists(html_path)) next

  # Default Quarto document rendering places HTML beside its source QMD.
  qmd_path <- sub("\\.html$", ".qmd", html_path, ignore.case = TRUE)

  if (!file.exists(qmd_path)) {
    message("HTML 제출 건너뜀: 대응하는 QMD를 찾지 못했습니다: ", basename(html_path))
    next
  }

  qmd_lines <- readLines(qmd_path, warn = FALSE, encoding = "UTF-8")
  student_id <- read_frontmatter_value(qmd_lines, "student-id")
  method <- tolower(read_frontmatter_value(qmd_lines, "design-method"))
  research_title <- read_research_title(qmd_lines)

  if (is.na(student_id) || is.na(method)) {
    message("HTML 제출 건너뜀: student-id 또는 design-method를 읽지 못했습니다.")
    next
  }

  html_content <- paste(
    readLines(html_path, warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )

  payload <- list(
    submission_type = "html",
    student_id = student_id,
    method = method,
    research_title = research_title,
    course_key = course_key,
    content = html_content
  )

  result <- tryCatch({
    resp <- httr2::request(endpoint) |>
      httr2::req_method("POST") |>
      httr2::req_headers(`Content-Type` = "application/json") |>
      httr2::req_body_json(payload, auto_unbox = TRUE) |>
      httr2::req_timeout(90) |>
      httr2::req_perform()

    body <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    message(
      "HTML 제출 완료: ",
      body$path %||% paste0(student_id, "_", method, ".html")
    )
    TRUE
  }, error = function(e) {
    # Preserve the student's successful local render even if upload fails.
    message(
      "HTML은 정상 렌더링되었지만 자동 제출에 실패했습니다: ",
      conditionMessage(e)
    )
    FALSE
  })
}
