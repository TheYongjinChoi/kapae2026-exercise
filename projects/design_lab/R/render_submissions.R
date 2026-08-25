# KAPAE 2026 — simple instructor batch renderer
# 학생 QMD 원본은 수정하지 않고, 임시 복사본에서 최신 report_helpers.R를 추가한 뒤 렌더합니다.

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite 패키지가 필요합니다: install.packages('jsonlite')")
}

repo_root <- system2(
  "git",
  c("rev-parse", "--show-toplevel"),
  stdout = TRUE
)[1]

repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)

qmd_dirs <- c(
  file.path(repo_root, "projects", "design_lab", "submissions"),
  file.path(repo_root, "design_lab", "submissions"),
  file.path(repo_root, "projects", "submissions")
)

qmd_dirs <- qmd_dirs[dir.exists(qmd_dirs)]

qmds <- sort(unique(unlist(lapply(
  qmd_dirs,
  list.files,
  pattern = "\\.qmd$",
  full.names = TRUE,
  ignore.case = TRUE
))))

if (!length(qmds)) stop("제출 QMD를 찾지 못했습니다.")

students_dir <- file.path(repo_root, "docs", "students")
meta_dir <- file.path(repo_root, "docs", "meta")
dir.create(students_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(meta_dir, recursive = TRUE, showWarnings = FALSE)

lab_source <- 'source("https://raw.githubusercontent.com/TheYongjinChoi/kapae2026-exercise/main/projects/design_lab/R/lab_helpers.R")'
report_source <- 'source("https://raw.githubusercontent.com/TheYongjinChoi/kapae2026-exercise/main/projects/design_lab/R/report_helpers.R")'

read_yaml <- function(lines, key) {
  z <- grep(paste0("^", key, ":"), lines, value = TRUE)
  if (!length(z)) return(NA_character_)
  x <- trimws(sub("^[^:]+:", "", z[1]))
  gsub("^['\"]|['\"]$", "", x)
}

read_title <- function(lines) {
  z <- grep("^\\s*research_title\\s*<-", lines, value = TRUE)
  if (!length(z)) return("연구 제목 미입력")
  x <- trimws(sub("^\\s*research_title\\s*<-\\s*", "", z[1]))
  x <- sub('^"(.*)"$', "\\1", x)
  x <- sub("^'(.*)'$", "\\1", x)
  if (nzchar(x)) x else "연구 제목 미입력"
}

remove_auto_submit <- function(lines) {
  hit <- grep("^#\\|\\s*label:\\s*auto-submit\\s*$", lines)
  if (!length(hit)) return(lines)

  h <- hit[1]
  starts <- which(seq_along(lines) < h & grepl("^```\\{r", lines))
  ends <- which(seq_along(lines) > h & grepl("^```\\s*$", lines))

  if (!length(starts) || !length(ends)) return(lines)

  lines[-seq.int(max(starts), min(ends))]
}

for (i in seq_along(qmds)) {

  qmd <- qmds[i]
  cat(sprintf("[%d/%d] %s\n", i, length(qmds), basename(qmd)))

  lines <- readLines(qmd, warn = FALSE, encoding = "UTF-8")

  student_id <- read_yaml(lines, "student-id")
  method <- tolower(read_yaml(lines, "design-method"))
  research_title <- read_title(lines)

  if (is.na(student_id) || is.na(method)) {
    cat("  ✗ student-id/design-method 없음\n")
    next
  }

  # 핵심: 학생 QMD의 기존 lab_helpers source 바로 다음 줄에
  # report_helpers source를 직접 삽입합니다.
  lab_hit <- which(trimws(lines) == lab_source)

  if (!length(lab_hit)) {
    cat("  ✗ lab_helpers.R source 줄을 찾지 못함\n")
    next
  }

  if (!any(trimws(lines) == report_source)) {
    lines <- append(lines, report_source, after = lab_hit[1])
  }

  # 모든 결과 코드 숨기기
  setup_end <- which(
    seq_along(lines) > lab_hit[1] &
      grepl("^```\\s*$", lines)
  )[1]

  lines <- append(
    lines,
    "knitr::opts_chunk$set(echo = FALSE)",
    after = setup_end - 1
  )

  # 편집용 제목/안내는 최종 결과에서 제거
  lines[trimws(lines) %in% c(
    "# 학생 입력 1. 연구 배경",
    "# 학생 입력 2. 데이터와 방법론"
  )] <- ""

  lines[grepl("^각 항목은 \\*\\*1~3문장", lines)] <- ""
  lines[grepl("^실제로 사용할 데이터를 \\*\\*직접 확인", lines)] <- ""
  lines[grepl("^이 질문에 답하는 과정 자체가", lines)] <- ""

  # 강사 렌더에서는 QMD를 다시 제출하지 않음
  lines <- remove_auto_submit(lines)

  work <- tempfile(paste0("kapae_", student_id, "_"))
  dir.create(work)

  temp_qmd <- file.path(work, basename(qmd))
  writeLines(lines, temp_qmd, useBytes = TRUE)

  html_name <- paste0(student_id, "_", method, ".html")

  oldwd <- getwd()
  setwd(work)

  status <- system2(
    "quarto",
    c(
      "render",
      shQuote(basename(temp_qmd)),
      "--to", "html",
      "--output", shQuote(html_name)
    )
  )

  setwd(oldwd)

  if (!identical(status, 0L)) {
    cat("  ✗ render 실패\n")
    unlink(work, recursive = TRUE)
    next
  }

  src_html <- file.path(work, html_name)

  if (!file.exists(src_html)) {
    cat("  ✗ HTML 없음\n")
    unlink(work, recursive = TRUE)
    next
  }

  file.copy(
    src_html,
    file.path(students_dir, html_name),
    overwrite = TRUE
  )

  jsonlite::write_json(
    list(
      student_id = student_id,
      method = method,
      research_title = research_title,
      html_path = file.path("students", html_name),
      submitted_at = format(Sys.time(), tz = "UTC", usetz = TRUE)
    ),
    file.path(meta_dir, paste0(student_id, "_", method, ".json")),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  unlink(work, recursive = TRUE)

  cat("  ✓ docs/students/", html_name, "\n", sep = "")
}

index_qmd <- file.path(repo_root, "docs", "index.qmd")

if (file.exists(index_qmd)) {
  cat("\n갤러리 갱신...\n")
  system2("quarto", c("render", shQuote(index_qmd)))
}
