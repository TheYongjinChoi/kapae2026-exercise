# KAPAE 2026 — submitted QMD batch renderer (fixed)
#
# 학생 제출 QMD는 그대로 보존하고 임시 복사본만 수정하여 렌더합니다.
#
# Fixes:
# 1) 최신 report_helpers.R를 setup 직후 반드시 source
# 2) 모든 결과 코드 echo 숨김
# 3) "학생 입력 1/2" 편집용 제목과 안내문을 최종 HTML에서 제거
# 4) 오래된 auto-submit chunk 제거
# 5) HTML -> docs/students, metadata -> docs/meta
# 6) 마지막에 docs/index.qmd 한 번 렌더

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(as.character(x))) y else x
}

repo_root <- tryCatch(
  system2("git", c("rev-parse", "--show-toplevel"), stdout = TRUE, stderr = FALSE)[1],
  error = function(e) NA_character_
)

if (is.na(repo_root) || !nzchar(repo_root)) {
  stop("kapae2026-exercise Git 저장소 안에서 실행하세요.")
}

repo_root <- normalizePath(repo_root, winslash = "/", mustWork = TRUE)

quarto <- Sys.which("quarto")
if (!nzchar(quarto)) {
  stop("Quarto CLI를 찾지 못했습니다.")
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite 패키지가 필요합니다: install.packages('jsonlite')")
}

submission_candidates <- c(
  file.path(repo_root, "projects", "design_lab", "submissions"),
  file.path(repo_root, "design_lab", "submissions"),
  file.path(repo_root, "projects", "submissions")
)

submission_dirs <- submission_candidates[dir.exists(submission_candidates)]

if (!length(submission_dirs)) {
  stop(
    "학생 제출 폴더를 찾지 못했습니다.\n",
    paste(submission_candidates, collapse = "\n")
  )
}

qmds <- sort(unique(unlist(lapply(
  submission_dirs,
  list.files,
  pattern = "\\.qmd$",
  full.names = TRUE,
  ignore.case = TRUE
))))

if (!length(qmds)) {
  stop("제출 폴더에 QMD 파일이 없습니다.")
}

students_dir <- file.path(repo_root, "docs", "students")
meta_dir <- file.path(repo_root, "docs", "meta")

dir.create(students_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(meta_dir, recursive = TRUE, showWarnings = FALSE)

read_yaml_value <- function(lines, key) {
  hit <- grep(
    paste0("^\\s*", gsub("-", "\\\\-", key), "\\s*:"),
    lines,
    value = TRUE
  )

  if (!length(hit)) return(NA_character_)

  out <- sub("^[^:]+:\\s*", "", hit[1])
  out <- trimws(out)
  gsub("^['\"]|['\"]$", "", out)
}

read_research_title <- function(lines) {
  hit <- grep(
    "^\\s*research_title\\s*<-\\s*",
    lines,
    value = TRUE
  )

  if (!length(hit)) return("연구 제목 미입력")

  out <- sub(
    "^\\s*research_title\\s*<-\\s*",
    "",
    hit[1]
  )

  out <- trimws(out)

  # 한 줄 문자열 입력을 기준으로 양쪽 quote 제거
  out <- sub('^"(.*)"\\s*$', "\\1", out)
  out <- sub("^'(.*)'\\s*$", "\\1", out)

  if (!nzchar(out)) "연구 제목 미입력" else out
}

parse_filename <- function(path) {
  bn <- basename(path)

  patterns <- c(
    "^02_(.+)_(prediction|dml|did|iv|matching|causal_forest|rd)_research_design\\.qmd$",
    "^(.+)_(prediction|dml|did|iv|matching|causal_forest|rd)\\.qmd$"
  )

  for (pat in patterns) {
    m <- regexec(pat, bn, ignore.case = TRUE)
    z <- regmatches(bn, m)[[1]]

    if (length(z) == 3) {
      return(list(
        student_id = z[2],
        method = tolower(z[3])
      ))
    }
  }

  list(student_id = NA_character_, method = NA_character_)
}

remove_labelled_chunk <- function(lines, label) {
  repeat {
    hit <- grep(
      paste0("^#\\|\\s*label:\\s*", label, "\\s*$"),
      lines
    )

    if (!length(hit)) break

    h <- hit[1]

    starts <- which(
      seq_along(lines) < h &
        grepl("^```\\{r[^}]*\\}\\s*$", lines)
    )

    ends <- which(
      seq_along(lines) > h &
        grepl("^```\\s*$", lines)
    )

    if (!length(starts) || !length(ends)) break

    a <- max(starts)
    b <- min(ends)

    lines <- lines[-seq.int(a, b)]
  }

  lines
}

insert_after_setup <- function(lines) {
  # 기존 파일에 이미 최신 helper가 있으면 중복 source하지 않습니다.
  if (any(grepl("projects/design_lab/R/report_helpers\\.R", lines))) {
    return(lines)
  }

  setup_label <- grep(
    "^#\\|\\s*label:\\s*setup\\s*$",
    lines
  )

  if (length(setup_label)) {
    h <- setup_label[1]

    ends <- which(
      seq_along(lines) > h &
        grepl("^```\\s*$", lines)
    )

    if (length(ends)) {
      pos <- min(ends)

      override_chunk <- c(
        "",
        "```{r}",
        "#| label: instructor-report-overrides",
        "#| include: false",
        'source("https://raw.githubusercontent.com/TheYongjinChoi/kapae2026-exercise/main/projects/design_lab/R/report_helpers.R")',
        "knitr::opts_chunk$set(echo = FALSE)",
        "```",
        ""
      )

      return(
        append(lines, override_chunk, after = pos)
      )
    }
  }

  # setup label이 없는 매우 오래된 파일의 fallback:
  # 첫 번째 R chunk 뒤에 삽입
  starts <- which(grepl("^```\\{r[^}]*\\}\\s*$", lines))
  if (length(starts)) {
    ends <- which(
      seq_along(lines) > starts[1] &
        grepl("^```\\s*$", lines)
    )

    if (length(ends)) {
      override_chunk <- c(
        "",
        "```{r}",
        "#| label: instructor-report-overrides",
        "#| include: false",
        'source("https://raw.githubusercontent.com/TheYongjinChoi/kapae2026-exercise/main/projects/design_lab/R/report_helpers.R")',
        "knitr::opts_chunk$set(echo = FALSE)",
        "```",
        ""
      )

      return(
        append(lines, override_chunk, after = min(ends))
      )
    }
  }

  stop("setup chunk를 찾지 못했습니다.")
}

hide_editing_sections <- function(lines) {
  # 편집할 때만 필요한 제목/안내는 최종 연구보고서에는 노출하지 않습니다.
  exact_remove <- c(
    "# 학생 입력 1. 연구 배경",
    "# 학생 입력 2. 데이터와 방법론"
  )

  lines[trimws(lines) %in% exact_remove] <- ""

  patterns <- c(
    "^각 항목은 \\*\\*1~3문장\\*\\*으로 짧게 작성하세요\\.",
    "^실제로 사용할 데이터를 \\*\\*직접 확인하거나 구체적으로 떠올리며\\*\\* 아래 질문에 답하세요\\.",
    "^이 질문에 답하는 과정 자체가 현재 데이터가 선택한 방법론에 적절한지 확인하는 과정입니다\\."
  )

  for (pat in patterns) {
    lines[grepl(pat, lines)] <- ""
  }

  lines
}

prepare_temp_qmd <- function(source_qmd, workdir) {
  lines <- readLines(
    source_qmd,
    warn = FALSE,
    encoding = "UTF-8"
  )

  # 오래된 QMD source 제출을 강사 렌더 때 다시 실행하지 않음
  lines <- remove_labelled_chunk(
    lines,
    "auto-submit"
  )

  # 중요: setup의 lab_helpers.R 실행이 끝난 뒤 최신 결과 helper 로드
  lines <- insert_after_setup(lines)

  # 학생 편집용 heading 제거
  lines <- hide_editing_sections(lines)

  out <- file.path(
    workdir,
    basename(source_qmd)
  )

  writeLines(
    lines,
    out,
    useBytes = TRUE
  )

  out
}

render_one <- function(qmd) {
  original <- readLines(
    qmd,
    warn = FALSE,
    encoding = "UTF-8"
  )

  student_id <- read_yaml_value(
    original,
    "student-id"
  )

  method <- tolower(
    read_yaml_value(
      original,
      "design-method"
    )
  )

  fallback <- parse_filename(qmd)

  if (is.na(student_id) || !nzchar(student_id)) {
    student_id <- fallback$student_id
  }

  if (is.na(method) || !nzchar(method)) {
    method <- fallback$method
  }

  valid_methods <- c(
    "prediction",
    "dml",
    "did",
    "iv",
    "matching",
    "causal_forest",
    "rd"
  )

  if (is.na(student_id) ||
      !grepl("^[A-Za-z0-9_-]{2,60}$", student_id)) {
    stop("student-id를 읽지 못했습니다.")
  }

  if (is.na(method) || !method %in% valid_methods) {
    stop("design-method를 읽지 못했습니다.")
  }

  research_title <- read_research_title(original)

  work <- tempfile(
    paste0("kapae_", student_id, "_")
  )

  dir.create(work)

  on.exit(
    unlink(work, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  temp_qmd <- prepare_temp_qmd(
    qmd,
    work
  )

  html_name <- paste0(
    student_id,
    "_",
    method,
    ".html"
  )

  oldwd <- getwd()
  setwd(work)
  on.exit(setwd(oldwd), add = TRUE)

  # 임시폴더에는 _quarto.yml이 없으므로 학생/저장소 post-render가 개입하지 않습니다.
  status <- system2(
    quarto,
    c(
      "render",
      shQuote(basename(temp_qmd)),
      "--to",
      "html",
      "--output",
      shQuote(html_name)
    )
  )

  setwd(oldwd)

  if (!identical(status, 0L)) {
    stop("Quarto render 실패")
  }

  rendered <- file.path(
    work,
    html_name
  )

  if (!file.exists(rendered)) {
    rendered2 <- file.path(
      work,
      sub("\\.qmd$", ".html", basename(temp_qmd), ignore.case = TRUE)
    )

    if (!file.exists(rendered2)) {
      stop("렌더된 HTML을 찾지 못했습니다.")
    }

    rendered <- rendered2
  }

  final_html <- file.path(
    students_dir,
    html_name
  )

  file.copy(
    rendered,
    final_html,
    overwrite = TRUE
  )

  meta <- list(
    student_id = student_id,
    method = method,
    research_title = research_title,
    html_path = file.path(
      "students",
      html_name
    ),
    submitted_at = format(
      Sys.time(),
      tz = "UTC",
      usetz = TRUE
    )
  )

  jsonlite::write_json(
    meta,
    file.path(
      meta_dir,
      paste0(
        student_id,
        "_",
        method,
        ".json"
      )
    ),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  invisible(html_name)
}

cat(
  "\n학생 제출 QMD 일괄 렌더\n",
  "총 ", length(qmds), "개\n\n",
  sep = ""
)

ok <- character()
fail <- character()

for (i in seq_along(qmds)) {
  f <- qmds[i]

  cat(
    sprintf(
      "[%d/%d] %s\n",
      i,
      length(qmds),
      basename(f)
    )
  )

  tryCatch(
    {
      html <- render_one(f)

      cat(
        "  ✓ ",
        html,
        "\n\n",
        sep = ""
      )

      ok <- c(ok, basename(f))
    },
    error = function(e) {
      cat(
        "  ✗ ",
        conditionMessage(e),
        "\n\n",
        sep = ""
      )

      fail <- c(
        fail,
        paste0(
          basename(f),
          ": ",
          conditionMessage(e)
        )
      )
    }
  )
}

# 모든 학생 HTML이 생성된 후 갤러리 index는 딱 한 번 갱신
index_qmd <- file.path(
  repo_root,
  "docs",
  "index.qmd"
)

if (file.exists(index_qmd)) {
  cat("갤러리 index 렌더...\n")

  status <- system2(
    quarto,
    c(
      "render",
      shQuote(index_qmd)
    )
  )

  if (identical(status, 0L)) {
    cat("✓ docs/index.html 갱신\n")
  } else {
    cat("⚠ docs/index.qmd 렌더 실패\n")
  }
}

cat(
  "\n완료: ",
  length(ok),
  "개 성공 / ",
  length(fail),
  "개 실패\n",
  sep = ""
)

if (length(fail)) {
  cat(
    "\n실패 목록:\n",
    paste0("- ", fail, collapse = "\n"),
    "\n",
    sep = ""
  )
}
