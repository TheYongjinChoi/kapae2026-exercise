# KAPAE 2026 standalone research-design lab bootstrap
#
# Purpose:
#   Allow a student to start with only 01_student_setup.qmd in a fresh local folder.
#   This script creates the individual Step 2 QMD and the hidden files required
#   to submit the completed HTML automatically after Quarto rendering.

KAPAE_PROJECT_MARKER <- "# KAPAE 2026 standalone research-design lab"

kapae_download <- function(url, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

  status <- tryCatch(
    utils::download.file(
      url = url,
      destfile = dest,
      mode = "wb",
      quiet = TRUE
    ),
    error = function(e) e
  )

  if (inherits(status, "error") || !file.exists(dest)) {
    msg <- if (inherits(status, "error")) conditionMessage(status) else "파일이 생성되지 않았습니다."
    stop(
      "실습 지원파일을 내려받지 못했습니다.\n",
      "URL: ", url, "\n",
      "원인: ", msg
    )
  }

  invisible(dest)
}


ensure_submission_packages <- function() {
  needed <- c("httr2", "jsonlite")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]

  if (!length(missing)) return(invisible(TRUE))

  message(
    "HTML 자동 제출에 필요한 R 패키지를 설치합니다: ",
    paste(missing, collapse = ", ")
  )

  utils::install.packages(
    missing,
    repos = "https://cloud.r-project.org"
  )

  still_missing <- missing[
    !vapply(missing, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(still_missing)) {
    stop(
      "다음 패키지를 설치하지 못했습니다: ",
      paste(still_missing, collapse = ", "),
      "\n직접 install.packages()로 설치한 뒤 다시 실행하세요."
    )
  }

  invisible(TRUE)
}


remove_legacy_qmd_submit <- function(qmd_path) {
  if (!file.exists(qmd_path)) return(invisible(FALSE))

  x <- readLines(qmd_path, warn = FALSE, encoding = "UTF-8")

  hit <- grep(
    "^#\\|\\s*label:\\s*auto-submit\\s*$",
    x
  )

  if (!length(hit)) return(invisible(FALSE))

  label_line <- hit[[1]]

  starts <- which(
    seq_along(x) < label_line &
      grepl("^```\\{r\\}\\s*$", x)
  )

  ends <- which(
    seq_along(x) > label_line &
      grepl("^```\\s*$", x)
  )

  if (!length(starts) || !length(ends)) {
    warning(
      "기존 QMD 제출 블록을 찾았지만 안전하게 제거하지 못했습니다. ",
      "HTML 제출에는 영향이 없지만 QMD도 함께 제출될 수 있습니다."
    )
    return(invisible(FALSE))
  }

  start <- max(starts)
  end <- min(ends)

  x <- x[-seq.int(start, end)]
  writeLines(x, qmd_path, useBytes = TRUE)

  invisible(TRUE)
}


setup_kapae_local_project <- function(output_dir = getwd()) {
  output_dir <- normalizePath(
    output_dir,
    winslash = "/",
    mustWork = TRUE
  )

  quarto_file <- file.path(output_dir, "_quarto.yml")

  if (file.exists(quarto_file)) {
    old <- readLines(quarto_file, warn = FALSE, encoding = "UTF-8")

    if (!any(trimws(old) == KAPAE_PROJECT_MARKER)) {
      stop(
        "현재 폴더에 이미 다른 _quarto.yml이 있습니다.\n",
        "기존 Quarto 프로젝트 설정을 변경하지 않기 위해 자동 설정을 중단했습니다.\n",
        "새 빈 작업폴더를 만든 뒤 01_student_setup.qmd를 그 폴더에서 다시 실행하세요."
      )
    }
  }

  support_dir <- file.path(output_dir, ".kapae")
  dir.create(support_dir, recursive = TRUE, showWarnings = FALSE)

  post_render_url <- kapae_repo_raw(
    "projects/design_lab/R/post_render_submit.R"
  )
  config_url <- kapae_repo_raw(
    "projects/design_lab/config.json"
  )

  kapae_download(
    post_render_url,
    file.path(support_dir, "post_render_submit.R")
  )

  kapae_download(
    config_url,
    file.path(support_dir, "config.json")
  )

  writeLines(
    c(
      KAPAE_PROJECT_MARKER,
      "project:",
      "  type: default",
      "  post-render:",
      "    - .kapae/post_render_submit.R"
    ),
    quarto_file,
    useBytes = TRUE
  )

  invisible(output_dir)
}


inject_report_helpers <- function(qmd_path) {
  if (!file.exists(qmd_path)) return(invisible(FALSE))

  x <- readLines(qmd_path, warn = FALSE, encoding = "UTF-8")

  report_source <- paste0(
    'source("',
    kapae_repo_raw("projects/design_lab/R/report_helpers.R"),
    '")'
  )

  if (any(grepl("report_helpers\\.R", x))) {
    return(invisible(TRUE))
  }

  helper_hit <- grep(
    "projects/design_lab/R/lab_helpers\\.R",
    x
  )

  if (!length(helper_hit)) {
    stop(
      "생성된 Step 2 파일에서 lab_helpers.R source 줄을 찾지 못했습니다."
    )
  }

  pos <- helper_hit[[1]]

  x <- append(
    x,
    report_source,
    after = pos
  )

  # These headings are useful while editing but should not appear as duplicate
  # report sections in the rendered paper-style output.
  x[x == "# 학생 입력 1. 연구 배경"] <-
    "<!-- 학생 입력 1. 연구 배경: 아래 숨김 코드블록의 값을 수정하세요. -->"

  x[x == "# 학생 입력 2. 데이터와 방법론"] <-
    "<!-- 학생 입력 2. 데이터와 방법론: 아래 숨김 코드블록의 값을 수정하세요. -->"

  x <- x[
    !grepl(
      "^각 항목은 \\*\\*1~3문장\\*\\*으로 짧게 작성하세요\\.",
      x
    )
  ]

  x <- x[
    !grepl(
      "^실제로 사용할 데이터를 \\*\\*직접 확인하거나 구체적으로 떠올리며\\*\\* 아래 질문에 답하세요\\.",
      x
    )
  ]

  x <- x[
    !grepl(
      "^이 질문에 답하는 과정 자체가 현재 데이터가 선택한 방법론에 적절한지 확인하는 과정입니다\\.",
      x
    )
  ]

  writeLines(
    x,
    qmd_path,
    useBytes = TRUE
  )

  invisible(TRUE)
}


create_standalone_design_lab <- function(student_id,
                                         method,
                                         output_dir = getwd(),
                                         overwrite = FALSE) {
  # sanitize_id(), check_method(), create_design_qmd(), and kapae_repo_raw()
  # are supplied by lab_helpers.R, which Step 1 sources immediately before this file.
  if (!exists("create_design_qmd", mode = "function")) {
    stop(
      "lab_helpers.R가 먼저 로드되어야 합니다. ",
      "01_student_setup.qmd의 제공된 코드블록을 그대로 실행하세요."
    )
  }

  student_id <- sanitize_id(student_id)
  method <- check_method(method)

  output_dir <- normalizePath(
    output_dir,
    winslash = "/",
    mustWork = TRUE
  )

  ensure_submission_packages()
  setup_kapae_local_project(output_dir)

  create_design_qmd(
    student_id = student_id,
    method = method,
    output_dir = output_dir,
    overwrite = overwrite
  )

  qmd_path <- file.path(
    output_dir,
    sprintf(
      "02_%s_%s_research_design.qmd",
      student_id,
      method
    )
  )

  if (!file.exists(qmd_path)) {
    stop(
      "Step 2 QMD 생성 후 파일을 찾지 못했습니다: ",
      qmd_path
    )
  }

  # The older template submitted the source QMD from an R chunk.
  # Standalone mode uses only the rendered HTML, so remove that chunk.
  remove_legacy_qmd_submit(qmd_path)
  inject_report_helpers(qmd_path)

  message("")
  message("준비 완료")
  message("1. 생성된 파일을 여세요: ", basename(qmd_path))
  message("2. 연구설계 내용을 작성하세요.")
  message("3. HTML Preview/Render를 실행하세요.")
  message("4. 렌더가 끝나면 HTML이 수업 갤러리에 자동 제출됩니다.")
  message("")
  message(
    "갤러리: https://theyongjinchoi.github.io/kapae2026-exercise/"
  )

  invisible(qmd_path)
}
