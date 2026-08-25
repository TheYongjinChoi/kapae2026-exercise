# Instructor-side collection and rendering helpers

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

find_repo_root <- function(start = getwd()) {
  p <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    if (dir.exists(file.path(p, ".git"))) return(p)
    parent <- dirname(p)
    if (identical(parent, p)) stop("Git repository root를 찾지 못했습니다.")
    p <- parent
  }
}

safe_git_pull <- function(repo_root) {
  git <- Sys.which("git")
  if (!nzchar(git)) stop("git CLI를 찾을 수 없습니다.")
  status <- system2(git, c("-C", shQuote(repo_root), "status", "--porcelain"), stdout = TRUE, stderr = TRUE)
  if (length(status) && any(nzchar(status))) {
    warning("로컬 변경사항이 있어 자동 pull을 건너뜁니다. 먼저 commit/stash한 뒤 다시 실행하세요.")
    return(FALSE)
  }
  code <- system2(git, c("-C", shQuote(repo_root), "pull", "--ff-only", "origin", "main"))
  identical(code, 0L)
}

read_frontmatter_value <- function(path, key) {
  x <- readLines(path, warn = FALSE, encoding = "UTF-8", n = 80)
  hit <- grep(paste0("^", gsub("-", "\\-", key), ":"), x, value = TRUE)
  if (!length(hit)) return(NA_character_)
  val <- sub(paste0("^", gsub("-", "\\-", key), ":\\s*"), "", hit[1])
  gsub('^"|"$', "", trimws(val))
}

collect_submissions <- function(repo_root = find_repo_root()) {
  submission_dir <- file.path(repo_root, "projects", "submissions")
  dir.create(submission_dir, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(submission_dir, pattern = "\\.qmd$", full.names = TRUE)
  if (!length(files)) {
    return(data.frame(
      student_id = character(), method = character(), title = character(),
      qmd = character(), modified = as.POSIXct(character()), html = character()
    ))
  }
  data.frame(
    student_id = vapply(files, read_frontmatter_value, character(1), key = "student-id"),
    method = vapply(files, read_frontmatter_value, character(1), key = "design-method"),
    title = vapply(files, read_frontmatter_value, character(1), key = "title"),
    qmd = files,
    modified = file.info(files)$mtime,
    html = file.path(repo_root, "projects", "rendered", paste0(tools::file_path_sans_ext(basename(files)), ".html")),
    stringsAsFactors = FALSE
  )
}

render_submission_html <- function(qmd_path, repo_root = find_repo_root(), force = FALSE) {
  quarto <- Sys.which("quarto")
  if (!nzchar(quarto)) stop("Quarto CLI를 찾을 수 없습니다.")
  submission_dir <- dirname(qmd_path)
  rendered_dir <- file.path(repo_root, "projects", "rendered")
  dir.create(rendered_dir, recursive = TRUE, showWarnings = FALSE)
  output <- file.path(rendered_dir, paste0(tools::file_path_sans_ext(basename(qmd_path)), ".html"))

  if (!force && file.exists(output) && file.info(output)$mtime >= file.info(qmd_path)$mtime) {
    return(output)
  }

  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(submission_dir)
  code <- system2(
    quarto,
    c("render", shQuote(basename(qmd_path)), "--to", "html", "--output-dir", "../rendered"),
    stdout = TRUE,
    stderr = TRUE
  )
  if (!file.exists(output)) {
    warning("렌더 실패: ", basename(qmd_path), "\n", paste(code, collapse = "\n"))
    return(NA_character_)
  }
  output
}

render_all_submissions <- function(projects, repo_root = find_repo_root(), force = FALSE) {
  if (!nrow(projects)) return(projects)
  projects$html <- vapply(projects$qmd, render_submission_html, character(1), repo_root = repo_root, force = force)
  projects
}

relative_dashboard_links <- function(projects) {
  if (!nrow(projects)) return(projects)
  projects$link <- ifelse(
    is.na(projects$html), NA_character_,
    paste0("../rendered/", basename(projects$html))
  )
  projects
}

student_cards_html <- function(projects) {
  if (!nrow(projects)) return("<p><em>아직 제출된 프로젝트가 없습니다.</em></p>")
  projects <- relative_dashboard_links(projects)
  ord <- order(projects$method, projects$student_id)
  projects <- projects[ord, , drop = FALSE]
  cards <- vapply(seq_len(nrow(projects)), function(i) {
    p <- projects[i, ]
    link <- if (is.na(p$link)) "#" else p$link
    sprintf(
      '<div class="project-card"><div class="project-method">%s</div><h3>%s</h3><p><strong>ID:</strong> %s</p><p><a href="%s">Rendered project page</a></p><p class="updated">Updated: %s</p></div>',
      escape_html(p$method),
      escape_html(p$title),
      escape_html(p$student_id),
      escape_html(link),
      format(p$modified, "%Y-%m-%d %H:%M")
    )
  }, character(1))
  paste(cards, collapse = "\n")
}

submission_summary <- function(projects) {
  if (!nrow(projects)) return(data.frame(method = character(), n = integer()))
  out <- as.data.frame(table(projects$method), stringsAsFactors = FALSE)
  names(out) <- c("method", "n")
  out[out$n > 0, , drop = FALSE]
}
