SUPABASE_URL <- "https://mztyhpckshnqcklogrsn.supabase.co"
SUPABASE_KEY <- "sb_publishable_pflU44StAqW5XTy94LsuJA_p_zMRtVv"   # anon public key

.tracker_env <- new.env()
.tracker_env$session_id <- paste0(
  format(Sys.time(), "%Y%m%d%H%M%S"), "-",
  paste(sample(letters, 6, TRUE), collapse = "")
)

set_student <- function(id) {
  if (missing(id) || !nzchar(id) || id == "ID 입력") {
    stop("첫 청크의 set_student()에 본인 ID를 입력하세요.", call. = FALSE)
  }
  .tracker_env$student_id <- id
  invisible(id)
}

track <- function(exercise_id, expr, chapter = NA_character_) {
  code <- paste(deparse(substitute(expr)), collapse = "\n")
  t0 <- Sys.time()

  out <- tryCatch(
    {
      val <- withVisible(eval.parent(substitute(expr)))
      list(status = "ok",
           output = paste(utils::capture.output(print(val$value)), collapse = "\n"),
           value = val$value, visible = val$visible)
    },
    error = function(e) {
      list(status = "error", output = conditionMessage(e),
           value = NULL, visible = FALSE)
    }
  )

  payload <- list(
    student_id  = .tracker_env$student_id %||% "unknown",
    chapter     = chapter,
    exercise_id = exercise_id,
    code        = code,
    output      = substr(out$output, 1, 5000),
    status      = out$status,
    elapsed_sec = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    session_id  = .tracker_env$session_id,
    r_version   = paste(R.version$major, R.version$minor, sep = ".")
  )

  try(
    httr2::request(paste0(SUPABASE_URL, "/rest/v1/submissions")) |>
      httr2::req_headers(
        apikey = SUPABASE_KEY,
        Authorization = paste("Bearer", SUPABASE_KEY),
        `Content-Type` = "application/json",
        Prefer = "return=minimal"
      ) |>
      httr2::req_body_json(payload) |>
      httr2::req_timeout(5) |>
      httr2::req_perform(),
    silent = TRUE
  )

  if (out$status == "error") stop(out$output, call. = FALSE)
  if (out$visible) out$value else invisible(out$value)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

classify_error <- function(msg) {
  if (grepl("could not find function", msg)) return("함수없음")
  if (grepl("object .* not found", msg))     return("객체없음")
  if (grepl("cannot open|No such file", msg)) return("파일경로")
  if (grepl("unused argument|argument", msg)) return("인수오류")
  if (grepl("undefined columns|subscript", msg)) return("인덱싱")
  if (grepl("missing value|NA/NaN", msg))    return("결측값")
  if (grepl("levels|factor", msg))           return("factor수준")
  if (grepl("non-numeric|invalid 'type'", msg)) return("자료형")
  "기타"
}
