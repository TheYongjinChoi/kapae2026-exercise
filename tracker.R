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

# ── 정답 규칙 ────────────────────────────────────────────────
# need  : 학생이 만들어야 하는 객체 이름
# rules : 순서대로 검사, 첫 실패의 msg가 힌트로 기록됨
# call_from: $call을 코드로 기록할 객체 (없으면 NULL)

CHECKS <- list(

  "task1-split" = list(
    need = c("df", "train_data", "test_data"),
    call_from = NULL,
    rules = list(
      list(f = function(e) nrow(e$train_data) == round(0.80 * nrow(e$df)),
           msg = "훈련 데이터 행 수가 전체의 80%가 아닙니다. round(0.80 * nrow(df))를 확인하세요."),
      list(f = function(e) nrow(e$train_data) + nrow(e$test_data) == nrow(e$df),
           msg = "훈련과 테스트를 합한 행 수가 전체와 다릅니다. -train_id를 썼는지 확인하세요."),
      list(f = function(e) length(intersect(rownames(e$train_data), rownames(e$test_data))) == 0,
           msg = "훈련과 테스트에 같은 행이 중복으로 들어갔습니다.")
    )),

  "task2-ols" = list(
    need = "ols_model",
    call_from = "ols_model",
    rules = list(
      list(f = function(e) inherits(e$ols_model, "lm") && !inherits(e$ols_model, "glm"),
           msg = "lm()으로 적합한 모델이 아닙니다."),
      list(f = function(e) all.vars(formula(e$ols_model))[1] == "oop_cost",
           msg = "결과변수가 oop_cost가 아닙니다."),
      list(f = function(e) setequal(all.vars(formula(e$ols_model))[-1],
                                    c("treatment", "income_fpl")),
           msg = "설명변수는 treatment와 income_fpl 두 개여야 합니다."),
      list(f = function(e) nrow(model.frame(e$ols_model)) == nrow(e$train_data),
           msg = "train_data가 아니라 다른 데이터로 적합했습니다.")
    )),

  "task3-predict" = list(
    need = c("test_data", "rmse"),
    call_from = NULL,
    rules = list(
      list(f = function(e) "pred_oop" %in% names(e$test_data),
           msg = "예측값을 test_data$pred_oop에 저장하세요."),
      list(f = function(e) is.numeric(e$rmse) && length(e$rmse) == 1,
           msg = "rmse는 숫자 하나여야 합니다."),
      list(f = function(e) isTRUE(all.equal(
             e$rmse,
             sqrt(mean((e$test_data$oop_cost - e$test_data$pred_oop)^2)),
             tolerance = 1e-6)),
           msg = "RMSE 계산식을 확인하세요. 제곱 → 평균 → 제곱근 순서입니다.")
    )),

  "task4-1-logit" = list(
    need = "logit_model",
    call_from = "logit_model",
    rules = list(
      list(f = function(e) inherits(e$logit_model, "glm"),
           msg = "glm()으로 적합한 모델이 아닙니다."),
      list(f = function(e) e$logit_model$family$family == "binomial",
           msg = "family = binomial()을 지정하세요."),
      list(f = function(e) all.vars(formula(e$logit_model))[1] == "insured",
           msg = "결과변수가 insured가 아닙니다.")
    )),

  "task4-2-prob" = list(
    need = c("test_data", "threshold"),
    call_from = NULL,
    rules = list(
      list(f = function(e) "pred_prob" %in% names(e$test_data),
           msg = "예측확률을 test_data$pred_prob에 저장하세요."),
      list(f = function(e) all(e$test_data$pred_prob >= 0 & e$test_data$pred_prob <= 1),
           msg = "값이 0~1 범위를 벗어났습니다. type = \"response\"를 빠뜨렸을 수 있습니다."),
      list(f = function(e) isTRUE(e$threshold == 0.50),
           msg = "threshold를 0.50으로 설정하세요."),
      list(f = function(e) all(as.integer(as.character(e$test_data$pred_class)) %in% c(0L, 1L)),
           msg = "pred_class는 0 또는 1이어야 합니다.")
    )),

  "task5-confmat" = list(
    need = "confusion_matrix",
    call_from = NULL,
    rules = list(
      list(f = function(e) inherits(e$confusion_matrix, "conf_mat"),
           msg = "conf_mat()의 결과를 confusion_matrix에 저장하세요."),
      list(f = function(e) identical(dim(e$confusion_matrix$table), c(2L, 2L)),
           msg = "혼동 행렬이 2×2가 아닙니다. factor 수준을 확인하세요."),
      list(f = function(e) colnames(e$confusion_matrix$table)[1] == "1",
           msg = "levels = c(1, 0)으로 지정해 1을 양성으로 두세요.")
    )),

  "task6-1-roc" = list(
    need = c("roc_result", "auc_value"),
    call_from = NULL,
    rules = list(
      list(f = function(e) inherits(e$roc_result, "roc"),
           msg = "roc()의 결과를 roc_result에 저장하세요."),
      list(f = function(e) is.numeric(e$auc_value) && e$auc_value > 0.5 && e$auc_value <= 1,
           msg = "AUC가 0.5 이하입니다. direction = \"<\" 와 levels = c(0, 1)을 확인하세요.")
    )),

  "task6-2-plot" = list(
    need = "roc_plot",
    call_from = NULL,
    rules = list(
      list(f = function(e) inherits(e$roc_plot, "ggplot"),
           msg = "ggroc()로 만든 그래프를 roc_plot에 저장하세요."),
      list(f = function(e) any(vapply(e$roc_plot$layers,
             function(L) inherits(L$geom, "GeomAbline"), logical(1))),
           msg = "geom_abline()으로 대각선을 추가하세요.")
    ))
)

# ── 채점 함수 ────────────────────────────────────────────────
check <- function(exercise_id, chapter = "d1-01") {

  spec <- CHECKS[[exercise_id]]
  if (is.null(spec)) { message("등록되지 않은 문항입니다: ", exercise_id); return(invisible()) }

  env  <- parent.frame()
  t0   <- Sys.time()
  miss <- spec$need[!vapply(spec$need, exists, logical(1),
                            envir = env, inherits = TRUE)]

  if (length(miss)) {
    # 객체가 없다 = 위 청크가 실패했거나 이름이 다르다
    last <- tryCatch(geterrmessage(), error = function(e) "")
    out  <- paste0("객체를 찾을 수 없습니다: ", paste(miss, collapse = ", "),
                   if (nzchar(last)) paste0("\n[직전 오류] ", trimws(last)) else "")
    .send(exercise_id, chapter, status = "error", correct = FALSE,
          output = out, hint = NA_character_, code = NA_character_, t0)
    message("\u274C 실행되지 않았습니다.\n   ", out)
    return(invisible(FALSE))
  }

  # 규칙 검사
  hint <- NA_character_; ok <- TRUE
  for (r in spec$rules) {
    res <- tryCatch(isTRUE(r$f(env)), error = function(e) FALSE)
    if (!res) { ok <- FALSE; hint <- r$msg; break }
  }

  code <- NA_character_
  if (!is.null(spec$call_from) && exists(spec$call_from, envir = env, inherits = TRUE))
    code <- paste(deparse(get(spec$call_from, envir = env)$call), collapse = " ")

  .send(exercise_id, chapter,
        status  = if (ok) "ok" else "wrong",
        correct = ok, output = if (ok) "OK" else hint,
        hint = hint, code = code, t0)

  if (ok) message("\u2705 정답입니다. 다음 문항으로 넘어가세요.")
  else    message("\u274C 다시 확인해 보세요.\n   \U1F4A1 ", hint)

  invisible(ok)
}

# ── 전송 ─────────────────────────────────────────────────────
.send <- function(exercise_id, chapter, status, correct, output, hint, code, t0) {
  payload <- list(
    student_id  = .tracker_env$student_id %||% "unknown",
    chapter     = chapter,
    exercise_id = exercise_id,
    code        = code,
    output      = substr(output, 1, 4000),
    status      = status,
    is_correct  = correct,
    hint        = hint,
    error_type  = NULL,                     # 강사 쪽 워커가 채움
    elapsed_sec = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    session_id  = .tracker_env$session_id,
    r_version   = paste(R.version$major, R.version$minor, sep = ".")
  )
  try(
    httr2::request(paste0(SUPABASE_URL, "/rest/v1/submissions")) |>
      httr2::req_headers(apikey = SUPABASE_KEY,
                         `Content-Type` = "application/json",
                         Prefer = "return=minimal") |>
      httr2::req_body_json(payload) |>
      httr2::req_timeout(5) |>
      httr2::req_perform(),
    silent = TRUE)
}
