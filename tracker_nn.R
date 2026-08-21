# ══════════════════════════════════════════════════════════════════
#  KAPAE 2026 실습 진척 트래커 — 신경망 (1-2NN)
#  학생용: qmd 첫 청크에서 source() 후 set_student("학번")
# ══════════════════════════════════════════════════════════════════

SUPABASE_URL <- "https://mztyhpckshnqcklogrsn.supabase.co"
SUPABASE_KEY <- "sb_publishable_여기에_붙여넣기"   # publishable key (공개 가능)

CHAPTER <- "d2-02-nn"

# ── 내부 상태 ─────────────────────────────────────────────────────
.tracker_env <- new.env()
.tracker_env$session_id <- paste0(
  format(Sys.time(), "%Y%m%d%H%M%S"), "-",
  paste(sample(letters, 6, TRUE), collapse = "")
)

`%||%` <- function(a, b) if (is.null(a)) b else a

.eq <- function(a, b, tol = 1e-6) {
  isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol))
}

.has <- function(e, nm) exists(nm, envir = e, inherits = TRUE)

# keras 객체 속성을 안전하게 꺼내기 (없으면 NULL)
.kget <- function(expr) tryCatch(expr, error = function(err) NULL)

set_student <- function(id) {
  if (missing(id) || !nzchar(id) || id %in% c("학번입력", "2026001")) {
    stop("첫 청크의 set_student()에 본인 학번을 입력하세요.", call. = FALSE)
  }
  .tracker_env$student_id <- id
  message("\u2713 ", id, " 님, 실습을 시작합니다.")
  invisible(id)
}

# ══════════════════════════════════════════════════════════════════
#  정답 규칙
#  need  : 학생이 만들어야 하는 객체
#  rules : 순서대로 검사하며, 첫 실패의 msg가 힌트로 기록됨
# ══════════════════════════════════════════════════════════════════

CHECKS <- list(

# ─────────────────────────────────────────────────────────────────
# Task 1. 항등 활성화 은닉층 수동 구현
# ─────────────────────────────────────────────────────────────────
"nn-01-identity" = list(
  need = c("x_train", "y_train", "n", "p", "n_hidden", "lr", "n_iter",
           "W1", "b1", "w2", "b2", "loss_history"),
  call_from = NULL,
  rules = list(

    # 하이퍼파라미터
    list(f = function(e) .eq(e$n, nrow(e$x_train)) && .eq(e$p, ncol(e$x_train)),
         msg = "n과 p를 확인하세요. n <- nrow(x_train), p <- ncol(x_train) 입니다."),
    list(f = function(e) .eq(e$n_hidden, 5),
         msg = "은닉층 유닛 수 n_hidden은 5여야 합니다."),
    list(f = function(e) .eq(e$lr, 0.01),
         msg = "학습률 lr은 0.01로 고정합니다."),
    list(f = function(e) .eq(e$n_iter, 2000),
         msg = "반복 횟수 n_iter는 2000이어야 합니다."),

    # 초기화 — 차원
    list(f = function(e) is.matrix(e$W1),
         msg = "W1이 행렬이 아닙니다. matrix(rnorm(...), nrow = p) 형태여야 합니다."),
    list(f = function(e) nrow(e$W1) == e$p,
         msg = "W1의 행 수가 예측변수 개수 p와 다릅니다. nrow = p를 확인하세요."),
    list(f = function(e) ncol(e$W1) == e$n_hidden,
         msg = "W1의 열 수가 은닉층 유닛 수와 다릅니다. rnorm(p * n_hidden, ...)을 확인하세요."),
    list(f = function(e) length(e$b1) == e$n_hidden,
         msg = "b1의 길이가 은닉층 유닛 수와 다릅니다. rep(0, n_hidden)입니다."),
    list(f = function(e) length(e$w2) == e$n_hidden,
         msg = "w2의 길이가 은닉층 유닛 수와 다릅니다. rnorm(n_hidden, sd = 0.1)입니다."),
    list(f = function(e) length(e$b2) == 1 && is.numeric(e$b2),
         msg = "b2는 길이 1의 숫자여야 합니다."),

    # 학습이 실제로 일어났는가
    list(f = function(e) length(e$loss_history) == e$n_iter,
         msg = "loss_history의 길이가 n_iter와 다릅니다. rep(NA, n_iter)로 만드세요."),
    list(f = function(e) !any(is.na(e$loss_history)),
         msg = "loss_history에 NA가 남아 있습니다. 반복문이 끝까지 돌지 않았습니다. for (iter in 1:n_iter)를 확인하세요."),
    list(f = function(e) all(is.finite(e$loss_history)),
         msg = "손실이 발산했습니다(Inf 또는 NaN). 갱신 부호가 W1 <- W1 - lr * grad_W1 인지 확인하세요."),
    list(f = function(e) e$loss_history[e$n_iter] < e$loss_history[1],
         msg = "손실이 줄어들지 않았습니다. 갱신에서 기울기를 더하지 말고 빼야 합니다."),
    list(f = function(e) e$loss_history[e$n_iter] < 0.5 * e$loss_history[1],
         msg = "손실이 충분히 줄지 않았습니다. 순전파의 Z <- x_train %*% W1 부분을 확인하세요."),

    # 가중치가 갱신되었는가
    list(f = function(e) !.eq(e$b2, mean(e$y_train), tol = 1e-10),
         msg = "b2가 초기값 그대로입니다. 갱신 단계에 b2 <- b2 - lr * grad_b2가 빠졌습니다."),

    # 항등 활성화의 핵심 결과
    list(f = function(e) .has(e, "mse_id") && .has(e, "mse_lm"),
         msg = "테스트 손실 mse_id와 lm() 비교값 mse_lm을 계산하세요."),
    list(f = function(e) is.finite(e$mse_id) && e$mse_id > 0,
         msg = "mse_id가 유효한 값이 아닙니다. 테스트 순전파를 다시 확인하세요."),
    list(f = function(e) abs(e$mse_id - e$mse_lm) / e$mse_lm < 0.10,
         msg = "항등 활성화 신경망의 테스트 MSE가 lm()과 크게 다릅니다. 은닉층 출력 H <- Z 인지 확인하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 3. ReLU로 바꾸기
# ─────────────────────────────────────────────────────────────────
"nn-02-relu" = list(
  need = c("loss_history", "loss_history_relu", "mse_relu", "mse_id", "n_iter"),
  call_from = NULL,
  rules = list(

    list(f = function(e) length(e$loss_history_relu) == e$n_iter,
         msg = "loss_history_relu의 길이가 n_iter와 다릅니다."),
    list(f = function(e) !any(is.na(e$loss_history_relu)),
         msg = "loss_history_relu에 NA가 남아 있습니다. 반복문이 끝까지 돌지 않았습니다."),
    list(f = function(e) all(is.finite(e$loss_history_relu)),
         msg = "손실이 발산했습니다. 갱신 부호와 학습률을 확인하세요."),

    # 활성화 함수를 실제로 바꿨는가
    list(f = function(e) !.eq(e$loss_history_relu[1], e$loss_history[1], tol = 1e-10),
         msg = "첫 손실이 항등 모형과 같습니다. 순전파의 H <- pmax(Z, 0)으로 바꿨는지 확인하세요."),
    list(f = function(e) !isTRUE(all.equal(e$loss_history_relu, e$loss_history, tolerance = 1e-8)),
         msg = "손실 궤적이 항등 모형과 동일합니다. 활성화 함수가 바뀌지 않았습니다."),

    list(f = function(e) e$loss_history_relu[e$n_iter] < e$loss_history_relu[1],
         msg = "손실이 줄어들지 않았습니다. 역전파의 dZ <- dH * (Z > 0)을 확인하세요."),

    list(f = function(e) is.finite(e$mse_relu) && e$mse_relu > 0,
         msg = "mse_relu가 유효한 값이 아닙니다."),
    list(f = function(e) !.eq(e$mse_relu, e$mse_id, tol = 1e-8),
         msg = "테스트 MSE가 항등 모형과 같습니다. 테스트 순전파에도 H_test <- pmax(Z_test, 0)이 필요합니다.")
  )),

# ─────────────────────────────────────────────────────────────────
# Keras 1단계. 구조 정의
# ─────────────────────────────────────────────────────────────────
"nn-03-architecture" = list(
  need = c("x_train", "model"),
  call_from = NULL,
  rules = list(

    list(f = function(e) !is.null(.kget(e$model$layers)),
         msg = "model이 keras 모델이 아닙니다. keras_model_sequential()의 결과를 model에 저장하세요."),
    list(f = function(e) length(e$model$layers) == 2,
         msg = "층이 2개여야 합니다. layer_dense()를 은닉층과 출력층으로 두 번 이어 붙이세요."),
    list(f = function(e) .eq(.kget(e$model$layers[[1]]$units), 8),
         msg = "은닉층의 units가 8이 아닙니다."),
    list(f = function(e) .eq(.kget(e$model$layers[[2]]$units), 1),
         msg = "출력층의 units는 1이어야 합니다. 결과변수가 하나이기 때문입니다."),
    list(f = function(e) {
           a <- .kget(e$model$layers[[1]]$activation$`__name__`)
           is.null(a) || a == "relu"
         },
         msg = "은닉층의 activation이 \"relu\"가 아닙니다."),
    list(f = function(e) {
           a <- .kget(e$model$layers[[2]]$activation$`__name__`)
           is.null(a) || a == "linear"
         },
         msg = "출력층에는 activation을 지정하지 않습니다. 연속형 결과이므로 항등함수여야 합니다."),
    list(f = function(e) {
           np <- .kget(sum(sapply(e$model$layers, function(L) L$count_params())))
           is.null(np) || np == (ncol(e$x_train) * 8 + 8) + (8 + 1)
         },
         msg = "파라미터 개수가 맞지 않습니다. input_shape = ncol(x_train)을 지정했는지 확인하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Keras 2단계. 학습 방식 설정
# ─────────────────────────────────────────────────────────────────
"nn-04-compile" = list(
  need = "model",
  call_from = NULL,
  rules = list(

    list(f = function(e) !is.null(.kget(e$model$optimizer)),
         msg = "모델이 컴파일되지 않았습니다. model |> compile(...)을 실행하세요."),
    list(f = function(e) {
           lo <- .kget(e$model$loss)
           is.null(lo) || grepl("mse|mean_squared", tolower(paste(lo, collapse = " ")))
         },
         msg = "loss = \"mse\"를 지정하세요. 앞에서 직접 계산한 평균제곱오차와 같습니다."),
    list(f = function(e) {
           nm <- .kget(class(e$model$optimizer))
           is.null(nm) || any(grepl("adam", tolower(nm)))
         },
         msg = "optimizer는 optimizer_adam()이어야 합니다."),
    list(f = function(e) {
           lr <- .kget(as.numeric(e$model$optimizer$learning_rate))
           is.null(lr) || abs(lr - 0.01) < 1e-6
         },
         msg = "learning_rate를 0.01로 지정하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Keras 3단계. 학습 실행
# ─────────────────────────────────────────────────────────────────
"nn-05-fit" = list(
  need = "history",
  call_from = NULL,
  rules = list(

    list(f = function(e) !is.null(e$history$metrics$loss),
         msg = "history에 학습 기록이 없습니다. fit()의 결과를 history에 저장하세요."),
    list(f = function(e) length(e$history$metrics$loss) == 60,
         msg = "epochs = 60으로 지정하세요."),
    list(f = function(e) !is.null(e$history$metrics$val_loss),
         msg = "검증 손실이 기록되지 않았습니다. validation_split = 0.2를 지정하세요."),
    list(f = function(e) length(e$history$metrics$val_loss) ==
                         length(e$history$metrics$loss),
         msg = "훈련 손실과 검증 손실의 길이가 다릅니다."),
    list(f = function(e) all(is.finite(e$history$metrics$loss)),
         msg = "손실에 NaN이 있습니다. 입력이 표준화되었는지, 학습률이 지나치게 크지 않은지 확인하세요."),
    list(f = function(e) {
           L <- e$history$metrics$loss
           L[length(L)] < L[1]
         },
         msg = "훈련 손실이 줄지 않았습니다. compile() 설정을 확인하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Keras 예측과 평가
# ─────────────────────────────────────────────────────────────────
"nn-06-predict" = list(
  need = c("x_test", "y_test", "pred_keras", "rmse_keras", "rmse_lm"),
  call_from = NULL,
  rules = list(

    list(f = function(e) is.numeric(e$pred_keras) && is.null(dim(e$pred_keras)),
         msg = "pred_keras가 벡터가 아닙니다. as.numeric()으로 행렬을 벡터로 바꾸세요."),
    list(f = function(e) length(e$pred_keras) == nrow(e$x_test),
         msg = "예측값 개수가 테스트 표본 수와 다릅니다. predict(model, x_test)를 확인하세요."),
    list(f = function(e) .eq(e$rmse_keras,
                             sqrt(mean((e$y_test - e$pred_keras)^2)), tol = 1e-6),
         msg = "rmse_keras 계산식을 확인하세요. 제곱 → 평균 → 제곱근 순서입니다."),
    list(f = function(e) .has(e, "pred_lm"),
         msg = "비교를 위해 lm_fit의 테스트 예측값을 pred_lm에 저장하세요."),
    list(f = function(e) .eq(e$rmse_lm,
                             sqrt(mean((e$y_test - e$pred_lm)^2)), tol = 1e-6),
         msg = "rmse_lm 계산식을 확인하세요."),
    list(f = function(e) e$rmse_keras < 1.5 * e$rmse_lm,
         msg = "신경망 RMSE가 선형회귀보다 크게 나쁩니다. 학습이 제대로 되었는지 학습곡선을 다시 보세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# 그리드서치 (선택)
# ─────────────────────────────────────────────────────────────────
"nn-07-grid" = list(
  need = c("grid", "results"),
  call_from = NULL,
  rules = list(

    list(f = function(e) is.data.frame(e$grid),
         msg = "grid가 데이터프레임이 아닙니다. expand.grid()의 결과를 저장하세요."),
    list(f = function(e) all(c("units", "lr", "dropout") %in% names(e$grid)),
         msg = "grid에 units, lr, dropout 세 열이 있어야 합니다."),
    list(f = function(e) "cv_rmse" %in% names(e$grid),
         msg = "각 조합의 평균 교차검증 RMSE를 grid$cv_rmse에 저장하세요."),
    list(f = function(e) all(is.finite(e$grid$cv_rmse)),
         msg = "cv_rmse에 결측이나 무한값이 있습니다. run_one()이 모든 조합에서 값을 반환했는지 확인하세요."),
    list(f = function(e) is.data.frame(e$results) && "cv_rmse" %in% names(e$results),
         msg = "results에 cv_rmse 기준으로 정렬한 결과를 저장하세요."),
    list(f = function(e) !is.unsorted(e$results$cv_rmse),
         msg = "results가 cv_rmse 오름차순으로 정렬되지 않았습니다. arrange(cv_rmse)를 사용하세요."),
    list(f = function(e) min(e$grid$cv_rmse) > 0,
         msg = "cv_rmse가 0 이하입니다. evaluate()가 반환한 loss에 제곱근을 취했는지 확인하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# 최종 평가 (선택)
# ─────────────────────────────────────────────────────────────────
"nn-08-final" = list(
  need = c("results", "best", "pred_final", "rmse_final", "y_test", "x_test"),
  call_from = NULL,
  rules = list(

    list(f = function(e) nrow(e$best) == 1,
         msg = "best에는 results의 첫 행 하나만 담아야 합니다."),
    list(f = function(e) .eq(e$best$cv_rmse, min(e$results$cv_rmse)),
         msg = "best가 cv_rmse 최소 조합이 아닙니다. results[1, ]을 확인하세요."),
    list(f = function(e) length(e$pred_final) == nrow(e$x_test),
         msg = "최종 예측값 개수가 테스트 표본 수와 다릅니다."),
    list(f = function(e) .eq(e$rmse_final,
                             sqrt(mean((e$y_test - e$pred_final)^2)), tol = 1e-6),
         msg = "rmse_final 계산식을 확인하세요."),
    list(f = function(e) .has(e, "rmse_null") && .has(e, "rmse_keras"),
         msg = "비교표를 만들려면 rmse_null과 rmse_keras가 필요합니다. 앞 청크를 실행하세요.")
  ))
)

# ══════════════════════════════════════════════════════════════════
#  채점 함수
# ══════════════════════════════════════════════════════════════════

check <- function(exercise_id) {

  spec <- CHECKS[[exercise_id]]
  if (is.null(spec)) {
    message("등록되지 않은 문항입니다: ", exercise_id); return(invisible())
  }

  env  <- parent.frame()
  t0   <- Sys.time()
  miss <- spec$need[!vapply(spec$need, function(nm)
            exists(nm, envir = env, inherits = TRUE), logical(1))]

  if (length(miss)) {
    last <- tryCatch(geterrmessage(), error = function(err) "")
    out  <- paste0("객체를 찾을 수 없습니다: ", paste(miss, collapse = ", "),
                   if (nzchar(last)) paste0("\n[직전 오류] ", trimws(last)) else "")
    .send(exercise_id, "error", FALSE, out, NA_character_, NA_character_, t0)
    message("\u274C 실행되지 않았습니다.\n   ", out)
    return(invisible(FALSE))
  }

  hint <- NA_character_; ok <- TRUE
  for (r in spec$rules) {
    res <- tryCatch(isTRUE(r$f(env)), error = function(err) FALSE)
    if (!res) { ok <- FALSE; hint <- r$msg; break }
  }

  code <- NA_character_
  if (!is.null(spec$call_from) && exists(spec$call_from, envir = env, inherits = TRUE)) {
    code <- tryCatch(
      paste(deparse(get(spec$call_from, envir = env)$call), collapse = " "),
      error = function(err) NA_character_)
  }

  .send(exercise_id, if (ok) "ok" else "wrong", ok,
        if (ok) "OK" else hint, hint, code, t0)

  if (ok) message("\u2705 정답입니다. 다음 문항으로 넘어가세요.")
  else    message("\u274C 다시 확인해 보세요.\n   \U1F4A1 ", hint)

  invisible(ok)
}

# ── 전송 ──────────────────────────────────────────────────────────
.send <- function(exercise_id, status, correct, output, hint, code, t0) {
  payload <- list(
    student_id  = .tracker_env$student_id %||% "unknown",
    chapter     = CHAPTER,
    exercise_id = exercise_id,
    code        = code,
    output      = substr(output, 1, 4000),
    status      = status,
    is_correct  = correct,
    hint        = hint,
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
