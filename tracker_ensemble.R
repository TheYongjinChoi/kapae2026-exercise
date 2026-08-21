# ══════════════════════════════════════════════════════════════════
#  KAPAE 2026 실습 진척 트래커 — 앙상블 (03Ensemble)
#  학생용: qmd 첫 청크에서 source() 후 set_student("학번")
# ══════════════════════════════════════════════════════════════════

SUPABASE_URL <- "https://mztyhpckshnqcklogrsn.supabase.co"
SUPABASE_KEY <- "sb_publishable_여기에_붙여넣기"   # publishable key (공개 가능)

CHAPTER <- "d2-01-ensemble"

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

.leaves <- function(tr) sum(tr$frame$var == "<leaf>")

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
# ══════════════════════════════════════════════════════════════════

CHECKS <- list(

# ─────────────────────────────────────────────────────────────────
# Task 1-1. 트리 적합과 시각화
# ─────────────────────────────────────────────────────────────────
"ens-01-tree" = list(
  need = c("train_data", "tree_basic"),
  call_from = "tree_basic",
  rules = list(

    list(f = function(e) inherits(e$tree_basic, "rpart"),
         msg = "tree_basic이 rpart 객체가 아닙니다. rpart()의 결과를 tree_basic에 저장하세요."),
    list(f = function(e) identical(e$tree_basic$method, "anova"),
         msg = "method = \"anova\"를 지정하세요. 결과변수가 연속형이므로 잔차제곱합으로 분할합니다."),
    list(f = function(e) all.vars(formula(e$tree_basic))[1] == "rx_num",
         msg = "결과변수가 rx_num이 아닙니다. 공식은 rx_num ~ . 입니다."),
    list(f = function(e) {
           d <- e$tree_basic$call$data
           is.null(d) || identical(as.character(d), "train_data")
         },
         msg = "data 인수가 train_data가 아닙니다. 모델은 훈련 데이터로만 적합합니다."),
    list(f = function(e) nrow(e$tree_basic$frame) > 1,
         msg = "트리가 한 번도 분할되지 않았습니다. 공식과 데이터를 확인하세요."),
    list(f = function(e) .leaves(e$tree_basic) >= 2,
         msg = "잎이 2개 미만입니다. rpart()의 인수를 다시 확인하세요."),
    list(f = function(e) e$tree_basic$control$cp == 0.01,
         msg = "이 문항에서는 control을 지정하지 않고 기본 정지 규칙을 그대로 씁니다.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 1-2. 예측과 평가
# ─────────────────────────────────────────────────────────────────
"ens-02-tree-predict" = list(
  need = c("tree_basic", "test_data", "train_data", "y_test", "y_train",
           "pred_tree", "pred_tree_train", "rmse_tree", "rmse_tree_train"),
  call_from = NULL,
  rules = list(

    list(f = function(e) length(e$pred_tree) == nrow(e$test_data),
         msg = "pred_tree의 길이가 테스트 표본 수와 다릅니다. newdata = test_data를 확인하세요."),
    list(f = function(e) length(e$pred_tree_train) == nrow(e$train_data),
         msg = "pred_tree_train의 길이가 훈련 표본 수와 다릅니다. newdata = train_data입니다."),
    list(f = function(e) !.eq(e$rmse_tree, mean((e$y_test - e$pred_tree)^2)),
         msg = "RMSE가 아니라 MSE를 계산했습니다. 제곱근 sqrt()를 씌우세요."),
    list(f = function(e) .eq(e$rmse_tree,
                             sqrt(mean((e$y_test - e$pred_tree)^2)), tol = 1e-6),
         msg = "rmse_tree 계산식을 확인하세요. sqrt(mean((y_test - pred_tree)^2))입니다."),
    list(f = function(e) .eq(e$rmse_tree_train,
                             sqrt(mean((e$y_train - e$pred_tree_train)^2)), tol = 1e-6),
         msg = "rmse_tree_train에 훈련 데이터 예측값을 썼는지 확인하세요."),
    list(f = function(e) e$rmse_tree_train <= e$rmse_tree * 1.05,
         msg = "훈련 RMSE가 테스트 RMSE보다 큽니다. 두 값이 뒤바뀌지 않았는지 확인하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 1-3. 최대 트리와 가지치기 (선택)
# ─────────────────────────────────────────────────────────────────
"ens-03-prune" = list(
  need = c("tree_full", "best_cp", "tree_pruned", "pred_pruned", "rmse_pruned",
           "y_test", "test_data"),
  call_from = NULL,
  rules = list(

    list(f = function(e) inherits(e$tree_full, "rpart"),
         msg = "tree_full이 rpart 객체가 아닙니다."),
    list(f = function(e) .eq(e$tree_full$control$cp, 0.0001),
         msg = "cp = 0.0001로 설정해 정지 규칙을 풀어야 합니다."),
    list(f = function(e) .eq(e$tree_full$control$minsplit, 10),
         msg = "minsplit = 10으로 설정하세요."),
    list(f = function(e) !is.null(e$tree_full$cptable) &&
                         "xerror" %in% colnames(e$tree_full$cptable),
         msg = "cptable에 교차검증 결과가 없습니다. xval = 10을 지정했는지 확인하세요."),
    list(f = function(e) .leaves(e$tree_full) > 20,
         msg = "최대 트리의 잎이 너무 적습니다. control 인수가 제대로 전달되었는지 확인하세요."),

    list(f = function(e) .eq(e$best_cp,
           e$tree_full$cptable[which.min(e$tree_full$cptable[, "xerror"]), "CP"]),
         msg = "best_cp는 xerror가 최소인 행의 CP 값입니다. which.min()을 사용하세요."),

    list(f = function(e) inherits(e$tree_pruned, "rpart"),
         msg = "tree_pruned가 rpart 객체가 아닙니다. prune()의 결과를 저장하세요."),
    list(f = function(e) .leaves(e$tree_pruned) <= .leaves(e$tree_full),
         msg = "가지치기 후 잎이 더 많습니다. prune(tree_full, cp = best_cp)를 확인하세요."),
    list(f = function(e) .eq(e$rmse_pruned,
                             sqrt(mean((e$y_test - e$pred_pruned)^2)), tol = 1e-6),
         msg = "rmse_pruned 계산식을 확인하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 2-1. 배깅 직접 구현
# ─────────────────────────────────────────────────────────────────
"ens-04-bagging" = list(
  need = c("train_data", "test_data", "y_test",
           "n_bag", "fits", "pred_mat", "bag_pred", "rmse_single", "rmse_bag"),
  call_from = NULL,
  rules = list(

    list(f = function(e) .eq(e$n_bag, 30),
         msg = "n_bag은 30으로 지정합니다."),
    list(f = function(e) is.list(e$fits) && length(e$fits) == e$n_bag,
         msg = "fits의 길이가 n_bag과 다릅니다. 반복문이 끝까지 돌았는지 확인하세요."),
    list(f = function(e) all(vapply(e$fits, inherits, logical(1), "rpart")),
         msg = "fits의 원소가 모두 rpart 객체는 아닙니다."),

    # 부트스트랩 설정
    list(f = function(e) .eq(e$fits[[1]]$control$cp, 0),
         msg = "개별 트리는 cp = 0으로 깊게 키웁니다. 배깅은 편향이 낮은 트리를 평균 내는 방법입니다."),
    list(f = function(e) .eq(e$fits[[1]]$control$minsplit, 10),
         msg = "minsplit = 10으로 지정하세요."),
    list(f = function(e) {
           # 복원추출이면 원자료보다 고유 행이 줄어든다
           nu <- e$fits[[1]]$frame$n[1]
           .eq(nu, nrow(e$train_data))
         },
         msg = "각 트리는 훈련 데이터와 같은 크기의 부트스트랩 표본으로 학습해야 합니다."),
    list(f = function(e) {
           v1 <- as.character(e$fits[[1]]$frame$var[1])
           vs <- vapply(e$fits, function(f) as.character(f$frame$var[1]), character(1))
           length(unique(vs)) > 1 || length(e$fits) < 3
         },
         msg = "모든 트리가 동일합니다. sample()에 replace = TRUE를 지정했는지 확인하세요."),

    # 예측 합치기
    list(f = function(e) is.matrix(e$pred_mat),
         msg = "pred_mat이 행렬이 아닙니다. sapply(fits, predict, newdata = test_data)를 확인하세요."),
    list(f = function(e) nrow(e$pred_mat) == nrow(e$test_data),
         msg = "pred_mat의 행 수가 테스트 표본 수와 다릅니다. newdata = test_data입니다."),
    list(f = function(e) ncol(e$pred_mat) == e$n_bag,
         msg = "pred_mat의 열 수가 트리 개수와 다릅니다."),
    list(f = function(e) .eq(mean(e$bag_pred), mean(rowMeans(e$pred_mat)), tol = 1e-8) &&
                         length(e$bag_pred) == nrow(e$pred_mat),
         msg = "bag_pred는 rowMeans(pred_mat)입니다. colMeans가 아닙니다."),

    # 성능
    list(f = function(e) .eq(e$rmse_single,
                             sqrt(mean((e$pred_mat[, 1] - e$y_test)^2)), tol = 1e-6),
         msg = "rmse_single은 첫 번째 트리의 테스트 RMSE입니다."),
    list(f = function(e) .eq(e$rmse_bag,
                             sqrt(mean((e$bag_pred - e$y_test)^2)), tol = 1e-6),
         msg = "rmse_bag 계산식을 확인하세요."),
    list(f = function(e) e$rmse_bag < e$rmse_single,
         msg = "배깅이 단일 트리보다 나쁩니다. 평균을 제대로 냈는지 다시 확인하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 2-2. 랜덤 포레스트
# ─────────────────────────────────────────────────────────────────
"ens-05-rf" = list(
  need = c("form", "train_data", "test_data", "y_test",
           "fit_rf", "oob_rmse_rf", "pred_rf", "rmse_rf"),
  call_from = NULL,
  rules = list(

    list(f = function(e) inherits(e$fit_rf, "ranger"),
         msg = "fit_rf가 ranger 객체가 아닙니다. ranger()의 결과를 저장하세요."),
    list(f = function(e) e$fit_rf$treetype == "Regression",
         msg = "회귀 문제로 학습되지 않았습니다. 결과변수가 rx_num인지 확인하세요."),
    list(f = function(e) e$fit_rf$num.samples == nrow(e$train_data),
         msg = "data 인수가 train_data가 아닙니다."),

    list(f = function(e) .eq(e$oob_rmse_rf, sqrt(e$fit_rf$prediction.error), tol = 1e-8),
         msg = "prediction.error는 MSE입니다. sqrt()를 씌워 RMSE로 바꾸세요."),

    list(f = function(e) is.numeric(e$pred_rf) && is.null(dim(e$pred_rf)),
         msg = "pred_rf가 벡터가 아닙니다. predict()의 결과에서 $predictions를 꺼내세요."),
    list(f = function(e) length(e$pred_rf) == nrow(e$test_data),
         msg = "예측값 개수가 테스트 표본 수와 다릅니다. ranger는 newdata가 아니라 data = test_data입니다."),
    list(f = function(e) .eq(e$rmse_rf,
                             sqrt(mean((e$y_test - e$pred_rf)^2)), tol = 1e-6),
         msg = "rmse_rf 계산식을 확인하세요."),
    list(f = function(e) !.has(e, "rmse_bag") || e$rmse_rf < e$rmse_bag * 1.1,
         msg = "랜덤 포레스트가 배깅보다 크게 나쁩니다. 설정을 다시 확인하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 3-1. 스텀프 두 개로 부스팅 이해하기
# ─────────────────────────────────────────────────────────────────
"ens-06-boost-two" = list(
  need = c("x_train", "y_train", "stump1", "stump2", "r1", "pred2",
           "rmse_stump1", "rmse_null", "rmse_stump2"),
  call_from = NULL,
  rules = list(

    list(f = function(e) inherits(e$stump1, "rpart"),
         msg = "stump1이 rpart 객체가 아닙니다."),
    list(f = function(e) .leaves(e$stump1) == 2,
         msg = "stump1의 잎이 2개가 아닙니다. maxdepth = 1로 한 번만 분할해야 합니다."),
    list(f = function(e) .eq(e$rmse_null,
                             sqrt(mean((e$y_train - mean(e$y_train))^2)), tol = 1e-6),
         msg = "rmse_null은 훈련 데이터의 평균으로만 예측했을 때의 RMSE입니다."),
    list(f = function(e) .eq(e$rmse_stump1,
                             sqrt(mean(residuals(e$stump1)^2)), tol = 1e-6),
         msg = "rmse_stump1은 sqrt(mean(residuals(stump1)^2))입니다."),
    list(f = function(e) e$rmse_stump1 < e$rmse_null,
         msg = "스텀프 하나가 영모형보다 나쁩니다. stump1의 공식을 확인하세요."),

    list(f = function(e) .eq(mean(e$r1), mean(residuals(e$stump1)), tol = 1e-10) &&
                         length(e$r1) == length(residuals(e$stump1)),
         msg = "r1은 residuals(stump1)이어야 합니다."),
    list(f = function(e) inherits(e$stump2, "rpart"),
         msg = "stump2가 rpart 객체가 아닙니다."),
    list(f = function(e) all.vars(formula(e$stump2))[1] == "r1",
         msg = "stump2는 y_train이 아니라 잔차 r1을 맞혀야 합니다. 공식은 r1 ~ . 입니다."),
    list(f = function(e) .leaves(e$stump2) == 2,
         msg = "stump2의 잎이 2개가 아닙니다. maxdepth = 1을 확인하세요."),

    list(f = function(e) length(e$pred2) == length(e$y_train),
         msg = "pred2의 길이가 훈련 표본 수와 다릅니다."),
    list(f = function(e) .eq(mean(e$pred2),
                             mean(predict(e$stump1) + predict(e$stump2)), tol = 1e-8),
         msg = "pred2는 두 스텀프의 예측을 더한 값입니다. 부스팅은 곱이 아니라 합입니다."),
    list(f = function(e) .eq(e$rmse_stump2,
                             sqrt(mean((e$y_train - e$pred2)^2)), tol = 1e-6),
         msg = "rmse_stump2 계산식을 확인하세요."),
    list(f = function(e) e$rmse_stump2 < e$rmse_stump1,
         msg = "두 번째 스텀프를 더했는데 오차가 줄지 않았습니다. r1을 제대로 넘겼는지 확인하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 3-2. 100회 반복
# ─────────────────────────────────────────────────────────────────
"ens-07-boost-loop" = list(
  need = c("x_train", "x_test", "y_train", "y_test",
           "M", "f_tr", "f_te", "rmse_tr", "rmse_te", "best_m"),
  call_from = NULL,
  rules = list(

    list(f = function(e) .eq(e$M, 100),
         msg = "M은 100으로 지정합니다."),
    list(f = function(e) length(e$rmse_tr) == e$M && length(e$rmse_te) == e$M,
         msg = "rmse_tr과 rmse_te의 길이가 M과 다릅니다. numeric(M)으로 만드세요."),
    list(f = function(e) all(is.finite(e$rmse_tr)) && all(is.finite(e$rmse_te)),
         msg = "RMSE 벡터에 결측이나 무한값이 있습니다. 반복문이 끝까지 돌았는지 확인하세요."),

    list(f = function(e) length(e$f_tr) == length(e$y_train),
         msg = "f_tr의 길이가 훈련 표본 수와 다릅니다. predict(stump)를 누적했는지 확인하세요."),
    list(f = function(e) length(e$f_te) == length(e$y_test),
         msg = "f_te의 길이가 테스트 표본 수와 다릅니다. newdata = x_test를 확인하세요."),

    # 잔차 갱신이 실제로 일어났는가
    list(f = function(e) e$rmse_tr[e$M] < e$rmse_tr[1],
         msg = "훈련 RMSE가 줄지 않았습니다. 루프 끝에서 r <- residuals(stump)로 잔차를 갱신하세요."),
    list(f = function(e) length(unique(round(e$rmse_tr, 8))) > e$M / 2,
         msg = "RMSE가 거의 변하지 않습니다. 매 반복에서 같은 스텀프를 학습하고 있습니다. r 갱신을 확인하세요."),
    list(f = function(e) e$rmse_tr[e$M] < 0.98 * e$rmse_tr[1],
         msg = "훈련 RMSE 감소폭이 너무 작습니다. 스텀프가 잔차 r을 맞히도록 공식을 r ~ . 로 두었는지 확인하세요."),

    list(f = function(e) .eq(e$rmse_tr[e$M],
                             sqrt(mean((e$y_train - e$f_tr)^2)), tol = 1e-6),
         msg = "rmse_tr의 마지막 값이 f_tr과 맞지 않습니다. 누적 예측과 RMSE 계산 순서를 확인하세요."),
    list(f = function(e) .eq(e$best_m, which.min(e$rmse_te)),
         msg = "best_m은 테스트 RMSE가 최소인 반복 번호입니다. which.min(rmse_te)를 사용하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 3-3. XGBoost (선택)
# ─────────────────────────────────────────────────────────────────
"ens-08-xgboost" = list(
  need = c("train_data", "test_data", "y_train", "y_test",
           "x_mat_train", "x_mat_test", "dtrain", "xgcv",
           "nrounds_best", "xgb_final", "pred_xgb", "rmse_xgb"),
  call_from = NULL,
  rules = list(

    list(f = function(e) is.matrix(e$x_mat_train),
         msg = "x_mat_train이 행렬이 아닙니다. model.matrix()의 결과를 저장하세요."),
    list(f = function(e) nrow(e$x_mat_train) == nrow(e$train_data),
         msg = "x_mat_train의 행 수가 훈련 표본 수와 다릅니다. data = train_data를 확인하세요."),
    list(f = function(e) nrow(e$x_mat_test) == nrow(e$test_data),
         msg = "x_mat_test의 행 수가 테스트 표본 수와 다릅니다. data = test_data를 확인하세요."),
    list(f = function(e) ncol(e$x_mat_train) == ncol(e$x_mat_test),
         msg = "훈련과 테스트의 열 수가 다릅니다. 같은 공식으로 만들었는지 확인하세요."),
    list(f = function(e) !("(Intercept)" %in% colnames(e$x_mat_train)),
         msg = "절편 열이 남아 있습니다. 공식에 - 1을 넣어 제거하세요."),

    list(f = function(e) !is.null(e$xgcv$evaluation_log),
         msg = "xgcv에 교차검증 기록이 없습니다. xgb.cv()의 결과를 저장하세요."),
    list(f = function(e) nrow(e$xgcv$evaluation_log) == 200,
         msg = "nrounds = 200으로 지정하세요."),
    list(f = function(e) .eq(e$nrounds_best,
           which.min(e$xgcv$evaluation_log$test_rmse_mean)),
         msg = "nrounds_best는 test_rmse_mean이 최소인 반복 번호입니다. which.min()을 사용하세요."),

    list(f = function(e) length(e$pred_xgb) == nrow(e$x_mat_test),
         msg = "예측값 개수가 테스트 표본 수와 다릅니다."),
    list(f = function(e) .eq(e$rmse_xgb,
                             sqrt(mean((e$y_test - e$pred_xgb)^2)), tol = 1e-6),
         msg = "rmse_xgb 계산식을 확인하세요.")
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
