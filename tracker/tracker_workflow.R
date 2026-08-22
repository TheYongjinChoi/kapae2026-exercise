# ══════════════════════════════════════════════════════════════════════════════
#  KAPAE 2026 워크숍 · 1일차 1강 실습 채점 스크립트
#  대상 문서: 1-1Workflow_sol.qmd (기본 워크플로 + 정규화 회귀 통합본)
#
#  문항 ID 목록 (qmd의 각 빈칸 블록 하단에 아래 순서대로 삽입)
#    task1-1-split    Part 1 / Task 1-1  훈련테스트 분할
#    task1-2-ols      Part 1 / Task 1-2  OLS 적합
#    task1-3-predict  Part 1 / Task 1-3  예측과 RMSE
#    task2-1-matrix   Part 2 / Task 2-1  설계행렬 만들기
#    task2-2-lasso    Part 2 / Task 2-2  Lasso 교차검증
#    task2-3-eval     Part 2 / Task 2-3  Lasso 예측과 평가
#    task2-4-coef     Part 2 / Task 2-4  계수 확인
#    task3-1-ridge    Part 3 / Task 3-1  Ridge (선택사항)
#
# ──────────────────────────────────────────────────────────────────────────────
#  문항별 정답 기준 · 예상 오류 · 힌트
# ──────────────────────────────────────────────────────────────────────────────
#
#  [task1-1-split]  train_id <- sample(1:nrow(model_df), size = round(0.80 * nrow(model_df)))
#                   train_data <- model_df[train_id, ] ; test_data <- model_df[-train_id, ]
#   정답 기준
#     · train_id 가 중복 없는 정수 벡터이고 길이가 round(0.80 * n)
#     · train_data / test_data 가 데이터프레임이며 열 구성이 model_df 와 동일
#     · 두 데이터의 행 수 합이 n 이고 교집합이 없음
#     · set.seed(123) 기준 결과와 표본 구성이 일치
#   예상 오류
#     (1) 앞의 전처리 블록을 실행하지 않아 model_df 자체가 없음
#     (2) sample() 대신 sample_n() / slice_sample() 등 다른 함수 사용
#     (3) size 에 round() 를 빼서 소수점 → sample() 이 내부적으로 절사
#     (4) size 에 0.20 을 곱해 훈련/테스트 비율이 뒤바뀜
#     (5) test_data 에 음수 기호를 빼고 model_df[train_id, ] 를 그대로 씀 (동일 데이터 중복)
#     (6) 대괄호 안 쉼표 누락: model_df[train_id] → 행이 아니라 열이 선택됨
#     (7) sample(replace = TRUE) 로 복원추출 → 같은 행 중복
#     (8) set.seed 를 빼거나 다른 값 사용 → 표본 구성 불일치
#     (9) 1:nrow(model_df) 대신 1:nrow(df) 또는 1:n() 사용
#
#  [task1-2-ols]  ols_model <- lm(rx_num ~ ., data = train_data)
#   정답 기준
#     · lm 객체이며 glm 이 아님
#     · 결과변수가 rx_num, 우변이 . (예측변수 전체)
#     · 학습 데이터가 train_data (행 수 일치)
#   예상 오류
#     (1) glm() 사용 → 여기서는 연속형 결과이므로 lm()
#     (2) 결과변수를 rx_num_mod_6m (원 변수명) 으로 씀
#     (3) 우변에 . 대신 특정 변수만 나열
#     (4) data = model_df 또는 data = test_data → 테스트 데이터 누수
#     (5) data 인수를 생략해 전역 객체를 찾다가 실패
#     (6) summary() 결과를 ols_model 에 덮어씀
#
#  [task1-3-predict]  pred_ols <- predict(ols_model, newdata = test_data)
#                     rmse_ols <- sqrt(mean((test_data$rx_num - pred_ols)^2))
#   정답 기준
#     · pred_ols 길이가 nrow(test_data)
#     · rmse_ols 가 길이 1 숫자이며 기준 계산식과 일치
#   예상 오류
#     (1) newdata 를 생략 → 훈련 데이터 예측값이 반환되어 길이가 다름
#     (2) newdata = train_data → 훈련 데이터로 평가
#     (3) newdata = x_test (행렬) → lm 예측에서 오류
#     (4) sqrt 와 mean 의 순서를 바꿔 mean(sqrt(...)) 로 계산
#     (5) 제곱을 빼먹어 평균편차를 계산 (부호 상쇄로 0 근처 값)
#     (6) abs() 로 계산 → MAE 가 됨
#     (7) 결과변수를 oop_cost 등 다른 이름으로 참조
#
#  [task2-1-matrix]  x_all <- model.matrix(rx_num ~ ., data = model_df)[, -1]
#                    y_all <- model_df$rx_num ; x_train/x_test/y_train/y_test
#   정답 기준
#     · x_all 이 수치형 행렬, 절편 열이 제거되어 있음, 행 수가 nrow(model_df)
#     · y_all 이 model_df$rx_num 과 동일
#     · x_train = x_all[train_id, ], x_test = x_all[-train_id, ] (y 도 동일)
#   예상 오류
#     (1) [, -1] 누락 → (Intercept) 열이 남아 상수열이 들어감
#     (2) [-1, ] 로 잘못 써서 첫 행이 삭제됨
#     (3) data = train_data 로 만들어 행 수가 맞지 않음 (전체에서 한 번만 만들어야 함)
#     (4) x_all[train_id] 처럼 쉼표 누락 → 행렬이 아니라 벡터가 됨
#     (5) y_all 에 model_df["rx_num"] (데이터프레임) 을 넣음
#     (6) 훈련/테스트에 -train_id 를 반대로 적용
#     (7) as.matrix(model_df) 로 변환 → factor 가 문자열이 되어 비수치 행렬
#     (8) 앞 문항의 train_id 가 없어서 실행 실패
#
#  [task2-2-lasso]  cv_lasso <- cv.glmnet(x = x_train, y = y_train, alpha = 1,
#                     family = "gaussian", nfolds = 10, type.measure = "mse")
#   정답 기준
#     · cv.glmnet 객체이며 가우시안 계열(elnet)
#     · lambda.1se 에서 0 계수가 존재 (= Lasso)
#     · 학습에 사용한 관측치 수가 length(train_id)
#     · type.measure 가 MSE
#   예상 오류
#     (1) glmnet() 만 사용 → 교차검증 결과가 없어 lambda 선택 불가
#     (2) alpha = 0 → Ridge 가 적합됨 (0 계수 없음)
#     (3) alpha 를 "1" 문자열로 지정
#     (4) family = "binomial" → 연속형 결과에 부적합
#     (5) x = model_df / data.frame 전달 → 행렬이 아니어서 오류
#     (6) x = x_all, y = y_all → 테스트 데이터까지 학습에 포함
#     (7) x 와 y 의 행 수 불일치 (x_train 과 y_all 조합 등)
#     (8) type.measure = "auc" 등 분류용 지표 지정
#     (9) set.seed(123) 누락 → 강사 결과와 lambda 값이 달라짐 (경고만)
#
#  [task2-3-eval]  pred_lasso_min/1se <- as.numeric(predict(cv_lasso, newx = x_test, s = ...))
#                  rmse_lasso_min/1se <- sqrt(mean((y_test - pred)^2))
#   정답 기준
#     · 두 예측 벡터의 길이가 nrow(x_test)
#     · 각 예측이 해당 lambda 의 기준 예측과 일치
#     · 두 RMSE 가 기준 계산식과 일치
#   예상 오류
#     (1) newdata = 로 지정 → glmnet 계열은 newx
#     (2) newx = test_data (데이터프레임) 전달
#     (3) s = lambda.min 처럼 따옴표 누락 → 객체를 찾을 수 없음
#     (4) 두 예측 모두 같은 s 를 지정 (min/1se 가 동일한 값)
#     (5) as.numeric() 누락 → 한 열짜리 행렬 (계산은 되지만 형태가 다름)
#     (6) newx = x_train → 훈련 데이터로 평가
#     (7) RMSE 에 test_data$rx_num 대신 y_train 을 사용
#     (8) sqrt/mean 순서 오류, 제곱 누락
#
#  [task2-4-coef]  lasso_coef <- coef(cv_lasso, s = "lambda.1se") → tibble(term, estimate)
#   정답 기준
#     · lasso_coef 의 행 수가 ncol(x_train) + 1 (절편 포함)
#     · lasso_coef_df 에 term(문자) 과 estimate(숫자) 열이 있음
#     · sd_x 가 이름 있는 길이 ncol(x_train) 벡터
#     · lambda.1se 해이므로 0 계수가 존재
#   예상 오류
#     (1) s 를 지정하지 않음 → 전체 lambda 경로의 계수 행렬이 반환됨
#     (2) summary(cv_lasso) 로 계수를 보려 함 → glmnet 은 계수 요약 메서드가 없음
#     (3) term 에 colnames() 사용 → 행 이름이어야 하므로 rownames()
#     (4) as.numeric() 누락 → tibble 에 희소행렬 열이 들어감
#     (5) apply(x_train, 1, sd) → 행 단위 표준편차 (방향 오류)
#     (6) sd_x 를 x_test 나 model_df 로 계산 → 이름이 맞지 않아 결합 실패
#     (7) s = "lambda.min" 사용 → 정답으로 인정하되 안내 메시지 표시
#
#  [task3-1-ridge]  cv_ridge <- cv.glmnet(..., alpha = 0) 이후 예측·RMSE
#   정답 기준
#     · cv.glmnet 객체이며 0 계수가 없음 (= Ridge)
#     · 예측과 RMSE 가 기준 계산식과 일치
#   예상 오류
#     (1) alpha 를 1 그대로 두어 Lasso 가 반복됨
#     (2) 객체 이름을 cv_lasso 로 덮어써 앞 결과가 사라짐
#     (3) pred_ridge_min 에 as.numeric() 누락
#     (4) x_train/y_train 대신 전체 데이터 사용
#     (5) 앞 문항 실패로 rmse_lasso_1se 가 없어 cat() 출력에서 오류
#
#  ── 공통 오류 ───────────────────────────────────────────────────────────────
#     · 블록을 순서대로 실행하지 않아 앞 객체가 없음 → "객체를 찾을 수 없습니다"
#     · 빈칸 _____ 를 그대로 둔 채 실행 → 구문 오류
#     · set_student() 미실행 → 제출은 되지만 unknown 으로 기록됨
#     · 패키지 미설치 (glmnet, tidyr, tibble) → could not find function
# ══════════════════════════════════════════════════════════════════════════════

SUPABASE_URL <- "https://mztyhpckshnqcklogrsn.supabase.co"
SUPABASE_KEY <- "sb_publishable_pflU44StAqW5XTy94LsuJA_p_zMRtVv"   # anon public key

CHAPTER <- "d1-01"

# ── 내부 상태 ─────────────────────────────────────────────────────
.tracker_env <- new.env()
.tracker_env$session_id <- paste0(
  format(Sys.time(), "%Y%m%d%H%M%S"), "-",
  paste(sample(letters, 6, TRUE), collapse = "")
)

`%||%` <- function(a, b) if (is.null(a)) b else a

.has <- function(e, nm) !is.null(e[[nm]])

# 정답 메시지를 콘솔 폭에 맞춰 접습니다.
.wrap <- function(txt, width = 76, indent = "   ") {
  ws <- strwrap(txt, width = width)
  paste(paste0(indent, ws), collapse = "\n")
}

# 호출 환경과 전역 환경의 객체를 리스트로 모읍니다.
# 환경에 $로 접근하면 상위 환경을 찾지 않아 값이 NULL이 되므로,
# 규칙 검사 전에 반드시 리스트로 바꿔 두어야 합니다.
.snapshot <- function(env) {
  e <- as.list(env, all.names = TRUE)
  if (!identical(env, globalenv())) {
    g <- as.list(globalenv(), all.names = TRUE)
    e <- utils::modifyList(g, e)
  }
  e
}

set_student <- function(id) {
  if (missing(id) || !nzchar(id) || id %in% c("ID 입력", "학번입력", "abc")) {
    stop("첫 청크의 set_student()에 본인 ID를 입력하세요.", call. = FALSE)
  }
  .tracker_env$student_id <- id
  message("\u2713 ", id, " 님, 실습을 시작합니다.")
  invisible(id)
}

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

# ── 보조 함수 ────────────────────────────────────────────────
.num <- function(x) suppressWarnings(as.numeric(as.matrix(x)))

.eq <- function(a, b, tol = 1e-6) {
  a <- .num(a); b <- .num(b)
  if (length(a) != length(b) || anyNA(a) || anyNA(b)) return(FALSE)
  isTRUE(all.equal(a, b, tolerance = tol))
}

.scalar <- function(x) is.numeric(.num(x)) && length(.num(x)) == 1 && !anyNA(.num(x))

.rmse <- function(truth, pred) sqrt(mean((.num(truth) - .num(pred))^2))

.with_seed <- function(seed, expr) {
  old <- if (exists(".Random.seed", envir = .GlobalEnv))
           get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (is.null(old)) suppressWarnings(rm(".Random.seed", envir = .GlobalEnv))
    else assign(".Random.seed", old, envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  expr
}

# glmnet 적합에서 0 계수 개수
.n_zero <- function(fit, s = "lambda.1se") {
  cf <- tryCatch(as.numeric(coef(fit, s = s)), error = function(err) NULL)
  if (is.null(cf)) return(NA_integer_)
  sum(cf[-1] == 0)
}

# glmnet 호출에서 alpha 값 꺼내기 (없으면 NULL)
.alpha_of <- function(fit) {
  tryCatch(as.numeric(eval(fit$call$alpha)), error = function(err) NULL)
}

# ── 정답 규칙 ────────────────────────────────────────────────
# need  : 학생이 만들어야 하는 객체 이름
# rules : 순서대로 검사, 첫 실패의 msg가 힌트로 기록됨
# note  : 정답이지만 안내할 사항이 있을 때 (통과 후 메시지)
# call_from: $call을 코드로 기록할 객체 (없으면 NULL)

CHECKS <- list(

  # ══ Part 1 ══════════════════════════════════════════════════
  "task1-1-split" = list(
    need = c("model_df", "train_id", "train_data", "test_data"),
    call_from = NULL,
    note = "분할은 모형을 고르기 전에 먼저 해 두는 작업입니다. 테스트 데이터를 한 번이라도 보고 나면 그 정보가 판단에 섞여 들어가, 마지막에 계산하는 성능이 실제보다 좋게 나옵니다. set.seed(123)을 넣은 것은 같은 표본이 다시 뽑히게 하려는 것이고, 이 값을 바꾸면 이후 모든 결과가 조금씩 달라집니다. 무작위로 나눈다는 것은 두 집단이 같은 모집단에서 온 표본이 되도록 만드는 일이라, 훈련 데이터에서 배운 것이 테스트 데이터에도 통할 것이라는 기대의 근거가 됩니다.",
    rules = list(

      # ── 전처리 단계 확인 ──────────────────────────────
      list(f = function(e) is.data.frame(e$model_df),
           msg = "model_df가 데이터프레임이 아닙니다. 「변수 전처리」 블록부터 다시 실행하세요."),

      list(f = function(e) "rx_num" %in% names(e$model_df),
           msg = "model_df에 결과변수 rx_num이 없습니다. transmute() 블록을 먼저 실행하세요."),

      list(f = function(e) nrow(e$model_df) > 0,
           msg = "model_df에 행이 하나도 없습니다. drop_na()까지 정상 실행되었는지 확인하세요."),

      # ── train_id 확인 ─────────────────────────────────
      list(f = function(e) is.numeric(e$train_id) && !is.matrix(e$train_id),
           msg = "train_id가 숫자 벡터가 아닙니다. sample(1:nrow(model_df), size = ...) 형태로 만드세요."),

      list(f = function(e) all(e$train_id >= 1) && all(e$train_id <= nrow(e$model_df)),
           msg = "train_id에 데이터 범위를 벗어난 행 번호가 있습니다. 1:nrow(model_df)에서 추출했는지 확인하세요."),

      list(f = function(e) anyDuplicated(e$train_id) == 0,
           msg = "train_id에 같은 행 번호가 중복되어 있습니다. sample()에 replace = TRUE를 쓰지 마세요."),

      list(f = function(e) length(e$train_id) != round(0.20 * nrow(e$model_df)),
           msg = "훈련 데이터가 20%로 만들어졌습니다. size에 0.20이 아니라 0.80을 곱하세요."),

      list(f = function(e) length(e$train_id) == round(0.80 * nrow(e$model_df)),
           msg = "train_id의 길이가 전체의 80%가 아닙니다. size = round(0.80 * nrow(model_df))를 확인하세요."),

      # ── 객체 형태 확인 ────────────────────────────────
      list(f = function(e) is.data.frame(e$train_data) && is.data.frame(e$test_data),
           msg = "train_data와 test_data가 데이터프레임이 아닙니다. 대괄호 안에 쉼표를 넣어 model_df[train_id, ] 형태로 행을 선택하세요."),

      list(f = function(e) identical(names(e$train_data), names(e$model_df)) &&
                           identical(names(e$test_data),  names(e$model_df)),
           msg = "열 구성이 model_df와 다릅니다. 행만 선택하고 열은 그대로 두어야 합니다. model_df[train_id, ]처럼 쉼표를 빠뜨리지 마세요."),

      list(f = function(e) nrow(e$train_data) > 0 && nrow(e$test_data) > 0,
           msg = "훈련 또는 테스트 데이터가 비어 있습니다. train_id가 제대로 만들어졌는지 확인하세요."),

      # ── 분할 방식 확인 ────────────────────────────────
      list(f = function(e) nrow(e$train_data) == length(e$train_id),
           msg = "train_data의 행 수가 train_id의 길이와 다릅니다. train_data <- model_df[train_id, ]로 만드세요."),

      list(f = function(e) !identical(rownames(e$train_data), rownames(e$test_data)),
           msg = "test_data가 train_data와 같습니다. 테스트는 행 번호 앞에 음수 기호를 붙여 model_df[-train_id, ]로 만드세요."),

      list(f = function(e) nrow(e$train_data) + nrow(e$test_data) == nrow(e$model_df),
           msg = "훈련과 테스트를 합한 행 수가 전체와 다릅니다. test_data에 -train_id를 썼는지 확인하세요."),

      list(f = function(e) length(intersect(rownames(e$train_data),
                                            rownames(e$test_data))) == 0,
           msg = "훈련과 테스트에 같은 행이 함께 들어갔습니다. 테스트는 -train_id로 해당 행을 제외해야 합니다."),

      list(f = function(e) identical(
             rownames(e$train_data),
             rownames(e$model_df[e$train_id, , drop = FALSE])),
           msg = "train_data가 train_id로 선택한 행과 일치하지 않습니다. 두 줄 모두 model_df를 기준으로 했는지 확인하세요."),

      # ── 난수 고정 확인 ────────────────────────────────
      list(f = function(e) {
             ref <- tryCatch(
               .with_seed(123, sample(1:nrow(e$model_df),
                                      size = round(0.80 * nrow(e$model_df)))),
               error = function(err) NULL)
             if (is.null(ref)) return(TRUE)
             setequal(as.integer(e$train_id), ref)
           },
           msg = "표본 구성이 기준 결과와 다릅니다. set.seed(123)을 포함해 블록 전체를 처음부터 다시 실행하세요.")
    )),

  "task1-2-ols" = list(
    need = c("ols_model", "train_data"),
    call_from = "ols_model",
    note = "공식 우변의 점 하나로 전처리한 예측변수 전체가 모형에 들어갔습니다. summary()의 계수 표가 길게 나오는데, 이 가운데 상당수는 p값이 크고 부호도 이론과 어긋날 수 있습니다. OLS는 변수를 골라 주지 않고 주어진 것을 모두 써서 훈련 데이터의 잔차제곱합을 최소화하기 때문입니다. 예측변수가 많아질수록 이 성질이 부담이 되는데, Part 2에서 다룰 정규화 회귀는 바로 이 지점에 벌점을 걸어 계수를 줄이거나 0으로 만듭니다.",
    rules = list(
      list(f = function(e) inherits(e$ols_model, "lm") && !inherits(e$ols_model, "glm"),
           msg = "lm()으로 적합한 모델이 아닙니다. 결과변수가 연속형이므로 glm()이 아니라 lm()을 사용하세요."),

      list(f = function(e) all.vars(formula(e$ols_model))[1] == "rx_num",
           msg = "결과변수가 rx_num이 아닙니다. 전처리에서 이름을 바꿨으므로 원 변수명(rx_num_mod_6m)이 아닌 rx_num을 쓰세요."),

      list(f = function(e) {
             ref <- tryCatch(ncol(model.matrix(rx_num ~ ., data = e$train_data)),
                             error = function(err) NULL)
             is.null(ref) || length(coef(e$ols_model)) == ref
           },
           msg = "예측변수의 개수가 기준과 다릅니다. 공식 우변에 .을 넣어 결과변수를 제외한 나머지 변수 전체를 사용하세요."),

      list(f = function(e) nrow(model.frame(e$ols_model)) == nrow(e$train_data),
           msg = "train_data가 아닌 다른 데이터로 적합했습니다. data = train_data를 확인하세요. model_df를 쓰면 테스트 데이터가 학습에 섞입니다."),

      list(f = function(e) !any(is.na(coef(e$ols_model))),
           msg = "일부 계수가 NA입니다. 완전한 다중공선성이 있는 열이 포함되었는지 확인하세요.")
    )),

  "task1-3-predict" = list(
    need = c("pred_ols", "rmse_ols", "test_data", "ols_model"),
    call_from = NULL,
    note = "여기서 계산한 RMSE는 학습에 쓰이지 않은 데이터에서 나온 값이라 일반화 성능의 추정치입니다. 같은 모형으로 훈련 데이터를 예측하면 RMSE가 더 낮게 나오는데, 그 차이가 모형이 훈련 데이터에 맞춰진 정도입니다. 단위는 결과변수와 같으므로 처방약 개수 기준으로 평균적으로 몇 개쯤 빗나가는지로 읽으시면 됩니다. 다만 이 값 하나만으로는 좋고 나쁨을 말할 수 없고, 뒤에서 나올 영모형이나 다른 모형과 나란히 놓아야 의미가 생깁니다.",
    rules = list(
      list(f = function(e) length(.num(e$pred_ols)) == nrow(e$test_data),
           msg = "pred_ols의 길이가 테스트 데이터의 행 수와 다릅니다. newdata = test_data를 지정했는지 확인하세요. 생략하면 훈련 데이터 예측값이 반환됩니다."),

      list(f = function(e) .eq(e$pred_ols,
                               predict(e$ols_model, newdata = e$test_data)),
           msg = "예측값이 기준과 다릅니다. predict(ols_model, newdata = test_data) 형태인지 확인하세요."),

      list(f = function(e) .scalar(e$rmse_ols),
           msg = "rmse_ols는 숫자 하나여야 합니다. mean()과 sqrt()가 모두 적용되었는지 확인하세요."),

      list(f = function(e) !.eq(e$rmse_ols,
                                mean(sqrt((.num(e$test_data$rx_num) - .num(e$pred_ols))^2))),
           msg = "sqrt()와 mean()의 순서가 바뀌었습니다. 제곱 → 평균 → 제곱근 순서입니다."),

      list(f = function(e) .eq(e$rmse_ols, .rmse(e$test_data$rx_num, e$pred_ols)),
           msg = "RMSE 계산식을 확인하세요. (실제값 - 예측값)^2의 평균에 sqrt()를 취합니다. 실제값은 test_data$rx_num입니다.")
    )),

  # ══ Part 2 ══════════════════════════════════════════════════
  "task2-1-matrix" = list(
    need = c("x_all", "y_all", "x_train", "x_test", "y_train", "y_test",
             "model_df", "train_id"),
    call_from = NULL,
    note = "glmnet은 공식을 받지 않으므로 데이터프레임을 숫자 행렬로 바꾸는 단계가 필요합니다. model.matrix()가 factor를 더미로 펼치면서 열 개수가 원래 변수 개수보다 늘어난 것을 dim()에서 확인할 수 있습니다. 행렬을 전체 데이터에서 한 번만 만들고 나중에 나눈 이유는 훈련과 테스트에서 따로 만들면 범주 구성에 따라 열의 개수나 순서가 달라질 수 있기 때문입니다. train_id를 그대로 재사용한 것도 같은 이유로, Part 1과 Part 2의 결과를 같은 테스트 데이터에서 비교하기 위해서입니다.",
    rules = list(
      list(f = function(e) is.matrix(e$x_all),
           msg = "x_all이 행렬이 아닙니다. model.matrix()의 결과를 그대로 저장하세요."),

      list(f = function(e) is.numeric(e$x_all),
           msg = "x_all이 수치형 행렬이 아닙니다. as.matrix(model_df)가 아니라 model.matrix(rx_num ~ ., data = model_df)를 사용하세요."),

      list(f = function(e) !("(Intercept)" %in% colnames(e$x_all)),
           msg = "절편 열이 남아 있습니다. model.matrix() 뒤에 [, -1]을 붙여 첫 열을 제거하세요."),

      list(f = function(e) nrow(e$x_all) == nrow(e$model_df),
           msg = "x_all의 행 수가 model_df와 다릅니다. [-1, ]이 아니라 [, -1]인지, data = model_df인지 확인하세요."),

      list(f = function(e) {
             ref <- model.matrix(rx_num ~ ., data = e$model_df)[, -1]
             ncol(e$x_all) == ncol(ref)
           },
           msg = "x_all의 열 개수가 기준과 다릅니다. 공식 우변에 .을 넣어 전체 변수를 사용했는지 확인하세요."),

      list(f = function(e) is.numeric(e$y_all) && !is.matrix(e$y_all) &&
                           length(e$y_all) == nrow(e$model_df),
           msg = "y_all이 숫자 벡터가 아닙니다. model_df$rx_num처럼 $로 열을 꺼내세요."),

      list(f = function(e) .eq(e$y_all, e$model_df$rx_num),
           msg = "y_all이 rx_num과 일치하지 않습니다. 결과변수 열을 꺼냈는지 확인하세요."),

      list(f = function(e) is.matrix(e$x_train) && is.matrix(e$x_test),
           msg = "x_train 또는 x_test가 행렬이 아닙니다. x_all[train_id, ]처럼 대괄호 안에 쉼표를 넣어야 행이 선택됩니다."),

      list(f = function(e) nrow(e$x_train) == length(e$train_id),
           msg = "x_train의 행 수가 train_id의 길이와 다릅니다. 새로 분할하지 말고 앞에서 만든 train_id를 그대로 재사용하세요."),

      list(f = function(e) nrow(e$x_train) + nrow(e$x_test) == nrow(e$x_all),
           msg = "x_train과 x_test를 합한 행 수가 전체와 다릅니다. 테스트는 x_all[-train_id, ]입니다."),

      list(f = function(e) ncol(e$x_train) == ncol(e$x_all) &&
                           ncol(e$x_test) == ncol(e$x_all),
           msg = "훈련과 테스트의 열 개수가 다릅니다. 행렬은 전체 데이터에서 한 번만 만들고 그 다음에 나눠야 합니다."),

      list(f = function(e) .eq(e$x_train, e$x_all[e$train_id, , drop = FALSE]),
           msg = "x_train이 train_id로 선택한 행과 다릅니다. 훈련과 테스트에 -train_id를 반대로 적용하지 않았는지 확인하세요."),

      list(f = function(e) .eq(e$x_test, e$x_all[-e$train_id, , drop = FALSE]),
           msg = "x_test가 기준과 다릅니다. x_all[-train_id, ]로 만드세요. 음수 기호를 빠뜨리면 훈련 데이터가 그대로 들어갑니다."),

      list(f = function(e) length(e$y_train) == nrow(e$x_train) &&
                           length(e$y_test) == nrow(e$x_test),
           msg = "y와 x의 행 수가 맞지 않습니다. y_all에도 같은 train_id를 적용하세요."),

      list(f = function(e) .eq(e$y_test, e$y_all[-e$train_id]),
           msg = "y_test가 기준과 다릅니다. y_all[-train_id]로 만드세요.")
    )),

  "task2-2-lasso" = list(
    need = c("cv_lasso", "x_train", "train_id"),
    call_from = NULL,
    note = "lambda는 데이터에서 추정되는 모수가 아니라 연구자가 정해야 하는 하이퍼파라미터입니다. 어떤 값이 맞는지 미리 알 수 없으므로 후보를 넓게 계산해 두고 교차검증 오차가 낮은 쪽을 고릅니다. 곡선의 위쪽 축에 표시된 숫자가 각 lambda에서 살아남은 변수의 개수인데, 오른쪽으로 갈수록 벌점이 강해져 그 수가 줄어듭니다. lambda.min은 오차 자체를 최소화하고, lambda.1se는 오차가 최솟값에서 표준오차 하나 안에 머무는 범위에서 가장 강한 벌점을 고릅니다. 둘 중 무엇을 쓸지는 예측 정확도와 모형의 간결성 가운데 무엇을 우선할지에 달려 있습니다.",
    rules = list(
      list(f = function(e) inherits(e$cv_lasso, "cv.glmnet"),
           msg = "cv.glmnet()의 결과가 아닙니다. glmnet()만 쓰면 lambda별 예측오차가 계산되지 않아 lambda를 고를 수 없습니다."),

      list(f = function(e) inherits(e$cv_lasso$glmnet.fit, "elnet"),
           msg = "family가 gaussian이 아닙니다. 결과변수가 연속형이므로 family = \"gaussian\"을 지정하세요."),

      list(f = function(e) isTRUE(e$cv_lasso$glmnet.fit$nobs == length(e$train_id)),
           msg = "학습에 사용한 관측치 수가 훈련 데이터와 다릅니다. x = x_train, y = y_train을 지정했는지 확인하세요. x_all을 쓰면 테스트 데이터가 학습에 포함됩니다."),

      list(f = function(e) {
             a <- .alpha_of(e$cv_lasso)
             is.null(a) || length(a) == 0 || isTRUE(a == 1)
           },
           msg = "alpha가 1이 아닙니다. Lasso는 alpha = 1입니다. 0은 Ridge, 그 사이는 Elastic Net입니다."),

      list(f = function(e) isTRUE(.n_zero(e$cv_lasso, "lambda.1se") > 0),
           msg = "0으로 축소된 계수가 하나도 없습니다. alpha 값과 lambda.1se 지정을 확인하세요."),

      list(f = function(e) grepl("Squared", e$cv_lasso$name, ignore.case = TRUE),
           msg = "평가지표가 MSE가 아닙니다. type.measure = \"mse\"를 지정하세요."),

      list(f = function(e) isTRUE(e$cv_lasso$lambda.min <= e$cv_lasso$lambda.1se),
           msg = "lambda.min과 lambda.1se의 관계가 이상합니다. 교차검증 블록을 다시 실행해 보세요."),

      list(f = function(e) {
             nf <- tryCatch(eval(e$cv_lasso$call$nfolds), error = function(err) NULL)
             is.null(nf) || isTRUE(nf == 10)
           },
           msg = "nfolds가 10이 아닙니다. nfolds = 10으로 지정하세요.")
    )),

  "task2-3-eval" = list(
    need = c("pred_lasso_min", "pred_lasso_1se",
             "rmse_lasso_min", "rmse_lasso_1se",
             "cv_lasso", "x_test", "y_test"),
    call_from = NULL,
    note = "RMSE와 변수 개수를 함께 보시면 정규화가 무엇을 주고받는지 드러납니다. lambda.1se는 변수를 크게 줄이면서도 RMSE는 크게 나빠지지 않는 경우가 많은데, 이는 제외된 변수들이 예측에 기여하는 몫이 작았다는 뜻입니다. OLS가 RMSE에서 앞서더라도 수십 개 변수를 모두 쓴 결과라는 점을 함께 고려해야 합니다. 예측변수가 관측치 수에 비해 많아질수록 이 격차는 정규화 쪽에 유리해집니다.",
    rules = list(
      list(f = function(e) length(.num(e$pred_lasso_min)) == nrow(e$x_test) &&
                           length(.num(e$pred_lasso_1se)) == nrow(e$x_test),
           msg = "예측값의 길이가 테스트 데이터의 행 수와 다릅니다. newx = x_test를 지정했는지 확인하세요. glmnet 계열은 newdata가 아니라 newx입니다."),

      list(f = function(e) !.eq(e$pred_lasso_min, e$pred_lasso_1se) ||
                           .eq(e$cv_lasso$lambda.min, e$cv_lasso$lambda.1se),
           msg = "두 예측값이 완전히 같습니다. s에 각각 \"lambda.min\"과 \"lambda.1se\"를 지정했는지 확인하세요."),

      list(f = function(e) .eq(e$pred_lasso_min,
                               predict(e$cv_lasso, newx = e$x_test, s = "lambda.min")),
           msg = "lambda.min 예측값이 기준과 다릅니다. newx = x_test, s = \"lambda.min\"인지, 따옴표를 빠뜨리지 않았는지 확인하세요."),

      list(f = function(e) .eq(e$pred_lasso_1se,
                               predict(e$cv_lasso, newx = e$x_test, s = "lambda.1se")),
           msg = "lambda.1se 예측값이 기준과 다릅니다. s = \"lambda.1se\"를 확인하세요."),

      list(f = function(e) .scalar(e$rmse_lasso_min) && .scalar(e$rmse_lasso_1se),
           msg = "RMSE는 각각 숫자 하나여야 합니다. mean()과 sqrt()가 모두 적용되었는지 확인하세요."),

      list(f = function(e) .eq(e$rmse_lasso_min, .rmse(e$y_test, e$pred_lasso_min)),
           msg = "lambda.min의 RMSE 계산식을 확인하세요. 실제값은 y_test이고, 제곱 → 평균 → 제곱근 순서입니다."),

      list(f = function(e) .eq(e$rmse_lasso_1se, .rmse(e$y_test, e$pred_lasso_1se)),
           msg = "lambda.1se의 RMSE 계산식을 확인하세요. 두 줄에서 예측 객체 이름을 바꿔 쓰지 않았는지도 함께 확인하세요.")
    ),
    caution = function(e) {
      if (is.matrix(e$pred_lasso_min) || is.matrix(e$pred_lasso_1se))
        "예측값이 한 열짜리 행렬로 저장되어 있습니다. as.numeric()으로 벡터로 바꿔 두면 이후 계산이 안전합니다."
      else NULL
    }),

  "task2-4-coef" = list(
    need = c("lasso_coef", "lasso_coef_df", "sd_x", "cv_lasso", "x_train"),
    call_from = NULL,
    note = "표에 나타나지 않는 변수는 Lasso가 계수를 정확히 0으로 만들어 모형에서 제외한 변수입니다. 표준화 계수를 함께 계산한 것은 원래 계수의 크기가 변수의 단위에 좌우되기 때문입니다. 소득처럼 값이 큰 변수는 계수가 작게 나오고 이진변수는 크게 나오므로, 그대로 비교하면 어느 변수가 더 중요한지 잘못 읽게 됩니다. 다만 여기서 상위에 오른 변수를 인과적으로 해석하지는 마세요. Lasso는 예측에 유용한 변수를 남기는 것이지 결과를 변화시키는 변수를 찾는 것이 아니고, 서로 상관된 변수들 가운데 하나만 임의로 남기는 성질도 있습니다.",
    rules = list(
      list(f = function(e) nrow(as.matrix(e$lasso_coef)) == ncol(e$x_train) + 1,
           msg = "lasso_coef의 행 수가 예측변수 개수 + 1이 아닙니다. s를 지정하지 않으면 모든 lambda의 계수가 한꺼번에 반환됩니다."),

      list(f = function(e) ncol(as.matrix(e$lasso_coef)) == 1,
           msg = "lasso_coef에 여러 lambda의 계수가 들어 있습니다. coef(cv_lasso, s = \"lambda.1se\")처럼 s를 지정하세요."),

      list(f = function(e) is.data.frame(e$lasso_coef_df) &&
                           all(c("term", "estimate") %in% names(e$lasso_coef_df)),
           msg = "lasso_coef_df에 term과 estimate 열이 없습니다. tibble(term = ..., estimate = ...)로 만드세요."),

      list(f = function(e) is.character(e$lasso_coef_df$term),
           msg = "term이 문자형이 아닙니다. rownames(lasso_coef)로 변수 이름을 가져오세요. colnames()가 아닙니다."),

      list(f = function(e) is.numeric(e$lasso_coef_df$estimate),
           msg = "estimate가 숫자형이 아닙니다. as.numeric(lasso_coef)로 벡터로 변환하세요."),

      list(f = function(e) "(Intercept)" %in% e$lasso_coef_df$term,
           msg = "term에 (Intercept)가 없습니다. rownames()를 그대로 가져왔는지 확인하세요."),

      list(f = function(e) .eq(sort(e$lasso_coef_df$estimate),
                               sort(as.numeric(e$lasso_coef))),
           msg = "estimate 값이 lasso_coef와 일치하지 않습니다. 같은 객체에서 가져왔는지 확인하세요."),

      list(f = function(e) length(e$sd_x) == ncol(e$x_train),
           msg = "sd_x의 길이가 예측변수 개수와 다릅니다. apply()의 두 번째 인수를 2로 지정해 열 단위로 계산하세요."),

      list(f = function(e) !is.null(names(e$sd_x)) &&
                           setequal(names(e$sd_x), colnames(e$x_train)),
           msg = "sd_x에 변수 이름이 없습니다. x_train에 apply()를 적용해야 sd_x[term]으로 이름을 찾을 수 있습니다."),

      list(f = function(e) .eq(e$sd_x[colnames(e$x_train)],
                               apply(e$x_train, 2, sd)[colnames(e$x_train)]),
           msg = "sd_x 값이 기준과 다릅니다. x_test나 model_df가 아니라 x_train으로 계산하세요.")
    ),
    caution = function(e) {
      z <- sum(as.numeric(e$lasso_coef)[-1] == 0)
      if (z == 0)
        "0인 계수가 없습니다. s = \"lambda.min\"으로 꺼냈을 수 있습니다. 설명은 lambda.1se 기준입니다."
      else NULL
    }),

  # ══ Part 3 (선택사항) ═══════════════════════════════════════
  "task3-1-ridge" = list(
    need = c("cv_ridge", "pred_ridge_min", "pred_ridge_1se",
             "rmse_ridge_min", "rmse_ridge_1se", "x_test", "y_test", "train_id"),
    call_from = NULL,
    note = "Ridge의 계수 표에는 0이 하나도 없습니다. 제곱 벌점은 계수를 0 쪽으로 당기지만 정확히 0으로 만들지는 않기 때문이고, 절댓값 벌점을 쓰는 Lasso와 갈리는 지점이 여기입니다. 두 방법의 RMSE 차이가 크지 않다면 이 데이터에서는 변수를 골라내는 것과 전부 조금씩 줄이는 것이 비슷하게 작동한다는 뜻으로 읽을 수 있습니다. 어느 쪽을 고를지는 성능만이 아니라 목적에 달려 있습니다. 소수의 변수로 설명해야 하면 Lasso가, 상관된 변수들의 정보를 함께 살리고 싶으면 Ridge가 맞습니다.",
    rules = list(
      list(f = function(e) inherits(e$cv_ridge, "cv.glmnet"),
           msg = "cv.glmnet()의 결과가 아닙니다. cv_ridge에 저장하세요."),

      list(f = function(e) inherits(e$cv_ridge$glmnet.fit, "elnet"),
           msg = "family가 gaussian이 아닙니다. family = \"gaussian\"을 지정하세요."),

      list(f = function(e) isTRUE(e$cv_ridge$glmnet.fit$nobs == length(e$train_id)),
           msg = "학습에 사용한 관측치 수가 훈련 데이터와 다릅니다. x = x_train, y = y_train을 확인하세요."),

      list(f = function(e) {
             a <- .alpha_of(e$cv_ridge)
             is.null(a) || length(a) == 0 || isTRUE(a == 0)
           },
           msg = "alpha가 0이 아닙니다. Ridge는 alpha = 0입니다. Lasso 블록에서 이 값만 바꾸면 됩니다."),

      list(f = function(e) isTRUE(.n_zero(e$cv_ridge, "lambda.1se") == 0),
           msg = "0으로 축소된 계수가 있습니다. Ridge는 계수를 정확히 0으로 만들지 않습니다."),

      list(f = function(e) length(.num(e$pred_ridge_min)) == nrow(e$x_test) &&
                           length(.num(e$pred_ridge_1se)) == nrow(e$x_test),
           msg = "예측값의 길이가 테스트 데이터의 행 수와 다릅니다. newx = x_test를 지정하세요."),

      list(f = function(e) .eq(e$pred_ridge_min,
                               predict(e$cv_ridge, newx = e$x_test, s = "lambda.min")),
           msg = "lambda.min 예측값이 기준과 다릅니다. cv_lasso가 아니라 cv_ridge를 넣었는지 확인하세요."),

      list(f = function(e) .eq(e$pred_ridge_1se,
                               predict(e$cv_ridge, newx = e$x_test, s = "lambda.1se")),
           msg = "lambda.1se 예측값이 기준과 다릅니다. s = \"lambda.1se\"를 확인하세요."),

      list(f = function(e) .eq(e$rmse_ridge_min, .rmse(e$y_test, e$pred_ridge_min)) &&
                           .eq(e$rmse_ridge_1se, .rmse(e$y_test, e$pred_ridge_1se)),
           msg = "RMSE 계산식을 확인하세요. 실제값은 y_test이고, 제곱 → 평균 → 제곱근 순서입니다.")
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
  e    <- .snapshot(env)
  miss <- spec$need[!vapply(spec$need, function(nm) .has(e, nm), logical(1))]

  if (length(miss)) {
    last <- tryCatch(geterrmessage(), error = function(err) "")
    out  <- paste0("객체를 찾을 수 없습니다: ", paste(miss, collapse = ", "),
                   "\n   앞의 블록을 순서대로 실행했는지, 객체 이름의 철자가 맞는지 확인하세요.",
                   if (nzchar(last)) paste0("\n   [직전 오류] ", trimws(last)) else "")
    .send(exercise_id, "error", FALSE, out, NA_character_, NA_character_, t0)
    message("\u274C 실행되지 않았습니다.\n   ", out)
    return(invisible(FALSE))
  }

  hint <- NA_character_; ok <- TRUE
  for (r in spec$rules) {
    res <- tryCatch(isTRUE(r$f(e)), error = function(err) FALSE)
    if (!res) { ok <- FALSE; hint <- r$msg; break }
  }

  code <- NA_character_
  if (!is.null(spec$call_from) && .has(e, spec$call_from)) {
    code <- tryCatch(
      paste(deparse(e[[spec$call_from]]$call), collapse = " "),
      error = function(err) NA_character_)
  }

  .send(exercise_id, if (ok) "ok" else "wrong", ok,
        if (ok) "OK" else hint, hint, code, t0)

  if (ok) {
    message("\u2705 정답입니다. 잘 채우셨습니다. 다음 문항으로 넘어가세요.")
    if (!is.null(spec$caution)) {
      ct <- tryCatch(spec$caution(e), error = function(err) NULL)
      if (!is.null(ct)) message("   \U1F4DD ", ct)
    }
    if (!is.null(spec$note)) message("\n", .wrap(spec$note))
  } else {
    message("\u274C 다시 확인해 보세요.\n   \U1F4A1 ", hint)
  }

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
