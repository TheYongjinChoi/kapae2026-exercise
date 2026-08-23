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

.has <- function(e, nm) !is.null(e[[nm]])

# keras 객체 속성을 안전하게 꺼내기 (없으면 NULL)
.kget <- function(expr) tryCatch(expr, error = function(err) NULL)

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
  note = paste0(
    "은닉층을 하나 추가한 결과 테스트 오차가 회귀모형과 비슷한 수준으로 줄어 들었습니다. 활성화 함수가 항등함수이면 은닉층을 통과해도 선형결합의 선형결합이라 하나의 선형식으로 접히기 때문입니다. 남은 작은 차이는 lm()이 해를 한 번에 구하는 반면 경사하강은 반복적으로 개선하는 과정에서 회차당 개선폭이 점차 줄어들기 때문입니다."
  ),
  rules = list(

    # 하이퍼파라미터
    list(f = function(e) .eq(e$n, nrow(e$x_train)) && .eq(e$p, ncol(e$x_train)),
         msg = "n과 p를 확인하세요. n <- nrow(x_train), p <- ncol(x_train) 입니다."),
    list(f = function(e) .eq(e$n_hidden, 5),
         msg = "은닉층 유닛 수 n_hidden은 5여야 합니다."),
    # Part 3의 for (lr in lrs) 루프가 lr을 덮어쓰므로 값이 다를 수 있습니다.
    # 여기서는 존재와 범위만 확인합니다.
    list(f = function(e) is.numeric(e$lr) && e$lr > 0,
         msg = "학습률 lr을 양수로 지정하세요. 이 문항에서는 0.01입니다."),
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
    list(f = function(e) {
           L <- e$loss_history
           mean(L[1:100]) > mean(L[(e$n_iter - 99):e$n_iter])
         },
         msg = "손실 곡선이 내려가지 않았습니다. 순전파 Z <- x_train %*% W1과 갱신 부호를 확인하세요."),
    list(f = function(e) {
           L <- e$loss_history
           mean(diff(L) <= 1e-12) > 0.90
         },
         msg = "손실이 오르내립니다. 기울기 계산이나 갱신 부호를 확인하세요. 정상이라면 거의 매 반복에서 손실이 줄어듭니다."),

    # 항등 활성화의 핵심 결과
    list(f = function(e) .has(e, "mse_id") && .has(e, "mse_lm"),
         msg = "테스트 손실 mse_id와 lm() 비교값 mse_lm을 계산하세요."),
    list(f = function(e) is.finite(e$mse_id) && e$mse_id > 0,
         msg = "mse_id가 유효한 값이 아닙니다. 테스트 순전파를 다시 확인하세요."),
    list(f = function(e) abs(e$mse_id - e$mse_lm) / e$mse_lm < 0.35,
         msg = "항등 활성화 신경망의 테스트 MSE가 lm()과 크게 다릅니다. 은닉층 출력이 H <- Z 인지 확인하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 3. ReLU로 바꾸기
# ─────────────────────────────────────────────────────────────────
"nn-02-relu" = list(
  need = c("loss_history", "loss_history_relu", "mse_relu", "mse_id", "n_iter"),
  call_from = NULL,
  note = paste0(
    "이번 실습에서는 순전파의 pmax(Z, 0)과 역전파의 (Z > 0) 두 줄만 변경을 했습니다. 이에 따라 음수 구간이 잘리면 관측치마다 켜지는 유닛이 달라지고, 그 조합이 하나의 선형식으로 접히지 않는 함수를 만듭니다. 테스트 오차가 항등 모형보다 낮아졌는지, 아니면 오히려 높아졌는지도 함께 확인해 보세요"
  ),
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
  note = paste0(
    "summary()의 Output Shape는 각 층을 통과한 뒤 데이터의 모양입니다. None은 관측치 수 자리이고 뒤의 숫자가 그 층의 유닛 수를 의미합니다. 수동 구현에서 Z와 H가 n행 5열 행렬이었던 것과 같습니다. Param 열은 그 층에서 학습되는 숫자의 개수로 은닉층은 p 곱하기 8에 편향 8을 더한 값이고 출력층은 8에 1을 더한 값입니다."
  ),
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
           a <- .kget(tolower(paste(e$model$layers[[1]]$activation$`__name__`)))
           is.null(a) || !nzchar(a) || a == "relu"
         },
         msg = "은닉층의 activation이 \"relu\"가 아닙니다."),
    list(f = function(e) {
           a <- .kget(tolower(paste(e$model$layers[[2]]$activation$`__name__`)))
           is.null(a) || !nzchar(a) || a == "linear"
         },
         msg = "출력층에는 activation을 지정하지 않습니다. 연속형 결과이므로 항등함수여야 합니다."),
    list(f = function(e) {
           np <- .kget(as.numeric(e$model$count_params()))
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
  note = paste0(
    "세 인수가 각각 수동 구현의 어느 자리를 대신하는지 짚어 두면 이해에 도움이 됩니다. loss = \"mse\"는 반복문 안에서 계산하던 mean((y_train - pred)^2)이고, optimizer는 갱신 단계의 W1 <- W1 - lr * grad_W1을 맡습니다. 다만 Adam은 순수한 경사하강과 달리 파라미터마다 최근 기울기를 반영해 보폭을 조절하므로 같은 learning_rate라도 움직이는 방식이 다릅니다."
  ),
  rules = list(

    list(f = function(e) !is.null(.kget(e$model$optimizer)),
         msg = "모델이 컴파일되지 않았습니다. model |> compile(...)을 실행하세요."),
    list(f = function(e) {
           lo <- .kget(tolower(paste(as.character(e$model$loss), collapse = " ")))
           is.null(lo) || !nzchar(lo) || grepl("mse|mean_squared", lo)
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
  note = paste0(
     "두 곡선의 간격이 이 모형의 상태를 알려줍니다. Training은 학습에 사용한 데이터에서 계산한 손실이고, Validation은 validation_split = 0.2로 떼어 낸 20%에서 계산한 손실입니다. 가중치를 갱신할 때 쓰이지 않은 데이터라 처음 보는 관측치에서의 성능을 가늠하는 역할을 합니다. 두 곡선이 나란히 내려가 평평해지면 학습이 순조로운 것이고, Training만 계속 내려가는데 Validation이 따라오지 않으면 그 지점부터 훈련 데이터에 과도하게 맞춰지고 있는 것입니다. 떼어 낸 20%도 훈련 데이터 안에서 나온 몫이므로 테스트 데이터는 아직 손대지 않은 상태입니다."
  ),
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
  note = paste0(
    "세 값을 나란히 놓고 보시면 두 가지가 읽힙니다. 영모형과의 차이는 예측변수가 담고 있는 신호의 크기이고, ",
    "선형회귀와의 차이는 은닉층이 추가로 잡아낸 부분입니다. 뒤쪽 간격이 크지 않다면 이 데이터에서 비선형성이 크지 ",
    "않다는 뜻으로 읽을 수 있습니다. "
  ),
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
# Task 7. 학습률
# ─────────────────────────────────────────────────────────────────
"nn-07-lr" = list(
  need = c("lrs", "curves_lr", "rmse_val_lr", "rmse_val_base"),
  call_from = NULL,
  note = "세 패널의 증상이 서로 다르게 나타납니다. 학습률이 작으면 40 epoch 안에 충분히 내려가지 못하고, 크면 최소 지점을 넘나들며 곡선이 튑니다. 학습률은 다른 하이퍼파라미터보다 결과에 미치는 폭이 커서 대개 가장 먼저 조정합니다. 세 번 모두 set_random_seed(42)로 시작했으므로 초기 가중치가 같고, 남은 차이는 학습률에서 온 것입니다.",
  rules = list(
    list(f = function(e) length(e$lrs) == 3,
         msg = "lrs에 학습률 세 개를 넣으세요."),
    list(f = function(e) isTRUE(all.equal(sort(as.numeric(e$lrs)),
                                          c(0.0005, 0.01, 0.3))),
         msg = "lrs의 값을 확인하세요. 0.0005, 0.01, 0.3 세 개입니다."),
    list(f = function(e) length(e$curves_lr) == 3,
         msg = "curves_lr에 학습곡선 세 개가 담겨야 합니다. 반복문이 끝까지 돌았는지 확인하세요."),
    list(f = function(e) all(vapply(e$curves_lr, is.data.frame, logical(1))),
         msg = "curves_lr의 원소가 데이터프레임이 아닙니다. tibble() 부분을 확인하세요."),
    list(f = function(e) all(c("epoch", "loss", "set", "label") %in%
                             names(e$curves_lr[[1]])),
         msg = "학습곡선 데이터프레임에 epoch, loss, set, label 네 열이 있어야 합니다."),
    list(f = function(e) nrow(e$curves_lr[[1]]) == 80,
         msg = "행 수가 맞지 않습니다. epochs = 40이고 곡선이 두 개이므로 80행이어야 합니다."),
    list(f = function(e) length(unique(vapply(e$curves_lr,
             function(d) d$label[1], character(1)))) == 3,
         msg = "세 곡선의 label이 서로 다른지 확인하세요. lrs[j]를 넘겼는지 보시면 됩니다."),
    list(f = function(e) length(e$rmse_val_lr) == 3 && all(is.finite(e$rmse_val_lr)),
         msg = "rmse_val_lr에 세 값이 모두 채워지지 않았습니다."),
    list(f = function(e) all(e$rmse_val_lr > 0),
         msg = "rmse_val_lr이 0 이하입니다. 검증 손실의 최솟값에 제곱근을 취했는지 확인하세요."),
    list(f = function(e) {
           r <- e$rmse_val_lr
           r[2] <= r[1] && r[2] <= r[3]
         },
         msg = "가운데 학습률이 가장 낮게 나오지 않았습니다. lrs의 순서와 build_model(lr = lrs[j])를 확인하세요."),
    list(f = function(e) is.numeric(e$rmse_val_base) && e$rmse_val_base > 0,
         msg = "rmse_val_base가 없습니다. Part 3 도입부의 코드를 먼저 실행하세요.")
  )),
 
# ─────────────────────────────────────────────────────────────────
# Task 8. 노드 수
# ─────────────────────────────────────────────────────────────────
"nn-08-units" = list(
  need = c("units_list", "curves_u", "rmse_val_u", "rmse_val_base"),
  call_from = NULL,
  note = "노드 수는 모형의 용량입니다. units = 128 패널에서 훈련 곡선과 검증 곡선의 간격이 벌어지는데, 그 간격이 훈련 데이터에만 맞춰진 부분입니다. 훈련 손실은 용량을 키울수록 계속 좋아지므로 판단은 검증 손실로 합니다. 용량을 키운 만큼 검증 성능이 따라오지 않는다면, 용량을 줄이는 대신 학습에 제약을 거는 방법이 다음 Task의 주제입니다.",
  rules = list(
    list(f = function(e) length(e$units_list) == 3,
         msg = "units_list에 노드 수 세 개를 넣으세요."),
    list(f = function(e) isTRUE(all.equal(sort(as.numeric(e$units_list)),
                                          c(2, 8, 128))),
         msg = "units_list의 값을 확인하세요. 2, 8, 128 세 개입니다."),
    list(f = function(e) length(e$curves_u) == 3,
         msg = "curves_u에 학습곡선 세 개가 담겨야 합니다."),
    list(f = function(e) all(c("epoch", "loss", "set", "label") %in%
                             names(e$curves_u[[1]])),
         msg = "학습곡선 데이터프레임에 epoch, loss, set, label 네 열이 있어야 합니다."),
    list(f = function(e) nrow(e$curves_u[[1]]) == 120,
         msg = "행 수가 맞지 않습니다. epochs = 60이고 곡선이 두 개이므로 120행이어야 합니다."),
    list(f = function(e) length(e$rmse_val_u) == 3 && all(is.finite(e$rmse_val_u)),
         msg = "rmse_val_u에 세 값이 모두 채워지지 않았습니다."),
    list(f = function(e) all(e$rmse_val_u > 0),
         msg = "rmse_val_u가 0 이하입니다. min(h$metrics$val_loss)에 제곱근을 취했는지 확인하세요."),
    list(f = function(e) e$rmse_val_u[1] >= e$rmse_val_u[2],
         msg = "노드 2개가 8개보다 좋게 나왔습니다. units_list의 순서와 build_model(units = units_list[j])를 확인하세요.")
  )),
 
# ─────────────────────────────────────────────────────────────────
# Task 9. 규제
# ─────────────────────────────────────────────────────────────────
"nn-09-reg" = list(
  need = c("cb_es", "settings", "curves_reg", "rmse_val_reg", "rmse_val_base"),
  call_from = NULL,
  note = "세 곡선의 길이가 서로 다르게 나옵니다. 조기 종료가 각각 다른 시점에서 학습을 멈췄기 때문이고, 규제가 걸린 쪽이 더 오래 개선되는 경우가 많습니다. 세 값의 차이가 크지 않다면 이 데이터에서 과적합의 여지가 애초에 크지 않았다는 뜻으로 읽을 수 있습니다. 규제는 용량이 남아돌 때 효과를 내는 장치라, 모형이 데이터에 비해 작으면 걸어도 달라지는 것이 적습니다.",
  rules = list(
    list(f = function(e) !is.null(e$cb_es),
         msg = "cb_es에 조기 종료 콜백을 저장하세요."),
    list(f = function(e) length(e$settings) == 3,
         msg = "settings에 설정 세 개를 넣으세요."),
    list(f = function(e) all(vapply(e$settings,
             function(s) all(c("label", "dropout", "l2") %in% names(s)), logical(1))),
         msg = "각 설정에 label, dropout, l2 세 항목이 있어야 합니다."),
    list(f = function(e) .eq(e$settings[[2]]$dropout, 0.3),
         msg = "두 번째 설정의 dropout을 0.3으로 지정하세요."),
    list(f = function(e) .eq(e$settings[[3]]$l2, 0.01),
         msg = "세 번째 설정의 l2를 0.01로 지정하세요."),
    list(f = function(e) .eq(e$settings[[1]]$dropout, 0) &&
                         .eq(e$settings[[1]]$l2, 0),
         msg = "첫 번째 설정은 규제를 걸지 않은 기준입니다. dropout과 l2가 모두 0이어야 합니다."),
    list(f = function(e) length(e$curves_reg) == 3,
         msg = "curves_reg에 학습곡선 세 개가 담겨야 합니다."),
    list(f = function(e) all(c("epoch", "loss", "set", "label") %in%
                             names(e$curves_reg[[1]])),
         msg = "학습곡선 데이터프레임에 epoch, loss, set, label 네 열이 있어야 합니다."),
    list(f = function(e) nrow(e$curves_reg[[1]]) < 200,
         msg = "조기 종료가 작동하지 않았습니다. callbacks = list(cb_es)를 fit()에 넘겼는지 확인하세요."),
    list(f = function(e) length(e$rmse_val_reg) == 3 && all(is.finite(e$rmse_val_reg)),
         msg = "rmse_val_reg에 세 값이 모두 채워지지 않았습니다."),
    list(f = function(e) length(unique(round(e$rmse_val_reg, 6))) > 1,
         msg = "세 설정의 결과가 모두 같습니다. dropout과 l2가 build_model()에 전달되었는지 확인하세요.")
  )),
 
# ─────────────────────────────────────────────────────────────────
# Task 10. 격자 만들기와 교차검증
# ─────────────────────────────────────────────────────────────────
"nn-10-grid" = list(
  need = c("grid", "results", "K", "fold", "x_train"),
  call_from = NULL,
  note = "Part 3에서는 한 번에 한 축씩 움직였고 여기서는 여러 축을 동시에 봤습니다. 두 방식의 결과가 다르다면 값들이 서로 영향을 주고받는다는 뜻입니다. validation_split이 훈련 데이터의 일부를 한 번 떼어 고정해 두는 방식이었다면, 교차검증은 겹을 바꿔 가며 모든 관측치를 한 번씩 평가에 씁니다. 검증이 훈련 데이터 안에서만 이루어진다는 점은 같습니다.",
  rules = list(
    list(f = function(e) is.data.frame(e$grid),
         msg = "grid가 데이터프레임이 아닙니다. expand.grid()의 결과를 저장하세요."),
    list(f = function(e) all(c("units", "lr", "dropout") %in% names(e$grid)),
         msg = "grid에 units, lr, dropout 세 열이 있어야 합니다."),
    list(f = function(e) nrow(e$grid) %in% c(4, 8),
         msg = "조합 수가 맞지 않습니다. 세 축을 모두 쓰면 8조합, dropout을 고정하면 4조합입니다."),
    list(f = function(e) .eq(e$K, 3),
         msg = "겹의 수 K는 3입니다."),
    list(f = function(e) length(e$fold) == nrow(e$x_train),
         msg = "fold의 길이가 훈련 표본 수와 다릅니다. length.out = nrow(x_train)을 확인하세요."),
    list(f = function(e) setequal(unique(e$fold), 1:e$K),
         msg = "fold에 1부터 K까지의 번호가 모두 들어 있지 않습니다."),
    list(f = function(e) max(table(e$fold)) - min(table(e$fold)) <= 1,
         msg = "겹의 크기가 고르지 않습니다. rep(1:K, length.out = nrow(x_train))을 확인하세요."),
    list(f = function(e) "cv_rmse" %in% names(e$grid),
         msg = "각 조합의 평균 교차검증 RMSE를 grid$cv_rmse에 저장하세요."),
    list(f = function(e) all(is.finite(e$grid$cv_rmse)),
         msg = "cv_rmse에 결측이나 무한값이 있습니다. run_one()이 모든 조합에서 값을 반환했는지 확인하세요."),
    list(f = function(e) all(e$grid$cv_rmse > 0),
         msg = "cv_rmse가 0 이하입니다. evaluate()가 반환한 loss에 제곱근을 취했는지 확인하세요."),
    list(f = function(e) is.data.frame(e$results) && "cv_rmse" %in% names(e$results),
         msg = "results에 cv_rmse 기준으로 정렬한 결과를 저장하세요."),
    list(f = function(e) !is.unsorted(e$results$cv_rmse),
         msg = "results가 cv_rmse 오름차순으로 정렬되지 않았습니다. arrange(cv_rmse)를 사용하세요.")
  )),
 
# ─────────────────────────────────────────────────────────────────
# Task 11. 결과 읽기와 최종 평가
# ─────────────────────────────────────────────────────────────────
"nn-11-final" = list(
  need = c("results", "best", "final_model", "pred_final", "rmse_final",
           "x_test", "y_test", "rmse_null", "rmse_lm", "rmse_keras"),
  call_from = NULL,
  note = "테스트 데이터는 지금 한 번 쓰였습니다. 이 값을 보고 설정을 다시 고치면 테스트 데이터가 조정 과정의 일부가 되어 성능이 낙관적으로 나옵니다. 조정 후 막대가 선형회귀 아래로 내려갔다면 기본 설정이 이 데이터에 맞지 않았던 것이고, 여전히 비슷한 자리라면 은닉층이 추가로 잡아낼 부분이 크지 않았다는 쪽에 무게가 실립니다. 조정에 들인 계산량과 얻은 개선폭을 함께 보시면 다음에 어디에 시간을 쓸지 판단하는 데 도움이 됩니다.",
  rules = list(
    list(f = function(e) nrow(e$best) == 1,
         msg = "best에는 results의 첫 행 하나만 담아야 합니다."),
    list(f = function(e) .eq(e$best$cv_rmse, min(e$results$cv_rmse)),
         msg = "best가 cv_rmse 최소 조합이 아닙니다. results[1, ]을 확인하세요."),
    list(f = function(e) !is.null(.kget(e$final_model$layers)),
         msg = "final_model이 keras 모델이 아닙니다."),
    list(f = function(e) .eq(.kget(e$final_model$layers[[1]]$units), e$best$units),
         msg = "final_model의 노드 수가 best와 다릅니다. build_model(units = best$units)를 확인하세요."),
    list(f = function(e) is.numeric(e$pred_final) && is.null(dim(e$pred_final)),
         msg = "pred_final이 벡터가 아닙니다. as.numeric()으로 행렬을 벡터로 바꾸세요."),
    list(f = function(e) length(e$pred_final) == nrow(e$x_test),
         msg = "최종 예측값 개수가 테스트 표본 수와 다릅니다."),
    list(f = function(e) .eq(e$rmse_final,
                             sqrt(mean((e$y_test - e$pred_final)^2)), tol = 1e-6),
         msg = "rmse_final 계산식을 확인하세요. 제곱 → 평균 → 제곱근 순서입니다."),
    list(f = function(e) e$rmse_final < e$rmse_null,
         msg = "조정한 모형이 영모형보다 나쁩니다. 학습이 제대로 되었는지 확인하세요.")
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
                   if (nzchar(last)) paste0("\n[직전 오류] ", trimws(last)) else "")
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