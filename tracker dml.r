# ══════════════════════════════════════════════════════════════════════════════
#  KAPAE 2026 워크숍 · 2일차 1강 실습 채점 스크립트
#  대상 문서: 2-1DML_sol.qmd (Double/Debiased Machine Learning)
#
#  문항 ID 목록 (qmd의 각 빈칸 블록 하단에 아래 순서대로 삽입)
#    dml-01-resid     Part 1 / Task 1-1  교차적합 없는 잔차화
#    dml-02-crossfit  Part 1 / Task 1-2  5겹 교차적합
#    dml-03-plr       Part 2 / Task 2-1  DoubleMLPLR 기본 적합
#    dml-04-learners  Part 2 / Task 2-2  보조모형 알고리즘 비교
#    dml-05-tune      Part 2 / Task 2-3  하이퍼파라미터 튜닝
#    dml-06-coefplot  Part 2 / Task 2-4  coefficient plot
#    dml-07-irm       Part 3 / Task 3-1  DoubleMLIRM 적합
#    dml-08-compare   Part 3 / Task 3-2  단순 평균 차이와 비교
#
#  ── 공통 오류 ───────────────────────────────────────────────────────────────
#     · 블록을 순서대로 실행하지 않아 앞 객체가 없음 → "객체를 찾을 수 없습니다"
#     · 빈칸 _____ 를 그대로 둔 채 실행 → 구문 오류
#     · DoubleMLData$new()에 tibble을 그대로 전달 → as.data.frame() 필요
#     · IRM의 ml_m에 regr 학습기를 지정 → 성향점수가 나오지 않음
#     · 패키지 미설치 (DoubleML, mlr3learners, ranger, xgboost)
# ══════════════════════════════════════════════════════════════════════════════

SUPABASE_URL <- "https://mztyhpckshnqcklogrsn.supabase.co"
SUPABASE_KEY <- "sb_publishable_pflU44StAqW5XTy94LsuJA_p_zMRtVv"   # publishable key

CHAPTER <- "d2-01"

# ── 내부 상태 ─────────────────────────────────────────────────────
.tracker_env <- new.env()
.tracker_env$session_id <- paste0(
  format(Sys.time(), "%Y%m%d%H%M%S"), "-",
  paste(sample(letters, 6, TRUE), collapse = "")
)

`%||%` <- function(a, b) if (is.null(a)) b else a

.has <- function(e, nm) !is.null(e[[nm]])

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
  if (missing(id) || !nzchar(id) || id %in% c("ID 입력", "학번입력", "abcd", "abc")) {
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

# R6/DoubleML 객체 속성을 안전하게 꺼내기
.get <- function(expr) tryCatch(expr, error = function(err) NULL)

# mlr3 학습기의 id (예: "regr.ranger")
.lrn_id <- function(x) {
  id <- .get(x$id)
  if (is.null(id)) "" else as.character(id)
}

# DoubleML 모형에 실제로 들어간 학습기 id
.model_lrn <- function(m, slot) {
  l <- .get(m$learner[[slot]])
  if (is.null(l)) "" else .lrn_id(l)
}

# 두 벡터의 상관 (실패 시 NA)
.cor <- function(a, b) {
  tryCatch(stats::cor(.num(a), .num(b)), error = function(err) NA_real_)
}


# ══════════════════════════════════════════════════════════════════
#  정답 규칙
#  need    : 학생이 만들어야 하는 객체
#  rules   : 순서대로 검사하며, 첫 실패의 msg가 힌트로 기록됨
#  caution : 정답이지만 함께 짚어야 할 조건부 안내
#  note    : 정답일 때 출력되는 결과 해석
# ══════════════════════════════════════════════════════════════════

CHECKS <- list(

# ─────────────────────────────────────────────────────────────────
# Task 1-1. 교차적합 없는 잔차화
# ─────────────────────────────────────────────────────────────────
"dml-01-resid" = list(
  need = c("sim_df", "tau_true", "rf_l", "rf_m",
           "y_res_in", "d_res_in", "tau_in"),
  call_from = NULL,
  note = "머신러닝으로 두 변수를 모두 잔차화했는데도 추정치가 참값 1.0에서 벗어납니다. 같은 관측치로 랜덤 포레스트를 학습하고 다시 그 관측치의 잔차를 계산했기 때문입니다. 모형이 각 관측치의 잡음까지 일부 외웠으므로 잔차가 실제보다 작아지고, 그 축소가 분자와 분모에 서로 다른 비율로 들어가 추정치가 한쪽으로 밀립니다. Neyman 직교성은 보조모형의 작은 추정오차에 대한 민감도를 낮추지만, 자기 관측치를 학습에 쓴 문제까지 없애지는 않습니다. 이 부분을 해결하는 것이 다음 Task의 교차적합입니다.",
  rules = list(

    # 두 보조모형
    list(f = function(e) inherits(e$rf_l, "ranger"),
         msg = "rf_l이 ranger 객체가 아닙니다. ranger()의 결과를 rf_l에 저장하세요."),

    list(f = function(e) inherits(e$rf_m, "ranger"),
         msg = "rf_m이 ranger 객체가 아닙니다. ranger()의 결과를 rf_m에 저장하세요."),

    list(f = function(e) isTRUE(e$rf_l$treetype == "Regression"),
         msg = "rf_l이 회귀 포레스트가 아닙니다. 결과변수 y가 연속형인지 확인하세요."),

    list(f = function(e) .eq(e$rf_l$num.trees, 300) && .eq(e$rf_m$num.trees, 300),
         msg = "num.trees가 300이 아닙니다. 두 보조모형 모두 300으로 지정합니다."),

    list(f = function(e) .eq(e$rf_l$min.node.size, 5) && .eq(e$rf_m$min.node.size, 5),
         msg = "min.node.size가 5가 아닙니다. 두 보조모형 모두 5로 지정합니다."),

    list(f = function(e) isTRUE(e$rf_l$num.samples == nrow(e$sim_df)),
         msg = "data 인수가 sim_df가 아닙니다. 이 Task에서는 전체 표본으로 학습합니다."),

    # 두 모형이 서로 다른 대상을 학습했는가
    list(f = function(e) .cor(e$sim_df$y - e$y_res_in, e$sim_df$y) > 0.4,
         msg = "y_res_in이 y의 잔차로 보이지 않습니다. rf_l의 예측을 sim_df$y에서 빼야 합니다."),

    list(f = function(e) .cor(e$sim_df$d - e$d_res_in, e$sim_df$d) > 0.4,
         msg = "d_res_in이 d의 잔차로 보이지 않습니다. rf_m의 예측을 sim_df$d에서 빼야 합니다. 두 모형을 바꿔 넣지 않았는지 확인하세요."),

    # 잔차 형태
    list(f = function(e) length(e$y_res_in) == nrow(e$sim_df) &&
                         length(e$d_res_in) == nrow(e$sim_df),
         msg = "잔차의 길이가 표본 수와 다릅니다. predict()의 data 인수에 sim_df 전체를 넣으세요."),

    list(f = function(e) all(is.finite(.num(e$y_res_in))) &&
                         all(is.finite(.num(e$d_res_in))),
         msg = "잔차에 결측이나 무한값이 있습니다. $predictions를 꺼냈는지 확인하세요."),

    list(f = function(e) abs(mean(.num(e$y_res_in))) < 0.5,
         msg = "y 잔차의 평균이 0에서 크게 벗어났습니다. 실제값에서 예측값을 뺐는지 순서를 확인하세요."),

    # 잔차 회귀
    list(f = function(e) .scalar(e$tau_in),
         msg = "tau_in이 숫자 하나가 아닙니다."),

    list(f = function(e) !.eq(e$tau_in,
             sum(.num(e$d_res_in) * .num(e$y_res_in)) / sum(.num(e$y_res_in)^2)),
         msg = "분모가 잘못되었습니다. 처치 잔차의 제곱합으로 나눕니다. sum(d_res_in^2)입니다."),

    list(f = function(e) .eq(e$tau_in,
             sum(.num(e$d_res_in) * .num(e$y_res_in)) / sum(.num(e$d_res_in)^2)),
         msg = "tau_in 계산식을 확인하세요. sum(d_res_in * y_res_in) / sum(d_res_in^2)입니다."),

    list(f = function(e) is.finite(e$tau_in) && abs(e$tau_in) < 10,
         msg = "추정치가 비정상적으로 큽니다. 데이터 준비 블록부터 다시 실행해 보세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 1-2. 5겹 교차적합
# ─────────────────────────────────────────────────────────────────
"dml-02-crossfit" = list(
  need = c("sim_df", "tau_true", "K", "fold",
           "y_res", "d_res", "tau_manual", "u_res", "se_manual"),
  call_from = NULL,
  note = "바뀐 것은 학습에 쓰는 관측치를 잔차를 계산하는 관측치에서 제외한 것 하나뿐인데 추정치가 참값 쪽으로 이동합니다. 각 관측치의 잔차가 그 관측치를 보지 않은 모형에서 나오므로 자기 잡음을 외운 몫이 사라진 것입니다. 교차적합 잔차의 분산이 앞 Task보다 크게 나오는데, 이것이 축소가 없어졌다는 신호입니다. se_manual은 각 관측치가 최종 추정치에 얼마나 기여하는지를 이용해 계산한 영향함수 기반 표준오차입니다. 신뢰구간이 참값을 포함하는지 확인해 보세요. Part 2부터는 이 과정을 패키지가 대신합니다.",
  rules = list(

    list(f = function(e) .eq(e$K, 5),
         msg = "K는 5입니다. 5겹 교차적합을 수행합니다."),

    list(f = function(e) length(e$fold) == nrow(e$sim_df),
         msg = "fold의 길이가 표본 수와 다릅니다. rep(1:K, length.out = nrow(sim_df))로 만드세요."),

    list(f = function(e) setequal(unique(.num(e$fold)), 1:e$K),
         msg = "fold에 1부터 K까지의 번호가 모두 들어 있지 않습니다."),

    list(f = function(e) {
           tb <- table(e$fold)
           max(tb) - min(tb) <= 1
         },
         msg = "겹의 크기가 고르지 않습니다. rep()으로 만든 뒤 sample()로 섞으세요."),

    # 잔차
    list(f = function(e) length(e$y_res) == nrow(e$sim_df) &&
                         length(e$d_res) == nrow(e$sim_df),
         msg = "잔차 벡터의 길이가 표본 수와 다릅니다. numeric(nrow(sim_df))로 만들어 두고 겹마다 채우세요."),

    list(f = function(e) !any(.num(e$y_res) == 0) || sum(.num(e$y_res) == 0) < 5,
         msg = "잔차에 0이 그대로 남아 있습니다. 반복문이 모든 겹을 채우지 못했습니다. y_res[test_id] 형태로 저장했는지 확인하세요."),

    list(f = function(e) all(is.finite(.num(e$y_res))) && all(is.finite(.num(e$d_res))),
         msg = "잔차에 결측이나 무한값이 있습니다."),

    # 교차적합이 실제로 일어났는가
    list(f = function(e) {
           if (!.has(e, "d_res_in")) return(TRUE)
           stats::var(.num(e$d_res)) > stats::var(.num(e$d_res_in))
         },
         msg = "교차적합 잔차가 Task 1-1의 잔차보다 작습니다. train_id와 test_id를 바꿔 쓰지 않았는지 확인하세요. 학습은 fold != k, 예측은 fold == k입니다."),

    list(f = function(e) {
           if (!.has(e, "y_res_in")) return(TRUE)
           !.eq(e$y_res, e$y_res_in, tol = 1e-8)
         },
         msg = "잔차가 Task 1-1과 동일합니다. 반복문 안에서 겹마다 새로 학습했는지 확인하세요."),

    # 추정
    list(f = function(e) .scalar(e$tau_manual),
         msg = "tau_manual이 숫자 하나가 아닙니다."),

    list(f = function(e) .eq(e$tau_manual,
             sum(.num(e$d_res) * .num(e$y_res)) / sum(.num(e$d_res)^2)),
         msg = "tau_manual 계산식을 확인하세요. sum(d_res * y_res) / sum(d_res^2)입니다."),

    # 표준오차
    list(f = function(e) .eq(e$u_res, .num(e$y_res) - e$tau_manual * .num(e$d_res)),
         msg = "u_res는 y_res에서 tau_manual과 d_res의 곱을 뺀 값입니다."),

    list(f = function(e) .scalar(e$se_manual) && e$se_manual > 0,
         msg = "se_manual이 양수가 아닙니다."),

    list(f = function(e) .eq(e$se_manual,
             sqrt(mean(.num(e$d_res)^2 * .num(e$u_res)^2)) /
               (mean(.num(e$d_res)^2) * sqrt(nrow(e$sim_df))),
             tol = 1e-6),
         msg = "se_manual 계산식을 확인하세요. 분자는 sqrt(mean(d_res^2 * u_res^2)), 분모는 mean(d_res^2) * sqrt(n)입니다."),

    list(f = function(e) e$se_manual < 1,
         msg = "표준오차가 지나치게 큽니다. 분모에 sqrt(nrow(sim_df))를 넣었는지 확인하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 2-1. DoubleMLPLR 기본 적합
# ─────────────────────────────────────────────────────────────────
"dml-03-plr" = list(
  need = c("sim_df", "x_cols", "tau_true",
           "dml_data", "ml_l_rf", "ml_m_rf", "dml_plr_rf"),
  call_from = NULL,
  note = "Estimate가 처치효과 추정치이고 Std. Error가 영향함수 기반 표준오차입니다. 95% 신뢰구간이 참값 1.0을 포함하는지 확인해 보세요. 패키지가 내부에서 한 계산은 Part 1과 같습니다. 겹마다 두 보조모형을 학습하고, 학습에 쓰지 않은 관측치에서 잔차를 만들고, 그 잔차끼리 회귀했습니다. 달라진 것은 이 과정을 직접 쓰지 않았다는 점뿐입니다. score를 partialling out으로 지정한 것이 잔차화 방식을 고른 부분인데, 다른 선택지를 쓰면 같은 데이터에서도 다른 추정 대상을 겨냥하게 됩니다.",
  rules = list(

    # 데이터 객체
    list(f = function(e) inherits(e$dml_data, "DoubleMLData"),
         msg = "dml_data가 DoubleMLData 객체가 아닙니다. DoubleMLData$new()의 결과를 저장하세요."),

    list(f = function(e) {
           yc <- .get(e$dml_data$y_col)
           is.null(yc) || identical(as.character(yc), "y")
         },
         msg = "y_col이 y가 아닙니다."),

    list(f = function(e) {
           dc <- .get(e$dml_data$d_cols)
           is.null(dc) || identical(as.character(dc), "d")
         },
         msg = "d_cols가 d가 아닙니다."),

    list(f = function(e) {
           xc <- .get(e$dml_data$x_cols)
           is.null(xc) || setequal(as.character(xc), e$x_cols)
         },
         msg = "x_cols가 x1부터 x5까지가 아닙니다. 미리 만들어 둔 x_cols를 그대로 넣으세요."),

    list(f = function(e) {
           n <- .get(e$dml_data$n_obs)
           is.null(n) || isTRUE(n == nrow(e$sim_df))
         },
         msg = "관측치 수가 sim_df와 다릅니다. 데이터를 걸러내지 말고 그대로 넣으세요."),

    # 학습기
    list(f = function(e) grepl("^regr\\.", .lrn_id(e$ml_l_rf)),
         msg = "ml_l_rf가 회귀 학습기가 아닙니다. PLR에서는 두 보조모형 모두 regr 계열입니다."),

    list(f = function(e) grepl("ranger", .lrn_id(e$ml_l_rf)) &&
                         grepl("ranger", .lrn_id(e$ml_m_rf)),
         msg = "학습기가 regr.ranger가 아닙니다. lrn(\"regr.ranger\", ...)로 만드세요."),

    list(f = function(e) {
           p <- .get(e$ml_l_rf$param_set$values$num.trees)
           is.null(p) || isTRUE(p == 500)
         },
         msg = "num.trees가 500이 아닙니다. Task 1-1의 300과 다르니 주의하세요."),

    list(f = function(e) {
           p <- .get(e$ml_l_rf$param_set$values$min.node.size)
           is.null(p) || isTRUE(p == 5)
         },
         msg = "min.node.size가 5가 아닙니다."),

    # 모형
    list(f = function(e) inherits(e$dml_plr_rf, "DoubleMLPLR"),
         msg = "dml_plr_rf가 DoubleMLPLR 객체가 아닙니다. DoubleMLIRM이나 다른 클래스를 쓰지 않았는지 확인하세요."),

    list(f = function(e) {
           n <- .get(e$dml_plr_rf$n_folds)
           is.null(n) || isTRUE(n == 5)
         },
         msg = "n_folds가 5가 아닙니다."),

    list(f = function(e) {
           sc <- .get(e$dml_plr_rf$score)
           is.null(sc) || grepl("partialling", as.character(sc)[1])
         },
         msg = "score가 partialling out이 아닙니다. 잔차화 방식을 지정하는 인수입니다."),

    # 적합 여부
    list(f = function(e) {
           cf <- .get(e$dml_plr_rf$coef)
           !is.null(cf) && length(cf) >= 1 && is.finite(cf[1])
         },
         msg = "추정 결과가 없습니다. $fit()을 실행했는지 확인하세요."),

    list(f = function(e) {
           se <- .get(e$dml_plr_rf$se)
           !is.null(se) && is.finite(se[1]) && se[1] > 0
         },
         msg = "표준오차가 계산되지 않았습니다."),

    list(f = function(e) abs(.get(e$dml_plr_rf$coef)[1] - e$tau_true) < 0.6,
         msg = "추정치가 참값에서 크게 벗어났습니다. y_col과 d_cols를 바꿔 넣지 않았는지 확인하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 2-2. 보조모형 알고리즘 비교
# ─────────────────────────────────────────────────────────────────
"dml-04-learners" = list(
  need = c("dml_data", "dml_plr_rf", "fit_plr", "dml_plr_lm", "dml_plr_xgb"),
  call_from = NULL,
  note = "선형 보조모형의 nuisance loss가 가장 크게 나옵니다. 이 데이터는 공변량이 처치와 결과에 곡선으로 작용하도록 만들었으므로 직선으로는 잡아낼 수 없기 때문입니다. 그 결과 잔차에 공변량의 흔적이 남고, 남은 부분이 처치효과 추정치를 오염시킵니다. 여기서 주의할 점은 선택 기준입니다. 추정치가 참값에 가까운 모형을 고르면 안 됩니다. 참값은 시뮬레이션이라 알고 있을 뿐이고 실제 분석에서는 볼 수 없습니다. 판단은 nuisance function의 out-of-sample 예측 성능으로 합니다. 세 모형의 loss와 추정치를 나란히 놓고 두 순서가 일치하는지 확인해 보세요.",
  rules = list(

    list(f = function(e) is.function(e$fit_plr),
         msg = "fit_plr이 함수가 아닙니다. function(ml_l, ml_m, seed = 123) 형태로 정의하세요."),

    list(f = function(e) all(c("ml_l", "ml_m") %in% names(formals(e$fit_plr))),
         msg = "fit_plr의 인수 이름이 ml_l과 ml_m이 아닙니다."),

    list(f = function(e) inherits(e$dml_plr_lm, "DoubleMLPLR"),
         msg = "dml_plr_lm이 DoubleMLPLR 객체가 아닙니다."),

    list(f = function(e) inherits(e$dml_plr_xgb, "DoubleMLPLR"),
         msg = "dml_plr_xgb가 DoubleMLPLR 객체가 아닙니다."),

    # 학습기가 실제로 바뀌었는가
    list(f = function(e) grepl("lm", .model_lrn(e$dml_plr_lm, "ml_l")),
         msg = "dml_plr_lm에 선형 학습기가 들어가지 않았습니다. lrn(\"regr.lm\")을 두 자리 모두에 넣으세요."),

    list(f = function(e) grepl("xgboost", .model_lrn(e$dml_plr_xgb, "ml_l")),
         msg = "dml_plr_xgb에 xgboost 학습기가 들어가지 않았습니다."),

    list(f = function(e) grepl("xgboost", .model_lrn(e$dml_plr_xgb, "ml_m")),
         msg = "dml_plr_xgb의 처치 보조모형이 xgboost가 아닙니다. ml_l과 ml_m 두 자리 모두에 넣어야 합니다."),

    # xgboost 설정
    list(f = function(e) {
           p <- .get(e$dml_plr_xgb$learner$ml_l$param_set$values$nrounds)
           is.null(p) || isTRUE(p == 300)
         },
         msg = "xgboost의 nrounds가 300이 아닙니다."),

    list(f = function(e) {
           p <- .get(e$dml_plr_xgb$learner$ml_l$param_set$values$max_depth)
           is.null(p) || isTRUE(p == 4)
         },
         msg = "xgboost의 max_depth가 4가 아닙니다."),

    # 적합 여부
    list(f = function(e) {
           a <- .get(e$dml_plr_lm$coef); b <- .get(e$dml_plr_xgb$coef)
           !is.null(a) && !is.null(b) && is.finite(a[1]) && is.finite(b[1])
         },
         msg = "두 모형 중 적합되지 않은 것이 있습니다. fit_plr() 안에서 $fit()을 호출했는지 확인하세요."),

    # 세 모형이 서로 다른 결과인가
    list(f = function(e) !.eq(.get(e$dml_plr_lm$coef)[1],
                              .get(e$dml_plr_xgb$coef)[1], tol = 1e-8),
         msg = "두 추정치가 완전히 같습니다. 학습기를 바꿔 넘겼는지 확인하세요."),

    list(f = function(e) {
           ev <- .get(e$dml_plr_lm$evaluate_learners())
           !is.null(ev)
         },
         msg = "evaluate_learners()가 실행되지 않습니다. $fit(store_predictions = TRUE)로 적합해야 예측이 저장됩니다.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 2-3. 하이퍼파라미터 튜닝
# ─────────────────────────────────────────────────────────────────
"dml-05-tune" = list(
  need = c("dml_data", "dml_plr_tuned", "param_set", "tune_settings"),
  call_from = NULL,
  note = "ml_l과 ml_m에 서로 다른 값이 선택되는 경우가 많습니다. 두 보조함수가 서로 다른 예측문제이기 때문입니다. 결과를 y로 예측하는 일과 처치를 예측하는 일은 난이도도 최적 설정도 같지 않습니다. 여기서 겹이 두 종류라는 점을 구분해 두시면 좋습니다. n_folds는 처치효과 추정을 위한 out-of-fold 잔차를 만드는 외부 겹이고, rsmp에 지정한 3겹은 하이퍼파라미터를 고르기 위한 내부 겹입니다. 목적이 다르므로 값도 따로 정합니다. 튜닝 후 nuisance loss가 줄었는지, 그리고 그 감소가 처치효과 추정치와 표준오차에 어떤 차이를 만들었는지 함께 보세요. 예측이 좋아졌다고 추정치가 반드시 참값에 가까워지는 것은 아닙니다.",
  rules = list(

    list(f = function(e) inherits(e$dml_plr_tuned, "DoubleMLPLR"),
         msg = "dml_plr_tuned가 DoubleMLPLR 객체가 아닙니다."),

    # 탐색 범위
    list(f = function(e) is.list(e$param_set) &&
                         all(c("ml_l", "ml_m") %in% names(e$param_set)),
         msg = "param_set에 ml_l과 ml_m 두 항목이 있어야 합니다. 각 보조모형의 탐색 범위를 따로 지정합니다."),

    list(f = function(e) {
           ids <- .get(e$param_set$ml_l$ids())
           !is.null(ids) && all(c("mtry", "min.node.size", "max.depth") %in% ids)
         },
         msg = "탐색할 하이퍼파라미터가 부족합니다. mtry, min.node.size, max.depth 세 개를 넣으세요."),

    list(f = function(e) {
           lo <- .get(e$param_set$ml_l$params$mtry$lower)
           up <- .get(e$param_set$ml_l$params$mtry$upper)
           is.null(up) || isTRUE(up <= 5)
         },
         msg = "mtry의 상한이 공변량 개수를 넘습니다. 이 데이터의 공변량은 5개이므로 상한도 5입니다."),

    # 튜닝 설정
    list(f = function(e) is.list(e$tune_settings) &&
                         all(c("terminator", "algorithm", "rsmp_tune", "measure")
                             %in% names(e$tune_settings)),
         msg = "tune_settings에 terminator, algorithm, rsmp_tune, measure 네 항목이 있어야 합니다."),

    list(f = function(e) {
           a <- .get(e$tune_settings$algorithm$id)
           is.null(a) || grepl("random", tolower(a))
         },
         msg = "탐색 알고리즘이 random search가 아닙니다. tnr(\"random_search\")로 지정하세요."),

    list(f = function(e) {
           m <- .get(e$tune_settings$measure$ml_l$id)
           is.null(m) || grepl("mse", tolower(m))
         },
         msg = "평가지표가 regr.mse가 아닙니다. 회귀 보조모형이므로 평균제곱오차를 씁니다."),

    # 튜닝 실행
    list(f = function(e) {
           p <- .get(e$dml_plr_tuned$params)
           !is.null(p)
         },
         msg = "튜닝 결과가 없습니다. $tune()을 실행했는지 확인하세요."),

    list(f = function(e) {
           cf <- .get(e$dml_plr_tuned$coef)
           !is.null(cf) && is.finite(cf[1])
         },
         msg = "추정 결과가 없습니다. $tune() 뒤에 $fit()도 실행해야 합니다."),

    list(f = function(e) {
           se <- .get(e$dml_plr_tuned$se)
           !is.null(se) && is.finite(se[1]) && se[1] > 0
         },
         msg = "표준오차가 계산되지 않았습니다."),

    # 기본 모형과 다른 결과인가
    list(f = function(e) {
           if (!.has(e, "dml_plr_rf")) return(TRUE)
           !.eq(.get(e$dml_plr_tuned$coef)[1], .get(e$dml_plr_rf$coef)[1], tol = 1e-10)
         },
         msg = "튜닝 전후 추정치가 완전히 같습니다. 튜닝된 학습기가 적합에 반영되었는지 확인하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 2-4. coefficient plot
# ─────────────────────────────────────────────────────────────────
"dml-06-coefplot" = list(
  need = c("tau_true", "get_dml_result", "coef_df",
           "dml_plr_lm", "dml_plr_rf", "dml_plr_xgb", "dml_plr_tuned"),
  call_from = NULL,
  note = "점은 추정치, 가로선은 95% 신뢰구간, 세로 점선은 참값입니다. 네 신뢰구간이 대체로 겹친다면 이 데이터에서 알고리즘 선택이 결론을 바꿀 만큼은 아니라는 뜻입니다. 다만 선형 보조모형의 점이 다른 셋과 떨어져 있다면 그것은 nuisance function을 직선으로 근사한 대가입니다. 신뢰구간의 길이도 함께 보세요. 추정치가 참값에 가깝더라도 구간이 넓으면 그 정확성은 우연일 수 있습니다. 논문에서 DML 결과를 보고할 때 보조모형을 바꿔 가며 추정치가 안정적인지 보이는 것이 관례이고, 이 그림이 그 형태입니다.",
  rules = list(

    list(f = function(e) is.function(e$get_dml_result),
         msg = "get_dml_result가 함수가 아닙니다."),

    list(f = function(e) is.data.frame(e$coef_df),
         msg = "coef_df가 데이터프레임이 아닙니다. bind_rows()의 결과를 저장하세요."),

    list(f = function(e) nrow(e$coef_df) == 4,
         msg = "coef_df의 행이 4개가 아닙니다. 네 모형의 결과를 모두 넣으세요."),

    list(f = function(e) all(c("model", "estimate", "se", "conf_low", "conf_high")
                             %in% names(e$coef_df)),
         msg = "coef_df에 model, estimate, se, conf_low, conf_high 다섯 열이 있어야 합니다."),

    list(f = function(e) all(is.finite(.num(e$coef_df$estimate))),
         msg = "추정치에 결측이 있습니다. 네 모형이 모두 적합되었는지 확인하세요."),

    list(f = function(e) all(.num(e$coef_df$conf_low) < .num(e$coef_df$conf_high)),
         msg = "conf_low가 conf_high보다 큽니다. confint()의 열 순서가 뒤바뀌었습니다. ci[1, 1]이 하한, ci[1, 2]가 상한입니다."),

    list(f = function(e) all(.num(e$coef_df$conf_low) < .num(e$coef_df$estimate)) &&
                         all(.num(e$coef_df$estimate) < .num(e$coef_df$conf_high)),
         msg = "신뢰구간이 추정치를 포함하지 않습니다. estimate에 coef를, se에 se를 넣었는지 확인하세요."),

    list(f = function(e) {
           # 네 모형이 모두 들어갔는가 (tuned 누락 방지)
           ms <- as.character(e$coef_df$model)
           length(unique(ms)) == 4
         },
         msg = "coef_df의 model 값이 4종류가 아닙니다. 같은 모형을 두 번 넣지 않았는지 확인하세요."),

    list(f = function(e) {
           if (!.has(e, "dml_plr_tuned")) return(TRUE)
           est <- .num(e$coef_df$estimate)
           any(vapply(est, function(v) .eq(v, .get(e$dml_plr_tuned$coef)[1]),
                      logical(1)))
         },
         msg = "튜닝한 모형의 추정치가 coef_df에 없습니다. 네 번째 행에 dml_plr_tuned를 넣으세요."),

    list(f = function(e) length(unique(round(.num(e$coef_df$estimate), 8))) >= 3,
         msg = "같은 모형을 여러 번 넣은 것으로 보입니다. 네 모형이 서로 다른지 확인하세요."),

    list(f = function(e) all(.num(e$coef_df$se) > 0),
         msg = "표준오차가 0 이하인 행이 있습니다.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 3-1. DoubleMLIRM 적합
# ─────────────────────────────────────────────────────────────────
"dml-07-irm" = list(
  need = c("ohie_df", "ohie_x", "dml_ohie", "ml_g_irm", "ml_m_irm", "dml_irm"),
  call_from = NULL,
  note = "IRM에서 두 보조모형의 역할이 PLR보다 뚜렷하게 나뉩니다. ml_g는 처치집단과 통제집단 각각의 결과함수를 학습하고, ml_m은 성향점수를 학습합니다. ml_m에 분류기를 쓰고 predict_type을 prob로 지정한 이유가 여기 있습니다. 0과 1 중 하나로 예측하면 확률이 나오지 않아 가중치를 만들 수 없습니다. trimming_threshold는 성향점수가 0이나 1에 지나치게 가까운 관측치의 영향을 제한합니다. 그 영역에서는 비교할 대상이 사실상 없는데 역수를 취하면 가중치가 폭발하기 때문입니다. 추정치는 ATE이며, PLR처럼 하나의 잔차회귀 계수를 겨냥하는 대신 두 결과함수와 성향점수를 결합한 score의 평균을 씁니다. 처치효과가 개인마다 다를 때 두 방법이 겨냥하는 값이 갈리는 지점입니다.",
  rules = list(

    # 데이터
    list(f = function(e) inherits(e$dml_ohie, "DoubleMLData"),
         msg = "dml_ohie가 DoubleMLData 객체가 아닙니다."),

    list(f = function(e) {
           dc <- .get(e$dml_ohie$d_cols)
           is.null(dc) || identical(as.character(dc), "d")
         },
         msg = "d_cols가 d가 아닙니다. 보험 가입 여부가 처치변수입니다."),

    list(f = function(e) {
           yc <- .get(e$dml_ohie$y_col)
           is.null(yc) || identical(as.character(yc), "y")
         },
         msg = "y_col이 y가 아닙니다. 의사 방문 횟수가 결과변수입니다."),

    list(f = function(e) {
           xc <- .get(e$dml_ohie$x_cols)
           is.null(xc) || setequal(as.character(xc), e$ohie_x)
         },
         msg = "x_cols가 ohie_x와 다릅니다. 미리 만들어 둔 ohie_x를 그대로 넣으세요."),

    list(f = function(e) all(.num(e$ohie_df$d) %in% c(0, 1)),
         msg = "처치변수가 0/1로 코딩되어 있지 않습니다. 데이터 준비 블록을 다시 실행하세요."),

    # 학습기
    list(f = function(e) grepl("^regr\\.", .lrn_id(e$ml_g_irm)),
         msg = "ml_g_irm이 회귀 학습기가 아닙니다. 결과함수는 연속형 결과를 예측하므로 regr 계열입니다."),

    list(f = function(e) grepl("^classif\\.", .lrn_id(e$ml_m_irm)),
         msg = "ml_m_irm이 분류 학습기가 아닙니다. 성향점수를 추정해야 하므로 classif.ranger를 씁니다. PLR과 달라지는 지점입니다."),

    list(f = function(e) {
           pt <- .get(e$ml_m_irm$predict_type)
           !is.null(pt) && pt == "prob"
         },
         msg = "predict_type이 prob이 아닙니다. 이것을 빠뜨리면 처치 여부를 0 또는 1로 예측해 성향점수가 나오지 않습니다."),

    list(f = function(e) {
           p <- .get(e$ml_g_irm$param_set$values$num.trees)
           is.null(p) || isTRUE(p == 500)
         },
         msg = "num.trees가 500이 아닙니다."),

    # 모형
    list(f = function(e) inherits(e$dml_irm, "DoubleMLIRM"),
         msg = "dml_irm이 DoubleMLIRM 객체가 아닙니다. 이항 처치이므로 PLR이 아니라 IRM을 씁니다."),

    list(f = function(e) {
           n <- .get(e$dml_irm$n_folds)
           is.null(n) || isTRUE(n == 5)
         },
         msg = "n_folds가 5가 아닙니다."),

    list(f = function(e) {
           tt <- .get(e$dml_irm$trimming_threshold)
           is.null(tt) || isTRUE(tt == 0.01)
         },
         msg = "trimming_threshold가 0.01이 아닙니다. 극단적인 성향점수의 영향을 제한하는 값입니다."),

    # 적합
    list(f = function(e) {
           cf <- .get(e$dml_irm$coef)
           !is.null(cf) && is.finite(cf[1])
         },
         msg = "추정 결과가 없습니다. $fit()을 실행했는지 확인하세요."),

    list(f = function(e) {
           se <- .get(e$dml_irm$se)
           !is.null(se) && is.finite(se[1]) && se[1] > 0
         },
         msg = "표준오차가 계산되지 않았습니다."),

    list(f = function(e) abs(.get(e$dml_irm$coef)[1]) < 20,
         msg = "추정치가 비정상적으로 큽니다. 성향점수가 0이나 1에 몰려 있지 않은지 확인하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 3-2. 단순 평균 차이와 비교
# ─────────────────────────────────────────────────────────────────
"dml-08-compare" = list(
  need = c("ohie_df", "dml_irm", "naive_irm", "irm_ci", "irm_compare"),
  call_from = NULL,
  note = "두 값의 차이가 공변량 조정이 만들어 낸 몫입니다. 보험에 가입한 사람과 그렇지 않은 사람은 연령, 소득, 교육 수준이 애초에 다르므로 단순 차이에는 그 차이가 함께 섞여 있습니다. IRM은 두 결과함수와 성향점수를 학습해 그 부분을 걷어냅니다. 다만 걷어낸 것은 모형에 넣은 공변량이 설명하는 부분뿐입니다. 관측되지 않은 교란이 남아 있으면 IRM도 그것을 다루지 못하고, 유연한 학습기를 썼다는 사실이 조건부 교환가능성을 만들어 주지도 않습니다. 이 실습에서 보험 가입은 무작위 배정이 아니므로 여기서 얻은 값을 인과효과로 읽으려면 어떤 가정이 필요한지 먼저 따져야 합니다. 설계가 먼저이고 머신러닝은 그 안에서 공변량을 조정하는 자리에 들어간다는 점이 이번 세션의 요지입니다.",
  rules = list(

    list(f = function(e) .scalar(e$naive_irm),
         msg = "naive_irm이 숫자 하나가 아닙니다."),

    list(f = function(e) .eq(e$naive_irm,
             mean(.num(e$ohie_df$y)[.num(e$ohie_df$d) == 1]) -
             mean(.num(e$ohie_df$y)[.num(e$ohie_df$d) == 0])),
         msg = "naive_irm 계산식을 확인하세요. 처치군 평균에서 통제군 평균을 뺍니다. 순서가 바뀌면 부호가 반대가 됩니다."),

    list(f = function(e) is.data.frame(e$irm_ci) || is.matrix(e$irm_ci),
         msg = "irm_ci가 표 형태가 아닙니다. dml_irm$confint()의 결과를 저장하세요."),

    list(f = function(e) is.data.frame(e$irm_compare),
         msg = "irm_compare가 데이터프레임이 아닙니다."),

    list(f = function(e) nrow(e$irm_compare) == 2,
         msg = "irm_compare의 행이 2개가 아닙니다. 단순 평균 차이와 IRM 두 행을 넣습니다."),

    list(f = function(e) all(c("method", "estimate") %in% names(e$irm_compare)),
         msg = "irm_compare에 method와 estimate 열이 있어야 합니다."),

    list(f = function(e) {
           est <- .num(e$irm_compare$estimate)
           any(vapply(est, function(v) .eq(v, e$naive_irm), logical(1)))
         },
         msg = "irm_compare의 estimate에 naive_irm이 들어 있지 않습니다."),

    list(f = function(e) {
           est <- .num(e$irm_compare$estimate)
           any(vapply(est, function(v) .eq(v, .get(e$dml_irm$coef)[1]), logical(1)))
         },
         msg = "irm_compare에 IRM 추정치가 들어 있지 않습니다. dml_irm$coef에서 꺼내세요."),

    list(f = function(e) sum(is.na(.num(e$irm_compare$conf_low))) == 1,
         msg = "단순 평균 차이 행의 신뢰구간은 NA로 둡니다. 여기서는 점추정치만 비교합니다."),

    list(f = function(e) {
           lo <- .num(e$irm_compare$conf_low)
           hi <- .num(e$irm_compare$conf_high)
           i <- which(!is.na(lo))
           length(i) == 1 && lo[i] < hi[i]
         },
         msg = "conf_low가 conf_high보다 큽니다. irm_ci의 열 순서를 확인하세요. 첫 열이 하한입니다."),

    list(f = function(e) {
           lo <- .num(e$irm_compare$conf_low)
           hi <- .num(e$irm_compare$conf_high)
           i <- which(!is.na(lo))
           est <- .num(e$irm_compare$estimate)[i]
           lo[i] < est && est < hi[i]
         },
         msg = "IRM 행의 신뢰구간이 추정치를 포함하지 않습니다. coef와 confint를 같은 모형에서 꺼냈는지 확인하세요.")
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
