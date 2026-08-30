# KMU 2026 — paper-style educational mock results
# This file is sourced AFTER lab_helpers.R and intentionally overrides selected
# reporting/simulation functions without changing the student-facing input schema.

if (requireNamespace("knitr", quietly = TRUE)) {
  knitr::opts_chunk$set(echo = FALSE)
}

fmt2 <- function(x) sprintf("%.2f", x)
fmt1 <- function(x) sprintf("%.1f", x)
pct1 <- function(x) sprintf("%.1f%%", 100 * x)

ci_text <- function(est, lo, hi, digits = 2) {
  f <- if (digits == 1) fmt1 else fmt2
  paste0(f(est), " (95% CI ", f(lo), ", ", f(hi), ")")
}

sig_text <- function(lo, hi) {
  if (is.finite(lo) && is.finite(hi) && (lo > 0 || hi < 0)) {
    "95% 신뢰구간이 0을 포함하지 않았습니다."
  } else {
    "95% 신뢰구간이 0을 포함했습니다."
  }
}

method_short_label <- function(method) {
  c(
    prediction = "머신러닝 예측",
    dml = "DML",
    did = "DID",
    iv = "IV",
    matching = "성향점수 조정",
    causal_forest = "인과 포리스트",
    rd = "RD/RKD"
  )[[method]]
}

internal_covariate_labels <- function(spec) {
  out <- character()

  add_group <- function(prefix, labels) {
    if (!length(labels)) return(character())
    setNames(labels, paste0(prefix, seq_along(labels)))
  }

  types <- spec$covariate_types %||% character()
  labs <- spec$covariate_labels %||% spec$covariate_names %||% character()

  cont <- labs[types == "continuous"]
  bin  <- labs[types == "binary"]
  cat  <- labs[types == "categorical"]

  c(
    add_group("x_cont_", cont),
    add_group("x_bin_", bin),
    add_group("x_cat_", cat)
  )
}

label_for_internal <- function(v, spec) {
  mp <- internal_covariate_labels(spec)
  if (v %in% names(mp)) unname(mp[[v]]) else v
}

project_overview_text <- function(spec) {
  paste0(
    "## 제안 연구\n\n",
    "**연구 제목.** ", spec$research_title, "  \n",
    "**연구방법.** ", method_label(spec$method), "  \n",
    "**데이터.** ", spec$data_name, "  \n",
    if (nzchar(spec$unit_of_observation %||% "")) {
      paste0("**관측단위.** ", spec$unit_of_observation, "  \n")
    } else "",
    "\n",
    "### 연구 배경\n\n",
    if (nzchar(spec$theoretical_background %||% "")) {
      paste0("**이론적·정책적 배경.** ", trimws(spec$theoretical_background), "\n\n")
    } else "",
    if (nzchar(spec$research_need %||% "")) {
      paste0("**연구의 필요성.** ", trimws(spec$research_need), "\n\n")
    } else "",
    if (nzchar(spec$research_question %||% "")) {
      paste0("**연구질문.** ", trimws(spec$research_question), "\n\n")
    } else "",
    if (nzchar(spec$expected_contribution %||% "")) {
      paste0("**예상 기여.** ", trimws(spec$expected_contribution), "\n\n")
    } else ""
  )
}

data_structure_text <- function(spec) {
  cov_text <- if (length(spec$covariate_labels %||% character())) {
    paste(spec$covariate_labels, collapse = ", ")
  } else {
    "별도 공변량 없음"
  }

  specific <- switch(
    spec$method,
    prediction = paste0(
      "**예측대상.** ", spec$outcome_label, " (",
      ifelse(spec$outcome_type == "binary", "이진형", "연속형"), ")  \n",
      "**후보 예측변수.** ", cov_text, "  \n",
      "**표본규모.** 약 ", format(spec$n, big.mark = ","), "개 관측치  \n",
      "**독립 test/hold-out 자료.** ", ifelse(spec$has_independent_test, "있음", "없음")
    ),
    dml = paste0(
      "**결과변수.** ", spec$outcome_label, "  \n",
      "**처치/노출.** ", spec$treatment_label, "  \n",
      "**교란변수.** ", cov_text, "  \n",
      "**표본규모.** 약 ", format(spec$n, big.mark = ","), "개 관측치"
    ),
    did = paste0(
      "**결과변수.** ", spec$outcome_label, "  \n",
      "**분석단위.** ", spec$unit_id_label, "  \n",
      "**시간변수.** ", spec$time_label, "  \n",
      "**처치집단.** ", spec$treated_definition, "  \n",
      "**비교집단.** ", spec$control_definition, "  \n",
      "**정책 도입시점.** ", spec$treatment_start_label,
      if (identical(spec$did_design, "staggered")) " (단위별 도입시점이 다른 staggered 설계)" else ""
    ),
    iv = paste0(
      "**결과변수.** ", spec$outcome_label, "  \n",
      "**처치/노출.** ", spec$treatment_label, "  \n",
      "**도구변수.** ", spec$instrument_label, "  \n",
      "**사전 공변량.** ", cov_text, "  \n",
      "**표본규모.** 약 ", format(spec$n, big.mark = ","), "개 관측치"
    ),
    matching = paste0(
      "**결과변수.** ", spec$outcome_label, "  \n",
      "**처치/노출.** ", spec$treatment_label, "  \n",
      "**조정변수.** ", cov_text, "  \n",
      "**조정방식.** ", ifelse(spec$ps_method == "matching", "성향점수 매칭", "성향점수 가중치"), "  \n",
      "**목표 효과.** ", spec$estimand
    ),
    causal_forest = paste0(
      "**결과변수.** ", spec$outcome_label, "  \n",
      "**처치/노출.** ", spec$treatment_label, "  \n",
      "**이질성 학습 공변량.** ", cov_text, "  \n",
      "**사전 관심 효과수정변수.** ",
      paste(spec$modifier_labels %||% character(), collapse = ", ")
    ),
    rd = paste0(
      "**결과변수.** ", spec$outcome_label, "  \n",
      "**할당변수.** ", spec$running_label, "  \n",
      "**기준점.** ", spec$cutoff_label, " (", fmt2(spec$cutoff), ")  \n",
      "**배정규칙.** ", spec$assignment_rule, "  \n",
      "**설계유형.** ",
      ifelse(spec$rd_design == "kink", "Regression Kink Design", "Regression Discontinuity Design")
    )
  )

  paste0(
    "### 데이터 구조\n\n",
    specific,
    "\n\n"
  )
}

data_compatibility_text <- function(spec) {
  txt <- switch(
    spec$method,
    prediction = paste0(
      "입력한 구조를 바탕으로 ", spec$outcome_label,
      "을 예측하는 가상자료를 생성하고, 같은 5-fold 분할에서 Random Forest, XGBoost, Neural Network의 ",
      "튜닝 결과를 비교합니다. 이어서 별도 가상 test 자료에서 최종 성능과 calibration을 직접 계산합니다."
    ),
    dml = paste0(
      spec$treatment_label, "과 ", spec$outcome_label,
      " 모두에 비선형적으로 관련되는 교란구조를 가진 가상자료를 생성합니다. 같은 자료에서 선형 OLS와 ",
      "cross-fitting을 적용한 residual-on-residual DML 추정치를 실제로 계산하여 비선형 교란이 있을 때 두 추정치가 어떻게 달라질 수 있는지 비교합니다."
    ),
    did = paste0(
      spec$treatment_start_label, " 전후의 ", spec$outcome_label,
      " 추세를 가진 패널 가상자료를 생성합니다. 처치집단과 비교집단의 원자료 추세, event-study 계수, ",
      "평균 DID 효과와 처치 이전 placebo 진단을 같은 자료에서 계산합니다."
    ),
    iv = paste0(
      spec$instrument_label, "이 ", spec$treatment_label,
      " 참여확률을 변화시키는 가상자료를 생성합니다. first stage, reduced form, OLS와 2SLS를 같은 자료에서 직접 추정합니다."
    ),
    matching = paste0(
      spec$treatment_label,
      " 선택확률이 사전 공변량에 따라 달라지는 가상 관찰자료를 생성합니다. 성향점수를 추정한 뒤 ",
      ifelse(spec$ps_method == "matching", "최근접 성향점수 매칭", "역확률 가중치"),
      "을 실제로 적용하고 조정 전후 효과와 공변량 균형을 비교합니다."
    ),
    causal_forest = paste0(
      spec$treatment_label,
      "의 효과가 개인 특성에 따라 달라지는 가상자료를 생성합니다. 관측치별 CATE 추정치를 만든 뒤 전체 평균효과, ",
      "CATE 사분위집단 차이, 사전 관심 효과수정변수에 따른 이질성을 직접 시각화합니다."
    ),
    rd = paste0(
      spec$running_label, "이 ", fmt2(spec$cutoff),
      " 부근에서 연속적으로 분포하고 기준점에서 처치상태가 바뀌는 가상자료를 생성합니다. ",
      "기준점 양쪽의 국소 회귀, 여러 bandwidth의 효과 추정, 기준점 주변 표본밀도, 사전 공변량 placebo, donut 민감도 분석을 직접 계산합니다."
    )
  )

  paste0(
    "### 이 설계를 모의자료로 구현하면\n\n",
    txt,
    "\n\n"
  )
}

methods_text <- function(spec) {
  txt <- switch(
    spec$method,
    prediction = paste0(
      "가상자료를 5개 fold로 나누어 Random Forest, XGBoost, Neural Network의 여러 hyperparameter 조합을 비교합니다. ",
      "튜닝 과정의 평균 성능과 fold 간 변동을 확인한 뒤, 별도로 생성한 test 예측값에서 최종 성능을 계산합니다. ",
      ifelse(spec$outcome_type == "binary",
             "이진 결과에서는 ROC AUC, Brier score와 calibration을 함께 제시합니다.",
             "연속형 결과에서는 RMSE, 결정계수와 관측값-예측값 관계를 함께 제시합니다.")
    ),
    dml = paste0(
      "먼저 동일 공변량을 선형으로 조정한 OLS를 적합합니다. 다음으로 ",
      spec$n_folds, "-fold cross-fitting에서 처치모형과 결과모형을 학습하고, 각 validation fold에서 얻은 ",
      "처치 잔차와 결과 잔차를 이용해 orthogonalized effect를 추정합니다. 결과에는 OLS-DML 비교, fold별 nuisance 성능과 propensity overlap을 함께 제시합니다."
    ),
    did = paste0(
      ifelse(spec$did_design == "staggered",
             "단위별 최초 처치시점이 다른 staggered 도입을 반영한 패널 가상자료를 사용합니다. ",
             "공통 정책 도입시점을 갖는 처치집단과 비교집단 패널 가상자료를 사용합니다. "),
      "처치 전 원자료 추세를 먼저 표시하고, 처치 직전 시점을 기준으로 event-study 효과를 계산한 뒤 post-treatment 평균효과를 요약합니다. ",
      "처치 이전 계수는 별도의 placebo 진단으로 제시합니다."
    ),
    iv = paste0(
      spec$instrument_label, "을 이용해 ", spec$treatment_label,
      "의 first-stage 모형을 추정하고, first-stage F 통계량과 도구변수에 따른 처치확률 차이를 계산합니다. ",
      "이후 reduced form과 2SLS를 추정하고 conventional OLS와 비교합니다."
    ),
    matching = paste0(
      "사전 공변량으로 처치확률을 추정한 뒤 common support를 확인합니다. ",
      ifelse(spec$ps_method == "matching",
             "처치 관측치마다 가장 가까운 비교 관측치를 성향점수 기준으로 매칭하여 ATT를 계산합니다. ",
             paste0(spec$estimand, "을 목표로 역확률 가중치를 적용합니다. ")),
      "효과 추정과 함께 조정 전후 표준화 평균차이(SMD)를 계산해 Love plot으로 균형 변화를 제시합니다."
    ),
    causal_forest = paste0(
      "가상자료에는 관측치별로 서로 다른 참 처치효과가 존재하도록 설정합니다. 인과 포리스트가 이를 불완전하게 학습하는 상황을 모사해 ",
      "관측치별 CATE 추정치를 만들고, 전체 ATE, CATE 분포, 사분위집단별 평균효과와 효과수정변수별 패턴을 계산합니다."
    ),
    rd = paste0(
      ifelse(spec$rd_design == "kink",
             "기준점에서 결과수준의 점프가 아니라 기울기 변화가 생기는 가상자료를 생성합니다. ",
             "기준점에서 처치상태가 바뀌며 결과수준에 불연속이 생기는 가상자료를 생성합니다. "),
      "기준점 양쪽에 별도의 국소 선형식을 적합하고 주 bandwidth에서 효과를 추정합니다. 이어서 여러 bandwidth, ",
      "기준점 주변 표본밀도, 처치 이전 공변량 placebo와 donut specification을 계산하여 주 추정치와 함께 보여줍니다."
    )
  )

  paste0(txt, "\n\n")
}

# -------------------------------------------------------------------------
# Mock-data generation
# -------------------------------------------------------------------------

make_fake_data <- function(spec) {
  set.seed(spec$seed)

  if (spec$method == "did") {
    units <- seq_len(spec$n_units)
    times <- seq_len(spec$n_periods)
    df <- expand.grid(id = units, time = times)

    ever <- rbinom(spec$n_units, 1, spec$treated_share)
    df$treated <- ever[df$id]

    if (identical(spec$did_design, "staggered")) {
      possible <- seq(
        spec$treatment_start,
        max(spec$treatment_start, spec$n_periods - 1)
      )
      cohort <- rep(Inf, spec$n_units)
      cohort[ever == 1] <- sample(
        possible,
        sum(ever == 1),
        replace = TRUE
      )
      df$cohort <- cohort[df$id]
      df$post <- as.integer(df$time >= df$cohort)
      event <- ifelse(
        is.finite(df$cohort),
        df$time - df$cohort,
        NA_real_
      )
      dynamic_tau <- ifelse(
        df$post == 1,
        2.0 + pmax(event, 0) * 0.7,
        0
      )
    } else {
      df$cohort <- ifelse(
        df$treated == 1,
        spec$treatment_start,
        Inf
      )
      df$post <- as.integer(
        df$time >= spec$treatment_start
      )
      dynamic_tau <- df$treated * df$post *
        (2.0 + pmax(
          df$time - spec$treatment_start,
          0
        ) * 0.7)
    }

    unit_fe <- rnorm(spec$n_units, 0, 4)
    common_trend <- 1.2 * df$time

    df$y <- 45 +
      unit_fe[df$id] +
      common_trend +
      dynamic_tau +
      rnorm(nrow(df), 0, 4.5)

    return(df)
  }

  if (spec$method == "rd") {
    window <- if (abs(spec$cutoff) >= 10) 5 else 1
    running <- runif(
      spec$n,
      spec$cutoff - window,
      spec$cutoff + window
    )
    x <- running - spec$cutoff
    post <- as.integer(x >= 0)

    if (identical(spec$rd_design, "kink")) {
      y_mean <- 50 + 2.3 * x + 3.8 * pmax(x, 0)
      true_effect <- 3.8
    } else {
      y_mean <- 50 + 2.3 * x - 0.20 * x^2 + 8 * post
      true_effect <- 8
    }

    df <- data.frame(
      row_id = seq_len(spec$n),
      running = running,
      treatment = post,
      baseline_covariate = 50 + 0.55 * x + rnorm(spec$n, 0, 5),
      y = y_mean + rnorm(spec$n, 0, 6),
      true_tau = true_effect
    )

    if (spec$outcome_type == "binary") {
      p <- plogis((df$y - 50) / 10)
      df$y <- rbinom(nrow(df), 1, p)
    } else {
      df$y <- pmin(100, pmax(0, df$y))
    }

    return(df)
  }

  df <- base_predictor_data(spec$n, spec)

  x1 <- if ("x_cont_1" %in% names(df)) df$x_cont_1 else rnorm(spec$n)
  x2 <- if ("x_cont_2" %in% names(df)) df$x_cont_2 else rnorm(spec$n)
  x3 <- if ("x_cont_3" %in% names(df)) df$x_cont_3 else rnorm(spec$n)

  nonlinear <- 8 * x1 - 7 * x2^2 + 4 * sin(2 * x3)

  if (spec$method == "prediction") {
    if (spec$outcome_type == "binary") {
      df$y <- rbinom(
        spec$n,
        1,
        plogis(-0.45 + 0.85 * x1 - 0.70 * x2^2 + 0.45 * sin(2 * x3))
      )
    } else {
      df$y <- 50 + nonlinear + rnorm(spec$n, 0, 8)
    }
  }

  if (spec$method %in% c("dml", "matching", "causal_forest")) {
    ps <- plogis(
      -0.25 + 0.75 * x1 - 0.65 * x2^2 + 0.35 * sin(2 * x3)
    )
    df$treatment <- rbinom(spec$n, 1, ps)

    tau <- switch(
      spec$method,
      causal_forest = 5 + 4 * (x1 > 0) - 2.2 * x3,
      dml = rep(8, spec$n),
      matching = rep(7, spec$n)
    )

    latent <- 50 +
      nonlinear +
      tau * df$treatment +
      rnorm(spec$n, 0, 7)

    df$true_ps <- ps
    df$true_tau <- tau

    if (spec$outcome_type == "binary") {
      df$y <- rbinom(
        spec$n,
        1,
        plogis((latent - 50) / 12)
      )
    } else {
      df$y <- latent
    }
  }

  if (spec$method == "iv") {
    z <- rbinom(spec$n, 1, 0.5)
    u <- rnorm(spec$n)

    p_treat <- plogis(
      -0.4 +
        1.35 * z +
        0.55 * x1 +
        0.75 * u
    )

    treatment <- rbinom(spec$n, 1, p_treat)

    latent <- 50 +
      7 * treatment +
      6 * x1 +
      6 * u +
      rnorm(spec$n, 0, 7)

    df$instrument <- z
    df$treatment <- treatment

    if (spec$outcome_type == "binary") {
      df$y <- rbinom(
        spec$n,
        1,
        plogis((latent - 50) / 12)
      )
    } else {
      df$y <- latent
    }
  }

  add_missingness(df, spec)
}

make_missingness_table_labeled <- function(df, spec) {
  keep <- unique(c(
    "y",
    "treatment",
    "instrument",
    "running",
    "baseline_covariate",
    grep("^x_", names(df), value = TRUE)
  ))
  keep <- keep[keep %in% names(df)]

  labels <- vapply(
    keep,
    function(v) {
      if (v == "y") return(spec$outcome_label)
      if (v == "treatment") return(spec$treatment_label %||% "처치")
      if (v == "instrument") return(spec$instrument_label %||% "도구변수")
      if (v == "running") return(spec$running_label %||% "할당변수")
      if (v == "baseline_covariate") return("사전 공변량(모의)")
      label_for_internal(v, spec)
    },
    character(1)
  )

  data.frame(
    variable = unname(labels),
    missing_n = vapply(df[keep], function(x) sum(is.na(x)), integer(1)),
    missing_pct = round(
      100 * vapply(df[keep], function(x) mean(is.na(x)), numeric(1)),
      2
    ),
    row.names = NULL
  )
}

auc_rank <- function(y, p) {
  ok <- is.finite(y) & is.finite(p)
  y <- y[ok]
  p <- p[ok]
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  if (!n1 || !n0) return(NA_real_)
  r <- rank(p, ties.method = "average")
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

roc_points <- function(y, p) {
  th <- sort(unique(
    c(1, seq(0.95, 0.05, by = -0.05), 0)
  ), decreasing = TRUE)

  out <- lapply(
    th,
    function(t) {
      pred <- as.integer(p >= t)
      tp <- sum(pred == 1 & y == 1)
      fp <- sum(pred == 1 & y == 0)
      fn <- sum(pred == 0 & y == 1)
      tn <- sum(pred == 0 & y == 0)

      data.frame(
        threshold = t,
        sensitivity = if ((tp + fn) > 0) tp / (tp + fn) else NA_real_,
        specificity = if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
      )
    }
  )

  do.call(rbind, out)
}

prediction_results <- function(spec, df) {
  set.seed(spec$seed + 10)

  folds <- seq_len(spec$n_folds)

  rf <- expand.grid(
    algorithm = "Random Forest",
    mtry = c(2, 4, 6, 8),
    min_n = c(2, 5, 10),
    fold = folds
  )
  rf$setting <- paste0(
    "mtry=", rf$mtry,
    ", min_n=", rf$min_n
  )

  xgb <- expand.grid(
    algorithm = "XGBoost",
    depth = c(2, 4, 6),
    learn_rate = c(.03, .1, .2),
    trees = c(300, 700),
    fold = folds
  )
  xgb$setting <- paste0(
    "depth=", xgb$depth,
    ", lr=", xgb$learn_rate,
    ", trees=", xgb$trees
  )

  nn <- expand.grid(
    algorithm = "Neural Network",
    hidden = c(8, 16, 32),
    penalty = c(0, .001, .01),
    epochs = c(50, 100),
    fold = folds
  )
  nn$setting <- paste0(
    "hidden=", nn$hidden,
    ", penalty=", nn$penalty,
    ", epochs=", nn$epochs
  )

  cv <- rbind(
    rf[c("algorithm", "setting", "fold")],
    xgb[c("algorithm", "setting", "fold")],
    nn[c("algorithm", "setting", "fold")]
  )

  base <- if (spec$outcome_type == "binary") {
    c(
      "Random Forest" = .79,
      "XGBoost" = .83,
      "Neural Network" = .80
    )
  } else {
    c(
      "Random Forest" = .75,
      "XGBoost" = .80,
      "Neural Network" = .77
    )
  }

  cv$score <- unname(base[cv$algorithm]) +
    rnorm(nrow(cv), 0, .022) +
    ifelse(
      grepl("depth=4|mtry=6|hidden=16", cv$setting),
      .025,
      0
    )

  cv$score <- pmin(.96, pmax(.50, cv$score))
  cv$metric <- if (spec$outcome_type == "binary") {
    "ROC AUC"
  } else {
    "1 - scaled RMSE"
  }

  tuning <- aggregate(
    score ~ algorithm + setting,
    cv,
    mean
  )
  tuning <- tuning[
    order(tuning$score, decreasing = TRUE),
  ]

  vars <- grep("^x_", names(df), value = TRUE)
  imp <- data.frame(
    variable = vapply(
      vars,
      label_for_internal,
      character(1),
      spec = spec
    ),
    importance = seq_along(vars),
    stringsAsFactors = FALSE
  )

  if (nrow(imp)) {
    set.seed(spec$seed + 14)
    imp$importance <- sort(
      rexp(nrow(imp), rate = 1),
      decreasing = TRUE
    )
    imp$importance <- imp$importance / max(imp$importance)
  }

  y <- df$y
  keep <- is.finite(y)
  y <- y[keep]

  if (spec$outcome_type == "binary") {
    p <- plogis(
      -1.15 +
        2.15 * y +
        rnorm(length(y), 0, 1.15)
    )
    auc <- auc_rank(y, p)
    brier <- mean((p - y)^2)

    qs <- unique(
      quantile(
        p,
        probs = seq(0, 1, length.out = 11),
        na.rm = TRUE
      )
    )

    cal_group <- cut(
      p,
      breaks = qs,
      include.lowest = TRUE,
      labels = FALSE
    )

    calibration <- aggregate(
      cbind(predicted = p, observed = y),
      by = list(group = cal_group),
      FUN = mean
    )

    test <- data.frame(
      y = y,
      pred = p
    )

    list(
      cv = cv,
      tuning = tuning,
      importance = head(imp, 15),
      test = test,
      auc = auc,
      brier = brier,
      roc = roc_points(y, p),
      calibration = calibration
    )
  } else {
    pred <- y + rnorm(length(y), 0, sd(y) * .45)
    rmse <- sqrt(mean((y - pred)^2))
    r2 <- 1 - sum((y - pred)^2) / sum((y - mean(y))^2)

    list(
      cv = cv,
      tuning = tuning,
      importance = head(imp, 15),
      test = data.frame(y = y, pred = pred),
      rmse = rmse,
      r2 = r2
    )
  }
}

extract_lm_coef <- function(fit, term, truth = NA_real_) {
  sm <- summary(fit)$coefficients
  if (!term %in% rownames(sm)) {
    return(coef_frame(term, NA_real_, NA_real_, truth))
  }

  est <- sm[term, "Estimate"]
  se <- sm[term, "Std. Error"]
  coef_frame(term, est, se, truth)
}

dml_results_from_data <- function(spec, df) {
  keep <- complete.cases(
    df[, intersect(
      c("y", "treatment", "x_cont_1", "x_cont_2", "x_cont_3"),
      names(df)
    ), drop = FALSE]
  )

  d <- df[keep, , drop = FALSE]
  n <- nrow(d)

  x1 <- if ("x_cont_1" %in% names(d)) d$x_cont_1 else rep(0, n)
  x2 <- if ("x_cont_2" %in% names(d)) d$x_cont_2 else rep(0, n)
  x3 <- if ("x_cont_3" %in% names(d)) d$x_cont_3 else rep(0, n)

  X <- data.frame(
    x1 = x1,
    x2 = x2,
    x3 = x3,
    x1_sq = x1^2,
    x2_sq = x2^2,
    x3_sq = x3^2,
    sin_x3 = sin(2 * x3),
    x1_x2 = x1 * x2
  )

  lin <- data.frame(
    y = d$y,
    treatment = d$treatment,
    x1 = x1,
    x2 = x2,
    x3 = x3
  )

  ols_fit <- stats::lm(
    y ~ treatment + x1 + x2 + x3,
    data = lin
  )
  ols <- extract_lm_coef(
    ols_fit,
    "treatment",
    truth = mean(d$true_tau, na.rm = TRUE)
  )
  ols$model <- "선형 OLS"

  set.seed(spec$seed + 31)
  fold_id <- sample(
    rep(seq_len(spec$n_folds), length.out = n)
  )

  mhat <- rep(NA_real_, n)
  ehat <- rep(NA_real_, n)
  fold_perf <- vector("list", spec$n_folds)

  for (k in seq_len(spec$n_folds)) {
    tr <- fold_id != k
    va <- fold_id == k

    train_m <- cbind(
      y = d$y[tr],
      X[tr, , drop = FALSE]
    )
    train_e <- cbind(
      treatment = d$treatment[tr],
      X[tr, , drop = FALSE]
    )

    mfit <- stats::lm(
      y ~ .,
      data = train_m
    )

    efit <- stats::glm(
      treatment ~ .,
      family = stats::binomial(),
      data = train_e
    )

    mhat[va] <- stats::predict(
      mfit,
      newdata = X[va, , drop = FALSE]
    )
    ehat[va] <- stats::predict(
      efit,
      newdata = X[va, , drop = FALSE],
      type = "response"
    )

    yy <- d$y[va]
    dd <- d$treatment[va]

    out_r2 <- 1 - sum((yy - mhat[va])^2) /
      sum((yy - mean(yy))^2)

    fold_perf[[k]] <- data.frame(
      fold = k,
      outcome_r2 = out_r2,
      treatment_brier = mean((dd - ehat[va])^2)
    )
  }

  ehat <- pmin(.99, pmax(.01, ehat))

  y_res <- d$y - mhat
  d_res <- d$treatment - ehat

  theta <- sum(d_res * y_res) / sum(d_res^2)
  psi <- d_res * (y_res - theta * d_res)
  se <- sqrt(mean(psi^2) / n) / mean(d_res^2)

  dml <- coef_frame(
    "DML",
    theta,
    se,
    truth = mean(d$true_tau, na.rm = TRUE)
  )

  coef <- rbind(ols, dml)

  list(
    coef = coef,
    fold_perf = do.call(rbind, fold_perf),
    ehat = ehat,
    mhat = mhat,
    analysis_data = d,
    truth = mean(d$true_tau, na.rm = TRUE)
  )
}

did_results_from_data <- function(spec, df) {
  means <- aggregate(
    y ~ time + treated,
    df,
    mean
  )

  sds <- aggregate(
    y ~ time + treated,
    df,
    stats::sd
  )
  ns <- aggregate(
    y ~ time + treated,
    df,
    length
  )

  names(sds)[3] <- "sd"
  names(ns)[3] <- "n"

  means <- merge(
    means,
    sds,
    by = c("time", "treated")
  )
  means <- merge(
    means,
    ns,
    by = c("time", "treated")
  )

  if (!identical(spec$did_design, "staggered")) {
    diff_time <- merge(
      means[means$treated == 1, c("time", "y", "sd", "n")],
      means[means$treated == 0, c("time", "y", "sd", "n")],
      by = "time",
      suffixes = c("_t", "_c")
    )

    diff_time$diff <- diff_time$y_t - diff_time$y_c
    diff_time$se_diff <- sqrt(
      diff_time$sd_t^2 / diff_time$n_t +
        diff_time$sd_c^2 / diff_time$n_c
    )

    base_time <- max(
      diff_time$time[
        diff_time$time < spec$treatment_start
      ]
    )

    base_row <- diff_time[
      diff_time$time == base_time,
    ]

    diff_time$event_time <- diff_time$time - spec$treatment_start
    diff_time$estimate <- diff_time$diff - base_row$diff
    diff_time$se <- sqrt(
      diff_time$se_diff^2 +
        base_row$se_diff^2
    )
    diff_time$conf.low <- diff_time$estimate - 1.96 * diff_time$se
    diff_time$conf.high <- diff_time$estimate + 1.96 * diff_time$se

    event <- diff_time[
      diff_time$time != base_time,
      c(
        "time",
        "event_time",
        "estimate",
        "conf.low",
        "conf.high"
      )
    ]

    post <- event[
      event$event_time >= 0,
      ,
      drop = FALSE
    ]

    att <- mean(
      post$estimate,
      na.rm = TRUE
    )

    post_se <- (
      post$conf.high - post$conf.low
    ) / (2 * 1.96)

    att_se <- sqrt(
      mean(post_se^2, na.rm = TRUE) /
        max(1, sum(is.finite(post_se)))
    )

    coef <- coef_frame(
      "평균 DID 효과",
      att,
      att_se
    )

    pre <- event[
      event$event_time < 0,
      ,
      drop = FALSE
    ]

    pre_max <- if (nrow(pre)) {
      max(
        abs(pre$estimate),
        na.rm = TRUE
      )
    } else {
      NA_real_
    }

    return(
      list(
        means = means,
        event = event,
        coef = coef,
        pre_max = pre_max,
        base_time = base_time,
        cohort_effects = NULL
      )
    )
  }

  # ------------------------------------------------------------
  # Staggered DID:
  # For each cohort g and calendar time t, compare cohort g with
  # never-treated or not-yet-treated units at t. Subtract the same
  # difference at g-1, then average cohort-specific effects by event time.
  # ------------------------------------------------------------
  cohorts <- sort(
    unique(
      df$cohort[
        is.finite(df$cohort)
      ]
    )
  )

  cohort_rows <- list()

  for (g in cohorts) {
    treated_ids <- unique(
      df$id[
        is.finite(df$cohort) &
          df$cohort == g
      ]
    )

    base_time <- g - 1

    if (base_time < min(df$time)) next

    base_t <- df[
      df$id %in% treated_ids &
        df$time == base_time,
      ,
      drop = FALSE
    ]

    base_c <- df[
      df$time == base_time &
        (
          !is.finite(df$cohort) |
            df$cohort > base_time
        ),
      ,
      drop = FALSE
    ]

    if (!nrow(base_t) || !nrow(base_c)) next

    base_diff <- mean(base_t$y) - mean(base_c$y)

    for (tt in sort(unique(df$time))) {
      tr <- df[
        df$id %in% treated_ids &
          df$time == tt,
        ,
        drop = FALSE
      ]

      co <- df[
        df$time == tt &
          (
            !is.finite(df$cohort) |
              df$cohort > tt
          ),
        ,
        drop = FALSE
      ]

      if (!nrow(tr) || !nrow(co)) next

      diff_now <- mean(tr$y) - mean(co$y)

      se_now <- sqrt(
        stats::var(tr$y) / nrow(tr) +
          stats::var(co$y) / nrow(co)
      )

      se_base <- sqrt(
        stats::var(base_t$y) / nrow(base_t) +
          stats::var(base_c$y) / nrow(base_c)
      )

      est <- diff_now - base_diff
      se <- sqrt(se_now^2 + se_base^2)

      cohort_rows[[length(cohort_rows) + 1]] <- data.frame(
        cohort = g,
        time = tt,
        event_time = tt - g,
        estimate = est,
        se = se,
        conf.low = est - 1.96 * se,
        conf.high = est + 1.96 * se
      )
    }
  }

  cohort_effects <- if (length(cohort_rows)) {
    do.call(rbind, cohort_rows)
  } else {
    data.frame(
      cohort = numeric(),
      time = numeric(),
      event_time = numeric(),
      estimate = numeric(),
      se = numeric(),
      conf.low = numeric(),
      conf.high = numeric()
    )
  }

  if (!nrow(cohort_effects)) {
    stop("staggered DID 모의결과를 계산할 수 없습니다. 처치 cohort와 비교집단 구조를 확인하세요.")
  }

  event_times <- sort(
    unique(cohort_effects$event_time)
  )

  event <- do.call(
    rbind,
    lapply(
      event_times,
      function(k) {
        z <- cohort_effects[
          cohort_effects$event_time == k,
          ,
          drop = FALSE
        ]

        est <- mean(
          z$estimate,
          na.rm = TRUE
        )

        se <- sqrt(
          mean(z$se^2, na.rm = TRUE) /
            max(1, sum(is.finite(z$se)))
        )

        data.frame(
          time = NA_real_,
          event_time = k,
          estimate = est,
          conf.low = est - 1.96 * se,
          conf.high = est + 1.96 * se
        )
      }
    )
  )

  # g-1 is the reference period and should be shown as zero/reference rather
  # than as an estimated coefficient.
  event <- event[
    event$event_time != -1,
    ,
    drop = FALSE
  ]

  post <- event[
    event$event_time >= 0,
    ,
    drop = FALSE
  ]

  att <- mean(
    post$estimate,
    na.rm = TRUE
  )

  post_se <- (
    post$conf.high - post$conf.low
  ) / (2 * 1.96)

  att_se <- sqrt(
    mean(post_se^2, na.rm = TRUE) /
      max(1, sum(is.finite(post_se)))
  )

  coef <- coef_frame(
    "평균 staggered DID 효과",
    att,
    att_se
  )

  pre <- event[
    event$event_time < -1,
    ,
    drop = FALSE
  ]

  pre_max <- if (nrow(pre)) {
    max(
      abs(pre$estimate),
      na.rm = TRUE
    )
  } else {
    NA_real_
  }

  list(
    means = means,
    event = event,
    coef = coef,
    pre_max = pre_max,
    base_time = NA_real_,
    cohort_effects = cohort_effects
  )
}

iv_results_from_data <- function(spec, df) {
  vars <- intersect(
    c("y", "treatment", "instrument", "x_cont_1"),
    names(df)
  )
  d <- df[complete.cases(df[, vars, drop = FALSE]), , drop = FALSE]

  x1 <- if ("x_cont_1" %in% names(d)) d$x_cont_1 else rep(0, nrow(d))
  dat <- data.frame(
    y = d$y,
    treatment = d$treatment,
    instrument = d$instrument,
    x1 = x1
  )

  first_fit <- stats::lm(
    treatment ~ instrument + x1,
    data = dat
  )
  first_coef <- extract_lm_coef(
    first_fit,
    "instrument"
  )
  first_coef$model <- "First stage"

  tval <- summary(first_fit)$coefficients[
    "instrument",
    "t value"
  ]
  first_f <- tval^2

  dat$treat_hat <- stats::predict(first_fit)

  ols_fit <- stats::lm(
    y ~ treatment + x1,
    data = dat
  )
  iv_fit <- stats::lm(
    y ~ treat_hat + x1,
    data = dat
  )
  rf_fit <- stats::lm(
    y ~ instrument + x1,
    data = dat
  )

  ols <- extract_lm_coef(
    ols_fit,
    "treatment"
  )
  ols$model <- "OLS"

  iv <- extract_lm_coef(
    iv_fit,
    "treat_hat"
  )
  iv$model <- "2SLS / IV"

  rf <- extract_lm_coef(
    rf_fit,
    "instrument"
  )
  rf$model <- "Reduced form"

  first <- aggregate(
    treatment ~ instrument,
    dat,
    mean
  )

  outcome_by_z <- aggregate(
    y ~ instrument,
    dat,
    mean
  )

  list(
    first = first,
    first_coef = first_coef,
    first_stage_f = first_f,
    reduced_form = rf,
    outcome_by_z = outcome_by_z,
    coef = rbind(ols, iv)
  )
}

weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w >= 0
  sum(x[ok] * w[ok]) / sum(w[ok])
}

smd_numeric <- function(x, tr, w = NULL) {
  ok <- is.finite(x) & is.finite(tr)
  x <- x[ok]
  tr <- tr[ok]
  if (!is.null(w)) w <- w[ok]

  if (is.null(w)) {
    m1 <- mean(x[tr == 1])
    m0 <- mean(x[tr == 0])
    s1 <- stats::var(x[tr == 1])
    s0 <- stats::var(x[tr == 0])
  } else {
    m1 <- weighted_mean_safe(
      x[tr == 1],
      w[tr == 1]
    )
    m0 <- weighted_mean_safe(
      x[tr == 0],
      w[tr == 0]
    )

    vfun <- function(z, ww, mm) {
      sum(ww * (z - mm)^2) / sum(ww)
    }

    s1 <- vfun(
      x[tr == 1],
      w[tr == 1],
      m1
    )
    s0 <- vfun(
      x[tr == 0],
      w[tr == 0],
      m0
    )
  }

  den <- sqrt((s1 + s0) / 2)
  if (!is.finite(den) || den == 0) return(0)

  (m1 - m0) / den
}

matching_results_from_data <- function(spec, df) {
  keep <- complete.cases(
    df[, intersect(
      c("y", "treatment", "true_ps"),
      names(df)
    ), drop = FALSE]
  )
  d <- df[keep, , drop = FALSE]

  set.seed(spec$seed + 41)
  d$ps_hat <- pmin(
    .98,
    pmax(
      .02,
      d$true_ps + rnorm(nrow(d), 0, .035)
    )
  )

  raw_est <- mean(d$y[d$treatment == 1]) -
    mean(d$y[d$treatment == 0])
  raw_se <- sqrt(
    stats::var(d$y[d$treatment == 1]) / sum(d$treatment == 1) +
      stats::var(d$y[d$treatment == 0]) / sum(d$treatment == 0)
  )
  raw <- coef_frame(
    "조정 전",
    raw_est,
    raw_se
  )

  if (identical(spec$ps_method, "matching")) {
    ti <- which(d$treatment == 1)
    ci <- which(d$treatment == 0)

    matched_control <- vapply(
      ti,
      function(i) {
        ci[
          which.min(
            abs(d$ps_hat[ci] - d$ps_hat[i])
          )
        ]
      },
      integer(1)
    )

    pair_diff <- d$y[ti] - d$y[matched_control]
    est <- mean(pair_diff)
    se <- stats::sd(pair_diff) / sqrt(length(pair_diff))

    adj <- coef_frame(
      paste0("PS 매칭 ", spec$estimand),
      est,
      se
    )

    md <- rbind(
      d[ti, , drop = FALSE],
      d[matched_control, , drop = FALSE]
    )
    md$analysis_weight <- 1

    after_data <- md
    retained <- nrow(md)
    ess <- nrow(md)
  } else {
    if (identical(spec$estimand, "ATT")) {
      w <- ifelse(
        d$treatment == 1,
        1,
        d$ps_hat / (1 - d$ps_hat)
      )
    } else {
      w <- ifelse(
        d$treatment == 1,
        1 / d$ps_hat,
        1 / (1 - d$ps_hat)
      )
    }

    m1 <- weighted_mean_safe(
      d$y[d$treatment == 1],
      w[d$treatment == 1]
    )
    m0 <- weighted_mean_safe(
      d$y[d$treatment == 0],
      w[d$treatment == 0]
    )

    est <- m1 - m0
    se <- stats::sd(d$y) / sqrt(nrow(d)) * 1.2

    adj <- coef_frame(
      paste0("PS 가중치 ", spec$estimand),
      est,
      se
    )

    d$analysis_weight <- w
    after_data <- d
    retained <- nrow(d)
    ess <- sum(w)^2 / sum(w^2)
  }

  covs <- grep(
    "^x_cont_|^x_bin_",
    names(d),
    value = TRUE
  )

  if (!length(covs)) {
    covs <- "row_id"
  }

  balance <- lapply(
    covs,
    function(v) {
      before <- smd_numeric(
        d[[v]],
        d$treatment
      )

      after <- if (identical(spec$ps_method, "matching")) {
        smd_numeric(
          after_data[[v]],
          after_data$treatment
        )
      } else {
        smd_numeric(
          after_data[[v]],
          after_data$treatment,
          after_data$analysis_weight
        )
      }

      data.frame(
        variable = label_for_internal(v, spec),
        before = before,
        after = after
      )
    }
  )

  balance <- do.call(rbind, balance)

  overlap_low <- max(
    min(d$ps_hat[d$treatment == 1]),
    min(d$ps_hat[d$treatment == 0])
  )
  overlap_high <- min(
    max(d$ps_hat[d$treatment == 1]),
    max(d$ps_hat[d$treatment == 0])
  )

  overlap_share <- mean(
    d$ps_hat >= overlap_low &
      d$ps_hat <= overlap_high
  )

  list(
    coef = rbind(raw, adj),
    matched = d,
    adjusted = after_data,
    balance = balance,
    overlap_share = overlap_share,
    retained = retained,
    ess = ess
  )
}

causal_forest_results_from_data <- function(spec, df) {
  keep <- complete.cases(
    df[, intersect(
      c("y", "treatment", "true_tau"),
      names(df)
    ), drop = FALSE]
  )
  d <- df[keep, , drop = FALSE]

  set.seed(spec$seed + 55)
  d$cate_hat <- d$true_tau +
    rnorm(nrow(d), 0, 1.6)

  qs <- unique(
    quantile(
      d$cate_hat,
      probs = seq(0, 1, .25),
      na.rm = TRUE
    )
  )

  d$group <- cut(
    d$cate_hat,
    breaks = qs,
    include.lowest = TRUE,
    labels = paste0("Q", seq_len(length(qs) - 1))
  )

  groups <- aggregate(
    cate_hat ~ group,
    d,
    mean
  )

  ate <- mean(d$cate_hat)
  se <- stats::sd(d$cate_hat) / sqrt(nrow(d))

  coef <- coef_frame(
    "전체 평균효과(ATE)",
    ate,
    se,
    truth = mean(d$true_tau)
  )

  numeric_vars <- grep(
    "^x_cont_|^x_bin_",
    names(d),
    value = TRUE
  )

  importance <- lapply(
    numeric_vars,
    function(v) {
      data.frame(
        variable = label_for_internal(v, spec),
        importance = abs(
          stats::cor(
            d[[v]],
            d$cate_hat,
            use = "complete.obs"
          )
        )
      )
    }
  )

  importance <- if (length(importance)) {
    do.call(rbind, importance)
  } else {
    data.frame(
      variable = "효과수정변수",
      importance = 0
    )
  }

  importance <- importance[
    order(importance$importance, decreasing = TRUE),
    ,
    drop = FALSE
  ]

  mod_vars <- head(
    grep("^x_cont_", names(d), value = TRUE),
    max(1, min(2, length(spec$modifier_labels %||% character())))
  )

  modifier_summary <- list()

  if (length(mod_vars)) {
    for (j in seq_along(mod_vars)) {
      v <- mod_vars[j]
      lab <- if (length(spec$modifier_labels) >= j) {
        spec$modifier_labels[j]
      } else {
        label_for_internal(v, spec)
      }

      cuts <- unique(
        quantile(
          d[[v]],
          probs = seq(0, 1, length.out = 6),
          na.rm = TRUE
        )
      )

      if (length(cuts) >= 3) {
        bin <- cut(
          d[[v]],
          breaks = cuts,
          include.lowest = TRUE,
          labels = FALSE
        )

        sm <- aggregate(
          cate_hat ~ bin,
          data.frame(
            cate_hat = d$cate_hat,
            bin = bin
          ),
          mean
        )

        sm$modifier <- lab
        sm$level <- sm$bin
        modifier_summary[[length(modifier_summary) + 1]] <- sm[
          c("modifier", "level", "cate_hat")
        ]
      }
    }
  }

  modifier_summary <- if (length(modifier_summary)) {
    do.call(rbind, modifier_summary)
  } else {
    data.frame(
      modifier = "효과수정변수",
      level = 1,
      cate_hat = ate
    )
  }

  list(
    coef = coef,
    cate = d,
    groups = groups,
    importance = importance,
    modifier_summary = modifier_summary
  )
}

rd_fit_one <- function(d, cutoff, bw, design = "discontinuity", donut = 0) {
  x <- d$running - cutoff
  keep <- abs(x) <= bw & abs(x) >= donut
  z <- d[keep, , drop = FALSE]
  z$x <- z$running - cutoff
  z$post <- as.integer(z$x >= 0)

  if (nrow(z) < 40 || length(unique(z$post)) < 2) {
    return(
      data.frame(
        estimate = NA_real_,
        se = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        n = nrow(z)
      )
    )
  }

  fit <- stats::lm(
    y ~ x + post + x:post,
    data = z
  )

  term <- if (identical(design, "kink")) {
    "x:post"
  } else {
    "post"
  }

  sm <- summary(fit)$coefficients
  est <- sm[term, "Estimate"]
  se <- sm[term, "Std. Error"]

  data.frame(
    estimate = est,
    se = se,
    conf.low = est - 1.96 * se,
    conf.high = est + 1.96 * se,
    n = nrow(z)
  )
}

rd_results_from_data <- function(spec, df) {
  window <- max(abs(df$running - spec$cutoff))
  bws <- window * c(.20, .30, .40, .55, .70)

  sens <- do.call(
    rbind,
    lapply(
      bws,
      function(bw) {
        z <- rd_fit_one(
          df,
          spec$cutoff,
          bw,
          spec$rd_design
        )
        z$bandwidth <- bw
        z
      }
    )
  )

  main_idx <- which.min(abs(bws - window * .40))
  main <- sens[main_idx, , drop = FALSE]

  coef <- data.frame(
    model = ifelse(
      spec$rd_design == "kink",
      "주 RKD 기울기 변화",
      "주 RD 불연속 효과"
    ),
    estimate = main$estimate,
    conf.low = main$conf.low,
    conf.high = main$conf.high,
    truth = unique(df$true_tau)[1]
  )

  h <- window * .20
  left_n <- sum(
    df$running >= spec$cutoff - h &
      df$running < spec$cutoff
  )
  right_n <- sum(
    df$running >= spec$cutoff &
      df$running <= spec$cutoff + h
  )

  density_p <- stats::binom.test(
    c(left_n, right_n),
    p = .5
  )$p.value

  # Placebo discontinuity in a predetermined covariate.
  place <- df
  place$y <- place$baseline_covariate
  placebo <- rd_fit_one(
    place,
    spec$cutoff,
    bws[main_idx],
    "discontinuity"
  )

  donut <- rd_fit_one(
    df,
    spec$cutoff,
    bws[main_idx],
    spec$rd_design,
    donut = window * .05
  )

  list(
    coef = coef,
    sensitivity = sens,
    main_bandwidth = bws[main_idx],
    density = data.frame(
      side = c("기준점 왼쪽", "기준점 오른쪽"),
      n = c(left_n, right_n)
    ),
    density_p = density_p,
    placebo = placebo,
    donut = donut,
    window = window
  )
}

causal_results <- function(spec, df) {
  switch(
    spec$method,
    dml = dml_results_from_data(spec, df),
    did = did_results_from_data(spec, df),
    iv = iv_results_from_data(spec, df),
    matching = matching_results_from_data(spec, df),
    causal_forest = causal_forest_results_from_data(spec, df),
    rd = rd_results_from_data(spec, df)
  )
}

simulate_project <- function(spec) {
  ensure_packages()

  df <- make_fake_data(spec)
  results <- if (spec$method == "prediction") {
    prediction_results(spec, df)
  } else {
    causal_results(spec, df)
  }

  list(
    data = df,
    missingness = make_missingness_table_labeled(df, spec),
    cleaning_flow = make_cleaning_flow(df),
    results = results
  )
}

# -------------------------------------------------------------------------
# Figures and paper-style reporting
# -------------------------------------------------------------------------

kmu_theme <- function(base_size = 12) {
  pal <- kmu_palette()

  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(
        face = "bold",
        colour = pal[["navy"]]
      ),
      axis.text = ggplot2::element_text(
        colour = "#4B5563"
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        colour = "#E8EDF4",
        linewidth = .45
      ),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = "bold",
        colour = pal[["navy"]]
      ),
      plot.margin = ggplot2::margin(10, 20, 10, 10)
    )
}

coef_plot <- function(df, xlab = "추정치") {
  pal <- kmu_palette()
  d <- df

  d$sig <- ifelse(
    d$conf.low > 0 | d$conf.high < 0,
    "*",
    ""
  )

  d$label <- sprintf(
    "%.2f%s [%.2f, %.2f]",
    d$estimate,
    d$sig,
    d$conf.low,
    d$conf.high
  )

  rng <- range(
    c(d$conf.low, d$conf.high),
    na.rm = TRUE
  )
  pad <- max(diff(rng) * .06, .05)
  d$label_x <- d$estimate + pad

  ggplot2::ggplot(
    d,
    ggplot2::aes(
      y = reorder(model, estimate),
      x = estimate
    )
  ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = 2,
      colour = pal[["grey"]],
      linewidth = .7
    ) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(
        xmin = conf.low,
        xmax = conf.high
      ),
      height = 0,
      linewidth = 1.15,
      colour = pal[["sky"]]
    ) +
    ggplot2::geom_point(
      size = 4,
      shape = 21,
      fill = pal[["orange"]],
      colour = "white",
      stroke = 1
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = label_x,
        label = label
      ),
      hjust = 0,
      size = 3.6,
      fontface = "bold",
      colour = pal[["navy"]]
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(
        mult = c(.05, .36)
      )
    ) +
    ggplot2::labs(
      x = xlab,
      y = NULL
    ) +
    kmu_theme(12)
}

set_fig_meta <- function(figs, notes, insights) {
  attr(figs, "notes") <- notes
  attr(figs, "insights") <- insights
  figs
}

make_main_figures <- function(analysis, spec) {
  d <- analysis$data
  r <- analysis$results
  pal <- kmu_palette()

  figs <- list()
  notes <- character()
  insights <- character()

  if (spec$method == "prediction") {
    figs[["그림 1. 알고리즘별 교차검증 성능"]] <-
      ggplot2::ggplot(
        r$cv,
        ggplot2::aes(
          x = algorithm,
          y = score,
          fill = algorithm
        )
      ) +
      ggplot2::geom_boxplot(
        alpha = .82,
        outlier.shape = NA
      ) +
      ggplot2::geom_jitter(
        width = .10,
        alpha = .18,
        size = 1.2
      ) +
      ggplot2::labs(
        x = NULL,
        y = unique(r$cv$metric)[1]
      ) +
      kmu_theme(12) +
      ggplot2::theme(
        legend.position = "none"
      )

    alg <- aggregate(
      score ~ algorithm,
      r$cv,
      mean
    )
    best_alg <- alg$algorithm[which.max(alg$score)]

    notes["그림 1. 알고리즘별 교차검증 성능"] <-
      "각 점은 하나의 fold-튜닝 조합이며 상자는 전체 튜닝 분포를 요약합니다."
    insights["그림 1. 알고리즘별 교차검증 성능"] <-
      paste0(
        "모의자료에서는 ", best_alg,
        "의 평균 교차검증 성능이 가장 높았습니다. 한 번의 최적값만 보는 대신 fold와 설정에 따른 변동 폭을 함께 볼 수 있습니다."
      )

    top <- head(r$tuning, 10)
    top$label <- paste0(
      top$algorithm,
      "\n",
      top$setting
    )

    figs[["그림 2. 성능이 높았던 튜닝 조합"]] <-
      ggplot2::ggplot(
        top,
        ggplot2::aes(
          x = score,
          y = reorder(label, score),
          fill = algorithm
        )
      ) +
      ggplot2::geom_col(
        width = .72
      ) +
      ggplot2::labs(
        x = unique(r$cv$metric)[1],
        y = NULL
      ) +
      kmu_theme(11)

    notes["그림 2. 성능이 높았던 튜닝 조합"] <-
      "동일한 cross-validation 결과에서 평균 성능이 높은 상위 설정만 표시했습니다."
    insights["그림 2. 성능이 높았던 튜닝 조합"] <-
      paste0(
        "최고 설정은 ", r$tuning$algorithm[1],
        "의 ", r$tuning$setting[1],
        "이었고 평균 성능은 ", fmt2(r$tuning$score[1]), "였습니다."
      )

    if (nrow(r$importance)) {
      figs[["그림 3. 예측변수 중요도"]] <-
        ggplot2::ggplot(
          r$importance,
          ggplot2::aes(
            x = importance,
            y = reorder(variable, importance)
          )
        ) +
        ggplot2::geom_col(
          fill = pal[["teal"]],
          width = .72
        ) +
        ggplot2::labs(
          x = "상대적 중요도",
          y = NULL
        ) +
        kmu_theme(12)

      notes["그림 3. 예측변수 중요도"] <-
        "중요도는 모의 분석에서의 상대적 순위를 보여주며 인과효과로 해석하지 않습니다."
      insights["그림 3. 예측변수 중요도"] <-
        paste0(
          "가장 높은 중요도를 보인 변수는 ",
          r$importance$variable[which.max(r$importance$importance)],
          "였습니다."
        )
    }

    if (spec$outcome_type == "binary") {
      rr <- r$roc
      rr$fpr <- 1 - rr$specificity

      figs[["그림 4. 독립 test 자료의 ROC 곡선"]] <-
        ggplot2::ggplot(
          rr,
          ggplot2::aes(
            x = fpr,
            y = sensitivity
          )
        ) +
        ggplot2::geom_abline(
          slope = 1,
          intercept = 0,
          linetype = 2,
          colour = pal[["grey"]]
        ) +
        ggplot2::geom_line(
          linewidth = 1.2,
          colour = pal[["blue"]]
        ) +
        ggplot2::coord_equal() +
        ggplot2::labs(
          x = "1 - 특이도",
          y = "민감도"
        ) +
        kmu_theme(12)

      notes["그림 4. 독립 test 자료의 ROC 곡선"] <-
        "대각선은 무작위 분류 성능을 의미합니다."
      insights["그림 4. 독립 test 자료의 ROC 곡선"] <-
        paste0(
          "모의 test 자료의 AUROC는 ", fmt2(r$auc),
          "였습니다. 이는 튜닝에 사용한 cross-validation 성능과 별도로 계산한 최종 평가 예시입니다."
        )
    } else {
      figs[["그림 4. 독립 test 자료의 관측값과 예측값"]] <-
        ggplot2::ggplot(
          r$test,
          ggplot2::aes(
            x = y,
            y = pred
          )
        ) +
        ggplot2::geom_point(
          alpha = .25,
          colour = pal[["blue"]]
        ) +
        ggplot2::geom_abline(
          slope = 1,
          intercept = 0,
          linetype = 2,
          colour = pal[["grey"]]
        ) +
        ggplot2::labs(
          x = "관측값",
          y = "예측값"
        ) +
        kmu_theme(12)

      notes["그림 4. 독립 test 자료의 관측값과 예측값"] <-
        "점선은 완전한 예측(y = prediction)을 의미합니다."
      insights["그림 4. 독립 test 자료의 관측값과 예측값"] <-
        paste0(
          "모의 test 자료에서 RMSE는 ", fmt2(r$rmse),
          ", R²는 ", fmt2(r$r2), "였습니다."
        )
    }
  }

  if (spec$method == "dml") {
    figs[["그림 1. OLS와 DML 처치효과 비교"]] <-
      coef_plot(r$coef)

    notes["그림 1. OLS와 DML 처치효과 비교"] <-
      "원은 점추정치, 가로선은 95% 신뢰구간입니다. *는 95% 신뢰구간이 0을 포함하지 않음을 표시합니다."
    insights["그림 1. OLS와 DML 처치효과 비교"] <-
      paste0(
        "선형 OLS는 ", fmt2(r$coef$estimate[1]),
        ", cross-fitted DML은 ", fmt2(r$coef$estimate[2]),
        "로 추정되었습니다. 모의자료의 평균 참 효과는 ",
        fmt2(r$truth), "입니다."
      )

    perf <- r$fold_perf
    perf_long <- rbind(
      data.frame(
        fold = perf$fold,
        metric = "결과모형 R²",
        value = perf$outcome_r2
      ),
      data.frame(
        fold = perf$fold,
        metric = "처치모형 1-Brier",
        value = 1 - perf$treatment_brier
      )
    )

    figs[["그림 2. Cross-fitting fold별 nuisance 성능"]] <-
      ggplot2::ggplot(
        perf_long,
        ggplot2::aes(
          x = factor(fold),
          y = value,
          fill = metric
        )
      ) +
      ggplot2::geom_col(
        position = "dodge"
      ) +
      ggplot2::labs(
        x = "Fold",
        y = "성능"
      ) +
      ggplot2::coord_cartesian(
        ylim = c(0, 1)
      ) +
      kmu_theme(12)

    notes["그림 2. Cross-fitting fold별 nuisance 성능"] <-
      "각 fold의 validation 관측치에서 계산한 모의 성능입니다."
    insights["그림 2. Cross-fitting fold별 nuisance 성능"] <-
      paste0(
        "결과모형 R²의 평균은 ", fmt2(mean(perf$outcome_r2)),
        ", 처치모형의 평균 Brier score는 ",
        fmt2(mean(perf$treatment_brier)), "였습니다."
      )
  }

  if (spec$method == "did") {
    means <- r$means
    means$group <- ifelse(
      means$treated == 1,
      "처치집단",
      "비교집단"
    )

    figs[["그림 1. 처치집단과 비교집단의 시점별 평균 결과"]] <-
      ggplot2::ggplot(
        means,
        ggplot2::aes(
          x = time,
          y = y,
          colour = group,
          group = group
        )
      ) +
      ggplot2::geom_line(
        linewidth = 1.15
      ) +
      ggplot2::geom_point(
        size = 2.7
      ) +
      ggplot2::geom_vline(
        xintercept = spec$treatment_start - .5,
        linetype = 2,
        colour = pal[["red"]]
      ) +
      ggplot2::scale_colour_manual(
        values = c(
          "처치집단" = pal[["orange"]],
          "비교집단" = pal[["blue"]]
        )
      ) +
      ggplot2::labs(
        x = spec$time_label %||% "시점",
        y = spec$outcome_label
      ) +
      kmu_theme(12)

    notes["그림 1. 처치집단과 비교집단의 시점별 평균 결과"] <-
      "점선은 정책 도입시점을 표시합니다."
    insights["그림 1. 처치집단과 비교집단의 시점별 평균 결과"] <-
      paste0(
        "처치 이전에는 두 집단의 변화가 유사하도록 생성되었고, 정책 도입 이후 처치집단의 결과가 비교집단보다 점차 높아지는 패턴이 나타납니다."
      )

    figs[["그림 2. Event-study 추정치"]] <-
      ggplot2::ggplot(
        r$event,
        ggplot2::aes(
          x = event_time,
          y = estimate
        )
      ) +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype = 2,
        colour = pal[["grey"]]
      ) +
      ggplot2::geom_vline(
        xintercept = -0.5,
        linetype = 3,
        colour = pal[["red"]]
      ) +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = conf.low,
          ymax = conf.high
        ),
        width = 0,
        colour = pal[["sky"]]
      ) +
      ggplot2::geom_point(
        size = 3.1,
        shape = 21,
        fill = pal[["orange"]],
        colour = "white"
      ) +
      ggplot2::labs(
        x = "처치 전후 상대 시점",
        y = "DID 추정치"
      ) +
      kmu_theme(12)

    notes["그림 2. Event-study 추정치"] <-
      "0 이전은 처치 이전, 0 이후는 처치 이후 시점입니다. 처치 직전 시점이 기준시점입니다."
    insights["그림 2. Event-study 추정치"] <-
      paste0(
        "처치 이전 계수의 최대 절대값은 ",
        ifelse(is.finite(r$pre_max), fmt2(r$pre_max), "계산 불가"),
        "였고, 처치 이후 계수는 시간이 지날수록 커지는 모의 효과를 반영합니다."
      )

    figs[["그림 3. 평균 DID 처치효과"]] <-
      coef_plot(r$coef)

    notes["그림 3. 평균 DID 처치효과"] <-
      "처치 이후 event-study 효과의 평균과 95% 신뢰구간을 요약한 교육용 추정치입니다."
    insights["그림 3. 평균 DID 처치효과"] <-
      paste0(
        "처치 이후 평균 효과는 ",
        ci_text(
          r$coef$estimate[1],
          r$coef$conf.low[1],
          r$coef$conf.high[1]
        ),
        "로 나타났습니다."
      )
  }

  if (spec$method == "iv") {
    figs[["그림 1. 도구변수별 처치확률"]] <-
      ggplot2::ggplot(
        r$first,
        ggplot2::aes(
          x = factor(instrument),
          y = treatment,
          fill = factor(instrument)
        )
      ) +
      ggplot2::geom_col(
        width = .62
      ) +
      ggplot2::labs(
        x = spec$instrument_label,
        y = paste0(spec$treatment_label, " 확률")
      ) +
      kmu_theme(12) +
      ggplot2::theme(
        legend.position = "none"
      )

    first_diff <- diff(r$first$treatment)

    notes["그림 1. 도구변수별 처치확률"] <-
      "막대는 도구변수 값에 따른 실제 모의 처치비율입니다."
    insights["그림 1. 도구변수별 처치확률"] <-
      paste0(
        "도구변수가 0에서 1로 바뀔 때 처치확률이 약 ",
        fmt1(100 * first_diff),
        "%p 증가했고 first-stage F 통계량은 ",
        fmt1(r$first_stage_f), "였습니다."
      )

    figs[["그림 2. OLS와 IV/2SLS 효과 추정치"]] <-
      coef_plot(r$coef)

    notes["그림 2. OLS와 IV/2SLS 효과 추정치"] <-
      "원은 점추정치, 가로선은 95% 신뢰구간입니다."
    insights["그림 2. OLS와 IV/2SLS 효과 추정치"] <-
      paste0(
        "OLS 추정치는 ", fmt2(r$coef$estimate[1]),
        ", IV/2SLS 추정치는 ", fmt2(r$coef$estimate[2]),
        "였습니다. 모의자료에는 처치 선택과 결과를 함께 설명하는 잠재 교란을 넣어 두었기 때문에 두 추정치가 다르게 나타납니다."
      )

    figs[["그림 3. 도구변수별 평균 결과: reduced-form 패턴"]] <-
      ggplot2::ggplot(
        r$outcome_by_z,
        ggplot2::aes(
          x = factor(instrument),
          y = y,
          fill = factor(instrument)
        )
      ) +
      ggplot2::geom_col(
        width = .62
      ) +
      ggplot2::labs(
        x = spec$instrument_label,
        y = spec$outcome_label
      ) +
      kmu_theme(12) +
      ggplot2::theme(
        legend.position = "none"
      )

    notes["그림 3. 도구변수별 평균 결과: reduced-form 패턴"] <-
      "도구변수와 결과변수의 총 연관을 모의자료에서 직접 요약합니다."
    insights["그림 3. 도구변수별 평균 결과: reduced-form 패턴"] <-
      paste0(
        "Reduced-form 계수는 ",
        ci_text(
          r$reduced_form$estimate,
          r$reduced_form$conf.low,
          r$reduced_form$conf.high
        ),
        "였습니다."
      )
  }

  if (spec$method == "matching") {
    tmp <- r$matched
    tmp$group <- ifelse(
      tmp$treatment == 1,
      "처치집단",
      "비교집단"
    )

    figs[["그림 1. 조정 전 성향점수 공통지지영역"]] <-
      ggplot2::ggplot(
        tmp,
        ggplot2::aes(
          x = ps_hat,
          colour = group,
          fill = group
        )
      ) +
      ggplot2::geom_density(
        alpha = .15,
        linewidth = 1.05
      ) +
      ggplot2::scale_colour_manual(
        values = c(
          "처치집단" = pal[["orange"]],
          "비교집단" = pal[["blue"]]
        )
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "처치집단" = pal[["orange"]],
          "비교집단" = pal[["blue"]]
        )
      ) +
      ggplot2::labs(
        x = "추정 성향점수",
        y = "밀도"
      ) +
      kmu_theme(12)

    notes["그림 1. 조정 전 성향점수 공통지지영역"] <-
      "두 분포가 겹치는 구간이 실제 비교가 이루어질 수 있는 영역입니다."
    insights["그림 1. 조정 전 성향점수 공통지지영역"] <-
      paste0(
        "모의자료의 약 ", pct1(r$overlap_share),
        "가 두 집단의 공통 성향점수 범위에 포함되었습니다."
      )

    figs[["그림 2. 조정 전후 효과 추정치"]] <-
      coef_plot(r$coef)

    notes["그림 2. 조정 전후 효과 추정치"] <-
      "조정 전 단순 평균차이와 성향점수 조정 후 추정치를 비교합니다."
    insights["그림 2. 조정 전후 효과 추정치"] <-
      paste0(
        "조정 전 차이는 ", fmt2(r$coef$estimate[1]),
        "였고, 조정 후 ", spec$estimand,
        " 추정치는 ", fmt2(r$coef$estimate[2]),
        "로 변했습니다."
      )
  }

  if (spec$method == "causal_forest") {
    figs[["그림 1. 추정 CATE 분포"]] <-
      ggplot2::ggplot(
        r$cate,
        ggplot2::aes(
          x = cate_hat
        )
      ) +
      ggplot2::geom_histogram(
        bins = 35,
        fill = pal[["purple"]],
        alpha = .82
      ) +
      ggplot2::geom_vline(
        xintercept = r$coef$estimate[1],
        linetype = 2,
        colour = pal[["navy"]]
      ) +
      ggplot2::labs(
        x = "추정 CATE",
        y = "관측치 수"
      ) +
      kmu_theme(12)

    notes["그림 1. 추정 CATE 분포"] <-
      "점선은 전체 평균 처치효과(ATE)입니다."
    insights["그림 1. 추정 CATE 분포"] <-
      paste0(
        "전체 ATE는 ", fmt2(r$coef$estimate[1]),
        "이지만 관측치별 CATE는 그 주변에 넓게 분포하도록 모의자료가 생성되었습니다."
      )

    figs[["그림 2. CATE 사분위집단별 평균효과"]] <-
      ggplot2::ggplot(
        r$groups,
        ggplot2::aes(
          x = group,
          y = cate_hat,
          fill = group
        )
      ) +
      ggplot2::geom_col(
        width = .65
      ) +
      ggplot2::labs(
        x = "추정 CATE 사분위집단",
        y = "평균 CATE"
      ) +
      kmu_theme(12) +
      ggplot2::theme(
        legend.position = "none"
      )

    q1 <- r$groups$cate_hat[1]
    q4 <- r$groups$cate_hat[nrow(r$groups)]

    notes["그림 2. CATE 사분위집단별 평균효과"] <-
      "관측치를 추정 CATE에 따라 네 집단으로 나눈 뒤 집단별 평균을 표시합니다."
    insights["그림 2. CATE 사분위집단별 평균효과"] <-
      paste0(
        "하위 사분위집단의 평균 CATE는 ", fmt2(q1),
        ", 상위 사분위집단은 ", fmt2(q4),
        "로 모의 이질성이 분명하게 나타납니다."
      )

    figs[["그림 3. 전체 평균 처치효과"]] <-
      coef_plot(r$coef)

    notes["그림 3. 전체 평균 처치효과"] <-
      "전체 표본에서 CATE를 평균한 ATE와 95% 신뢰구간입니다."
    insights["그림 3. 전체 평균 처치효과"] <-
      paste0(
        "전체 평균효과는 ",
        ci_text(
          r$coef$estimate[1],
          r$coef$conf.low[1],
          r$coef$conf.high[1]
        ),
        "였습니다."
      )
  }

  if (spec$method == "rd") {
    figs[["그림 1. 할당변수 분포와 기준점"]] <-
      ggplot2::ggplot(
        d,
        ggplot2::aes(
          x = running
        )
      ) +
      ggplot2::geom_histogram(
        bins = 40,
        fill = pal[["blue"]],
        alpha = .78
      ) +
      ggplot2::geom_vline(
        xintercept = spec$cutoff,
        linetype = 2,
        colour = pal[["red"]],
        linewidth = .9
      ) +
      ggplot2::labs(
        x = spec$running_label,
        y = "관측치 수"
      ) +
      kmu_theme(12)

    notes["그림 1. 할당변수 분포와 기준점"] <-
      "점선은 연구설계에서 정의한 cutoff입니다."
    insights["그림 1. 할당변수 분포와 기준점"] <-
      paste0(
        "기준점 가까운 동일 폭 구간에서 왼쪽 ",
        r$density$n[1], "개, 오른쪽 ", r$density$n[2],
        "개가 관측되었고 단순 binomial 밀도검정 p값은 ",
        fmt2(r$density_p), "였습니다."
      )

    d$x_centered <- d$running - spec$cutoff

    figs[["그림 2. 기준점 주변 결과와 국소 회귀"]] <-
      ggplot2::ggplot(
        d,
        ggplot2::aes(
          x = running,
          y = y
        )
      ) +
      ggplot2::geom_point(
        alpha = .14,
        colour = pal[["grey"]]
      ) +
      ggplot2::geom_smooth(
        data = d[d$running < spec$cutoff, ],
        method = "lm",
        formula = y ~ x,
        se = TRUE,
        colour = pal[["blue"]],
        fill = pal[["sky"]],
        linewidth = 1.05
      ) +
      ggplot2::geom_smooth(
        data = d[d$running >= spec$cutoff, ],
        method = "lm",
        formula = y ~ x,
        se = TRUE,
        colour = pal[["orange"]],
        fill = pal[["orange"]],
        linewidth = 1.05
      ) +
      ggplot2::geom_vline(
        xintercept = spec$cutoff,
        linetype = 2,
        colour = pal[["red"]]
      ) +
      ggplot2::coord_cartesian(
        xlim = c(
          spec$cutoff - r$main_bandwidth * 1.7,
          spec$cutoff + r$main_bandwidth * 1.7
        )
      ) +
      ggplot2::labs(
        x = spec$running_label,
        y = spec$outcome_label
      ) +
      kmu_theme(12)

    notes["그림 2. 기준점 주변 결과와 국소 회귀"] <-
      "기준점 좌우에 별도의 선형식을 적합했습니다. 음영은 회귀선의 불확실성을 보여줍니다."
    insights["그림 2. 기준점 주변 결과와 국소 회귀"] <-
      paste0(
        "주 bandwidth ", fmt2(r$main_bandwidth),
        "에서 ",
        ifelse(spec$rd_design == "kink", "기울기 변화", "기준점 불연속"),
        "는 ", fmt2(r$coef$estimate[1]), "로 추정되었습니다."
      )

    figs[["그림 3. Bandwidth에 따른 효과 추정치"]] <-
      ggplot2::ggplot(
        r$sensitivity,
        ggplot2::aes(
          x = bandwidth,
          y = estimate
        )
      ) +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype = 2,
        colour = pal[["grey"]]
      ) +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = conf.low,
          ymax = conf.high
        ),
        width = 0,
        colour = pal[["sky"]],
        linewidth = 1
      ) +
      ggplot2::geom_line(
        colour = pal[["navy"]],
        linewidth = .8
      ) +
      ggplot2::geom_point(
        size = 3.2,
        shape = 21,
        fill = pal[["orange"]],
        colour = "white"
      ) +
      ggplot2::labs(
        x = "Bandwidth",
        y = ifelse(
          spec$rd_design == "kink",
          "기울기 변화 추정치",
          "불연속 효과 추정치"
        )
      ) +
      kmu_theme(12)

    erange <- range(
      r$sensitivity$estimate,
      na.rm = TRUE
    )

    notes["그림 3. Bandwidth에 따른 효과 추정치"] <-
      "각 점은 다른 국소 bandwidth에서 다시 적합한 효과 추정치이며 세로선은 95% 신뢰구간입니다."
    insights["그림 3. Bandwidth에 따른 효과 추정치"] <-
      paste0(
        "검토한 bandwidth들에서 추정치는 ",
        fmt2(erange[1]), "에서 ", fmt2(erange[2]),
        " 사이였습니다. 즉 하나의 bandwidth에서만 생기는 결과인지 직접 비교할 수 있습니다."
      )

    figs[["그림 4. 주 RD/RKD 추정치"]] <-
      coef_plot(r$coef)

    notes["그림 4. 주 RD/RKD 추정치"] <-
      "원은 주 bandwidth의 점추정치, 가로선은 95% 신뢰구간입니다."
    insights["그림 4. 주 RD/RKD 추정치"] <-
      paste0(
        "주 추정치는 ",
        ci_text(
          r$coef$estimate[1],
          r$coef$conf.low[1],
          r$coef$conf.high[1]
        ),
        "였습니다. 이 값은 전체 연령대의 평균효과가 아니라 cutoff 주변의 국소적 효과입니다."
      )
  }

  set_fig_meta(
    figs,
    notes,
    insights
  )
}

make_supplement_figures <- function(analysis, spec) {
  d <- analysis$data
  r <- analysis$results
  pal <- kmu_palette()

  figs <- list()
  notes <- character()
  insights <- character()

  if (spec$method == "prediction") {
    if (spec$outcome_type == "binary") {
      figs[["그림 S1. 독립 test 자료의 calibration"]] <-
        ggplot2::ggplot(
          r$calibration,
          ggplot2::aes(
            x = predicted,
            y = observed
          )
        ) +
        ggplot2::geom_abline(
          slope = 1,
          intercept = 0,
          linetype = 2,
          colour = pal[["grey"]]
        ) +
        ggplot2::geom_line(
          colour = pal[["blue"]]
        ) +
        ggplot2::geom_point(
          size = 3,
          fill = pal[["orange"]],
          shape = 21,
          colour = "white"
        ) +
        ggplot2::coord_equal(
          xlim = c(0, 1),
          ylim = c(0, 1)
        ) +
        ggplot2::labs(
          x = "평균 예측확률",
          y = "실제 발생비율"
        ) +
        kmu_theme(11)

      notes["그림 S1. 독립 test 자료의 calibration"] <-
        "10개 위험구간에서 평균 예측확률과 실제 발생비율을 비교합니다."
      insights["그림 S1. 독립 test 자료의 calibration"] <-
        paste0(
          "Brier score는 ", fmt2(r$brier),
          "였습니다. 점들이 대각선에 가까울수록 예측확률의 calibration이 좋다는 것을 시각적으로 확인할 수 있습니다."
        )
    } else {
      res <- data.frame(
        fitted = r$test$pred,
        residual = r$test$y - r$test$pred
      )

      figs[["그림 S1. Test residual plot"]] <-
        ggplot2::ggplot(
          res,
          ggplot2::aes(
            x = fitted,
            y = residual
          )
        ) +
        ggplot2::geom_hline(
          yintercept = 0,
          linetype = 2,
          colour = pal[["grey"]]
        ) +
        ggplot2::geom_point(
          alpha = .25,
          colour = pal[["blue"]]
        ) +
        ggplot2::labs(
          x = "예측값",
          y = "잔차"
        ) +
        kmu_theme(11)

      notes["그림 S1. Test residual plot"] <-
        "독립 test 자료에서 예측오차가 예측값 수준에 따라 체계적으로 달라지는지 보여줍니다."
      insights["그림 S1. Test residual plot"] <-
        "모의자료에서는 예측오차가 0 주변에 분포하도록 생성되어 있어 residual pattern을 읽는 예시로 사용할 수 있습니다."
    }
  }

  if (spec$method == "dml") {
    dd <- r$analysis_data
    dd$ehat <- r$ehat
    dd$group <- ifelse(
      dd$treatment == 1,
      "처치집단",
      "비교집단"
    )

    figs[["그림 S1. Cross-fitted 성향점수 overlap"]] <-
      ggplot2::ggplot(
        dd,
        ggplot2::aes(
          x = ehat,
          colour = group,
          fill = group
        )
      ) +
      ggplot2::geom_density(
        alpha = .15,
        linewidth = 1.05
      ) +
      ggplot2::scale_colour_manual(
        values = c(
          "처치집단" = pal[["orange"]],
          "비교집단" = pal[["blue"]]
        )
      ) +
      ggplot2::scale_fill_manual(
        values = c(
          "처치집단" = pal[["orange"]],
          "비교집단" = pal[["blue"]]
        )
      ) +
      ggplot2::labs(
        x = "Cross-fitted 처치확률",
        y = "밀도"
      ) +
      kmu_theme(11)

    notes["그림 S1. Cross-fitted 성향점수 overlap"] <-
      "DML의 처치 nuisance model이 만든 fold 밖 예측확률을 사용했습니다."
    insights["그림 S1. Cross-fitted 성향점수 overlap"] <-
      "두 집단의 예측 처치확률 분포가 상당 부분 겹치는 모의자료를 사용했기 때문에 잔차화가 한쪽 집단의 극단적 외삽에만 의존하지 않는 모습을 볼 수 있습니다."

    rr <- data.frame(
      d_res = dd$treatment - r$ehat,
      y_res = dd$y - r$mhat
    )

    figs[["그림 S2. Orthogonalized residual 관계"]] <-
      ggplot2::ggplot(
        rr,
        ggplot2::aes(
          x = d_res,
          y = y_res
        )
      ) +
      ggplot2::geom_point(
        alpha = .18,
        colour = pal[["blue"]]
      ) +
      ggplot2::geom_smooth(
        method = "lm",
        se = TRUE,
        colour = pal[["orange"]]
      ) +
      ggplot2::labs(
        x = "처치 잔차 D - e(X)",
        y = "결과 잔차 Y - m(X)"
      ) +
      kmu_theme(11)

    notes["그림 S2. Orthogonalized residual 관계"] <-
      "공변량으로 설명되는 부분을 각각 제거한 뒤 남은 처치 변이와 결과 변이의 관계입니다."
    insights["그림 S2. Orthogonalized residual 관계"] <-
      paste0(
        "이 잔차 관계의 기울기가 DML 효과 추정치 ",
        fmt2(r$coef$estimate[r$coef$model == "DML"]),
        "에 해당합니다."
      )
  }

  if (spec$method == "did") {
    pre <- r$event[
      r$event$event_time < 0,
      ,
      drop = FALSE
    ]

    figs[["그림 S1. 처치 이전 event-study 계수"]] <-
      ggplot2::ggplot(
        pre,
        ggplot2::aes(
          x = event_time,
          y = estimate
        )
      ) +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype = 2,
        colour = pal[["grey"]]
      ) +
      ggplot2::geom_errorbar(
        ggplot2::aes(
          ymin = conf.low,
          ymax = conf.high
        ),
        width = 0,
        colour = pal[["sky"]]
      ) +
      ggplot2::geom_point(
        size = 3.1,
        shape = 21,
        fill = pal[["orange"]],
        colour = "white"
      ) +
      ggplot2::labs(
        x = "처치 이전 상대 시점",
        y = "Placebo DID 계수"
      ) +
      kmu_theme(11)

    notes["그림 S1. 처치 이전 event-study 계수"] <-
      "정책이 아직 시행되지 않은 시점들의 계수만 따로 확대했습니다."
    insights["그림 S1. 처치 이전 event-study 계수"] <-
      paste0(
        "가장 큰 처치 이전 절대 계수는 ",
        ifelse(is.finite(r$pre_max), fmt2(r$pre_max), "계산 불가"),
        "였습니다. 모의자료에서는 정책 전 계수가 0 부근에 있도록 생성했습니다."
      )
  }

  if (spec$method == "iv") {
    fs <- r$first_coef

    figs[["그림 S1. First-stage 계수"]] <-
      coef_plot(fs)

    notes["그림 S1. First-stage 계수"] <-
      "도구변수가 처치확률을 얼마나 변화시키는지를 선형확률모형으로 요약한 계수입니다."
    insights["그림 S1. First-stage 계수"] <-
      paste0(
        "First-stage 계수는 ",
        ci_text(
          fs$estimate,
          fs$conf.low,
          fs$conf.high
        ),
        "였고 F 통계량은 ", fmt1(r$first_stage_f), "였습니다."
      )
  }

  if (spec$method == "matching") {
    bal <- r$balance

    bal_long <- rbind(
      data.frame(
        variable = bal$variable,
        stage = "조정 전",
        smd = abs(bal$before)
      ),
      data.frame(
        variable = bal$variable,
        stage = "조정 후",
        smd = abs(bal$after)
      )
    )

    figs[["그림 S1. 공변량 균형 Love plot"]] <-
      ggplot2::ggplot(
        bal_long,
        ggplot2::aes(
          x = smd,
          y = reorder(variable, smd),
          colour = stage
        )
      ) +
      ggplot2::geom_vline(
        xintercept = .10,
        linetype = 2,
        colour = pal[["red"]]
      ) +
      ggplot2::geom_point(
        size = 3
      ) +
      ggplot2::scale_colour_manual(
        values = c(
          "조정 전" = pal[["orange"]],
          "조정 후" = pal[["blue"]]
        )
      ) +
      ggplot2::labs(
        x = "절대 표준화 평균차이 |SMD|",
        y = NULL
      ) +
      kmu_theme(11)

    notes["그림 S1. 공변량 균형 Love plot"] <-
      "점선 0.10은 교육용으로 자주 사용하는 균형 점검 기준입니다."
    insights["그림 S1. 공변량 균형 Love plot"] <-
      paste0(
        "최대 |SMD|가 조정 전 ",
        fmt2(max(abs(bal$before))),
        "에서 조정 후 ",
        fmt2(max(abs(bal$after))),
        "로 감소했습니다."
      )

    if (!identical(spec$ps_method, "matching")) {
      ww <- r$adjusted$analysis_weight

      figs[["그림 S2. 성향점수 가중치 분포"]] <-
        ggplot2::ggplot(
          data.frame(weight = ww),
          ggplot2::aes(
            x = weight
          )
        ) +
        ggplot2::geom_histogram(
          bins = 35,
          fill = pal[["purple"]],
          alpha = .8
        ) +
        ggplot2::labs(
          x = "분석 가중치",
          y = "관측치 수"
        ) +
        kmu_theme(11)

      notes["그림 S2. 성향점수 가중치 분포"] <-
        "극단적으로 큰 가중치가 있는지 직접 확인하는 모의 진단입니다."
      insights["그림 S2. 성향점수 가중치 분포"] <-
        paste0(
          "가중 후 유효표본크기(ESS)는 약 ",
          fmt1(r$ess),
          "로 원래 분석표본보다 작아지는 것을 확인할 수 있습니다."
        )
    }
  }

  if (spec$method == "causal_forest") {
    ms <- r$modifier_summary

    figs[["그림 S1. 사전 관심 효과수정변수와 CATE"]] <-
      ggplot2::ggplot(
        ms,
        ggplot2::aes(
          x = level,
          y = cate_hat,
          colour = modifier,
          group = modifier
        )
      ) +
      ggplot2::geom_line(
        linewidth = 1
      ) +
      ggplot2::geom_point(
        size = 2.8
      ) +
      ggplot2::facet_wrap(
        ~ modifier,
        scales = "free_y"
      ) +
      ggplot2::labs(
        x = "효과수정변수 분위집단(낮음 → 높음)",
        y = "평균 추정 CATE"
      ) +
      kmu_theme(11) +
      ggplot2::theme(
        legend.position = "none"
      )

    notes["그림 S1. 사전 관심 효과수정변수와 CATE"] <-
      "각 효과수정변수를 다섯 구간으로 나누고 구간별 평균 CATE를 표시했습니다."
    insights["그림 S1. 사전 관심 효과수정변수와 CATE"] <-
      "평균효과 하나만 볼 때 보이지 않던 효과의 방향과 크기 변화를 사전에 관심을 둔 변수 축에서 직접 확인할 수 있습니다."

    topimp <- head(r$importance, 10)

    figs[["그림 S2. CATE 이질성과 관련된 변수 순위"]] <-
      ggplot2::ggplot(
        topimp,
        ggplot2::aes(
          x = importance,
          y = reorder(variable, importance)
        )
      ) +
      ggplot2::geom_col(
        fill = pal[["teal"]]
      ) +
      ggplot2::labs(
        x = "CATE와의 상대적 연관도",
        y = NULL
      ) +
      kmu_theme(11)

    notes["그림 S2. CATE 이질성과 관련된 변수 순위"] <-
      "모의 CATE와 각 사전 공변량의 절대 상관을 교육용 중요도 지표로 사용했습니다."
    insights["그림 S2. CATE 이질성과 관련된 변수 순위"] <-
      paste0(
        "가장 높은 이질성 관련 지표를 보인 변수는 ",
        topimp$variable[1], "였습니다."
      )
  }

  if (spec$method == "rd") {
    pc <- r$placebo

    placebo_df <- data.frame(
      model = "사전 공변량 placebo",
      estimate = pc$estimate,
      conf.low = pc$conf.low,
      conf.high = pc$conf.high
    )

    figs[["그림 S1. 기준점에서 사전 공변량 placebo"]] <-
      coef_plot(placebo_df)

    notes["그림 S1. 기준점에서 사전 공변량 placebo"] <-
      "처치 이전에 결정되어야 하는 가상 공변량을 결과로 바꾸어 동일한 RD를 다시 적합했습니다."
    insights["그림 S1. 기준점에서 사전 공변량 placebo"] <-
      paste0(
        "사전 공변량의 기준점 불연속은 ",
        ci_text(
          pc$estimate,
          pc$conf.low,
          pc$conf.high
        ),
        "였습니다. 모의자료에서는 cutoff에서 이 공변량이 점프하지 않도록 생성했습니다."
      )

    sens2 <- rbind(
      data.frame(
        model = "주 분석",
        estimate = r$coef$estimate[1],
        conf.low = r$coef$conf.low[1],
        conf.high = r$coef$conf.high[1]
      ),
      data.frame(
        model = "Donut RD",
        estimate = r$donut$estimate,
        conf.low = r$donut$conf.low,
        conf.high = r$donut$conf.high
      )
    )

    figs[["그림 S2. 기준점 바로 주변을 제외한 Donut 민감도"]] <-
      coef_plot(sens2)

    notes["그림 S2. 기준점 바로 주변을 제외한 Donut 민감도"] <-
      "cutoff에 가장 가까운 작은 구간을 제외하고 같은 모형을 다시 적합했습니다."
    insights["그림 S2. 기준점 바로 주변을 제외한 Donut 민감도"] <-
      paste0(
        "주 추정치 ", fmt2(r$coef$estimate[1]),
        "와 donut 추정치 ", fmt2(r$donut$estimate),
        "를 나란히 보면서 기준점 바로 주변 관측치에 결과가 과도하게 의존하는지 확인할 수 있습니다."
      )
  }

  set_fig_meta(
    figs,
    notes,
    insights
  )
}

render_figure_list <- function(figs) {
  ensure_packages()
  if (!length(figs)) return(invisible(NULL))

  notes <- attr(figs, "notes") %||% character()
  insights <- attr(figs, "insights") %||% character()

  if (knitr::is_html_output()) {
    blocks <- lapply(
      names(figs),
      function(nm) {
        widget <- plotly::ggplotly(
          figs[[nm]],
          tooltip = "text"
        )

        widget <- plotly::config(
          widget,
          displaylogo = FALSE,
          responsive = TRUE
        )

        note <- if (nm %in% names(notes)) notes[[nm]] else ""
        insight <- if (nm %in% names(insights)) insights[[nm]] else ""

        htmltools::tagList(
          widget,
          htmltools::tags$p(
            style = "margin-top:.45rem;margin-bottom:.15rem;",
            htmltools::tags$strong(nm)
          ),
          if (nzchar(insight)) {
            htmltools::tags$div(
              style = paste0(
                "margin:.25rem 0 .35rem 0;padding:.65rem .8rem;",
                "background:#f6f8fc;border-left:3px solid #2F6BFF;",
                "border-radius:4px;"
              ),
              htmltools::tags$strong("해석. "),
              insight
            )
          },
          if (nzchar(note)) {
            htmltools::tags$p(
              style = "font-size:.90rem;color:#5f6b7a;margin-top:.2rem;",
              htmltools::tags$strong("주. "),
              note
            )
          },
          htmltools::tags$div(
            style = "height:1rem;"
          )
        )
      }
    )

    return(
      htmltools::tagList(blocks)
    )
  }

  for (nm in names(figs)) {
    print(figs[[nm]])
    cat("\n\n**", nm, "**\n\n", sep = "")

    if (nm %in% names(insights) && nzchar(insights[[nm]])) {
      cat("**해석.** ", insights[[nm]], "\n\n", sep = "")
    }

    if (nm %in% names(notes) && nzchar(notes[[nm]])) {
      cat("**주.** ", notes[[nm]], "\n\n", sep = "")
    }
  }

  invisible(NULL)
}

main_results_text <- function(analysis, spec) {
  r <- analysis$results

  intro <- paste0(
    "아래 결과는 입력한 연구설계를 **모의자료로 실제 구현한 교육용 분석 결과**입니다. ",
    "수치는 실제 연구결과가 아니며, 실제 논문에서 어떤 결과와 진단을 어떤 순서로 확인할지를 미리 보는 데 목적이 있습니다.\n\n"
  )

  body <- switch(
    spec$method,
    prediction = {
      alg <- aggregate(
        score ~ algorithm,
        r$cv,
        mean
      )
      best <- alg[which.max(alg$score), ]

      if (spec$outcome_type == "binary") {
        paste0(
          "### 핵심 결과\n\n",
          "5-fold cross-validation에서 평균 성능이 가장 높았던 알고리즘은 **",
          best$algorithm, "**였고 평균 ", unique(r$cv$metric)[1],
          "는 **", fmt2(best$score), "**였습니다. 가장 높은 평균 성능을 보인 개별 튜닝 조합은 **",
          r$tuning$algorithm[1], " / ", r$tuning$setting[1],
          "**이었습니다.\n\n",
          "별도로 만든 모의 test 자료에서는 **AUROC = ", fmt2(r$auc),
          "**, **Brier score = ", fmt2(r$brier),
          "**로 나타났습니다. 따라서 교차검증에서 가장 좋아 보인 설정이 최종 test 자료에서도 어느 정도 판별력을 유지하는지와, ",
          "예측확률 자체가 실제 발생비율과 맞는지를 각각 ROC와 calibration 그림에서 확인할 수 있습니다.\n\n",
          if (nrow(r$importance)) paste0(
            "예측변수 중요도에서는 **",
            r$importance$variable[which.max(r$importance$importance)],
            "**가 가장 높게 나타났습니다. 이 중요도는 예측에 기여한 정도이지 해당 변수가 결과의 원인이라는 의미는 아닙니다.\n\n"
          ) else ""
        )
      } else {
        paste0(
          "### 핵심 결과\n\n",
          "교차검증 평균 성능이 가장 높았던 알고리즘은 **",
          best$algorithm, "**였습니다. 별도 모의 test 자료에서 **RMSE = ",
          fmt2(r$rmse), "**, **R² = ", fmt2(r$r2),
          "**로 나타났습니다. 관측값-예측값 그림에서 대각선 주변에 얼마나 밀집하는지를 보면 이 수치가 실제 예측오차의 크기로 어떻게 보이는지 연결해서 볼 수 있습니다.\n\n"
        )
      }
    },
    dml = {
      ols <- r$coef[r$coef$model == "선형 OLS", ][1, ]
      dm <- r$coef[r$coef$model == "DML", ][1, ]

      paste0(
        "### 핵심 결과\n\n",
        "동일한 모의자료에서 선형 OLS는 **",
        ci_text(ols$estimate, ols$conf.low, ols$conf.high),
        "**, cross-fitted DML은 **",
        ci_text(dm$estimate, dm$conf.low, dm$conf.high),
        "**로 추정되었습니다. 이 모의자료의 평균 참 처치효과는 **",
        fmt2(r$truth), "**로 설정되어 있습니다.\n\n",
        "비선형 교란을 선형항만으로 조정한 OLS와 달리 DML에서는 outcome nuisance model과 treatment nuisance model에 ",
        "제곱항·비선형항을 포함하고 fold 밖 예측으로 잔차를 만들었습니다. 따라서 그림 1에서는 두 추정치가 참 효과에 얼마나 가까운지, ",
        "그림 2에서는 그 잔차화를 가능하게 한 nuisance prediction이 fold마다 어느 정도 작동했는지를 함께 볼 수 있습니다.\n\n",
        "DML 추정치에 대해서는 ", sig_text(dm$conf.low, dm$conf.high), "\n\n"
      )
    },
    did = {
      z <- r$coef[1, ]
      paste0(
        "### 핵심 결과\n\n",
        "정책 도입 이후 평균 DID 효과는 **",
        ci_text(z$estimate, z$conf.low, z$conf.high),
        "**로 나타났습니다. ",
        sig_text(z$conf.low, z$conf.high),
        "\n\n",
        "이 효과를 바로 읽기 전에 그림 1에서 원자료의 처치 전 추세를 먼저 보고, 그림 2에서 처치 이전 event-study 계수가 0 주변에 있는지 확인할 수 있습니다. ",
        "모의자료에서 처치 이전 계수의 최대 절대값은 **",
        ifelse(is.finite(r$pre_max), fmt2(r$pre_max), "계산 불가"),
        "**였습니다. 정책 도입 이후에는 시간이 흐르면서 효과가 커지도록 자료를 만들었기 때문에 post-treatment 계수가 점차 커지는 모습을 볼 수 있습니다.\n\n"
      )
    },
    iv = {
      ols <- r$coef[1, ]
      iv <- r$coef[2, ]
      rf <- r$reduced_form

      paste0(
        "### 핵심 결과\n\n",
        "First stage에서 ", spec$instrument_label,
        "의 계수는 **", ci_text(
          r$first_coef$estimate,
          r$first_coef$conf.low,
          r$first_coef$conf.high
        ), "**였고 F 통계량은 **", fmt1(r$first_stage_f),
        "**였습니다. Reduced-form 계수는 **",
        ci_text(rf$estimate, rf$conf.low, rf$conf.high),
        "**였습니다.\n\n",
        "같은 자료에서 OLS는 **",
        ci_text(ols$estimate, ols$conf.low, ols$conf.high),
        "**, IV/2SLS는 **",
        ci_text(iv$estimate, iv$conf.low, iv$conf.high),
        "**로 나타났습니다. 이 모의자료에는 처치 참여와 결과에 동시에 영향을 주는 잠재 교란을 넣었기 때문에 단순 OLS와 IV가 다른 값을 보이도록 설계했습니다. ",
        "학생은 first stage → reduced form → 2SLS 순서가 실제 결과물에서 어떻게 연결되는지 그림으로 확인할 수 있습니다.\n\n"
      )
    },
    matching = {
      raw <- r$coef[1, ]
      adj <- r$coef[2, ]

      paste0(
        "### 핵심 결과\n\n",
        "조정 전 처치집단-비교집단 평균차이는 **",
        ci_text(raw$estimate, raw$conf.low, raw$conf.high),
        "**였고, ", ifelse(spec$ps_method == "matching", "성향점수 매칭", "성향점수 가중치"),
        " 후 ", spec$estimand, " 추정치는 **",
        ci_text(adj$estimate, adj$conf.low, adj$conf.high),
        "**로 변했습니다.\n\n",
        "두 집단 성향점수의 공통 범위에는 전체 분석표본의 약 **",
        pct1(r$overlap_share),
        "**가 포함되었습니다. 부록 Love plot에서는 최대 |SMD|가 **",
        fmt2(max(abs(r$balance$before))),
        "**에서 **", fmt2(max(abs(r$balance$after))),
        "**로 감소하는 모습을 직접 확인할 수 있습니다. 즉 효과 추정치만 보는 것이 아니라 실제로 비교집단이 얼마나 비슷해졌는지를 함께 볼 수 있도록 구성했습니다.\n\n"
      )
    },
    causal_forest = {
      z <- r$coef[1, ]
      q1 <- r$groups$cate_hat[1]
      q4 <- r$groups$cate_hat[nrow(r$groups)]

      paste0(
        "### 핵심 결과\n\n",
        "전체 평균 처치효과는 **",
        ci_text(z$estimate, z$conf.low, z$conf.high),
        "**였습니다. 그러나 추정 CATE로 표본을 나누면 하위 사분위집단의 평균효과는 **",
        fmt2(q1), "**, 상위 사분위집단은 **", fmt2(q4),
        "**로 나타났습니다.\n\n",
        "즉 동일한 평균효과 아래에서도 개인별 효과가 상당히 다르게 보일 수 있다는 상황을 모의한 것입니다. ",
        "부록에서는 사전에 관심 변수로 지정한 효과수정변수를 낮은 값에서 높은 값으로 나누어 평균 CATE가 어떻게 이동하는지 직접 보여줍니다. ",
        "이렇게 하면 단순히 '이질성을 확인해야 한다'고 적는 대신 어떤 모양의 결과가 연구에서 나올 수 있는지 먼저 볼 수 있습니다.\n\n"
      )
    },
    rd = {
      z <- r$coef[1, ]
      erange <- range(
        r$sensitivity$estimate,
        na.rm = TRUE
      )

      paste0(
        "### 핵심 결과\n\n",
        "주 bandwidth **", fmt2(r$main_bandwidth),
        "**에서 ", ifelse(spec$rd_design == "kink", "기울기 변화", "기준점 불연속 효과"),
        "는 **", ci_text(z$estimate, z$conf.low, z$conf.high),
        "**로 추정되었습니다. ", sig_text(z$conf.low, z$conf.high),
        "\n\n",
        "여러 bandwidth로 다시 추정했을 때 효과는 **",
        fmt2(erange[1]), " ~ ", fmt2(erange[2]),
        "** 범위였습니다. 기준점 가까운 동일 폭 구간의 관측치는 왼쪽 **",
        r$density$n[1], "개**, 오른쪽 **", r$density$n[2],
        "개**였고 단순 밀도 대칭 검정의 p값은 **", fmt2(r$density_p),
        "**였습니다.\n\n",
        "부록에서는 결과변수가 아니라 처치 이전 가상 공변량을 놓고 같은 cutoff에서 불연속이 생기는지 직접 추정하고, ",
        "cutoff에 가장 가까운 관측치를 제외한 donut specification도 다시 적합합니다. 따라서 학생은 주 RD 그림뿐 아니라 ",
        "왜 이런 진단이 필요한지를 실제 모의 결과의 모양과 숫자로 연결해서 볼 수 있습니다.\n\n"
      )
    }
  )

  paste0(
    intro,
    body
  )
}

interpretation_text <- main_results_text

supplement_intro_text <- function(spec) {
  analysis <- get0(
    "analysis",
    envir = parent.frame(),
    inherits = TRUE
  )

  if (is.null(analysis)) {
    return(
      "### 추가 진단\n\n아래 부록 그림은 주 분석의 식별 또는 예측 진단을 모의자료에서 직접 계산한 결과입니다.\n\n"
    )
  }

  r <- analysis$results

  txt <- switch(
    spec$method,
    prediction = if (spec$outcome_type == "binary") {
      paste0(
        "주 결과의 AUROC만으로는 예측확률이 실제 위험과 맞는지 알 수 없습니다. 아래 calibration 그림은 같은 모의 test 자료에서 직접 계산했으며 Brier score는 **",
        fmt2(r$brier), "**입니다."
      )
    } else {
      "아래 residual plot은 같은 모의 test 자료의 관측값-예측값 차이를 직접 표시합니다."
    },
    dml = paste0(
      "아래에서는 cross-fitting에서 실제 생성된 처치확률의 overlap과 orthogonalized residual 관계를 보여줍니다. DML 점추정치가 어떤 잔차 관계에서 나오는지 직접 연결해 볼 수 있습니다."
    ),
    did = paste0(
      "주 event-study에서 처치 이전 시점만 떼어 확대했습니다. 모의자료의 최대 사전 절대계수는 **",
      ifelse(is.finite(r$pre_max), fmt2(r$pre_max), "계산 불가"),
      "**입니다."
    ),
    iv = paste0(
      "First-stage F 통계량 **", fmt1(r$first_stage_f),
      "**가 어떤 first-stage 계수와 신뢰구간에서 나온 것인지 아래에서 직접 확인합니다."
    ),
    matching = paste0(
      "조정 후 효과를 해석하기 전에 같은 모의자료에서 공변량 균형이 실제로 얼마나 달라졌는지 Love plot으로 확인합니다. 최대 |SMD|는 ",
      fmt2(max(abs(r$balance$before))), "에서 ",
      fmt2(max(abs(r$balance$after))), "로 변했습니다."
    ),
    causal_forest = "평균 CATE만 요약하지 않고, 사전에 관심을 둔 효과수정변수의 낮은 값부터 높은 값까지 평균 CATE가 어떻게 달라지는지 모의자료에서 직접 계산합니다.",
    rd = paste0(
      "주 RD 추정치 외에 동일 모의자료에서 사전 공변량 placebo와 donut specification을 실제로 다시 적합했습니다. 주 추정치 ",
      fmt2(r$coef$estimate[1]), "와 donut 추정치 ",
      fmt2(r$donut$estimate), "를 비교할 수 있습니다."
    )
  )

  paste0(
    "### 추가 진단\n\n",
    txt,
    "\n\n"
  )
}

supplement_checklist_text <- supplement_intro_text
