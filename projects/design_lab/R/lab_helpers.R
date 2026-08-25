# KAPAE 2026 Research Design Lab helpers
# Student-facing functions for generating research-design QMD files,
# simulating illustrative data/results, rendering figures, and submitting work.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || identical(x, "")) y else x

kapae_repo_raw <- function(path = "") {
  paste0(
    "https://raw.githubusercontent.com/TheYongjinChoi/kapae2026-exercise/main/",
    path
  )
}

sanitize_id <- function(x) {
  x <- trimws(as.character(x))
  if (!grepl("^[A-Za-z0-9_-]{3,30}$", x)) {
    stop("student_id는 영문자, 숫자, _ 또는 -만 사용해 3~30자로 입력하세요.")
  }
  x
}

check_method <- function(method) {
  allowed <- c("prediction", "dml", "did", "iv", "matching", "causal_forest", "rd")
  method <- trimws(tolower(method))
  if (!method %in% allowed) {
    stop(
      "method는 다음 중 하나여야 합니다: ",
      paste(allowed, collapse = ", ")
    )
  }
  method
}


method_label <- function(method) {
  c(
    prediction = "머신러닝 예측",
    dml = "이중/디바이어스드 머신러닝(DML)",
    did = "차분의 차분법(DID)",
    iv = "도구변수(IV)",
    matching = "성향점수 매칭/가중치",
    causal_forest = "인과 포리스트",
    rd = "회귀불연속설계(RD/RKD)"
  )[[method]]
}


method_input_block <- function(method) {
  blocks <- list(
    prediction = c(
      'outcome_var <- "outcome"', 'outcome_label <- "결과변수 레이블"',
      'outcome_type <- "binary"', 'outcome_range <- c(0, 1)', '',
      'covariate_names <- c("x1", "x2", "x3")',
      'covariate_labels <- c("예측변수 1", "예측변수 2", "예측변수 3")', '',
      'n_obs <- 1200', 'n_continuous_predictors <- 8',
      'n_binary_predictors <- 4', 'n_categorical_predictors <- 2', '',
      'has_independent_test <- TRUE',
      'prediction_use <- "예측값을 실제 연구 또는 정책에서 어떻게 사용할지 1~2문장으로 적으세요."'
    ),
    dml = c(
      'outcome_var <- "outcome"', 'outcome_label <- "결과변수 레이블"',
      'treatment_var <- "treatment"', 'treatment_label <- "처치/노출 변수 레이블"',
      'treatment_type <- "binary"', 'outcome_type <- "continuous"',
      'outcome_range <- c(0, 100)', '',
      'covariate_names <- c("x1", "x2", "x3")',
      'covariate_labels <- c("교란변수 1", "교란변수 2", "교란변수 3")',
      'confounding_set_justification <- "이 변수들이 처치와 결과 모두에 관련된 교란변수라고 판단한 이유를 1~3문장으로 적으세요."', '',
      'n_obs <- 1500', 'n_continuous_predictors <- 8',
      'n_binary_predictors <- 4', 'n_categorical_predictors <- 2',
      'n_folds <- 5'
    ),
    did = c(
      'did_design <- "standard"', '',
      'outcome_var <- "outcome"', 'outcome_label <- "결과변수 레이블"',
      'unit_id_var <- "unit_id"', 'unit_id_label <- "분석 단위"',
      'time_var <- "time"', 'time_label <- "시간"',
      'treatment_group_var <- "treated"', 'treatment_group_label <- "처치집단 구분 변수"', '',
      'treated_definition <- "어떤 관측치가 처치집단인지 구체적으로 적으세요."',
      'control_definition <- "어떤 관측치가 비교집단인지 구체적으로 적으세요."', '',
      'n_units <- 400', 'n_periods <- 8',
      'time_values <- c("T1","T2","T3","T4","T5","T6","T7","T8")',
      'treatment_start <- 5', 'treatment_start_label <- "T5"',
      'treated_share <- 0.45', '',
      'cohort_var <- "first_treated_period"', 'cohort_label <- "최초 처치시점"',
      'outcome_type <- "continuous"', 'outcome_range <- c(0, 100)',
      'n_continuous_predictors <- 4', 'n_binary_predictors <- 2', 'n_categorical_predictors <- 1'
    ),
    iv = c(
      'outcome_var <- "outcome"', 'outcome_label <- "결과변수 레이블"',
      'treatment_var <- "treatment"', 'treatment_label <- "내생적 처치/노출 레이블"',
      'instrument_var <- "instrument"', 'instrument_label <- "도구변수 레이블"',
      'instrument_justification <- "도구변수가 처치에는 영향을 주지만 결과에는 직접 영향을 주지 않는다고 볼 근거를 1~3문장으로 적으세요."', '',
      'n_obs <- 1800', 'outcome_type <- "continuous"', 'outcome_range <- c(0, 100)',
      'n_continuous_predictors <- 6', 'n_binary_predictors <- 3',
      'n_categorical_predictors <- 1', 'instrument_strength <- 0.35'
    ),
    matching = c(
      'ps_method <- "matching"', '',
      'outcome_var <- "outcome"', 'outcome_label <- "결과변수 레이블"',
      'treatment_var <- "treatment"', 'treatment_label <- "처치/노출 레이블"',
      'covariate_names <- c("x1", "x2", "x3")',
      'covariate_labels <- c("공변량 1", "공변량 2", "공변량 3")',
      'covariate_timing_note <- "위 공변량들이 처치 이전에 측정되었거나 처치에 의해 영향을 받지 않았는지 확인해 적으세요."', '',
      'estimand <- "ATE"', 'caliper <- 0.2',
      'n_obs <- 1600', 'outcome_type <- "continuous"', 'outcome_range <- c(0, 100)',
      'n_continuous_predictors <- 8', 'n_binary_predictors <- 4', 'n_categorical_predictors <- 2'
    ),
    causal_forest = c(
      'outcome_var <- "outcome"', 'outcome_label <- "결과변수 레이블"',
      'treatment_var <- "treatment"', 'treatment_label <- "처치/노출 레이블"',
      'covariate_names <- c("x1", "x2", "x3")',
      'covariate_labels <- c("공변량 1", "공변량 2", "공변량 3")',
      'modifier_names <- c("x1", "x2")',
      'modifier_labels <- c("사전에 관심 있는 효과수정변수 1", "효과수정변수 2")',
      'heterogeneity_rationale <- "왜 이 변수들에서 처치효과 이질성이 예상되는지 1~3문장으로 적으세요."', '',
      'n_obs <- 2000', 'outcome_type <- "continuous"', 'outcome_range <- c(0, 100)',
      'n_continuous_predictors <- 10', 'n_binary_predictors <- 4', 'n_categorical_predictors <- 2',
      'num_trees <- 2000', 'honesty <- TRUE'
    ),
    rd = c(
      'rd_design <- "discontinuity"', '',
      'outcome_var <- "outcome"', 'outcome_label <- "결과변수 레이블"',
      'running_var <- "running_variable"', 'running_label <- "할당변수 레이블"',
      'cutoff <- 0', 'cutoff_label <- "정책/처치 기준점"',
      'assignment_rule <- "기준점에서 처치 여부 또는 처치 강도가 어떻게 달라지는지 1~3문장으로 적으세요."', '',
      'bandwidth <- 0.45', 'n_obs <- 1800',
      'outcome_type <- "continuous"', 'outcome_range <- c(0, 100)',
      'n_continuous_predictors <- 3', 'n_binary_predictors <- 2', 'n_categorical_predictors <- 1'
    )
  )
  paste(blocks[[method]], collapse = "\\n")
}


create_design_qmd <- function(student_id,
                              method,
                              output_dir = getwd(),
                              overwrite = FALSE) {
  student_id <- sanitize_id(student_id)
  method <- check_method(method)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  filename <- sprintf("02_%s_%s_research_design.qmd", student_id, method)
  path <- file.path(output_dir, filename)
  if (file.exists(path) && !isTRUE(overwrite)) {
    stop("이미 파일이 있습니다: ", path, "\\noverwrite = TRUE로 다시 생성할 수 있습니다.")
  }

  txt <- sprintf('---
title: "연구설계 실습: %s"
author: "%s"
student-id: "%s"
design-method: "%s"
format:
  html:
    toc: true
    embed-resources: true
  docx:
    toc: true
  pdf:
    toc: true
  revealjs:
    slide-number: true
    scrollable: true
execute:
  warning: false
  message: false
---

```{r}
#| label: setup
#| include: false
source("%s")
student_id <- "%s"
method <- "%s"
```

# 학생 입력 1. 연구 배경

각 항목은 **1~3문장**으로 짧게 작성하세요. 수업 마지막 약 **30분 동안** 서로의 연구설계를 함께 살펴보고 토론합니다.

```{r}
#| label: research-background
#| include: false
research_title <- "연구 제목을 입력하세요"
theoretical_background <- "이론적 또는 정책적 배경을 1~3문장으로 적으세요."
research_need <- "왜 이 연구가 필요한지 1~3문장으로 적으세요."
research_question <- "핵심 연구질문을 1~2문장으로 적으세요."
expected_contribution <- "학술적 또는 정책적 기여를 1~3문장으로 적으세요."
```

# 학생 입력 2. 방법론과 데이터 구조

실제 연구에 사용할 데이터를 직접 확인하거나 구체적으로 떠올리며 아래 질문에 답하세요. 이 과정 자체가 **현재 데이터가 선택한 방법론에 적절한지 확인하는 절차**입니다. 필수 질문에 답하기 어렵다면 다른 방법론이 더 적절할 수 있습니다.

```{r}
#| label: method-data-inputs
#| include: false
data_name <- "실제 데이터셋 이름 또는 출처"
unit_of_observation <- "관측단위를 적으세요"
data_description <- "자료의 설계, 조사/관측 시점, 반복측정 여부 등 핵심 구조를 1~3문장으로 적으세요."

%s

max_missing_rate <- 0.08
seed <- 2026
submission_endpoint <- ""
course_key <- ""

spec <- build_spec_from_environment(environment())
```

# 1. 연구 배경과 연구질문

```{r}
#| results: asis
cat(project_overview_text(spec))
```

# 2. 데이터와 방법론 적합성

```{r}
#| results: asis
cat(data_structure_text(spec))
cat(data_compatibility_text(spec))
```

# 3. 분석 방법

```{r}
#| results: asis
cat(methods_text(spec))
```

```{r}
#| label: simulate
#| include: false
analysis <- simulate_project(spec)
```

# 4. 주요 분석 결과

```{r}
#| results: asis
cat(main_results_text(analysis, spec))
```

```{r}
#| label: main-results
main_figures <- make_main_figures(analysis, spec)
render_figure_list(main_figures)
```

# 5. 부록 결과

## 부록 1. 변수별 결측치

```{r}
knitr::kable(
  analysis$missingness,
  digits = 2,
  caption = "표 S1. 변수별 결측치 현황",
  col.names = c("변수명", "결측치 수", "결측치 비율(%%)")
)
```

```{r}
#| results: asis
cat(supplement_intro_text(spec))
```

```{r}
supp_figures <- make_supplement_figures(analysis, spec)
render_figure_list(supp_figures)
```

```{r}
#| include: false
#| eval: false
render_project_outputs(knitr::current_input())
```

```{r}
#| include: false
#| eval: false
submit_project(
  qmd_path = knitr::current_input(),
  endpoint = submission_endpoint,
  course_key = course_key
)
```
',
  method_label(method), student_id, student_id, method,
  kapae_repo_raw("projects/design_lab/R/lab_helpers.R"),
  student_id, method, method_input_block(method)
  )

  writeLines(txt, path, useBytes = TRUE)
  message("생성 완료: ", normalizePath(path, winslash = "/", mustWork = FALSE))
  invisible(path)
}

build_spec_from_environment <- function(env = parent.frame()) {
  getv <- function(name, default = NULL) {
    if (exists(name, envir = env, inherits = FALSE)) get(name, envir = env) else default
  }
  method <- check_method(getv("method"))
  outcome_type <- getv("outcome_type", "continuous")
  if (!outcome_type %in% c("continuous", "binary")) {
    stop('outcome_type은 "continuous" 또는 "binary"여야 합니다.')
  }

  spec <- list(
    student_id = sanitize_id(getv("student_id")),
    method = method,
    research_title = getv("research_title", "Untitled study"),
    theoretical_background = getv("theoretical_background", ""),
    research_need = getv("research_need", ""),
    research_question = getv("research_question", ""),
    expected_contribution = getv("expected_contribution", ""),
    unit_of_observation = getv("unit_of_observation", ""),
    data_name = getv("data_name", "Unspecified data"),
    data_description = getv("data_description", ""),
    outcome_var = getv("outcome_var", "outcome"),
    outcome_label = getv("outcome_label", "Outcome"),
    treatment_var = getv("treatment_var", "treatment"),
    treatment_label = getv("treatment_label", "Treatment"),
    instrument_var = getv("instrument_var", "instrument"),
    instrument_label = getv("instrument_label", "Instrument"),
    running_var = getv("running_var", "running_variable"),
    running_label = getv("running_label", "Running variable"),
    covariate_names = getv("covariate_names", character()),
    covariate_labels = getv("covariate_labels", character()),
    modifier_names = getv("modifier_names", character()),
    modifier_labels = getv("modifier_labels", character()),
    did_design = getv("did_design", "standard"),
    rd_design = getv("rd_design", "discontinuity"),
    unit_id_var = getv("unit_id_var", "unit_id"),
    unit_id_label = getv("unit_id_label", "Unit"),
    time_var = getv("time_var", "time"),
    time_label = getv("time_label", "Time"),
    treatment_group_var = getv("treatment_group_var", "treated"),
    treatment_group_label = getv("treatment_group_label", "Treatment group"),
    treated_definition = getv("treated_definition", ""),
    control_definition = getv("control_definition", ""),
    time_values = getv("time_values", character()),
    treatment_start_label = getv("treatment_start_label", ""),
    cohort_var = getv("cohort_var", "first_treated_period"),
    cohort_label = getv("cohort_label", "First treatment period"),
    assignment_rule = getv("assignment_rule", ""),
    cutoff_label = getv("cutoff_label", ""),
    instrument_justification = getv("instrument_justification", ""),
    confounding_set_justification = getv("confounding_set_justification", ""),
    covariate_timing_note = getv("covariate_timing_note", ""),
    heterogeneity_rationale = getv("heterogeneity_rationale", ""),
    prediction_use = getv("prediction_use", ""),
    has_independent_test = isTRUE(getv("has_independent_test", FALSE)),
    ps_method = getv("ps_method", "matching"),
    outcome_type = outcome_type,
    outcome_range = getv("outcome_range", if (outcome_type == "binary") c(0, 1) else c(0, 100)),
    n_cont = as.integer(getv("n_continuous_predictors", 6)),
    n_bin = as.integer(getv("n_binary_predictors", 3)),
    n_cat = as.integer(getv("n_categorical_predictors", 1)),
    n = as.integer(getv("n_obs", 1500)),
    max_missing_rate = as.numeric(getv("max_missing_rate", 0.08)),
    seed = as.integer(getv("seed", 2026)),
    n_folds = as.integer(getv("n_folds", 5)),
    n_units = as.integer(getv("n_units", 400)),
    n_periods = as.integer(getv("n_periods", 8)),
    treatment_start = as.integer(getv("treatment_start", 5)),
    treated_share = as.numeric(getv("treated_share", 0.45)),
    instrument_strength = as.numeric(getv("instrument_strength", 0.35)),
    estimand = getv("estimand", "ATE"),
    caliper = as.numeric(getv("caliper", 0.2)),
    num_trees = as.integer(getv("num_trees", 2000)),
    honesty = isTRUE(getv("honesty", TRUE)),
    cutoff = as.numeric(getv("cutoff", 0)),
    bandwidth = as.numeric(getv("bandwidth", 0.45))
  )

  if (any(c(spec$n_cont, spec$n_bin, spec$n_cat) < 0)) stop("예측변수 개수는 0 이상이어야 합니다.")
  if (spec$max_missing_rate < 0 || spec$max_missing_rate > 0.2) stop("max_missing_rate는 0~0.20 범위를 권장하며 이 템플릿에서는 그 범위로 제한합니다.")
  if (method == "did" && (spec$treatment_start <= 1 || spec$treatment_start > spec$n_periods)) stop("DID의 treatment_start를 확인하세요.")
  if (method == "matching" && !spec$estimand %in% c("ATE", "ATT")) stop('estimand는 "ATE" 또는 "ATT"여야 합니다.')
  spec
}

ensure_packages <- function(extra = character()) {
  pkgs <- unique(c("ggplot2", "plotly", "tidyr", "knitr", "htmltools", extra))
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "다음 패키지가 필요합니다: ", paste(missing, collapse = ", "),
      "\n한 번만 install.packages(c(",
      paste(sprintf('"%s"', missing), collapse = ", "), "))를 실행하세요."
    )
  }
  invisible(TRUE)
}


project_overview_text <- function(spec) {
  paste0(
    "## 제안 연구\\n\\n",
    "**연구 제목.** ", spec$research_title, "  \\n",
    "**연구방법.** ", method_label(spec$method), "  \\n",
    "**데이터.** ", spec$data_name, "  \\n",
    if (nzchar(spec$unit_of_observation %||% "")) paste0("**관측단위.** ", spec$unit_of_observation, "  \\n") else "",
    "\\n### 연구 배경\\n\\n",
    if (nzchar(spec$theoretical_background %||% "")) paste0("**이론적·정책적 배경.** ", spec$theoretical_background, "\\n\\n") else "",
    if (nzchar(spec$research_need %||% "")) paste0("**연구의 필요성.** ", spec$research_need, "\\n\\n") else "",
    if (nzchar(spec$research_question %||% "")) paste0("**연구질문.** ", spec$research_question, "\\n\\n") else "",
    if (nzchar(spec$expected_contribution %||% "")) paste0("**예상 기여.** ", spec$expected_contribution, "\\n\\n") else ""
  )
}


data_structure_text <- function(spec) {
  extra <- switch(
    spec$method,
    did = sprintf("약 %d개 단위와 %d개 시점을 가정하며 처치는 %s부터 시작하도록 설정했습니다.", spec$n_units, spec$n_periods, spec$treatment_start_label %||% as.character(spec$treatment_start)),
    rd = sprintf("기준점은 %.2f이고 주 분석 bandwidth는 %.2f로 설정했습니다.", spec$cutoff, spec$bandwidth),
    iv = sprintf("교육용 가상자료의 도구변수 강도 설정값은 %.2f입니다.", spec$instrument_strength),
    matching = sprintf("목표 estimand는 %s이고 caliper 설정값은 %.2f입니다.", spec$estimand, spec$caliper),
    causal_forest = sprintf("인과 포리스트는 %d개 tree와 honesty=%s로 설정했습니다.", spec$num_trees, spec$honesty),
    dml = sprintf("cross-fitting은 %d개 fold로 설정했습니다.", spec$n_folds),
    prediction = sprintf("연속형 %d개, 이진형 %d개, 범주형 %d개 예측변수를 가정합니다.", spec$n_cont, spec$n_bin, spec$n_cat)
  )
  paste0(
    "**데이터 특성.** ", spec$data_description, "  \\n",
    "**결과변수 유형.** ", ifelse(spec$outcome_type == "binary", "이진형", "연속형"), "  \\n",
    extra, "\\n\\n"
  )
}

data_compatibility_text <- function(spec) {
  bullet <- function(ok, yes, no) {
    if (isTRUE(ok)) paste0("- ✓ ", yes, "\n") else paste0("- ⚠ ", no, "\n")
  }
  has_text <- function(x) !is.null(x) && length(x) > 0 && any(nzchar(trimws(as.character(x))))
  has_covs <- length(spec$covariate_names %||% character()) > 0

  intro <- paste0(
    "### 방법론-데이터 적합성 점검\n\n",
    "아래 항목은 입력한 답변을 바탕으로 한 **설계 점검용 안내**입니다. ",
    "경고가 있다고 해서 분석이 불가능하다는 뜻은 아니지만, 해당 질문에 답하기 어렵다면 현재 데이터가 이 방법론의 핵심 식별조건을 충족하는지 다시 확인해야 합니다.\n\n"
  )

  body <- switch(
    spec$method,
    prediction = paste0(
      bullet(has_text(spec$outcome_label), "예측할 결과가 명확히 정의되어 있습니다.", "예측할 결과변수와 실질적 의미를 명확히 정의하세요."),
      bullet(has_covs, "후보 예측변수가 정의되어 있습니다.", "실제 연구에서 사용 가능한 예측변수 목록을 확인하세요."),
      bullet(isTRUE(spec$has_independent_test), "독립적인 test/hold-out 평가 계획이 있습니다.", "가능하면 최종 성능을 확인할 독립 test set 또는 명시적인 resampling 전략을 계획하세요.")
    ),
    dml = paste0(
      bullet(has_text(spec$treatment_label), "처치/노출이 명확히 정의되어 있습니다.", "처치 또는 노출변수를 명확히 정의하세요."),
      bullet(has_covs, "측정된 교란변수 집합이 정의되어 있습니다.", "처치와 결과를 함께 설명할 교란변수 집합을 확인하세요."),
      bullet(has_text(spec$confounding_set_justification), "교란변수 선택 근거가 작성되어 있습니다.", "왜 이 변수들이 교란변수인지 인과적으로 설명할 수 있어야 합니다."),
      bullet(spec$n_folds >= 2, "cross-fitting을 위한 fold 수가 설정되어 있습니다.", "DML에는 표본분할/cross-fitting 계획이 필요합니다.")
    ),
    did = paste0(
      bullet(has_text(spec$unit_id_var) && has_text(spec$time_var), "반복 관측되는 unit과 time 변수가 정의되어 있습니다.", "DID에는 unit과 time을 구분할 수 있는 패널/반복횡단면 구조가 필요합니다."),
      bullet(has_text(spec$treated_definition) && has_text(spec$control_definition), "처치집단과 비교집단이 구체적으로 정의되어 있습니다.", "누가 treated이고 누가 comparison group인지 데이터에서 구분할 수 있어야 합니다."),
      bullet(spec$n_periods >= 4 && spec$treatment_start > 1, "처치 전후 여러 시점이 확보되어 있습니다.", "처치 이전 추세를 점검할 수 있도록 충분한 pre-treatment 시점이 필요합니다."),
      if (identical(spec$did_design, "staggered")) bullet(has_text(spec$cohort_var), "최초 처치시점(cohort) 변수가 정의되어 있습니다.", "Staggered DID에는 unit별 최초 처치시점을 식별할 수 있어야 합니다.") else ""
    ),
    iv = paste0(
      bullet(has_text(spec$instrument_label), "도구변수가 정의되어 있습니다.", "도구변수를 명확히 정의하세요."),
      bullet(has_text(spec$treatment_label), "내생적 처치/노출이 정의되어 있습니다.", "instrument가 변화시키는 처치/노출을 명확히 정의하세요."),
      bullet(has_text(spec$instrument_justification), "relevance와 exclusion restriction을 검토할 근거가 작성되어 있습니다.", "왜 instrument가 처치에는 영향을 주지만 결과에는 직접 영향을 주지 않는지 설명할 수 있어야 합니다.")
    ),
    matching = paste0(
      bullet(has_text(spec$treatment_label), "처치/노출이 정의되어 있습니다.", "매칭/가중치를 위한 처치변수가 필요합니다."),
      bullet(has_covs, "balance를 맞출 pretreatment covariates가 정의되어 있습니다.", "처치 이전 공변량 집합이 필요합니다."),
      bullet(has_text(spec$covariate_timing_note), "공변량의 측정시점에 대한 확인이 작성되어 있습니다.", "처치 이후 변수나 mediator를 propensity model에 넣지 않는지 확인하세요."),
      bullet(spec$estimand %in% c("ATE","ATT"), "estimand가 명시되어 있습니다.", "ATE 또는 ATT 등 목표 estimand를 먼저 정하세요.")
    ),
    causal_forest = paste0(
      bullet(has_text(spec$treatment_label), "처치가 정의되어 있습니다.", "CATE를 정의할 처치변수가 필요합니다."),
      bullet(has_covs, "이질성을 학습할 공변량이 정의되어 있습니다.", "효과 이질성을 학습할 pretreatment covariates가 필요합니다."),
      bullet(length(spec$modifier_names %||% character()) > 0, "사전에 관심 있는 effect modifier가 지정되어 있습니다.", "데이터 주도적 탐색과 별도로 이론적으로 중요한 effect modifier를 사전에 지정하는 것이 좋습니다."),
      bullet(isTRUE(spec$honesty), "honesty를 사용하는 설계입니다.", "가능하면 honesty 또는 이에 준하는 sample-splitting 전략을 사용하세요.")
    ),
    rd = paste0(
      bullet(has_text(spec$running_label), "running variable이 정의되어 있습니다.", "cutoff를 기준으로 하는 running variable이 필요합니다."),
      bullet(is.finite(spec$cutoff), "cutoff가 명시되어 있습니다.", "정책/배정 규칙의 cutoff를 명확히 확인하세요."),
      bullet(has_text(spec$assignment_rule), "cutoff와 처치배정 규칙의 관계가 설명되어 있습니다.", "왜 cutoff에서 처치 또는 처치강도가 달라지는지 설명해야 합니다."),
      if (identical(spec$rd_design, "kink")) "- ✓ 이 설계는 level discontinuity가 아니라 **slope change (regression kink)** 를 대상으로 합니다.\n"
      else "- ✓ 이 설계는 cutoff에서의 **level discontinuity** 를 대상으로 합니다.\n"
    )
  )

  paste0(intro, body, "\n")
}



methods_text <- function(spec) {
  specific <- switch(
    spec$method,
    prediction = "동일한 resampling fold에서 random forest, XGBoost, neural network의 예측성능을 비교합니다. 최종 평가는 tuning과 분리하며, 이진 결과에서는 ROC AUC와 calibration을 함께 확인하는 것이 핵심입니다.",
    dml = "처치모형과 결과모형을 유연한 머신러닝으로 적합하고 cross-fitting을 적용한 뒤 orthogonal score로 평균 처치효과를 추정합니다. 동일 공변량을 사용한 conventional OLS와 비교하되 OLS-DML 차이 자체를 우월성의 증거로 해석하지 않습니다.",
    did = if (identical(spec$did_design, "staggered")) "처치 도입시점이 단위마다 다른 staggered DID를 가정합니다. cohort별 최초 처치시점을 명시하고 효과 추정 전에 처치 이전 추세와 event-study를 확인합니다." else "처치집단과 비교집단의 처치 전후 변화를 비교합니다. 효과 추정 전에 raw group-time trend와 처치 이전 event-study 계수를 확인하여 parallel trends의 개연성을 점검합니다.",
    iv = "도구변수의 relevance를 first stage에서 먼저 확인하고 2SLS 추정치를 conventional OLS와 비교합니다. 인과해석에는 exclusion restriction, independence, monotonicity가 추가로 요구되며 추정대상은 일반적으로 compliers의 LATE입니다.",
    matching = "성향점수 조정 전에 common support를 확인하고 조정 후에는 결과 추정치보다 covariate balance가 충분히 개선되었는지를 먼저 평가합니다. 매칭/가중치 방식과 estimand를 사전에 명확히 합니다.",
    causal_forest = "평균효과와 함께 조건부 처치효과의 이질성을 탐색합니다. CATE 분포 자체를 실질적 이질성의 증거로 단정하지 않고 사전에 지정한 효과수정변수와 안정성을 중심으로 해석합니다.",
    rd = if (identical(spec$rd_design, "kink")) "기준점에서 결과 수준의 점프가 아니라 기울기 변화를 이용하는 regression kink design을 가정합니다. 기준점 주변 자료 분포와 국소적 관계를 확인하고 bandwidth 민감도를 평가합니다." else "기준점에서 처치상태가 불연속적으로 변하는 구조를 이용합니다. 효과 추정 전에 running variable 분포와 기준점 주변 outcome-running variable 관계를 확인하고 bandwidth 민감도를 평가합니다."
  )
  paste0(specific, "\\n\\n")
}

base_predictor_data <- function(n, spec) {
  out <- data.frame(row_id = seq_len(n))
  if (spec$n_cont > 0) {
    for (j in seq_len(spec$n_cont)) out[[paste0("x_cont_", j)]] <- rnorm(n)
  }
  if (spec$n_bin > 0) {
    for (j in seq_len(spec$n_bin)) out[[paste0("x_bin_", j)]] <- rbinom(n, 1, plogis(-0.2 + 0.15 * j))
  }
  if (spec$n_cat > 0) {
    for (j in seq_len(spec$n_cat)) out[[paste0("x_cat_", j)]] <- sample(c("A", "B", "C"), n, replace = TRUE, prob = c(.45, .35, .20))
  }
  out
}

scale_to_range <- function(x, range) {
  if (length(range) != 2 || !is.finite(diff(range)) || diff(range) <= 0) return(x)
  z <- as.numeric(scale(x))
  mid <- mean(range)
  span <- diff(range)
  pmin(range[2], pmax(range[1], mid + z * span / 6))
}

add_missingness <- function(df, spec) {
  set.seed(spec$seed + 91)
  candidate <- grep("^x_", names(df), value = TRUE)
  if (!length(candidate)) return(df)
  for (v in candidate) {
    rate <- runif(1, 0, spec$max_missing_rate)
    if (rate > 0) df[sample.int(nrow(df), max(1, round(nrow(df) * rate))), v] <- NA
  }
  df
}

make_fake_data <- function(spec) {
  set.seed(spec$seed)
  method <- spec$method

  if (method == "did") {
    units <- seq_len(spec$n_units)
    times <- seq_len(spec$n_periods)
    df <- expand.grid(id = units, time = times)
    tr_unit <- rbinom(spec$n_units, 1, spec$treated_share)
    df$treated <- tr_unit[df$id]
    df$post <- as.integer(df$time >= spec$treatment_start)
    unit_fe <- rnorm(spec$n_units, sd = 5)
    trend <- 1.2 * df$time
    dynamic_tau <- pmax(0, df$time - spec$treatment_start + 1) * 1.1
    df$y <- 45 + unit_fe[df$id] + trend + df$treated * df$post * dynamic_tau + rnorm(nrow(df), sd = 5)
    df$y <- scale_to_range(df$y, spec$outcome_range)
    return(df)
  }

  df <- base_predictor_data(spec$n, spec)
  x1 <- if ("x_cont_1" %in% names(df)) df$x_cont_1 else rnorm(spec$n)
  x2 <- if ("x_cont_2" %in% names(df)) df$x_cont_2 else rnorm(spec$n)
  x3 <- if ("x_cont_3" %in% names(df)) df$x_cont_3 else rnorm(spec$n)
  base_mu <- 0.7 * x1 - 0.55 * x2^2 + 0.35 * sin(x3 * 2)

  if (method == "prediction") {
    if (spec$outcome_type == "binary") {
      df$y <- rbinom(spec$n, 1, plogis(-0.3 + base_mu))
    } else {
      df$y <- scale_to_range(base_mu + rnorm(spec$n, sd = 0.9), spec$outcome_range)
    }
  }

  if (method %in% c("dml", "matching", "causal_forest")) {
    ps <- plogis(-0.2 + 0.8 * x1 - 0.7 * x2^2 + 0.35 * sin(2 * x3))
    df$treatment <- rbinom(spec$n, 1, ps)
    tau <- if (method == "causal_forest") 1.0 + 0.9 * (x1 > 0) - 0.5 * x3 else rep(1.4, spec$n)
    y_latent <- 2.2 * x1 - 1.7 * x2^2 + 0.9 * sin(2 * x3) + tau * df$treatment + rnorm(spec$n)
    df$true_ps <- ps
    df$true_tau <- tau
    if (spec$outcome_type == "binary") df$y <- rbinom(spec$n, 1, plogis(y_latent / 3)) else df$y <- scale_to_range(y_latent, spec$outcome_range)
  }

  if (method == "iv") {
    z <- rbinom(spec$n, 1, 0.5)
    u <- rnorm(spec$n)
    p_treat <- plogis(-0.2 + spec$instrument_strength * 3 * z + 0.7 * x1 + 0.8 * u)
    treatment <- rbinom(spec$n, 1, p_treat)
    y_latent <- 1.6 * treatment + 1.1 * x1 + 1.0 * u + rnorm(spec$n)
    df$instrument <- z
    df$treatment <- treatment
    df$y <- scale_to_range(y_latent, spec$outcome_range)
  }

  if (method == "rd") {
    running <- runif(spec$n, -1, 1)
    treatment <- as.integer(running >= spec$cutoff)
    y_latent <- 40 + 7 * running - 3 * running^2 + 7 * treatment + rnorm(spec$n, sd = 4)
    df$running <- running
    df$treatment <- treatment
    df$y <- scale_to_range(y_latent, spec$outcome_range)
  }

  add_missingness(df, spec)
}

make_missingness_table <- function(df) {
  data.frame(
    variable = names(df),
    missing_n = vapply(df, function(x) sum(is.na(x)), integer(1)),
    missing_pct = round(100 * vapply(df, function(x) mean(is.na(x)), numeric(1)), 2),
    row.names = NULL
  )
}

make_cleaning_flow <- function(df) {
  n0 <- nrow(df)
  anymiss <- apply(is.na(df), 1, any)
  n1 <- sum(!anymiss)
  data.frame(
    step = c("Raw simulated records", "Records with any predictor missing flagged", "Complete-case analysis set (illustrative)"),
    n = c(n0, sum(anymiss), n1),
    note = c("Generated from the student's declared data structure", "Do not automatically drop in a real study; define the missing-data strategy", "Shown only as a transparent workflow example")
  )
}

prediction_results <- function(spec, df) {
  set.seed(spec$seed + 10)
  folds <- seq_len(5)
  rf <- expand.grid(algorithm = "Random forest", mtry = c(2, 4, 6, 8), min_n = c(2, 5, 10), fold = folds)
  rf$setting <- paste0("mtry=", rf$mtry, ", min_n=", rf$min_n)
  xgb <- expand.grid(algorithm = "XGBoost", depth = c(2, 4, 6), learn_rate = c(.03, .1, .2), trees = c(300, 700), fold = folds)
  xgb$setting <- paste0("depth=", xgb$depth, ", lr=", xgb$learn_rate, ", trees=", xgb$trees)
  nn <- expand.grid(algorithm = "Neural network", hidden = c(8, 16, 32), penalty = c(0, .001, .01), epochs = c(50, 100), fold = folds)
  nn$setting <- paste0("hidden=", nn$hidden, ", penalty=", nn$penalty, ", epochs=", nn$epochs)
  cv <- rbind(
    rf[c("algorithm", "setting", "fold")],
    xgb[c("algorithm", "setting", "fold")],
    nn[c("algorithm", "setting", "fold")]
  )
  base <- if (spec$outcome_type == "binary") c("Random forest"=.80, "XGBoost"=.84, "Neural network"=.81) else c("Random forest"=.74, "XGBoost"=.79, "Neural network"=.76)
  score <- unname(base[cv$algorithm]) + rnorm(nrow(cv), 0, .025)
  # Give a subset of settings a clear tuning advantage.
  score <- score + ifelse(grepl("depth=4|mtry=6|hidden=16", cv$setting), .025, 0)
  cv$score <- pmin(.95, pmax(.50, score))
  cv$metric <- if (spec$outcome_type == "binary") "ROC AUC" else "1 - scaled RMSE"

  vars <- grep("^x_", names(df), value = TRUE)
  imp <- data.frame(variable = vars, importance = sort(rexp(length(vars), rate = 1), decreasing = TRUE))
  if (nrow(imp)) imp$importance <- imp$importance / max(imp$importance)
  list(cv = cv, importance = head(imp, 15))
}

coef_frame <- function(model, estimate, se, truth = NA_real_) {
  data.frame(
    model = model,
    estimate = estimate,
    conf.low = estimate - 1.96 * se,
    conf.high = estimate + 1.96 * se,
    truth = truth
  )
}

causal_results <- function(spec, df) {
  set.seed(spec$seed + 20)
  switch(
    spec$method,
    dml = {
      # Intentionally separate OLS from DML to demonstrate nonlinear-confounding bias.
      coef <- rbind(
        coef_frame("OLS", 0.82, .12, 1.40),
        coef_frame("DML", 1.36, .11, 1.40)
      )
      list(coef = coef, nuisance = data.frame(model = c("Treatment nuisance", "Outcome nuisance"), score = c(.77, .71)))
    },
    did = {
      event_time <- seq(-(spec$treatment_start - 1), spec$n_periods - spec$treatment_start)
      est <- ifelse(event_time < 0, rnorm(length(event_time), 0, .20), pmax(0, event_time + 1) * .75 + rnorm(length(event_time), 0, .18))
      se <- rep(.20, length(est))
      event <- data.frame(event_time = event_time, estimate = est, conf.low = est - 1.96 * se, conf.high = est + 1.96 * se)
      coef <- coef_frame("DID ATT", mean(est[event_time >= 0]), .16)
      list(event = event, coef = coef)
    },
    iv = {
      first <- aggregate(treatment ~ instrument, data = df, mean)
      coef <- rbind(
        coef_frame("OLS", 2.45, .13, 1.60),
        coef_frame("2SLS / IV", 1.68, .24, 1.60)
      )
      list(first = first, coef = coef, first_stage_f = 18 + 55 * spec$instrument_strength)
    },
    matching = {
      keep <- complete.cases(df)
      d <- df[keep, , drop = FALSE]
      ps_hat <- pmin(.98, pmax(.02, d$true_ps + rnorm(nrow(d), 0, .04)))
      d$ps_hat <- ps_hat
      d$matched_weight <- ifelse(d$treatment == 1, 1 / ps_hat, 1 / (1 - ps_hat))
      coef <- rbind(
        coef_frame("Unadjusted", 2.20, .16, 1.40),
        coef_frame(paste0("PS ", spec$estimand), 1.46, .13, 1.40)
      )
      vars <- grep("^x_cont_|^x_bin_", names(d), value = TRUE)
      if (!length(vars)) vars <- "row_id"
      bal <- data.frame(variable = vars)
      bal$before <- runif(length(vars), .12, .42) * sample(c(-1,1), length(vars), replace = TRUE)
      bal$after <- bal$before * runif(length(vars), .08, .28)
      list(coef = coef, matched = d, balance = bal)
    },
    causal_forest = {
      keep <- complete.cases(df)
      d <- df[keep, , drop = FALSE]
      d$cate_hat <- d$true_tau + rnorm(nrow(d), 0, .30)
      d$group <- cut(d$cate_hat, breaks = quantile(d$cate_hat, probs = seq(0,1,.25)), include.lowest = TRUE, labels = c("Q1", "Q2", "Q3", "Q4"))
      grp <- aggregate(cate_hat ~ group, d, mean)
      coef <- coef_frame("Causal forest ATE", mean(d$cate_hat), .08, mean(d$true_tau))
      list(coef = coef, cate = d, groups = grp)
    },
    rd = {
      bws <- c(.20, .30, .45, .60, .80)
      sens <- data.frame(bandwidth = bws)
      sens$estimate <- 7 + rnorm(length(bws), 0, c(.7,.5,.35,.45,.6))
      sens$se <- c(.8,.6,.45,.5,.6)
      sens$conf.low <- sens$estimate - 1.96 * sens$se
      sens$conf.high <- sens$estimate + 1.96 * sens$se
      coef <- coef_frame("Local linear RD", sens$estimate[which.min(abs(bws - spec$bandwidth))], .45, 7)
      list(coef = coef, sensitivity = sens)
    }
  )
}

simulate_project <- function(spec) {
  ensure_packages()
  df <- make_fake_data(spec)
  list(
    data = df,
    missingness = make_missingness_table(df),
    cleaning_flow = make_cleaning_flow(df),
    results = if (spec$method == "prediction") prediction_results(spec, df) else causal_results(spec, df)
  )
}


kapae_palette <- function() {
  c(
    blue="#2F6BFF", sky="#64B5F6", teal="#22A699", orange="#F28E2B",
    coral="#E76F51", purple="#7B61FF", green="#59A14F", red="#D1495B",
    navy="#23395B", grey="#7A869A", light="#E9EEF6"
  )
}

kapae_theme <- function(base_size = 12) {
  pal <- kapae_palette()
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      axis.title = ggplot2::element_text(face="bold", colour=pal[["navy"]]),
      axis.text = ggplot2::element_text(colour="#4B5563"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour="#E8EDF4", linewidth=.45),
      legend.position = "top",
      legend.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face="bold", colour=pal[["navy"]]),
      plot.margin = ggplot2::margin(10,16,10,10)
    )
}


coef_plot <- function(df, title = NULL, xlab = "추정치") {
  pal <- kapae_palette()
  d <- df
  rng <- range(c(d$conf.low, d$conf.high), na.rm = TRUE)
  pad <- max(diff(rng) * .07, .05)
  d$label_x <- d$estimate + pad
  d$sig <- ifelse(d$conf.low > 0 | d$conf.high < 0, "*", "")
  d$label <- sprintf("%.2f%s [%.2f, %.2f]", d$estimate, d$sig, d$conf.low, d$conf.high)

  ggplot2::ggplot(d, ggplot2::aes(y = reorder(model, estimate), x = estimate)) +
    ggplot2::geom_vline(xintercept = 0, linetype = 2, colour = pal[["grey"]], linewidth = .7) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = conf.low, xmax = conf.high),
      height = 0, linewidth = 1.2, colour = pal[["sky"]]
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_nudge(y = .16),
      size = 4, shape = 21, stroke = 1.1,
      fill = pal[["orange"]], colour = "white"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = label_x, label = label),
      position = ggplot2::position_nudge(y = .24),
      hjust = 0, vjust = 0, size = 3.7,
      fontface = "bold", colour = pal[["navy"]]
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(.04, .34))) +
    ggplot2::labs(x = xlab, y = NULL) +
    kapae_theme(12)
}


make_main_figures <- function(analysis, spec) {
  d <- analysis$data
  r <- analysis$results
  pal <- kapae_palette()
  figs <- list()

  if (spec$method == "prediction") {
    figs[["그림 1. 알고리즘별 교차검증 예측성능"]] <-
      ggplot2::ggplot(r$cv, ggplot2::aes(x=algorithm, y=score, fill=algorithm)) +
      ggplot2::geom_boxplot(alpha=.82, outlier.shape=NA) +
      ggplot2::geom_jitter(width=.10, alpha=.20, size=1.3) +
      ggplot2::scale_fill_manual(values=c("Random forest"=pal[["blue"]], "XGBoost"=pal[["orange"]], "Neural network"=pal[["purple"]])) +
      ggplot2::labs(x=NULL, y=unique(r$cv$metric)) + kapae_theme(12) +
      ggplot2::theme(legend.position="none")
    if (nrow(r$importance)) {
      figs[["그림 2. 예측변수 중요도"]] <-
        ggplot2::ggplot(r$importance, ggplot2::aes(x=importance, y=reorder(variable, importance))) +
        ggplot2::geom_col(fill=pal[["teal"]], width=.72) +
        ggplot2::labs(x="상대적 중요도", y=NULL) + kapae_theme(12)
    }
  }

  if (spec$method == "dml") {
    figs[["그림 1. OLS와 DML 처치효과 추정치 비교"]] <- coef_plot(r$coef)
    figs[["그림 2. nuisance model 예측성능"]] <-
      ggplot2::ggplot(r$nuisance, ggplot2::aes(x=model, y=score, fill=model)) +
      ggplot2::geom_col(width=.65) +
      ggplot2::scale_fill_manual(values=c("Treatment nuisance"=pal[["blue"]], "Outcome nuisance"=pal[["teal"]])) +
      ggplot2::coord_cartesian(ylim=c(0,1)) +
      ggplot2::labs(x=NULL, y="예측성능 지표") + kapae_theme(12) +
      ggplot2::theme(legend.position="none")
  }

  if (spec$method == "did") {
    means <- aggregate(y ~ time + treated, d, mean)
    means$group <- ifelse(means$treated == 1, "처치집단", "비교집단")
    figs[["그림 1. 처치집단과 비교집단의 시점별 평균 결과"]] <-
      ggplot2::ggplot(means, ggplot2::aes(x=time, y=y, colour=group, group=group)) +
      ggplot2::geom_line(linewidth=1.15) + ggplot2::geom_point(size=2.6) +
      ggplot2::geom_vline(xintercept=spec$treatment_start-.5, linetype=2, colour=pal[["red"]]) +
      ggplot2::scale_colour_manual(values=c("처치집단"=pal[["orange"]], "비교집단"=pal[["blue"]])) +
      ggplot2::labs(x=spec$time_label %||% "시점", y=spec$outcome_label %||% "평균 결과") + kapae_theme(12)
    figs[["그림 2. Event-study 추정치"]] <-
      ggplot2::ggplot(r$event, ggplot2::aes(x=event_time, y=estimate)) +
      ggplot2::geom_hline(yintercept=0, linetype=2, colour=pal[["grey"]]) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin=conf.low, ymax=conf.high), width=0, linewidth=1.05, colour=pal[["sky"]]) +
      ggplot2::geom_point(position=ggplot2::position_nudge(x=.10), size=3.3, shape=21, fill=pal[["orange"]], colour="white", stroke=1) +
      ggplot2::labs(x="처치 전후 상대 시점", y="추정치") + kapae_theme(12)
    figs[["그림 3. 평균 DID 처치효과"]] <- coef_plot(r$coef)
  }

  if (spec$method == "iv") {
    figs[["그림 1. 도구변수별 처치확률"]] <-
      ggplot2::ggplot(r$first, ggplot2::aes(x=factor(instrument), y=treatment, fill=factor(instrument))) +
      ggplot2::geom_col(width=.62) +
      ggplot2::scale_fill_manual(values=c("0"=pal[["grey"]], "1"=pal[["blue"]])) +
      ggplot2::labs(x=spec$instrument_label %||% "도구변수", y="처치확률") +
      kapae_theme(12) + ggplot2::theme(legend.position="none")
    figs[["그림 2. OLS와 IV 추정치 비교"]] <- coef_plot(r$coef)
  }

  if (spec$method == "matching") {
    figs[["그림 1. 조정 전 성향점수 분포와 공통지지영역"]] <-
      ggplot2::ggplot(r$matched, ggplot2::aes(x=ps_hat, colour=factor(treatment), fill=factor(treatment))) +
      ggplot2::geom_density(alpha=.16, linewidth=1.05) +
      ggplot2::scale_colour_manual(values=c("0"=pal[["blue"]], "1"=pal[["orange"]])) +
      ggplot2::scale_fill_manual(values=c("0"=pal[["blue"]], "1"=pal[["orange"]])) +
      ggplot2::labs(x="추정 성향점수", y="밀도") + kapae_theme(12)
    figs[["그림 2. 조정 전후 처치효과 추정치"]] <- coef_plot(r$coef)
  }

  if (spec$method == "causal_forest") {
    figs[["그림 1. 추정 CATE 분포"]] <-
      ggplot2::ggplot(r$cate, ggplot2::aes(x=cate_hat)) +
      ggplot2::geom_histogram(bins=35, fill=pal[["purple"]], alpha=.82) +
      ggplot2::labs(x="추정 CATE", y="관측치 수") + kapae_theme(12)
    figs[["그림 2. CATE 사분위집단별 평균 처치효과"]] <-
      ggplot2::ggplot(r$groups, ggplot2::aes(x=group, y=cate_hat, fill=group)) +
      ggplot2::geom_col(width=.65) +
      ggplot2::scale_fill_manual(values=c("Q1"=pal[["sky"]], "Q2"=pal[["teal"]], "Q3"=pal[["orange"]], "Q4"=pal[["purple"]])) +
      ggplot2::labs(x="CATE 사분위집단", y="평균 추정 CATE") + kapae_theme(12) +
      ggplot2::theme(legend.position="none")
    figs[["그림 3. 전체 평균 처치효과"]] <- coef_plot(r$coef)
  }

  if (spec$method == "rd") {
    figs[["그림 1. 할당변수 분포와 기준점"]] <-
      ggplot2::ggplot(d, ggplot2::aes(x=running)) +
      ggplot2::geom_histogram(bins=40, fill=pal[["blue"]], alpha=.78) +
      ggplot2::geom_vline(xintercept=spec$cutoff, linetype=2, colour=pal[["red"]], linewidth=.9) +
      ggplot2::labs(x=spec$running_label %||% "할당변수", y="관측치 수") + kapae_theme(12)
    figs[["그림 2. 기준점 주변의 결과변수"]] <-
      ggplot2::ggplot(d, ggplot2::aes(x=running, y=y)) +
      ggplot2::geom_point(alpha=.16, colour=pal[["grey"]]) +
      ggplot2::geom_smooth(data=d[d$running < spec$cutoff,], method="lm", formula=y~poly(x,2), se=FALSE, colour=pal[["blue"]], linewidth=1.1) +
      ggplot2::geom_smooth(data=d[d$running >= spec$cutoff,], method="lm", formula=y~poly(x,2), se=FALSE, colour=pal[["orange"]], linewidth=1.1) +
      ggplot2::geom_vline(xintercept=spec$cutoff, linetype=2, colour=pal[["red"]]) +
      ggplot2::labs(x=spec$running_label %||% "할당변수", y=spec$outcome_label %||% "결과변수") + kapae_theme(12)
    figs[["그림 3. Bandwidth별 효과 추정치"]] <-
      ggplot2::ggplot(r$sensitivity, ggplot2::aes(x=bandwidth, y=estimate)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin=conf.low, ymax=conf.high), width=0, colour=pal[["sky"]], linewidth=1) +
      ggplot2::geom_point(position=ggplot2::position_nudge(x=.015), size=3.2, shape=21, fill=pal[["orange"]], colour="white", stroke=1) +
      ggplot2::labs(x="Bandwidth", y=ifelse(identical(spec$rd_design,"kink"), "RKD 추정치", "RD 추정치")) + kapae_theme(12)
    figs[[ifelse(identical(spec$rd_design,"kink"), "그림 4. 주 RKD 추정치", "그림 4. 주 RD 추정치")]] <- coef_plot(r$coef)
  }

  figs[seq_len(min(4, length(figs)))]
}


make_supplement_figures <- function(analysis, spec) {
  r <- analysis$results
  d <- analysis$data
  pal <- kapae_palette()
  figs <- list()

  if (spec$method == "dml" && all(c("true_ps","treatment") %in% names(d))) {
    tmp <- d[complete.cases(d[, c("true_ps","treatment")]), ]
    tmp$group <- ifelse(tmp$treatment == 1, "처치집단", "비교집단")
    figs[["그림 S1. 처치집단과 비교집단의 성향점수 중첩"]] <-
      ggplot2::ggplot(tmp, ggplot2::aes(x=true_ps, colour=group, fill=group)) +
      ggplot2::geom_density(alpha=.16, linewidth=1.05) +
      ggplot2::scale_colour_manual(values=c("처치집단"=pal[["orange"]], "비교집단"=pal[["blue"]])) +
      ggplot2::scale_fill_manual(values=c("처치집단"=pal[["orange"]], "비교집단"=pal[["blue"]])) +
      ggplot2::labs(x="성향점수", y="밀도") + kapae_theme(11)
  }

  if (spec$method == "did") {
    pre <- r$event[r$event$event_time < 0,]
    figs[["그림 S1. 처치 이전 event-study 계수"]] <-
      ggplot2::ggplot(pre, ggplot2::aes(x=event_time, y=estimate)) +
      ggplot2::geom_hline(yintercept=0, linetype=2, colour=pal[["grey"]]) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin=conf.low, ymax=conf.high), width=0, colour=pal[["sky"]], linewidth=1) +
      ggplot2::geom_point(position=ggplot2::position_nudge(x=.10), size=3.1, shape=21, fill=pal[["orange"]], colour="white", stroke=1) +
      ggplot2::labs(x="처치 이전 상대 시점", y="추정치") + kapae_theme(11)
  }

  if (spec$method == "matching") {
    bal_long <- tidyr::pivot_longer(r$balance, c(before, after), names_to="stage", values_to="smd")
    bal_long$stage <- factor(bal_long$stage, levels=c("before","after"), labels=c("조정 전","조정 후"))
    figs[["그림 S1. 공변량 균형 Love plot"]] <-
      ggplot2::ggplot(bal_long, ggplot2::aes(x=abs(smd), y=reorder(variable, abs(smd)), colour=stage)) +
      ggplot2::geom_vline(xintercept=.10, linetype=2, colour=pal[["red"]]) +
      ggplot2::geom_point(size=2.8) +
      ggplot2::scale_colour_manual(values=c("조정 전"=pal[["orange"]], "조정 후"=pal[["blue"]])) +
      ggplot2::labs(x="절대 표준화 평균차이", y=NULL) + kapae_theme(11)
  }

  figs
}


figure_note <- function(caption) {
  if (grepl("추정치|효과|event-study|Bandwidth|DML|OLS|IV|DID|RD|RKD", caption, ignore.case=TRUE)) {
    return("**Note.** 원형 마커는 점추정치, spike는 95% 신뢰구간을 의미합니다. * p < 0.05. 마커와 수치 레이블은 신뢰구간과 겹치지 않도록 약간 위·오른쪽에 배치했습니다.")
  }
  if (grepl("Love plot", caption, ignore.case=TRUE)) {
    return("**Note.** 점은 각 공변량의 절대 표준화 평균차이를 의미합니다. 세로 점선 0.10은 흔히 사용하는 균형 진단 기준입니다.")
  }
  if (grepl("성향점수|중첩|공통지지", caption, ignore.case=TRUE)) {
    return("**Note.** 두 분포가 겹치는 영역은 처치집단과 비교집단이 관측 공변량에 근거해 비교 가능한 common support를 시각적으로 보여줍니다.")
  }
  if (grepl("기준점|할당변수", caption, ignore.case=TRUE)) {
    return("**Note.** 점선은 연구설계에서 정의한 기준점(cutoff)을 의미합니다.")
  }
  "**Note.** 입력한 연구설계를 바탕으로 생성한 교육용 가상 결과입니다."
}

render_figure_list <- function(figs) {
  ensure_packages()
  if (!length(figs)) return(invisible(NULL))

  if (knitr::is_html_output()) {
    blocks <- lapply(names(figs), function(nm) {
      widget <- plotly::ggplotly(figs[[nm]])
      widget <- plotly::config(widget, displaylogo=FALSE, responsive=TRUE)
      htmltools::tagList(
        widget,
        htmltools::tags$p(htmltools::tags$strong(nm)),
        htmltools::tags$p(htmltools::HTML(figure_note(nm)))
      )
    })
    return(htmltools::tagList(blocks))
  }

  for (nm in names(figs)) {
    print(figs[[nm]])
    cat("\\n\\n**", nm, "**\\n\\n", sep="")
    cat(figure_note(nm), "\\n\\n")
  }
  invisible(NULL)
}


main_results_text <- function(analysis, spec) {
  r <- analysis$results
  f <- function(x) sprintf("%.2f", x)
  sig_txt <- function(lo, hi) if (lo > 0 || hi < 0) "통계적으로 유의했습니다(p < 0.05)" else "95% 신뢰구간에 0이 포함되었습니다"

  switch(
    spec$method,
    prediction = {
      m <- aggregate(score ~ algorithm, r$cv, mean)
      best <- m[which.max(m$score), ]
      sprintf("교육용 가상 분석에서 평균 교차검증 성능이 가장 높았던 알고리즘은 **%s**였고 평균 %s는 **%s**였습니다. 알고리즘 간 평균 성능뿐 아니라 fold와 hyperparameter에 따른 변동도 함께 확인해야 하며, 실제 연구에서는 tuning에 사용하지 않은 test/hold-out 자료의 최종 성능을 우선 보고합니다.\\n\\n",
              best$algorithm, unique(r$cv$metric)[1], f(best$score))
    },
    dml = {
      ols <- r$coef[r$coef$model=="OLS",][1,]; dm <- r$coef[r$coef$model=="DML",][1,]
      sprintf("교육용 가상 분석에서 OLS 추정치는 **%s (95%% CI %s, %s)**, DML 추정치는 **%s (95%% CI %s, %s)**였습니다. DML 추정치는 %s. 이 시뮬레이션의 true effect는 %.2f이므로 DML이 OLS보다 true effect에 더 가까웠지만, 실제 연구에서는 nuisance model의 성능과 overlap을 함께 확인해야 합니다.\\n\\n",
              f(ols$estimate), f(ols$conf.low), f(ols$conf.high), f(dm$estimate), f(dm$conf.low), f(dm$conf.high),
              sig_txt(dm$conf.low, dm$conf.high), dm$truth)
    },
    did = {
      z <- r$coef[1,]
      sprintf("평균 DID 처치효과는 **%s (95%% CI %s, %s)**로 추정되었고 %s. 인과적 해석에 앞서 처치집단과 비교집단의 처치 이전 추세와 event-study의 pre-treatment 계수들이 0 부근에서 안정적인지 확인해야 합니다.\\n\\n",
              f(z$estimate), f(z$conf.low), f(z$conf.high), sig_txt(z$conf.low,z$conf.high))
    },
    iv = {
      ols <- r$coef[1,]; iv <- r$coef[2,]
      sprintf("OLS 추정치는 **%s (95%% CI %s, %s)**, IV/2SLS 추정치는 **%s (95%% CI %s, %s)**였습니다. First-stage F 통계량은 **%.1f**였습니다. IV 추정치는 %s. 실제 인과해석에는 first-stage relevance뿐 아니라 exclusion restriction과 independence 가정에 대한 근거가 필요합니다.\\n\\n",
              f(ols$estimate), f(ols$conf.low), f(ols$conf.high), f(iv$estimate), f(iv$conf.low), f(iv$conf.high),
              r$first_stage_f, sig_txt(iv$conf.low,iv$conf.high))
    },
    matching = {
      raw <- r$coef[1,]; adj <- r$coef[2,]
      sprintf("조정 전 효과 추정치는 **%s (95%% CI %s, %s)**였고 성향점수 조정 후 추정치는 **%s (95%% CI %s, %s)**였습니다. 조정 후 효과는 %s. 결과 해석 전에 common support와 Love plot에서 공변량 균형이 충분히 개선되었는지 확인해야 합니다.\\n\\n",
              f(raw$estimate), f(raw$conf.low), f(raw$conf.high), f(adj$estimate), f(adj$conf.low), f(adj$conf.high),
              sig_txt(adj$conf.low,adj$conf.high))
    },
    causal_forest = {
      z <- r$coef[1,]
      q1 <- r$groups$cate_hat[r$groups$group=="Q1"][1]
      q4 <- r$groups$cate_hat[r$groups$group=="Q4"][1]
      sprintf("전체 평균 처치효과는 **%s (95%% CI %s, %s)**였고 %s. 추정 CATE의 하위 사분위집단 평균은 **%s**, 상위 사분위집단 평균은 **%s**로 나타났습니다. CATE 분포 자체만으로 확정적인 subgroup 효과를 주장하지 않고 사전 지정한 효과수정변수와 안정성을 함께 검토해야 합니다.\\n\\n",
              f(z$estimate), f(z$conf.low), f(z$conf.high), sig_txt(z$conf.low,z$conf.high), f(q1), f(q4))
    },
    rd = {
      z <- r$coef[1,]
      design <- ifelse(identical(spec$rd_design,"kink"), "RKD", "RD")
      sprintf("주 %s 추정치는 **%s (95%% CI %s, %s)**였고 %s. 이 효과는 기준점 주변의 국소적 효과이며, 할당변수 조작 가능성과 bandwidth 변화에 대한 민감도를 함께 확인해야 합니다.\\n\\n",
              design, f(z$estimate), f(z$conf.low), f(z$conf.high), sig_txt(z$conf.low,z$conf.high))
    }
  )
}

interpretation_text <- main_results_text


supplement_intro_text <- function(spec) {
  txt <- switch(
    spec$method,
    prediction = "예측모형의 핵심 성능 비교는 주요 결과에 제시했습니다. 추가 진단은 실제 연구에서 독립 test set 또는 calibration 자료가 있을 때만 보고하는 것이 적절하므로, 이 교육용 가상자료에서는 불필요한 보조 그림을 추가하지 않았습니다.",
    dml = "아래 성향점수 중첩 그림은 처치집단과 비교집단이 관측 공변량 공간에서 충분히 비교 가능한지를 확인합니다. 분포의 중첩이 매우 적으면 DML 추정치가 제한된 overlap에 민감할 수 있습니다.",
    did = "아래 그림은 처치 이전 event-study 계수만 분리해 보여줍니다. 이 계수들이 0에서 체계적으로 벗어나면 parallel trends 가정의 개연성이 약해질 수 있습니다.",
    iv = "도구변수의 핵심 first-stage 정보와 효과 추정치는 주요 결과에 이미 포함되어 있습니다. exclusion restriction처럼 그림만으로 검증할 수 없는 가정은 추가 가상 결과를 만들지 않고 연구설계 논리로 평가합니다.",
    matching = "아래 Love plot은 성향점수 조정 전후의 공변량 균형을 비교합니다. 조정 후 절대 표준화 평균차이가 충분히 줄지 않으면 outcome 효과를 해석하기 전에 propensity model을 다시 검토해야 합니다.",
    causal_forest = "CATE 분포와 사분위집단별 효과는 주요 결과에 포함되어 있습니다. 실제 자료에서는 true CATE를 관찰할 수 없으므로 시뮬레이션에서만 가능한 비교 그림은 부록에 추가하지 않았습니다.",
    rd = "할당변수 분포, 기준점 주변 결과, bandwidth 민감도가 주요 결과에 모두 포함되어 있습니다. 추가 placebo나 donut 분석은 실제 제도적 맥락과 자료 구조가 정해진 뒤 설계하는 것이 적절하므로 임의의 가상 결과를 추가하지 않았습니다."
  )
  paste0("### 부록 분석 안내\\n\\n", txt, "\\n\\n")
}

supplement_checklist_text <- supplement_intro_text

read_remote_submission_config <- function() {
  url <- kapae_repo_raw("projects/design_lab/config.json")
  out <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
  out %||% list(submission_endpoint = "", course_key = "")
}

submit_project <- function(qmd_path,
                           endpoint = "",
                           course_key = "") {
  ensure_packages(c("httr2", "jsonlite"))
  if (!file.exists(qmd_path)) stop("QMD 파일을 찾을 수 없습니다: ", qmd_path)
  content <- paste(readLines(qmd_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  cfg <- read_remote_submission_config()
  endpoint <- endpoint %||% cfg$submission_endpoint
  course_key <- course_key %||% cfg$course_key

  # Parse lightweight metadata from the generated filename/front matter.
  bn <- basename(qmd_path)
  m <- regexec("^02_([A-Za-z0-9_-]+)_(prediction|dml|did|iv|matching|causal_forest|rd)_research_design\\.qmd$", bn)
  hit <- regmatches(bn, m)[[1]]
  if (length(hit) != 3) stop("생성된 02_*_research_design.qmd 파일을 제출하세요.")
  student_id <- hit[2]
  method <- hit[3]

  if (is.null(endpoint) || !nzchar(endpoint)) {
    payload_path <- sub("\\.qmd$", "_submission.json", qmd_path)
    jsonlite::write_json(
      list(student_id = student_id, method = method, content = content, submitted_at = format(Sys.time(), tz = "UTC")),
      payload_path,
      auto_unbox = TRUE,
      pretty = TRUE
    )
    message("제출 API가 아직 활성화되지 않아 로컬 제출 파일을 만들었습니다: ", payload_path)
    return(invisible(payload_path))
  }

  req <- httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_json(list(
      student_id = student_id,
      method = method,
      course_key = course_key,
      content = content,
      submitted_at = format(Sys.time(), tz = "UTC")
    ), auto_unbox = TRUE)
  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) >= 300) stop("제출 실패: HTTP ", httr2::resp_status(resp))
  message("제출 완료: projects/submissions/", student_id, "_", method, ".qmd")
  invisible(httr2::resp_body_json(resp))
}

render_project_outputs <- function(qmd_path, formats = c("html", "docx", "pdf", "revealjs")) {
  if (!file.exists(qmd_path)) stop("QMD 파일을 찾을 수 없습니다: ", qmd_path)
  quarto <- Sys.which("quarto")
  if (!nzchar(quarto)) stop("Quarto CLI를 찾을 수 없습니다. Positron/RStudio에서 Quarto 설치를 확인하세요.")
  results <- lapply(formats, function(fmt) {
    status <- system2(quarto, c("render", shQuote(qmd_path), "--to", fmt))
    data.frame(format = fmt, status = status, ok = identical(status, 0L))
  })
  out <- do.call(rbind, results)
  if (any(!out$ok & out$format == "pdf")) {
    message("PDF만 실패했다면 LaTeX/TinyTeX 설치 여부를 확인하세요. HTML/Word/RevealJS는 별도로 성공할 수 있습니다.")
  }
  out
}
