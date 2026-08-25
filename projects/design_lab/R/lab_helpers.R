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
    prediction = "Machine-learning prediction",
    dml = "Double/debiased machine learning (DML)",
    did = "Difference-in-differences (DID)",
    iv = "Instrumental variables (IV)",
    matching = "Propensity-score matching/weighting",
    causal_forest = "Causal forest",
    rd = "Regression discontinuity (RD)"
  )[[method]]
}

method_input_block <- function(method) {
  blocks <- list(
    prediction = c(
      '# 결과변수 유형: "continuous" 또는 "binary"',
      'outcome_type <- "binary"',
      '',
      '# 연속형이면 대략적인 범위 예: c(0, 100). 이진이면 c(0, 1)을 유지합니다.',
      'outcome_range <- c(0, 1)',
      '',
      '# 예측변수 개수. 0 이상의 정수로 입력합니다.',
      'n_continuous_predictors <- 8',
      'n_binary_predictors <- 4',
      'n_categorical_predictors <- 2',
      '',
      '# 대략적인 표본크기. 교육용 시뮬레이션은 300~5000 정도를 권장합니다.',
      'n_obs <- 1200'
    ),
    dml = c(
      '# 결과변수 유형: "continuous" 권장. binary도 허용합니다.',
      'outcome_type <- "continuous"',
      'outcome_range <- c(0, 100)',
      '',
      '# 교란변수의 대략적인 구성',
      'n_continuous_predictors <- 8',
      'n_binary_predictors <- 4',
      'n_categorical_predictors <- 2',
      'n_obs <- 1500',
      '',
      '# 처치는 이 예제에서 binary treatment로 가정합니다.',
      'treatment_type <- "binary"',
      '# 교차적합 fold 수: 보통 2~10, 여기서는 5 권장',
      'n_folds <- 5'
    ),
    did = c(
      '# 패널의 개체 수와 시점 수. 최소 4개 시점을 권장합니다.',
      'n_units <- 400',
      'n_periods <- 8',
      '# 처치가 시작되는 시점. 1 < treatment_start <= n_periods',
      'treatment_start <- 5',
      '# 처치집단 비율: 0.1~0.9 권장',
      'treated_share <- 0.45',
      '',
      'outcome_type <- "continuous"',
      'outcome_range <- c(0, 100)',
      'n_continuous_predictors <- 4',
      'n_binary_predictors <- 2',
      'n_categorical_predictors <- 1'
    ),
    iv = c(
      'n_obs <- 1800',
      'outcome_type <- "continuous"',
      'outcome_range <- c(0, 100)',
      'n_continuous_predictors <- 6',
      'n_binary_predictors <- 3',
      'n_categorical_predictors <- 1',
      '# first-stage strength의 교육용 설정: 0.15~0.60 정도',
      'instrument_strength <- 0.35'
    ),
    matching = c(
      'n_obs <- 1600',
      'outcome_type <- "continuous"',
      'outcome_range <- c(0, 100)',
      'n_continuous_predictors <- 8',
      'n_binary_predictors <- 4',
      'n_categorical_predictors <- 2',
      '# 추정대상: "ATE" 또는 "ATT"',
      'estimand <- "ATE"',
      '# caliper는 propensity score의 SD 단위 예시입니다.',
      'caliper <- 0.2'
    ),
    causal_forest = c(
      'n_obs <- 2000',
      'outcome_type <- "continuous"',
      'outcome_range <- c(0, 100)',
      'n_continuous_predictors <- 10',
      'n_binary_predictors <- 4',
      'n_categorical_predictors <- 2',
      '# causal forest tree 수의 논문 specification 예시',
      'num_trees <- 2000',
      '# honesty를 기본 원칙으로 둡니다.',
      'honesty <- TRUE'
    ),
    rd = c(
      'n_obs <- 1800',
      'outcome_type <- "continuous"',
      'outcome_range <- c(0, 100)',
      '# cutoff는 running variable의 0으로 고정한 교육용 예시입니다.',
      'cutoff <- 0',
      '# 분석 bandwidth 예: 0.2~0.8',
      'bandwidth <- 0.45',
      'n_continuous_predictors <- 3',
      'n_binary_predictors <- 2',
      'n_categorical_predictors <- 1'
    )
  )
  paste(blocks[[method]], collapse = "\n")
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
    stop("이미 파일이 있습니다: ", path, "\noverwrite = TRUE로 다시 생성할 수 있습니다.")
  }

  txt <- sprintf('---
title: "Research Design Lab: %s"
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
    keep-tex: false
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

# ============================================================
# 학생 입력 영역: 아래 값만 수정하세요.
# ============================================================

student_id <- "%s"
method <- "%s"

# 연구 제목 예시: "Predicting ...", "The effect of X on Y ..."
research_title <- "여기에 연구 제목을 입력하세요"

# 데이터셋 이름 또는 출처 예시: "Korea Community Health Survey 2025"
data_name <- "여기에 데이터셋 이름을 입력하세요"

# 데이터의 관측단위와 핵심 특성을 1~3문장으로 적습니다.
# 예: "개인 단위 횡단면 자료이며 지역 식별자가 있다."
data_description <- "여기에 데이터 특성을 간략히 입력하세요"

%s

# 변수별 결측률을 교육용으로 재현하기 위한 최대값입니다. 0~0.20 권장.
max_missing_rate <- 0.08

# 재현 가능한 가짜 데이터를 위한 seed입니다.
seed <- 2026

# 제출 API가 활성화되면 비워두어도 원격 config를 자동 확인합니다.
submission_endpoint <- ""
course_key <- ""

spec <- build_spec_from_environment(environment())
```

# 1. Research question and design

```{r}
#| results: asis
cat(project_overview_text(spec))
```

# 2. Data and expected structure

학생은 실제 분석을 수행하는 것이 아니라, 자신이 알고 있는 데이터의 구조를 바탕으로 **논문에 필요한 분석 설계와 결과의 흐름**을 먼저 설계합니다. 아래 결과는 입력한 구조에 맞춰 자동 생성한 교육용 가상 데이터에 기반합니다.

```{r}
#| results: asis
cat(data_structure_text(spec))
```

# 3. Methods

```{r}
#| results: asis
cat(methods_text(spec))
```

# 4. Simulated analysis

```{r}
#| label: simulate
analysis <- simulate_project(spec)
```

## Main results

메인 본문에는 표와 그림을 합쳐 **최대 4개**만 배치합니다. 인과추론 설계는 진단 또는 식별 가정 확인을 결과 추정치보다 앞에 둡니다.

```{r}
#| label: main-results
main_figures <- make_main_figures(analysis, spec)
render_figure_list(main_figures)
```

# 5. Interpretation template

```{r}
#| results: asis
cat(interpretation_text(analysis, spec))
```

# Supplementary material

## A1. Missing records by variable

```{r}
knitr::kable(analysis$missingness, digits = 2)
```

## A2. Data-cleaning flow

```{r}
knitr::kable(analysis$cleaning_flow)
```

## A3. Design-specific diagnostics

```{r}
supp_figures <- make_supplement_figures(analysis, spec)
render_figure_list(supp_figures)
```

## A4. Required robustness and sensitivity checks

```{r}
#| results: asis
cat(supplement_checklist_text(spec))
```

## A5. Reproducibility record

```{r}
sessionInfo()
```

# 6. Render outputs

최종 확인 후 아래 셀을 실행하면 HTML, Word, PDF, RevealJS slides를 순차적으로 렌더링합니다. HTML/RevealJS에서는 Plotly가 interactive하게 표시되고, Word/PDF에서는 같은 시각화를 static ggplot으로 출력합니다.

```{r}
#| eval: false
render_project_outputs(knitr::current_input())
```

# Submission

작업을 마친 뒤 아래 셀을 실행합니다. 제출 API가 활성화되어 있으면 현재 QMD가 `projects/submissions/`에 자동 제출됩니다. API가 아직 설정되지 않았으면 로컬 제출 패키지를 생성합니다.

```{r}
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
    "## Proposed study

",
    "**Title.** ", spec$research_title, "  
",
    "**Design.** ", method_label(spec$method), "  
",
    "**Data.** ", spec$data_name, "  
",
    if (nzchar(spec$unit_of_observation %||% "")) paste0("**Unit of observation.** ", spec$unit_of_observation, "  
") else "",
    "
### Research background

",
    if (nzchar(spec$theoretical_background %||% "")) paste0("**Theoretical/policy background.** ", spec$theoretical_background, "

") else "",
    if (nzchar(spec$research_need %||% "")) paste0("**Why this study is needed.** ", spec$research_need, "

") else "",
    if (nzchar(spec$research_question %||% "")) paste0("**Research question.** ", spec$research_question, "

") else "",
    if (nzchar(spec$expected_contribution %||% "")) paste0("**Expected contribution.** ", spec$expected_contribution, "

") else "",
    "수업 마지막 약 **30분 동안** 각 연구설계를 함께 살펴보고 데이터-방법론 적합성, 식별가정, 필요한 진단을 중심으로 토론합니다.

"
  )
}

data_structure_text <- function(spec) {
  extra <- switch(
    spec$method,
    did = sprintf("패널 구조는 약 %d개 단위 × %d개 시점이며 처치는 %d번째 시점부터 시작한다고 가정합니다.", spec$n_units, spec$n_periods, spec$treatment_start),
    rd = sprintf("running variable의 cutoff는 %.2f이고 주 분석 bandwidth는 %.2f로 계획합니다.", spec$cutoff, spec$bandwidth),
    iv = sprintf("도구변수의 first-stage strength를 교육용으로 %.2f 수준으로 설정합니다.", spec$instrument_strength),
    matching = sprintf("추정대상은 %s이며 caliper 예시는 %.2f입니다.", spec$estimand, spec$caliper),
    causal_forest = sprintf("causal forest는 %d trees와 honesty=%s를 논문 specification 예시로 둡니다.", spec$num_trees, spec$honesty),
    dml = sprintf("cross-fitting은 %d folds로 계획합니다.", spec$n_folds),
    prediction = sprintf("연속형 %d개, 이진형 %d개, 범주형 %d개 예측변수를 가정합니다.", spec$n_cont, spec$n_bin, spec$n_cat)
  )

  sprintf(
    "**Known data characteristics.** %s  \nOutcome type: **%s**. Predictor structure: continuous %d, binary %d, categorical %d. %s\n\n",
    spec$data_description, spec$outcome_type, spec$n_cont, spec$n_bin, spec$n_cat, extra
  )
}


data_compatibility_text <- function(spec) {
  bullet <- function(ok, yes, no) {
    if (isTRUE(ok)) paste0("- ✓ ", yes, "\n") else paste0("- ⚠ ", no, "\n")
  }
  has_text <- function(x) !is.null(x) && length(x) > 0 && any(nzchar(trimws(as.character(x))))
  has_covs <- length(spec$covariate_names %||% character()) > 0

  intro <- paste0(
    "### Method-data compatibility check\n\n",
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
  common <- "모든 specification은 결과를 본 뒤 임의로 선택하지 않는다는 원칙을 둡니다. 결측자료 처리, 표본 제외, 변수 coding, 주요 hyperparameter 또는 bandwidth/caliper를 Methods 또는 supplement에 명시합니다."
  specific <- switch(
    spec$method,
    prediction = paste0(
      "예측 분석은 random forest, XGBoost, feed-forward neural network를 비교합니다. 동일한 resampling folds를 사용해 공정하게 비교하고, binary outcome은 ROC AUC와 보조적으로 PR AUC/분류 임계값을, continuous outcome은 RMSE와 MAE를 보고하도록 설계합니다. ",
      "Random forest는 mtry와 minimum node size, XGBoost는 tree depth/learning rate/number of trees, neural network는 hidden units/penalty/epochs를 tuning 대상으로 기술합니다. 최종 모델은 test set 또는 nested/resampled estimate와 분리해 평가합니다."
    ),
    dml = paste0(
      "DML은 treatment model과 outcome model을 유연한 ML로 적합하고 cross-fitting으로 nuisance-function overfitting bias를 줄인 뒤 orthogonal score로 평균 처치효과를 추정하는 구조로 기술합니다. ",
      "Robustness check로 동일한 공변량을 사용한 conventional OLS를 함께 제시하되, 비선형 교란이 있을 때 OLS와 DML이 어떻게 달라질 수 있는지 시각적으로 비교합니다."
    ),
    did = paste0(
      "DID는 treated/control group과 pre/post timing을 명시하고 unit 및 time fixed effects와 적절한 clustered standard errors를 사용합니다. ",
      "효과 추정보다 먼저 raw group-time trajectories와 event-study pre-period coefficients를 시각적으로 확인해 parallel-trends 가정의 개연성을 점검합니다. staggered adoption이 있으면 단순 TWFE 대신 cohort-time ATT 계열 추정량을 고려합니다."
    ),
    iv = paste0(
      "IV 분석은 instrument relevance, independence/exogeneity, exclusion restriction, monotonicity가 요구됨을 명시합니다. ",
      "first-stage coefficient/F statistic을 먼저 제시하고, 2SLS 추정치는 OLS와 같은 축에서 비교합니다. 결과 해석은 instrument가 영향을 미치는 compliers의 LATE라는 점을 분명히 합니다."
    ),
    matching = paste0(
      "Propensity-score 분석은 treatment assignment model을 먼저 정의하고 common support를 점검합니다. 매칭 또는 weighting 후에는 outcome model보다 먼저 covariate balance를 평가합니다. ",
      "Love plot, propensity-score overlap, effective sample size/weight distribution을 supplement에 포함하고, 결과는 지정한 estimand에 맞춰 해석합니다."
    ),
    causal_forest = paste0(
      "Causal forest는 평균효과뿐 아니라 CATE의 이질성을 탐색하기 위해 사용합니다. treatment/outcome nuisance components와 honesty를 활용해 과적합을 줄이고, ATE와 CATE 분포를 함께 보고합니다. ",
      "변수 중요도를 인과적 effect modifier의 증거로 단정하지 않고, calibration/heterogeneity 검정과 정책적으로 해석 가능한 subgroup summary를 supplement에 둡니다."
    ),
    rd = paste0(
      "RD는 cutoff 주변에서 처치확률이 불연속적으로 바뀐다는 설계를 이용합니다. 효과 추정보다 먼저 running variable 분포, cutoff 주변 관측치 밀도, outcome-running-variable 관계를 시각적으로 확인합니다. ",
      "주 추정은 local polynomial과 data-driven 또는 사전 지정 bandwidth를 사용하고, bandwidth/order 변화 및 donut RD를 sensitivity analysis로 제시합니다."
    )
  )
  paste0(specific, "\n\n", common, "\n\n")
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

coef_plot <- function(df, title = NULL, xlab = "Estimate") {
  ggplot2::ggplot(df, ggplot2::aes(y = reorder(model, estimate), x = estimate)) +
    ggplot2::geom_vline(xintercept = 0, linetype = 2) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf.low, xmax = conf.high), height = .12) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f [%.2f, %.2f]", estimate, conf.low, conf.high)),
      hjust = -0.05, size = 3.6
    ) +
    ggplot2::labs(title = title, x = xlab, y = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(plot.margin = ggplot2::margin(5.5, 90, 5.5, 5.5))
}

make_main_figures <- function(analysis, spec) {
  d <- analysis$data
  r <- analysis$results
  figs <- list()

  if (spec$method == "prediction") {
    figs[["Figure 1. Cross-validated performance by algorithm"]] <-
      ggplot2::ggplot(r$cv, ggplot2::aes(x = algorithm, y = score)) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_jitter(width = .12, alpha = .18) +
      ggplot2::labs(x = NULL, y = unique(r$cv$metric)) +
      ggplot2::theme_minimal(base_size = 12)

    figs[["Figure 2. Hyperparameter comparison across all tested settings"]] <-
      ggplot2::ggplot(r$cv, ggplot2::aes(x = score, y = reorder(setting, score))) +
      ggplot2::geom_boxplot() +
      ggplot2::facet_wrap(~algorithm, scales = "free_y") +
      ggplot2::labs(x = unique(r$cv$metric), y = NULL) +
      ggplot2::theme_minimal(base_size = 10)

    if (nrow(r$importance)) {
      figs[["Figure 3. Illustrative variable importance"]] <-
        ggplot2::ggplot(r$importance, ggplot2::aes(x = importance, y = reorder(variable, importance))) +
        ggplot2::geom_col() +
        ggplot2::labs(x = "Relative importance", y = NULL) +
        ggplot2::theme_minimal(base_size = 12)
    }
  }

  if (spec$method == "dml") {
    figs[["Figure 1. DML estimate versus conventional OLS"]] <- coef_plot(r$coef)
    figs[["Figure 2. Nuisance-model predictive performance"]] <-
      ggplot2::ggplot(r$nuisance, ggplot2::aes(x = model, y = score)) + ggplot2::geom_col() +
      ggplot2::coord_cartesian(ylim = c(0, 1)) + ggplot2::labs(x = NULL, y = "Illustrative score") + ggplot2::theme_minimal(base_size = 12)
  }

  if (spec$method == "did") {
    means <- aggregate(y ~ time + treated, d, mean)
    means$group <- ifelse(means$treated == 1, "Treated", "Control")
    figs[["Figure 1. Mandatory pre-estimation visual inspection"]] <-
      ggplot2::ggplot(means, ggplot2::aes(x = time, y = y, linetype = group, group = group)) +
      ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point() +
      ggplot2::geom_vline(xintercept = spec$treatment_start - .5, linetype = 2) +
      ggplot2::labs(x = "Time", y = "Mean outcome", linetype = NULL) + ggplot2::theme_minimal(base_size = 12)
    figs[["Figure 2. Event-study coefficients"]] <-
      ggplot2::ggplot(r$event, ggplot2::aes(x = event_time, y = estimate)) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = .12) +
      ggplot2::geom_point(size = 2.6) +
      ggplot2::labs(x = "Event time", y = "Estimate") + ggplot2::theme_minimal(base_size = 12)
    figs[["Figure 3. Average DID effect"]] <- coef_plot(r$coef)
  }

  if (spec$method == "iv") {
    figs[["Figure 1. First-stage relevance"]] <-
      ggplot2::ggplot(r$first, ggplot2::aes(x = factor(instrument), y = treatment)) +
      ggplot2::geom_col() + ggplot2::labs(x = "Instrument", y = "Pr(treatment)") + ggplot2::theme_minimal(base_size = 12)
    figs[["Figure 2. OLS versus IV estimate"]] <- coef_plot(r$coef)
  }

  if (spec$method == "matching") {
    figs[["Figure 1. Propensity-score overlap before adjustment"]] <-
      ggplot2::ggplot(r$matched, ggplot2::aes(x = ps_hat, linetype = factor(treatment))) +
      ggplot2::geom_density(linewidth = 1) + ggplot2::labs(x = "Estimated propensity score", y = "Density", linetype = "Treatment") + ggplot2::theme_minimal(base_size = 12)
    figs[["Figure 2. Unadjusted and propensity-score-adjusted effects"]] <- coef_plot(r$coef)
  }

  if (spec$method == "causal_forest") {
    figs[["Figure 1. Estimated CATE distribution"]] <-
      ggplot2::ggplot(r$cate, ggplot2::aes(x = cate_hat)) + ggplot2::geom_histogram(bins = 35) +
      ggplot2::labs(x = "Estimated CATE", y = "Count") + ggplot2::theme_minimal(base_size = 12)
    figs[["Figure 2. Average CATE by CATE quartile"]] <-
      ggplot2::ggplot(r$groups, ggplot2::aes(x = group, y = cate_hat)) + ggplot2::geom_col() +
      ggplot2::labs(x = "CATE quartile", y = "Mean estimated CATE") + ggplot2::theme_minimal(base_size = 12)
    figs[["Figure 3. Overall causal-forest ATE"]] <- coef_plot(r$coef)
  }

  if (spec$method == "rd") {
    figs[["Figure 1. Mandatory running-variable inspection"]] <-
      ggplot2::ggplot(d, ggplot2::aes(x = running)) + ggplot2::geom_histogram(bins = 40) +
      ggplot2::geom_vline(xintercept = spec$cutoff, linetype = 2) + ggplot2::labs(x = "Running variable", y = "Count") + ggplot2::theme_minimal(base_size = 12)
    figs[["Figure 2. Outcome around the cutoff"]] <-
      ggplot2::ggplot(d, ggplot2::aes(x = running, y = y)) + ggplot2::geom_point(alpha = .18) +
      ggplot2::geom_smooth(data = d[d$running < spec$cutoff,], method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
      ggplot2::geom_smooth(data = d[d$running >= spec$cutoff,], method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
      ggplot2::geom_vline(xintercept = spec$cutoff, linetype = 2) + ggplot2::theme_minimal(base_size = 12)
    figs[["Figure 3. Bandwidth sensitivity"]] <-
      ggplot2::ggplot(r$sensitivity, ggplot2::aes(x = bandwidth, y = estimate)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = .025) + ggplot2::geom_point(size = 2.5) +
      ggplot2::labs(x = "Bandwidth", y = "RD estimate") + ggplot2::theme_minimal(base_size = 12)
    figs[["Figure 4. Main RD estimate"]] <- coef_plot(r$coef)
  }

  figs[seq_len(min(4, length(figs)))]
}

make_supplement_figures <- function(analysis, spec) {
  r <- analysis$results
  figs <- list()

  figs[["Figure S1. Missingness profile"]] <-
    ggplot2::ggplot(analysis$missingness, ggplot2::aes(x = missing_pct, y = reorder(variable, missing_pct))) +
    ggplot2::geom_col() + ggplot2::labs(x = "Missing (%)", y = NULL) + ggplot2::theme_minimal(base_size = 10)

  if (spec$method == "prediction") {
    figs[["Figure S2. Complete tuning-result distribution"]] <-
      ggplot2::ggplot(r$cv, ggplot2::aes(x = score, y = algorithm)) + ggplot2::geom_boxplot() + ggplot2::geom_jitter(height = .12, alpha = .15) + ggplot2::theme_minimal(base_size = 11)
  }
  if (spec$method == "matching") {
    bal_long <- tidyr::pivot_longer(r$balance, c(before, after), names_to = "stage", values_to = "smd")
    figs[["Figure S2. Covariate balance (Love plot)"]] <-
      ggplot2::ggplot(bal_long, ggplot2::aes(x = abs(smd), y = reorder(variable, abs(smd)), shape = stage)) +
      ggplot2::geom_vline(xintercept = .10, linetype = 2) + ggplot2::geom_point(size = 2.5) +
      ggplot2::labs(x = "Absolute standardized mean difference", y = NULL, shape = NULL) + ggplot2::theme_minimal(base_size = 11)
  }
  if (spec$method == "iv") {
    figs[[sprintf("Figure S2. First-stage strength (illustrative F = %.1f)", r$first_stage_f)]] <-
      ggplot2::ggplot(r$first, ggplot2::aes(x = factor(instrument), y = treatment)) + ggplot2::geom_point(size = 4) + ggplot2::theme_minimal(base_size = 11)
  }
  if (spec$method == "did") {
    pre <- r$event[r$event$event_time < 0,]
    figs[["Figure S2. Pre-treatment event-study coefficients only"]] <-
      ggplot2::ggplot(pre, ggplot2::aes(x = event_time, y = estimate)) + ggplot2::geom_hline(yintercept = 0, linetype = 2) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = .1) + ggplot2::geom_point() + ggplot2::theme_minimal(base_size = 11)
  }
  if (spec$method == "rd") {
    local <- analysis$data[abs(analysis$data$running - spec$cutoff) <= spec$bandwidth, ]
    figs[["Figure S2. Local bandwidth sample"]] <-
      ggplot2::ggplot(local, ggplot2::aes(x = running, y = y)) + ggplot2::geom_point(alpha = .25) +
      ggplot2::geom_vline(xintercept = spec$cutoff, linetype = 2) + ggplot2::theme_minimal(base_size = 11)
  }
  if (spec$method == "causal_forest") {
    figs[["Figure S2. Predicted versus true CATE in simulation"]] <-
      ggplot2::ggplot(r$cate, ggplot2::aes(x = true_tau, y = cate_hat)) + ggplot2::geom_point(alpha = .18) +
      ggplot2::geom_smooth(method = "lm", se = FALSE) + ggplot2::labs(x = "True simulated CATE", y = "Estimated CATE") + ggplot2::theme_minimal(base_size = 11)
  }
  if (spec$method == "dml") {
    figs[["Figure S2. Orthogonal-learning diagnostic placeholder"]] <-
      ggplot2::ggplot(r$nuisance, ggplot2::aes(x = score, y = model)) + ggplot2::geom_point(size = 3) + ggplot2::coord_cartesian(xlim = c(0,1)) + ggplot2::theme_minimal(base_size = 11)
  }
  figs
}

render_figure_list <- function(figs) {
  ensure_packages()

  if (!length(figs)) {
    return(invisible(NULL))
  }

  # HTML / RevealJS: return the widgets as one HTML object.
  # Returning (rather than print() inside a loop) lets knitr register
  # the htmlwidget dependencies and embed the Plotly figures correctly.
  if (knitr::is_html_output()) {
    blocks <- lapply(names(figs), function(nm) {
      widget <- plotly::ggplotly(figs[[nm]])
      widget <- plotly::config(widget, displaylogo = FALSE, responsive = TRUE)

      htmltools::tagList(
        htmltools::tags$h3(nm),
        widget
      )
    })

    return(htmltools::tagList(blocks))
  }

  # Word / PDF: use the original static ggplot objects.
  for (nm in names(figs)) {
    cat("\n\n### ", nm, "\n\n", sep = "")
    print(figs[[nm]])
    cat("\n\n")
  }

  invisible(NULL)
}

interpretation_text <- function(analysis, spec) {
  switch(
    spec$method,
    prediction = "예측 연구에서는 단일 최고 점수만 보고하지 말고 resampling 분포를 함께 해석합니다. 알고리즘 간 차이가 fold-to-fold 변동보다 작은지, tuning이 실제로 의미 있는 개선을 주는지, 최종 모델 평가가 tuning 자료와 분리되어 있는지를 서술합니다.\n\n",
    dml = "이 교육용 예시에서는 OLS가 비선형 교란구조를 충분히 포착하지 못해 DML과 차이가 나도록 구성했습니다. 실제 논문에서는 이 차이를 자동으로 DML의 우월성으로 해석하지 말고 nuisance-model specification, overlap, sample size, cross-fitting stability와 함께 검토합니다.\n\n",
    did = "DID에서는 처치 이후 계수보다 처치 이전 계수가 0 부근에서 안정적인지가 먼저 중요합니다. pre-trend가 뚜렷하면 단순 DID 결과의 인과적 해석을 약화시키고 다른 비교집단, trend specification 또는 연구설계 자체를 재검토합니다.\n\n",
    iv = "IV에서는 2SLS 계수가 OLS와 다르다는 사실만으로 IV가 옳다고 결론내리지 않습니다. 도구변수의 relevance와 배제제약의 실질적 타당성, 그리고 LATE의 대상 집단을 함께 논의합니다.\n\n",
    matching = "성향점수 방법의 핵심 결과는 처치효과 하나가 아니라 adjustment 이후 실제로 balance와 overlap이 개선되었는지입니다. balance가 남아 있으면 outcome estimate를 해석하기 전에 propensity model을 재검토합니다.\n\n",
    causal_forest = "CATE의 넓은 분포는 곧바로 실질적 이질성을 의미하지 않습니다. calibration, uncertainty, subgroup stability를 확인하고 사전에 의미 있는 effect modifier를 중심으로 해석합니다.\n\n",
    rd = "RD 효과는 cutoff 주변의 local effect입니다. running variable 조작 가능성, cutoff 주변 표본수, bandwidth와 polynomial order에 대한 민감도를 먼저 확인한 뒤 외적 타당성을 제한적으로 해석합니다.\n\n"
  )
}

supplement_checklist_text <- function(spec) {
  common <- c(
    "변수별 missing n/%와 missing-data 처리 규칙",
    "데이터 cleaning 및 제외 단계별 표본수",
    "모든 주요 변수 coding과 분석 표본 정의",
    "software/package version, seed, session information"
  )
  extra <- switch(
    spec$method,
    prediction = c("전체 tuning grid와 resampling scheme", "test/nested-CV 성능", "binary outcome이면 calibration/threshold/PR AUC"),
    dml = c("nuisance-model specification과 성능", "cross-fitting fold sensitivity", "overlap/propensity distribution", "OLS 등 conventional model robustness comparison"),
    did = c("처치 이전 group trends", "event-study pre-period coefficients", "placebo timing/outcome", "alternative control groups 또는 staggered-adoption estimator"),
    iv = c("first-stage coefficient와 F statistic", "reduced form", "instrument balance/exogeneity evidence", "weak-IV robust inference"),
    matching = c("propensity-score overlap", "Love plot/SMD before-after", "weight distribution/effective sample size", "caliper/matching-ratio/estimand sensitivity"),
    causal_forest = c("honesty/sample-splitting specification", "CATE calibration", "heterogeneity tests", "subgroup stability and sensitivity"),
    rd = c("running-variable density/manipulation check", "bandwidth sensitivity", "polynomial-order sensitivity", "donut RD/placebo cutoffs")
  )
  items <- c(common, extra)
  paste0("- ", items, collapse = "\n") |> paste0("\n\n")
}

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
