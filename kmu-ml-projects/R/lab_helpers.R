# KMU 2026 Public Administration Methodology Workshop helpers
# Student-facing functions for generating local research-design QMD files,
# simulating illustrative data/results, and rendering figures.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || identical(x, "")) y else x


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
      '# 3. 무엇을 예측하려고 하나요?',
      'outcome_var <- "fiscal_stress_next_year"',
      'outcome_label <- "다음 연도 재정스트레스 고위험 여부"',
      '',
      '# 4. 결과변수는 어떤 형태인가요? "binary" 또는 "continuous"',
      'outcome_type <- "binary"',
      '',
      '# 5. 예측에 사용할 실제 변수명을 적으세요.',
      'predictor_vars <- c(',
      '  "fiscal_self_reliance",',
      '  "population_change",',
      '  "aging_rate",',
      '  "debt_ratio",',
      '  "employment_rate"',
      ')',
      '',
      '# 6. 위 변수들이 결과물에서 어떻게 표시되면 좋을지 같은 순서로 적으세요.',
      'predictor_labels <- c(',
      '  "재정자립도",',
      '  "인구증감률",',
      '  "고령인구 비율",',
      '  "채무비율",',
      '  "고용률"',
      ')',
      '',
      '# 7. 각 변수의 형태를 같은 순서로 적으세요.',
      '# "continuous", "binary", "categorical" 중 하나를 사용합니다.',
      'predictor_types <- c(',
      '  "continuous",',
      '  "continuous",',
      '  "continuous",',
      '  "continuous",',
      '  "continuous"',
      ')',
      '',
      '# 8. 대략 몇 개의 관측치가 있나요?',
      'sample_size <- 250',
      '',
      '# 9. 최종 모델을 별도로 평가할 test/hold-out 자료가 있나요?',
      'has_test_set <- TRUE',
      '',
      '# 10. 이 예측 결과를 연구나 정책에서 어떻게 활용하고 싶나요?',
      'prediction_use <- "재정위험이 높아질 가능성이 있는 지방정부를 조기에 파악해 추가적인 재정 모니터링 대상을 선정하고자 한다."'
    ),

    dml = c(
      '# 3. 결과변수는 무엇인가요?',
      'outcome_var <- "service_satisfaction"',
      'outcome_label <- "행정서비스 만족도"',
      'outcome_type <- "continuous"',
      '',
      '# 4. 효과를 추정하려는 처치 또는 노출은 무엇인가요?',
      'treatment_var <- "digital_service_use"',
      'treatment_label <- "디지털 행정서비스 이용"',
      '',
      '# 5. 처치와 결과 모두에 관련될 수 있는 교란변수들을 적으세요.',
      'confounder_vars <- c("age", "education", "digital_literacy", "prior_service_use", "income")',
      'confounder_labels <- c("연령", "교육수준", "디지털 활용역량", "기존 행정서비스 이용", "소득수준")',
      'confounder_types <- c("continuous", "categorical", "continuous", "continuous", "continuous")',
      '',
      '# 6. 왜 이 변수들을 교란변수로 고려해야 하나요?',
      'confounding_set_justification <- "이 변수들은 디지털 행정서비스를 이용할 가능성과 행정서비스 만족도 모두에 관련될 수 있다."',
      '',
      '# 7. 대략 몇 개의 관측치가 있나요?',
      'sample_size <- 1500'
    ),

    did = c(
      '# 3. 결과변수는 무엇인가요?',
      'outcome_var <- "complaint_processing_days"',
      'outcome_label <- "민원 평균 처리일수"',
      '',
      '# 4. 같은 분석 단위를 여러 시점에서 구분하는 변수는 무엇인가요?',
      'unit_id_var <- "local_government"',
      'unit_id_label <- "지방정부"',
      '',
      '# 5. 시간 또는 조사시점을 나타내는 변수는 무엇인가요?',
      'time_var <- "year"',
      'time_label <- "연도"',
      '',
      '# 6. 실제 데이터에 포함된 분석 시점을 순서대로 적으세요.',
      'time_points <- c(2019, 2020, 2021, 2022, 2023, 2024, 2025)',
      '',
      '# 7. 처치집단과 비교집단을 구분하는 변수는 무엇인가요?',
      'treatment_group_var <- "one_stop_service"',
      'treatment_group_label <- "원스톱 민원서비스 도입 여부"',
      '',
      '# 8. 누가 처치집단이고 누가 비교집단인가요?',
      'treated_definition <- "2023년에 원스톱 민원서비스를 도입한 지방정부"',
      'control_definition <- "분석기간 동안 해당 서비스를 도입하지 않은 지방정부"',
      '',
      '# 9. 처치가 시작된 시점은 언제인가요?',
      '# time_points에 적은 값 중 하나를 사용합니다.',
      'treatment_start_value <- 2023',
      '',
      '# 10. 분석 단위마다 처치 시작시점이 다른가요?',
      'staggered_treatment <- FALSE',
      '',
      '# 11. TRUE라면 각 단위의 최초 처치시점을 알려주는 변수는 무엇인가요?',
      'first_treatment_var <- "first_adoption_year"',
      'first_treatment_label <- "최초 도입연도"',
      '',
      '# 12. 대략 몇 개의 분석 단위가 있나요?',
      'number_of_units <- 120'
    ),

    iv = c(
      '# 3. 결과변수는 무엇인가요?',
      'outcome_var <- "service_satisfaction"',
      'outcome_label <- "행정서비스 만족도"',
      'outcome_type <- "continuous"',
      '',
      '# 4. 인과효과를 추정하려는 처치 또는 노출은 무엇인가요?',
      'treatment_var <- "online_service_use"',
      'treatment_label <- "온라인 행정서비스 이용"',
      '',
      '# 5. 사용할 도구변수는 무엇인가요?',
      'instrument_var <- "randomised_guidance"',
      'instrument_label <- "온라인 서비스 무작위 이용안내"',
      '',
      '# 6. 왜 이 변수가 적절한 도구변수라고 생각하나요?',
      'instrument_justification <- "무작위 이용안내는 온라인 서비스 이용 가능성에는 영향을 주지만, 서비스 이용을 통하지 않고 만족도에 직접 영향을 주지 않는다고 가정한다."',
      '',
      '# 7. 함께 고려할 사전 공변량이 있다면 적으세요.',
      'covariate_vars <- c("age", "education", "digital_literacy", "baseline_satisfaction")',
      'covariate_labels <- c("연령", "교육수준", "디지털 활용역량", "기초 서비스 만족도")',
      'covariate_types <- c("continuous", "categorical", "continuous", "continuous")',
      '',
      '# 8. 대략 몇 개의 관측치가 있나요?',
      'sample_size <- 1800'
    ),

    matching = c(
      '# 3. 결과변수는 무엇인가요?',
      'outcome_var <- "policy_satisfaction"',
      'outcome_label <- "정책 만족도"',
      'outcome_type <- "continuous"',
      '',
      '# 4. 비교하려는 처치 또는 노출은 무엇인가요?',
      'treatment_var <- "public_hearing_participation"',
      'treatment_label <- "공청회 참여"',
      '',
      '# 5. 처치 이전에 측정된 비교가능성 조정 변수들을 적으세요.',
      'covariate_vars <- c("age", "education", "political_interest", "prior_policy_awareness", "income")',
      'covariate_labels <- c("연령", "교육수준", "정치 관심도", "사전 정책인지도", "소득수준")',
      'covariate_types <- c("continuous", "categorical", "continuous", "continuous", "continuous")',
      '',
      '# 6. 위 변수들이 처치 이전 변수인지 확인해 간단히 적으세요.',
      'covariate_timing_note <- "모든 공변량은 공청회 참여 이전에 측정되었거나 참여에 의해 영향을 받지 않는 특성이다."',
      '',
      '# 7. 매칭과 가중치 중 어떤 방식을 생각하고 있나요? "matching" 또는 "weighting"',
      'adjustment_method <- "matching"',
      '',
      '# 8. 어떤 효과가 관심인가요? 전체 집단이면 "ATE", 처치받은 집단이면 "ATT"',
      'target_effect <- "ATT"',
      '',
      '# 9. 대략 몇 개의 관측치가 있나요?',
      'sample_size <- 1600'
    ),

    causal_forest = c(
      '# 3. 결과변수는 무엇인가요?',
      'outcome_var <- "policy_support"',
      'outcome_label <- "정책 지지도"',
      'outcome_type <- "continuous"',
      '',
      '# 4. 효과의 이질성을 살펴볼 처치 또는 노출은 무엇인가요?',
      'treatment_var <- "personalised_policy_message"',
      'treatment_label <- "개인화 정책정보 메시지"',
      '',
      '# 5. 처치효과 이질성을 학습할 사전 공변량을 적으세요.',
      'covariate_vars <- c("baseline_support", "age", "education", "political_interest", "government_trust")',
      'covariate_labels <- c("기초 정책지지도", "연령", "교육수준", "정치 관심도", "정부 신뢰")',
      'covariate_types <- c("continuous", "continuous", "categorical", "continuous", "continuous")',
      '',
      '# 6. 그중 이론적으로 특히 관심 있는 효과수정변수는 무엇인가요?',
      'effect_modifier_vars <- c("baseline_support", "government_trust")',
      'effect_modifier_labels <- c("기초 정책지지도", "정부 신뢰")',
      '',
      '# 7. 왜 이 변수에서 효과가 달라질 것으로 예상하나요?',
      'heterogeneity_rationale <- "기초 정책태도와 정부 신뢰 수준에 따라 정책정보 메시지에 대한 반응이 달라질 수 있다."',
      '',
      '# 8. 대략 몇 개의 관측치가 있나요?',
      'sample_size <- 2000'
    ),

    rd = c(
      '# 3. 결과변수는 무엇인가요?',
      'outcome_var <- "welfare_service_use"',
      'outcome_label <- "복지서비스 이용횟수"',
      'outcome_type <- "continuous"',
      '',
      '# 4. 기준점이 있는 할당변수(running variable)는 무엇인가요?',
      'running_var <- "age"',
      'running_label <- "연령"',
      '',
      '# 5. 정책 또는 처치 기준점은 얼마인가요?',
      'cutoff <- 65',
      'cutoff_label <- "복지서비스 지원자격 연령 65세"',
      '',
      '# 6. 기준점에서 무엇이 달라지나요?',
      'assignment_rule <- "65세부터 특정 복지서비스 지원대상이 된다."',
      '',
      '# 7. 수준이 갑자기 바뀌는 RD인가요, 기울기가 바뀌는 RKD인가요?',
      '# "discontinuity" 또는 "kink"',
      'rd_type <- "discontinuity"',
      '',
      '# 8. 대략 몇 개의 관측치가 있나요?',
      'sample_size <- 1800'
    )
  )

  paste(blocks[[method]], collapse = "
")
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
    stop("이미 파일이 있습니다: ", path, "
overwrite = TRUE로 다시 생성할 수 있습니다.")
  }

  txt <- sprintf('---
title: "국민대학교 행정학과 방법론 워크숍: %s"
author: "%s"
student-id: "%s"
design-method: "%s"
format:
  html:
    toc: true
    embed-resources: true
execute:
  echo: false
  warning: false
  message: false
---

```{r}
#| label: setup
#| include: false

source("https://raw.githubusercontent.com/TheYongjinChoi/kmu-ml-projects/main/standalone/R/lab_helpers.R")
source("https://raw.githubusercontent.com/TheYongjinChoi/kmu-ml-projects/main/standalone/R/report_helpers.R")

student_id <- "%s"
method <- "%s"
```

<!--
학생 입력 1. 연구 배경
아래 숨김 R 코드블록의 문자열을 자신의 연구주제에 맞게 수정하세요.
각 항목은 1~3문장 정도면 충분합니다.
-->

```{r}
#| label: research-background
#| include: false

research_title <- "연구 제목을 입력하세요"

theoretical_background <- "
이 연구와 관련된 이론적 또는 정책적 배경을 1~3문장으로 적으세요.
"

research_need <- "
왜 이 연구가 필요한지 1~3문장으로 적으세요.
"

research_question <- "
핵심 연구질문을 1~2문장으로 적으세요.
"

expected_contribution <- "
이 연구가 추가할 수 있는 학술적 또는 정책적 기여를 1~3문장으로 적으세요.
"
```

<!--
학생 입력 2. 데이터와 방법론
실제로 사용할 데이터를 직접 확인하거나 구체적으로 떠올리며 아래 값을 수정하세요.
-->

```{r}
#| label: method-data-inputs
#| include: false

# 1. 어떤 데이터를 사용할 예정인가요?
data_name <- "예: 지방재정365 기초지방자치단체 연도별 자료"

# 2. 데이터의 한 행(row)은 무엇을 의미하나요?
unit_of_observation <- "예: 특정 연도의 기초지방자치단체 1개"

%s

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

아래 결과는 실제 데이터 분석 결과가 아니라, 입력한 연구설계와 데이터 구조를 바탕으로 생성한 **교육용 가상 결과**입니다.

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

::: {.callout-note}
이 문서의 표와 그림은 연구설계를 이해하기 위한 교육용 시뮬레이션입니다. 실제 논문에서는 실제 데이터, 식별가정, 변수 정의와 분석 사양을 바탕으로 다시 추정해야 합니다.
:::
',
    method_label(method),
    student_id,
    student_id,
    method,
    student_id,
    method,
    method_input_block(method)
  )

  writeLines(txt, path, useBytes = TRUE)

  message("")
  message("Step 2 생성 완료: ", normalizePath(path, winslash = "/", mustWork = FALSE))
  message("파일을 열어 연구설계 입력값을 수정한 뒤 Render/Preview 하세요.")
  message("완성한 QMD와 HTML은 현재 작업폴더에 저장됩니다. 자동 제출 기능은 없습니다.")

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

  # 학생에게 보이는 서로 다른 이름들을 내부 공통 구조로 정리합니다.
  covariate_names <- getv(
    "covariate_vars",
    getv("confounder_vars", getv("predictor_vars", character()))
  )
  covariate_labels <- getv(
    "covariate_labels",
    getv("confounder_labels", getv("predictor_labels", covariate_names))
  )
  covariate_types <- getv(
    "covariate_types",
    getv("confounder_types", getv("predictor_types", rep("continuous", length(covariate_names))))
  )

  if (length(covariate_names) && length(covariate_labels) != length(covariate_names)) {
    stop("변수명과 변수 레이블의 개수가 같아야 합니다.")
  }
  if (length(covariate_names) && length(covariate_types) != length(covariate_names)) {
    stop("변수명과 변수 형태의 개수가 같아야 합니다.")
  }
  if (length(covariate_types) &&
      !all(covariate_types %in% c("continuous", "binary", "categorical"))) {
    stop('변수 형태는 "continuous", "binary", "categorical" 중 하나를 사용하세요.')
  }

  time_points <- getv("time_points", character())
  treatment_start_value <- getv("treatment_start_value", NULL)
  treatment_start <- 5L
  if (length(time_points) && !is.null(treatment_start_value)) {
    hit <- match(as.character(treatment_start_value), as.character(time_points))
    if (is.na(hit)) stop("treatment_start_value는 time_points에 포함된 값이어야 합니다.")
    treatment_start <- as.integer(hit)
  }

  did_design <- if (isTRUE(getv("staggered_treatment", FALSE))) "staggered" else "standard"

  spec <- list(
    student_id = sanitize_id(getv("student_id")),
    method = method,

    research_title = getv("research_title", "연구 제목"),
    theoretical_background = getv("theoretical_background", ""),
    research_need = getv("research_need", ""),
    research_question = getv("research_question", ""),
    expected_contribution = getv("expected_contribution", ""),

    data_name = getv("data_name", "데이터"),
    unit_of_observation = getv("unit_of_observation", ""),
    data_description = getv("data_description", ""),

    outcome_var = getv("outcome_var", "outcome"),
    outcome_label = getv("outcome_label", "결과변수"),
    outcome_type = outcome_type,
    outcome_range = if (outcome_type == "binary") c(0, 1) else c(0, 100),

    treatment_var = getv("treatment_var", "treatment"),
    treatment_label = getv("treatment_label", "처치/노출"),
    treatment_type = "binary",

    instrument_var = getv("instrument_var", "instrument"),
    instrument_label = getv("instrument_label", "도구변수"),
    instrument_justification = getv("instrument_justification", ""),

    running_var = getv("running_var", "running_variable"),
    running_label = getv("running_label", "할당변수"),
    cutoff = as.numeric(getv("cutoff", 0)),
    cutoff_label = getv("cutoff_label", "기준점"),
    assignment_rule = getv("assignment_rule", ""),
    rd_design = getv("rd_type", "discontinuity"),

    covariate_names = covariate_names,
    covariate_labels = covariate_labels,
    covariate_types = covariate_types,
    modifier_names = getv("effect_modifier_vars", character()),
    modifier_labels = getv("effect_modifier_labels", character()),

    confounding_set_justification = getv("confounding_set_justification", ""),
    covariate_timing_note = getv("covariate_timing_note", ""),
    heterogeneity_rationale = getv("heterogeneity_rationale", ""),
    prediction_use = getv("prediction_use", ""),
    has_independent_test = isTRUE(getv("has_test_set", FALSE)),

    did_design = did_design,
    unit_id_var = getv("unit_id_var", "unit_id"),
    unit_id_label = getv("unit_id_label", "분석 단위"),
    time_var = getv("time_var", "time"),
    time_label = getv("time_label", "시점"),
    treatment_group_var = getv("treatment_group_var", "treated"),
    treatment_group_label = getv("treatment_group_label", "처치집단"),
    treated_definition = getv("treated_definition", ""),
    control_definition = getv("control_definition", ""),
    time_values = time_points,
    treatment_start = treatment_start,
    treatment_start_label = if (!is.null(treatment_start_value)) as.character(treatment_start_value) else "",
    cohort_var = getv("first_treatment_var", "first_treated_period"),
    cohort_label = getv("first_treatment_label", "최초 처치시점"),

    ps_method = getv("adjustment_method", "matching"),
    estimand = getv("target_effect", "ATE"),

    # 아래 값들은 학생 입력이 아니라 교육용 가상결과 생성에 필요한 내부 설정입니다.
    n = as.integer(getv("sample_size", 1500)),
    n_cont = as.integer(sum(covariate_types == "continuous")),
    n_bin = as.integer(sum(covariate_types == "binary")),
    n_cat = as.integer(sum(covariate_types == "categorical")),
    max_missing_rate = 0.08,
    seed = 2026,
    n_folds = 5,
    n_units = as.integer(getv("number_of_units", 120)),
    n_periods = if (length(time_points)) length(time_points) else 8L,
    treated_share = 0.45,
    instrument_strength = 0.35,
    caliper = 0.20,
    num_trees = 2000,
    honesty = TRUE,
    bandwidth = 0.45
  )

  if (spec$n < 50) stop("표본크기를 다시 확인하세요.")
  if (method == "did" && (spec$treatment_start <= 1 || spec$treatment_start > spec$n_periods)) {
    stop("DID 처치시점을 다시 확인하세요. 처치 이전과 이후 시점이 모두 필요합니다.")
  }
  if (method == "matching" && !spec$estimand %in% c("ATE", "ATT")) {
    stop('관심 효과는 "ATE" 또는 "ATT"로 입력하세요.')
  }
  if (method == "matching" && !spec$ps_method %in% c("matching", "weighting")) {
    stop('조정 방식은 "matching" 또는 "weighting"으로 입력하세요.')
  }
  if (method == "rd" && !spec$rd_design %in% c("discontinuity", "kink")) {
    stop('RD 유형은 "discontinuity" 또는 "kink"로 입력하세요.')
  }

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


kmu_palette <- function() {
  c(
    blue="#2F6BFF", sky="#64B5F6", teal="#22A699", orange="#F28E2B",
    coral="#E76F51", purple="#7B61FF", green="#59A14F", red="#D1495B",
    navy="#23395B", grey="#7A869A", light="#E9EEF6"
  )
}

kmu_theme <- function(base_size = 12) {
  pal <- kmu_palette()
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
  pal <- kmu_palette()
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
    kmu_theme(12)
}


make_main_figures <- function(analysis, spec) {
  d <- analysis$data
  r <- analysis$results
  pal <- kmu_palette()
  figs <- list()

  if (spec$method == "prediction") {
    figs[["그림 1. 알고리즘별 교차검증 예측성능"]] <-
      ggplot2::ggplot(r$cv, ggplot2::aes(x=algorithm, y=score, fill=algorithm)) +
      ggplot2::geom_boxplot(alpha=.82, outlier.shape=NA) +
      ggplot2::geom_jitter(width=.10, alpha=.20, size=1.3) +
      ggplot2::scale_fill_manual(values=c("Random forest"=pal[["blue"]], "XGBoost"=pal[["orange"]], "Neural network"=pal[["purple"]])) +
      ggplot2::labs(x=NULL, y=unique(r$cv$metric)) + kmu_theme(12) +
      ggplot2::theme(legend.position="none")
    if (nrow(r$importance)) {
      figs[["그림 2. 예측변수 중요도"]] <-
        ggplot2::ggplot(r$importance, ggplot2::aes(x=importance, y=reorder(variable, importance))) +
        ggplot2::geom_col(fill=pal[["teal"]], width=.72) +
        ggplot2::labs(x="상대적 중요도", y=NULL) + kmu_theme(12)
    }
  }

  if (spec$method == "dml") {
    figs[["그림 1. OLS와 DML 처치효과 추정치 비교"]] <- coef_plot(r$coef)
    figs[["그림 2. nuisance model 예측성능"]] <-
      ggplot2::ggplot(r$nuisance, ggplot2::aes(x=model, y=score, fill=model)) +
      ggplot2::geom_col(width=.65) +
      ggplot2::scale_fill_manual(values=c("Treatment nuisance"=pal[["blue"]], "Outcome nuisance"=pal[["teal"]])) +
      ggplot2::coord_cartesian(ylim=c(0,1)) +
      ggplot2::labs(x=NULL, y="예측성능 지표") + kmu_theme(12) +
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
      ggplot2::labs(x=spec$time_label %||% "시점", y=spec$outcome_label %||% "평균 결과") + kmu_theme(12)
    figs[["그림 2. Event-study 추정치"]] <-
      ggplot2::ggplot(r$event, ggplot2::aes(x=event_time, y=estimate)) +
      ggplot2::geom_hline(yintercept=0, linetype=2, colour=pal[["grey"]]) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin=conf.low, ymax=conf.high), width=0, linewidth=1.05, colour=pal[["sky"]]) +
      ggplot2::geom_point(position=ggplot2::position_nudge(x=.10), size=3.3, shape=21, fill=pal[["orange"]], colour="white", stroke=1) +
      ggplot2::labs(x="처치 전후 상대 시점", y="추정치") + kmu_theme(12)
    figs[["그림 3. 평균 DID 처치효과"]] <- coef_plot(r$coef)
  }

  if (spec$method == "iv") {
    figs[["그림 1. 도구변수별 처치확률"]] <-
      ggplot2::ggplot(r$first, ggplot2::aes(x=factor(instrument), y=treatment, fill=factor(instrument))) +
      ggplot2::geom_col(width=.62) +
      ggplot2::scale_fill_manual(values=c("0"=pal[["grey"]], "1"=pal[["blue"]])) +
      ggplot2::labs(x=spec$instrument_label %||% "도구변수", y="처치확률") +
      kmu_theme(12) + ggplot2::theme(legend.position="none")
    figs[["그림 2. OLS와 IV 추정치 비교"]] <- coef_plot(r$coef)
  }

  if (spec$method == "matching") {
    figs[["그림 1. 조정 전 성향점수 분포와 공통지지영역"]] <-
      ggplot2::ggplot(r$matched, ggplot2::aes(x=ps_hat, colour=factor(treatment), fill=factor(treatment))) +
      ggplot2::geom_density(alpha=.16, linewidth=1.05) +
      ggplot2::scale_colour_manual(values=c("0"=pal[["blue"]], "1"=pal[["orange"]])) +
      ggplot2::scale_fill_manual(values=c("0"=pal[["blue"]], "1"=pal[["orange"]])) +
      ggplot2::labs(x="추정 성향점수", y="밀도") + kmu_theme(12)
    figs[["그림 2. 조정 전후 처치효과 추정치"]] <- coef_plot(r$coef)
  }

  if (spec$method == "causal_forest") {
    figs[["그림 1. 추정 CATE 분포"]] <-
      ggplot2::ggplot(r$cate, ggplot2::aes(x=cate_hat)) +
      ggplot2::geom_histogram(bins=35, fill=pal[["purple"]], alpha=.82) +
      ggplot2::labs(x="추정 CATE", y="관측치 수") + kmu_theme(12)
    figs[["그림 2. CATE 사분위집단별 평균 처치효과"]] <-
      ggplot2::ggplot(r$groups, ggplot2::aes(x=group, y=cate_hat, fill=group)) +
      ggplot2::geom_col(width=.65) +
      ggplot2::scale_fill_manual(values=c("Q1"=pal[["sky"]], "Q2"=pal[["teal"]], "Q3"=pal[["orange"]], "Q4"=pal[["purple"]])) +
      ggplot2::labs(x="CATE 사분위집단", y="평균 추정 CATE") + kmu_theme(12) +
      ggplot2::theme(legend.position="none")
    figs[["그림 3. 전체 평균 처치효과"]] <- coef_plot(r$coef)
  }

  if (spec$method == "rd") {
    figs[["그림 1. 할당변수 분포와 기준점"]] <-
      ggplot2::ggplot(d, ggplot2::aes(x=running)) +
      ggplot2::geom_histogram(bins=40, fill=pal[["blue"]], alpha=.78) +
      ggplot2::geom_vline(xintercept=spec$cutoff, linetype=2, colour=pal[["red"]], linewidth=.9) +
      ggplot2::labs(x=spec$running_label %||% "할당변수", y="관측치 수") + kmu_theme(12)
    figs[["그림 2. 기준점 주변의 결과변수"]] <-
      ggplot2::ggplot(d, ggplot2::aes(x=running, y=y)) +
      ggplot2::geom_point(alpha=.16, colour=pal[["grey"]]) +
      ggplot2::geom_smooth(data=d[d$running < spec$cutoff,], method="lm", formula=y~poly(x,2), se=FALSE, colour=pal[["blue"]], linewidth=1.1) +
      ggplot2::geom_smooth(data=d[d$running >= spec$cutoff,], method="lm", formula=y~poly(x,2), se=FALSE, colour=pal[["orange"]], linewidth=1.1) +
      ggplot2::geom_vline(xintercept=spec$cutoff, linetype=2, colour=pal[["red"]]) +
      ggplot2::labs(x=spec$running_label %||% "할당변수", y=spec$outcome_label %||% "결과변수") + kmu_theme(12)
    figs[["그림 3. Bandwidth별 효과 추정치"]] <-
      ggplot2::ggplot(r$sensitivity, ggplot2::aes(x=bandwidth, y=estimate)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin=conf.low, ymax=conf.high), width=0, colour=pal[["sky"]], linewidth=1) +
      ggplot2::geom_point(position=ggplot2::position_nudge(x=.015), size=3.2, shape=21, fill=pal[["orange"]], colour="white", stroke=1) +
      ggplot2::labs(x="Bandwidth", y=ifelse(identical(spec$rd_design,"kink"), "RKD 추정치", "RD 추정치")) + kmu_theme(12)
    figs[[ifelse(identical(spec$rd_design,"kink"), "그림 4. 주 RKD 추정치", "그림 4. 주 RD 추정치")]] <- coef_plot(r$coef)
  }

  figs[seq_len(min(4, length(figs)))]
}


make_supplement_figures <- function(analysis, spec) {
  r <- analysis$results
  d <- analysis$data
  pal <- kmu_palette()
  figs <- list()

  if (spec$method == "dml" && all(c("true_ps","treatment") %in% names(d))) {
    tmp <- d[complete.cases(d[, c("true_ps","treatment")]), ]
    tmp$group <- ifelse(tmp$treatment == 1, "처치집단", "비교집단")
    figs[["그림 S1. 처치집단과 비교집단의 성향점수 중첩"]] <-
      ggplot2::ggplot(tmp, ggplot2::aes(x=true_ps, colour=group, fill=group)) +
      ggplot2::geom_density(alpha=.16, linewidth=1.05) +
      ggplot2::scale_colour_manual(values=c("처치집단"=pal[["orange"]], "비교집단"=pal[["blue"]])) +
      ggplot2::scale_fill_manual(values=c("처치집단"=pal[["orange"]], "비교집단"=pal[["blue"]])) +
      ggplot2::labs(x="성향점수", y="밀도") + kmu_theme(11)
  }

  if (spec$method == "did") {
    pre <- r$event[r$event$event_time < 0,]
    figs[["그림 S1. 처치 이전 event-study 계수"]] <-
      ggplot2::ggplot(pre, ggplot2::aes(x=event_time, y=estimate)) +
      ggplot2::geom_hline(yintercept=0, linetype=2, colour=pal[["grey"]]) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin=conf.low, ymax=conf.high), width=0, colour=pal[["sky"]], linewidth=1) +
      ggplot2::geom_point(position=ggplot2::position_nudge(x=.10), size=3.1, shape=21, fill=pal[["orange"]], colour="white", stroke=1) +
      ggplot2::labs(x="처치 이전 상대 시점", y="추정치") + kmu_theme(11)
  }

  if (spec$method == "matching") {
    bal_long <- tidyr::pivot_longer(r$balance, c(before, after), names_to="stage", values_to="smd")
    bal_long$stage <- factor(bal_long$stage, levels=c("before","after"), labels=c("조정 전","조정 후"))
    figs[["그림 S1. 공변량 균형 Love plot"]] <-
      ggplot2::ggplot(bal_long, ggplot2::aes(x=abs(smd), y=reorder(variable, abs(smd)), colour=stage)) +
      ggplot2::geom_vline(xintercept=.10, linetype=2, colour=pal[["red"]]) +
      ggplot2::geom_point(size=2.8) +
      ggplot2::scale_colour_manual(values=c("조정 전"=pal[["orange"]], "조정 후"=pal[["blue"]])) +
      ggplot2::labs(x="절대 표준화 평균차이", y=NULL) + kmu_theme(11)
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
