# ============================================================
#  KAPAE 2026 워크숍 · 실습 환경 자동 설치
#  파일명: env00.R
#
#  사용법: R 콘솔에서 아래 한 줄을 실행하세요.
#    source("env00.R")
#
#  하는 일
#    1. 문서 렌더링에 필요한 패키지 설치
#    2. 분석에 필요한 R 패키지 설치
#    3. Quarto 실행 파일 확인
#    4. Python + Keras 환경 설치 및 연결
#    5. 최종 확인 결과 출력
# ============================================================


# ------------------------------------------------------------
# 1. 패키지 설치
# ------------------------------------------------------------

# qmd 문서를 HTML로 렌더링할 때 필요한 패키지
render_packages <- c(
  "knitr",        # 코드 블록을 실행해 결과를 문서에 삽입
  "rmarkdown",    # 렌더링 파이프라인
  "markdown",     # 마크다운 변환
  "evaluate",     # 코드 실행과 출력 수집
  "yaml",         # YAML 머리말 해석
  "xfun",         # knitr 계열 보조 함수
  "htmltools",    # HTML 출력 생성
  "jsonlite",     # JSON 처리
  "digest",       # 캐시 식별자 생성
  "downlit"       # 코드 구문 강조와 링크
)

# 실습에서 사용하는 분석 패키지
analysis_packages <- c(
  "dplyr",
  "tidyr",
  "tibble",
  "ggplot2",
  "glmnet",
  "ranger",
  "rpart",
  "rpart.plot",
  "xgboost",
  "keras3",
  "reticulate",
  "httr2"         # 진척 기록 전송
)

packages <- c(render_packages, analysis_packages)

new_packages <- packages[
  !packages %in% rownames(installed.packages())
]

if (length(new_packages) > 0) {
  message("설치할 패키지: ", paste(new_packages, collapse = ", "))
  install.packages(new_packages)
} else {
  message("모든 패키지가 이미 설치되어 있습니다.")
}


# ------------------------------------------------------------
# 2. Quarto 실행 파일 확인
# ------------------------------------------------------------
# Quarto는 R 패키지가 아니라 별도의 프로그램입니다.
# RStudio와 Positron에는 함께 설치되어 있지만, VS Code나
# 터미널에서만 작업하는 경우에는 따로 받아야 합니다.

quarto_path <- Sys.which("quarto")

quarto_ok <- nzchar(quarto_path)

if (quarto_ok) {
  quarto_ver <- tryCatch(
    system2("quarto", "--version", stdout = TRUE),
    error = function(e) "확인 불가"
  )
} else {
  quarto_ver <- NA_character_
  message(
    "Quarto 실행 파일을 찾지 못했습니다.\n",
    "https://quarto.org/docs/get-started/ 에서 설치한 뒤 ",
    "R 세션을 재시작하고 이 스크립트를 다시 실행하세요.\n",
    "RStudio나 Positron을 쓰신다면 대개 이미 설치되어 있습니다."
  )
}


# ------------------------------------------------------------
# 3. Python + Keras 환경
# ------------------------------------------------------------

library(reticulate)

keras_env <- "r-keras"
python_version <- "3.10"

# 중요: py_config()는 호출 자체가 reticulate를 초기화시킬 수 있습니다.
# 초기화 여부만 "부작용 없이" 확인하려면 py_available(initialize = FALSE)를 씁니다.
python_already_initialized <- py_available(initialize = FALSE)

# 이미 r-keras 환경이 존재하는지 확인
envs <- tryCatch(
  virtualenv_list(),
  error = function(e) character(0)
)

keras_env_exists <- keras_env %in% envs


# ------------------------------------------------------------
# 4. r-keras 환경 설치 (안전할 때만 시도)
# ------------------------------------------------------------
# install_keras()는 내부적으로 파이썬 초기화를 유발합니다.
# 이미 다른 파이썬이 초기화된 세션에서 이걸 부르면 바로 에러가 납니다.
# 따라서 "환경이 없다"는 조건 하나만으로 설치를 시도하지 않고,
# "다른 파이썬이 이미 초기화돼 있지는 않은가"도 함께 확인합니다.

if (!keras_env_exists && !python_already_initialized) {

  message("Python 3.10 기반의 Keras 환경을 설치합니다.")

  keras3::install_keras(
    envname = keras_env,
    python_version = python_version,
    backend = "tensorflow",
    restart_session = FALSE
  )

  # 설치 직후 상태 갱신
  python_already_initialized <- py_available(initialize = FALSE)

} else if (!keras_env_exists && python_already_initialized) {

  message(
    "'r-keras' 환경이 없지만, 이 세션에는 이미 다른 Python이 초기화되어 있어 ",
    "설치를 진행할 수 없습니다.\n",
    "R 세션을 재시작한 뒤(Session > Restart R) 이 스크립트를 다시 실행해 주세요."
  )

} else {

  message("기존 'r-keras' 환경을 사용합니다.")

}


# ------------------------------------------------------------
# 5. Python 환경 연결
# ------------------------------------------------------------
# 이미 다른 Python이 초기화되어 있다면 use_virtualenv()로
# 강제 변경하지 않습니다 (세션 재시작 없이는 불가능하고, 강행하면 에러).

if (!python_already_initialized && keras_env_exists) {

  use_virtualenv(
    keras_env,
    required = TRUE
  )
  python_already_initialized <- TRUE

} else if (python_already_initialized) {

  # py_config() 호출도 이 시점에는 안전합니다 (이미 초기화된 상태이므로).
  current_python <- tryCatch(
    py_config()$python,
    error = function(e) NULL
  )

  message(
    "이미 초기화된 Python을 그대로 사용합니다:\n",
    if (is.null(current_python)) "(확인 불가)" else current_python
  )

}


# ------------------------------------------------------------
# 6. 최종 환경 확인
# ------------------------------------------------------------

cat("\n")
cat("========================================\n")
cat(" 문서 렌더링 패키지\n")
cat("========================================\n")

for (pkg in render_packages) {
  status <- if (requireNamespace(pkg, quietly = TRUE)) "OK" else "FAILED"
  cat(sprintf("%-12s : %s\n", pkg, status))
}


cat("\n")
cat("========================================\n")
cat(" 분석 패키지\n")
cat("========================================\n")

for (pkg in analysis_packages) {
  status <- if (requireNamespace(pkg, quietly = TRUE)) "OK" else "FAILED"
  cat(sprintf("%-12s : %s\n", pkg, status))
}


cat("\n")
cat("========================================\n")
cat(" Quarto\n")
cat("========================================\n")

if (quarto_ok) {
  cat(sprintf("%-12s : %s\n", "quarto", paste(quarto_ver, collapse = " ")))
  cat(sprintf("%-12s : %s\n", "경로", quarto_path))
} else {
  cat("quarto       : NOT FOUND\n")
  cat("https://quarto.org/docs/get-started/ 에서 설치하세요.\n")
}


cat("\n")
cat("========================================\n")
cat(" Python 환경\n")
cat("========================================\n")

if (python_already_initialized) {
  tryCatch(
    {
      print(py_config())
    },
    error = function(e) {
      message("Python 환경 확인 실패: ", conditionMessage(e))
    }
  )
} else {
  message(
    "Python이 아직 연결되지 않았습니다. ",
    "위 안내에 따라 R 세션을 재시작한 뒤 다시 실행해 주세요."
  )
}


cat("\n")
cat("========================================\n")
cat(" 설치 완료\n")
cat("========================================\n")
cat("FAILED 항목이 없다면 실습을 시작하셔도 됩니다.\n")
