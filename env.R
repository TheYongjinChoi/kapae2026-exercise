# ============================================================
# ML 실습 환경 자동 설치
# ============================================================

# ------------------------------------------------------------
# 1. R 패키지 설치
# ------------------------------------------------------------

packages <- c(
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
  "reticulate"
)

new_packages <- packages[
  !packages %in% rownames(installed.packages())
]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}


# ------------------------------------------------------------
# 2. Python + Keras 환경
# ------------------------------------------------------------

library(reticulate)

keras_env <- "r-keras"
python_version <- "3.10"


# 이미 r-keras 환경이 존재하는지 확인
envs <- tryCatch(
  virtualenv_list(),
  error = function(e) character(0)
)

keras_env_exists <- keras_env %in% envs


# ------------------------------------------------------------
# 3. r-keras 환경이 없을 때만 새로 생성
# ------------------------------------------------------------

if (!keras_env_exists) {

  message("Python 3.10 기반의 Keras 환경을 설치합니다.")

  keras3::install_keras(
    envname = keras_env,
    python_version = python_version,
    backend = "tensorflow",
    restart_session = FALSE
  )

} else {

  message("기존 'r-keras' 환경을 사용합니다.")

}


# ------------------------------------------------------------
# 4. Python 환경 연결
# ------------------------------------------------------------

# 중요:
# R 세션에서 이미 다른 Python이 초기화되어 있다면
# use_python()으로 강제로 변경하지 않습니다.

current_python <- tryCatch(
  py_config()$python,
  error = function(e) NULL
)

if (is.null(current_python)) {

  # 아직 Python이 초기화되지 않은 경우에만 r-keras 연결
  use_virtualenv(
    keras_env,
    required = TRUE
  )

} else {

  message(
    "이미 초기화된 Python을 그대로 사용합니다:\n",
    current_python
  )

}


# ------------------------------------------------------------
# 5. 최종 환경 확인
# ------------------------------------------------------------

cat("\n")
cat("========================================\n")
cat(" R package installation\n")
cat("========================================\n")

for (pkg in packages) {

  status <- if (requireNamespace(pkg, quietly = TRUE)) {
    "OK"
  } else {
    "FAILED"
  }

  cat(sprintf("%-12s : %s\n", pkg, status))
}


cat("\n")
cat("========================================\n")
cat(" Python environment\n")
cat("========================================\n")

tryCatch(
  {
    print(py_config())
  },
  error = function(e) {
    message("Python 환경 확인 실패: ", conditionMessage(e))
  }
)


cat("\n")
cat("========================================\n")
cat(" Installation complete!\n")
cat("========================================\n")
