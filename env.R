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
# 3. r-keras 환경 설치 (안전할 때만 시도)
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
# 4. Python 환경 연결
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
# 5. 최종 환경 확인
# ------------------------------------------------------------

cat("\n")
cat("========================================\n")
cat(" R 패키지 설치 확인\n")
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
cat(" Python 환경 체크\n")
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
cat(" 설치 완료!\n")
cat("========================================\n")
