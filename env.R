# ============================================================
# ML 실습 환경 설치
# ============================================================

# R packages --------------------------------------------------

packages <- c(
  "dplyr",
  "ggplot2",
  "tibble",
  "tidyr",
  "glmnet",
  "keras3",
  "ranger",
  "rpart",
  "rpart.plot",
  "xgboost",
  "reticulate"
)

new_packages <- packages[
  !packages %in% rownames(installed.packages())
]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}


# Python + Keras environment ---------------------------------

library(reticulate)

# Python 3.10이 없으면 설치
tryCatch(
  {
    use_python_version("3.10", required = TRUE)
  },
  error = function(e) {
    message("Python 3.10이 없습니다. 설치합니다...")
    install_python("3.10:latest")
    use_python_version("3.10", required = TRUE)
  }
)

# Keras + TensorFlow 설치
keras3::install_keras(
  envname = "r-keras",
  python_version = "3.10",
  backend = "tensorflow",
  restart_session = FALSE
)


# 환경 확인 ---------------------------------------------------

cat("\n==============================\n")
cat("R package installation\n")
cat("==============================\n")

for (pkg in packages) {
  cat(
    sprintf(
      "%-12s : %s\n",
      pkg,
      if (requireNamespace(pkg, quietly = TRUE))
        "OK"
      else
        "FAILED"
    )
  )
}

cat("\n==============================\n")
cat("Python environment\n")
cat("==============================\n")

use_virtualenv("r-keras", required = TRUE)

print(py_config())

cat("\n==============================\n")
cat("Installation complete!\n")
cat("==============================\n")