# ============================================================
# DML 실습 환경 자동 설치
# ============================================================

# 이 파일을 DML 실습 qmd와 같은 폴더에 두고 한 번 실행합니다.
# 이미 설치된 패키지는 다시 설치하지 않습니다.

# ------------------------------------------------------------
# 1. CRAN 저장소 확인
# ------------------------------------------------------------

repos <- getOption("repos")

if (is.null(repos) ||
    identical(unname(repos["CRAN"]), "@CRAN@") ||
    is.na(repos["CRAN"])) {

  options(
    repos = c(
      CRAN = "https://cloud.r-project.org"
    )
  )
}


# ------------------------------------------------------------
# 2. DML 실습에 필요한 패키지
# ------------------------------------------------------------

packages <- c(
  # 데이터 처리와 시각화
  "dplyr",
  "tidyr",
  "tibble",
  "ggplot2",
  "patchwork",

  # DoubleML
  "DoubleML",
  "mlr3",
  "mlr3learners",
  "mlr3tuning",
  "paradox",

  # 학습 알고리즘
  "ranger",
  "xgboost",
  "glmnet",

  # DoubleML / mlr3 출력과 튜닝
  "data.table",
  "lgr",
  "bbotk"
)


# ------------------------------------------------------------
# 3. 설치되지 않은 패키지만 설치
# ------------------------------------------------------------

installed <- rownames(
  installed.packages()
)

new_packages <- setdiff(
  packages,
  installed
)

if (length(new_packages) > 0) {

  cat(
    "\n설치가 필요한 패키지:\n",
    paste(new_packages, collapse = ", "),
    "\n\n"
  )

  install.packages(
    new_packages,
    dependencies = TRUE
  )

} else {

  message(
    "DML 실습에 필요한 R 패키지가 모두 설치되어 있습니다."
  )

}


# ------------------------------------------------------------
# 4. 핵심 패키지 로드 확인
# ------------------------------------------------------------

cat("\n")
cat("========================================\n")
cat(" DML 실습 패키지 확인\n")
cat("========================================\n")

status <- vapply(
  packages,
  function(pkg) {
    requireNamespace(
      pkg,
      quietly = TRUE
    )
  },
  logical(1)
)

for (pkg in packages) {

  result <- if (status[[pkg]]) {
    "OK"
  } else {
    "FAILED"
  }

  cat(
    sprintf(
      "%-14s : %s\n",
      pkg,
      result
    )
  )
}


# ------------------------------------------------------------
# 5. 설치 실패 여부 확인
# ------------------------------------------------------------

failed <- names(
  status[!status]
)

if (length(failed) > 0) {

  cat("\n")
  stop(
    paste0(
      "다음 패키지 설치를 확인해 주세요: ",
      paste(failed, collapse = ", "),
      "\n",
      "패키지 설치 오류가 있었다면 R 세션을 재시작한 뒤 env.R을 다시 실행하세요."
    ),
    call. = FALSE
  )

}


# ------------------------------------------------------------
# 6. DoubleML learner 확인
# ------------------------------------------------------------

suppressPackageStartupMessages(
  library(mlr3)
)

suppressPackageStartupMessages(
  library(mlr3learners)
)

required_learners <- c(
  "regr.lm",
  "regr.ranger",
  "regr.xgboost",
  "classif.ranger"
)

available_learners <- mlr3::mlr_learners$keys()

missing_learners <- setdiff(
  required_learners,
  available_learners
)

cat("\n")
cat("========================================\n")
cat(" mlr3 learner 확인\n")
cat("========================================\n")

for (learner in required_learners) {

  result <- if (
    learner %in% available_learners
  ) {
    "OK"
  } else {
    "FAILED"
  }

  cat(
    sprintf(
      "%-18s : %s\n",
      learner,
      result
    )
  )
}

if (length(missing_learners) > 0) {

  stop(
    paste0(
      "다음 mlr3 learner를 사용할 수 없습니다: ",
      paste(missing_learners, collapse = ", "),
      "\n",
      "mlr3learners와 해당 알고리즘 패키지 설치 상태를 확인하세요."
    ),
    call. = FALSE
  )

}


# ------------------------------------------------------------
# 7. 최종 안내
# ------------------------------------------------------------

cat("\n")
cat("========================================\n")
cat(" DML 실습 환경 설정 완료\n")
cat("========================================\n")
cat(
  "이제 2-1DML 실습 파일을 위에서부터 순서대로 실행하세요.\n"
)
