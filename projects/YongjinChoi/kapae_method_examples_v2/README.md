# KAPAE 2026 방법론별 예시 QMD

이 예시들은 최신 `lab_helpers.R` + `report_helpers.R` 구조에 맞춰 작성했습니다.

## 포함 예시

1. `example_01_prediction_classification.qmd`
   - 이진 분류
   - 아동 MMR 접종 누락 예측

2. `example_02_prediction_regression.qmd`
   - 연속형 예측
   - 지역별 다음 해 독감 예방접종률 예측

3. `example_03_dml.qmd`
   - DML
   - 온라인 백신 허위정보 노출 → 백신 신뢰도

4. `example_04_did_standard.qmd`
   - 공통 도입시점의 표준 DID
   - 학교 기반 HPV 문자 알림 정책

5. `example_05_did_staggered.qmd`
   - staggered DID
   - 지방정부별 예약지원 서비스 순차 도입

6. `example_06_iv.qmd`
   - IV / 2SLS
   - 무작위 상담 초대장을 도구변수로 사용

7. `example_07_propensity_matching.qmd`
   - propensity-score matching
   - ATT

8. `example_08_propensity_weighting.qmd`
   - propensity-score weighting
   - ATE

9. `example_09_causal_forest.qmd`
   - CATE / heterogeneous treatment effects

10. `example_10_rd_discontinuity.qmd`
    - 표준 RD, level discontinuity

11. `example_11_rkd_kink.qmd`
    - Regression Kink Design, slope change

## 중요: report_helpers.R 업데이트

이 번들의 `projects/design_lab/R/report_helpers.R`에는 staggered DID를
처치 cohort별 event time으로 계산하고, 각 시점에서 never-treated 또는
not-yet-treated 단위를 비교집단으로 사용하는 교육용 구현을 추가했습니다.

기존 `report_helpers.R`를 이 파일로 교체한 뒤 예시를 렌더하는 것을 권장합니다.

## 로컬 렌더

예시 파일명은 일부러 `02_..._research_design.qmd` 형식을 사용하지 않았습니다.
따라서 제출용 `_quarto.yml`이 있는 프로젝트 안에서 렌더하더라도
post-render 자동 제출 대상에서 제외됩니다.

예:

```bash
quarto render examples/example_05_did_staggered.qmd
```

또는 Positron에서 각 QMD를 열고 Preview/Render를 누르면 됩니다.

예시 QMD는 GitHub `main` 브랜치의 다음 두 helper를 source합니다.

- `projects/design_lab/R/lab_helpers.R`
- `projects/design_lab/R/report_helpers.R`

따라서 먼저 수정된 `report_helpers.R`를 GitHub에 push해야 합니다.
