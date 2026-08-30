# 2026 국민대학교 행정학과 방법론 워크숍 — Standalone 버전

이 버전에서 **standalone**은 학생이 GitHub 저장소를 clone하거나 프로젝트 설정을 할 필요 없이 `01_student_setup.qmd` 하나로 실습을 시작한다는 의미입니다.

실습을 구동하는 R 함수 자체는 GitHub의 `standalone/R/` 폴더에 그대로 두고, Step 1과 Step 2가 필요할 때 raw GitHub URL로 직접 불러옵니다.

학생 결과를 GitHub로 보고하거나 제출하는 기능만 제거했습니다.

## GitHub에 필요한 파일

```text
kmu-ml-projects/
└── standalone/
    ├── README.md
    ├── 01_student_setup.qmd
    └── R/
        ├── lab_helpers.R
        └── report_helpers.R
```

이 네 파일이면 충분합니다.

기존 KAPAE 버전에 있던 아래 파일과 기능은 사용하지 않습니다.

```text
config.json
standalone_setup.R
post_render_submit.R
render_submissions.R
submission_api/
worker.js
wrangler.jsonc
docs/
_quarto.yml
```

## 동작 구조

```text
학생이 01_student_setup.qmd 실행
        ↓
GitHub standalone/R/lab_helpers.R source
        ↓
02_<ID>_<method>_research_design.qmd 생성
        ↓
학생이 Step 2 작성
        ↓
Render / Preview
        ↓
GitHub에서 lab_helpers.R + report_helpers.R source
        ↓
로컬 HTML 생성
```

GitHub는 **R 함수를 배포하는 곳**으로만 사용됩니다. 학생 QMD나 HTML은 GitHub API, Cloudflare Worker, post-render hook 등으로 전송되지 않습니다.

## Step 2가 불러오는 파일

생성된 Step 2 QMD에는 다음 source가 들어갑니다.

```r
source("https://raw.githubusercontent.com/TheYongjinChoi/kmu-ml-projects/main/standalone/R/lab_helpers.R")
source("https://raw.githubusercontent.com/TheYongjinChoi/kmu-ml-projects/main/standalone/R/report_helpers.R")
```

따라서 학생 로컬 폴더에는 helper R 파일을 복사하지 않습니다.

## 장단점

이 구조의 장점은 학생에게 전달할 파일이 사실상 `01_student_setup.qmd` 하나뿐이라는 점입니다. 강사는 GitHub의 `standalone/R/`만 관리하면 됩니다.

반대로 렌더할 때마다 GitHub에서 helper를 불러오므로 인터넷 연결이 필요하며, 수업 도중 helper를 수정하면 이후 렌더 결과에 즉시 반영됩니다. 따라서 수업 직전에는 `lab_helpers.R`와 `report_helpers.R`를 테스트한 뒤 가능하면 수업 중에는 수정하지 않는 것이 안전합니다.

## GitHub에 올리기

저장소 루트에서 이 폴더를 `standalone/`으로 복사한 뒤:

```bash
git add standalone
git commit -m "Add KMU standalone methodology workshop"
git pull --rebase origin main
git push origin main
```

올린 뒤 아래 두 raw URL이 브라우저에서 열리는지 확인합니다.

```text
https://raw.githubusercontent.com/TheYongjinChoi/kmu-ml-projects/main/standalone/R/lab_helpers.R
https://raw.githubusercontent.com/TheYongjinChoi/kmu-ml-projects/main/standalone/R/report_helpers.R
```

## 수업 전 테스트

빈 폴더에 `01_student_setup.qmd` 하나만 넣고 다음 세 방법은 최소한 한 번씩 확인하는 것을 권장합니다.

- `prediction`: 일반적인 Step 2 생성과 표/그림 렌더 확인
- `did`: 패널·시점 구조와 event-study 결과 확인
- `rd`: cutoff, bandwidth, density/donut 진단 확인

테스트할 때는 `student_id <- "TEST01"`처럼 임시 ID를 사용합니다.

## 중요한 구분

`report_helpers.R`라는 이름의 **report**는 논문형 결과와 그림을 만드는 R 함수라는 뜻이므로 그대로 유지합니다.

이번 버전에서 제거한 것은 학생 결과를 GitHub로 **report/submit**하는 네트워크 기능입니다. 즉 `report_helpers.R`는 필요하지만 `post_render_submit.R`, Worker, config, gallery는 필요하지 않습니다.
