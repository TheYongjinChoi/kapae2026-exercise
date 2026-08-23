# ══════════════════════════════════════════════════════════════════════════════
#  KAPAE 2026 워크숍 · 1일차 4강 실습 채점 스크립트
#  대상 문서: 1-4Causal_sol.qmd (인과추론 리뷰: DID, RD, IV)
#
#  문항 ID 목록 (qmd의 각 빈칸 블록 하단에 아래 순서대로 삽입)
#    causal-01-did22      Part 1 / Task 1-1  2x2 DID
#    causal-02-event      Part 1 / Task 1-2  사건연구
#    causal-03-rdplot     Part 2 / Task 2-1  rdplot
#    causal-04-rd         Part 2 / Task 2-2  RD 추정
#    causal-05-rdbw       Part 2 / Task 2-3  조작 검정과 대역폭 민감도
#    causal-06-wald       Part 3 / Task 3-1  Wald 추정량
#    causal-07-2sls       Part 3 / Task 3-2  2SLS
#    causal-08-controls   Part 3 / Task 3-3  통제변수 포함 모형 (선택)
#
#  ── 공통 오류 ───────────────────────────────────────────────────────────────
#     · 블록을 순서대로 실행하지 않아 앞 객체가 없음 → "객체를 찾을 수 없습니다"
#     · 빈칸 _____ 를 그대로 둔 채 실행 → 구문 오류
#     · set_student() 미실행 → 제출은 되지만 unknown 으로 기록됨
#     · 패키지 미설치 (fixest, rdrobust, rddensity) → could not find function
# ══════════════════════════════════════════════════════════════════════════════

SUPABASE_URL <- "https://mztyhpckshnqcklogrsn.supabase.co"
SUPABASE_KEY <- "sb_publishable_pflU44StAqW5XTy94LsuJA_p_zMRtVv"   # publishable key

CHAPTER <- "d1-04"

# ── 내부 상태 ─────────────────────────────────────────────────────
.tracker_env <- new.env()
.tracker_env$session_id <- paste0(
  format(Sys.time(), "%Y%m%d%H%M%S"), "-",
  paste(sample(letters, 6, TRUE), collapse = "")
)

`%||%` <- function(a, b) if (is.null(a)) b else a

.has <- function(e, nm) !is.null(e[[nm]])

# 정답 메시지를 콘솔 폭에 맞춰 접습니다.
.wrap <- function(txt, width = 76, indent = "   ") {
  ws <- strwrap(txt, width = width)
  paste(paste0(indent, ws), collapse = "\n")
}

# 호출 환경과 전역 환경의 객체를 리스트로 모읍니다.
# 환경에 $로 접근하면 상위 환경을 찾지 않아 값이 NULL이 되므로,
# 규칙 검사 전에 반드시 리스트로 바꿔 두어야 합니다.
.snapshot <- function(env) {
  e <- as.list(env, all.names = TRUE)
  if (!identical(env, globalenv())) {
    g <- as.list(globalenv(), all.names = TRUE)
    e <- utils::modifyList(g, e)
  }
  e
}

set_student <- function(id) {
  if (missing(id) || !nzchar(id) || id %in% c("ID 입력", "학번입력", "are", "abc")) {
    stop("첫 청크의 set_student()에 본인 ID를 입력하세요.", call. = FALSE)
  }
  .tracker_env$student_id <- id
  message("\u2713 ", id, " 님, 실습을 시작합니다.")
  invisible(id)
}

classify_error <- function(msg) {
  if (grepl("could not find function", msg)) return("함수없음")
  if (grepl("object .* not found", msg))     return("객체없음")
  if (grepl("cannot open|No such file", msg)) return("파일경로")
  if (grepl("unused argument|argument", msg)) return("인수오류")
  if (grepl("undefined columns|subscript", msg)) return("인덱싱")
  if (grepl("missing value|NA/NaN", msg))    return("결측값")
  if (grepl("levels|factor", msg))           return("factor수준")
  if (grepl("non-numeric|invalid 'type'", msg)) return("자료형")
  "기타"
}

# ── 보조 함수 ────────────────────────────────────────────────
.num <- function(x) suppressWarnings(as.numeric(as.matrix(x)))

.eq <- function(a, b, tol = 1e-6) {
  a <- .num(a); b <- .num(b)
  if (length(a) != length(b) || anyNA(a) || anyNA(b)) return(FALSE)
  isTRUE(all.equal(a, b, tolerance = tol))
}

.scalar <- function(x) is.numeric(.num(x)) && length(.num(x)) == 1 && !anyNA(.num(x))

# fixest 객체를 안전하게 다루기
.cf <- function(m, nm) {
  cf <- tryCatch(stats::coef(m), error = function(e) NULL)
  if (is.null(cf) || !(nm %in% names(cf))) return(NA_real_)
  unname(cf[[nm]])
}

.cfnames <- function(m) {
  cf <- tryCatch(names(stats::coef(m)), error = function(e) NULL)
  if (is.null(cf)) character(0) else cf
}

# 모형 공식의 문자열 표현 (fixest 포함)
.fml_txt <- function(m) {
  f <- tryCatch(stats::formula(m), error = function(e) NULL)
  if (is.null(f)) return("")
  paste(deparse(f), collapse = " ")
}

# 호출에 쓰인 인수를 문자열로 (없으면 NA)
.callarg <- function(m, nm) {
  cl <- tryCatch(m$call, error = function(e) NULL)
  if (is.null(cl) || is.null(cl[[nm]])) return(NA_character_)
  paste(deparse(cl[[nm]]), collapse = "")
}

# 안전 접근
.get <- function(expr) tryCatch(expr, error = function(e) NULL)


# ══════════════════════════════════════════════════════════════════
#  정답 규칙
#  need    : 학생이 만들어야 하는 객체
#  rules   : 순서대로 검사하며, 첫 실패의 msg가 힌트로 기록됨
#  caution : 정답이지만 함께 짚어야 할 조건부 안내
#  note    : 정답일 때 출력되는 결과 해석
# ══════════════════════════════════════════════════════════════════

CHECKS <- list(

# ─────────────────────────────────────────────────────────────────
# Task 1-1. 2x2 DID
# ─────────────────────────────────────────────────────────────────
"causal-01-did22" = list(
  need = c("did_df", "did_true", "did_fit"),
  call_from = "did_fit",
  note = "추정치가 참값 2.0보다 위에 놓입니다. x1과 x2를 통제했는데도 그렇습니다. 이 데이터에서는 정책 도입 여부가 두 지역 특성과 관련되어 있고, 2019년 이후 정책과 무관하게 발생한 변화 역시 같은 특성과 관련되어 있습니다. 처치집단은 정책 때문이 아니라 원래 그런 지역이어서 사후에 더 올라간 부분이 있는데, 그 몫이 처치효과 계수에 함께 잡힌 것입니다. 통제항 x1:post와 x2:post가 그 변화를 직선으로 근사하고 있어서, 특성과 사후 변화의 관계가 직선이 아니면 통제하고 남는 부분이 계속 남습니다. 추정치가 참값에 도달하지 못한 이유를 변수를 빠뜨린 데서 찾을 수 없다는 점이 이 결과의 핵심입니다.",
  rules = list(

    # 객체 형태
    list(f = function(e) inherits(e$did_fit, "fixest"),
         msg = "did_fit이 fixest 객체가 아닙니다. feols()의 결과를 did_fit에 저장하세요."),

    # 결과변수
    list(f = function(e) grepl("^\\s*y\\s*~", .fml_txt(e$did_fit)),
         msg = "결과변수가 y가 아닙니다. 공식의 ~ 왼쪽에는 y를 씁니다."),

    # 핵심 상호작용
    list(f = function(e) "treat:post" %in% .cfnames(e$did_fit),
         msg = "treat:post 계수가 없습니다. treat * post로 써야 두 변수와 상호작용항이 함께 들어갑니다. +로 이으면 상호작용항이 생기지 않습니다."),

    list(f = function(e) all(c("treat", "post") %in% .cfnames(e$did_fit)),
         msg = "treat 또는 post의 주효과가 빠졌습니다. treat:post처럼 콜론만 쓰면 주효과 없이 상호작용만 들어갑니다. *를 쓰세요."),

    # 공변량 상호작용
    list(f = function(e) all(c("x1", "x2") %in% .cfnames(e$did_fit)),
         msg = "x1 또는 x2가 모형에 없습니다. x1 * post와 x2 * post를 함께 넣으세요."),

    list(f = function(e) any(c("post:x1", "x1:post") %in% .cfnames(e$did_fit)),
         msg = "x1과 post의 상호작용항이 없습니다. x1 + post가 아니라 x1 * post로 쓰세요."),

    list(f = function(e) any(c("post:x2", "x2:post") %in% .cfnames(e$did_fit)),
         msg = "x2와 post의 상호작용항이 없습니다. x2 * post로 쓰세요."),

    # 고정효과를 넣으면 treat이 흡수되어 이 문항의 구조가 달라집니다
    list(f = function(e) {
           fx <- .get(e$did_fit$fixef_vars)
           is.null(fx) || length(fx) == 0
         },
         msg = "이 문항에서는 고정효과를 넣지 않습니다. 수직선(|) 없이 y ~ treat * post + x1 * post + x2 * post 형태로 쓰세요. 고정효과는 다음 Task에서 다룹니다."),

    # 데이터
    list(f = function(e) {
           d <- .callarg(e$did_fit, "data")
           is.na(d) || identical(d, "did_df")
         },
         msg = "data 인수가 did_df가 아닙니다. DID는 패널 데이터로 추정합니다."),

    list(f = function(e) {
           nn <- .get(nobs(e$did_fit))
           is.null(nn) || isTRUE(nn == nrow(e$did_df))
         },
         msg = "추정에 사용된 관측치 수가 did_df와 다릅니다. 데이터를 걸러내지 말고 그대로 넣으세요."),

    # 군집 표준오차
    list(f = function(e) {
           v <- paste(.callarg(e$did_fit, "vcov"), .callarg(e$did_fit, "cluster"))
           grepl("id", v) || is.na(v)
         },
         msg = "군집 표준오차가 지정되지 않았습니다. vcov = ~ id로 지역 단위 군집을 지정하세요. 같은 지역의 여러 연도 관측치가 서로 상관되어 있습니다."),

    # 추정 자체가 성립했는가
    list(f = function(e) is.finite(.cf(e$did_fit, "treat:post")),
         msg = "treat:post 계수가 계산되지 않았습니다. 변수 간 완전한 공선성이 없는지 확인하세요."),

    list(f = function(e) abs(.cf(e$did_fit, "treat:post")) < 20,
         msg = "추정치가 비정상적으로 큽니다. 데이터 생성 블록부터 다시 실행해 보세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 1-2. 사건연구
# ─────────────────────────────────────────────────────────────────
"causal-02-event" = list(
  need = c("did_df", "did_es"),
  call_from = "did_es",
  note = "처치 이전 계수들은 0 근처에 머무는데 처치 이후 계수는 위로 벌어집니다. 이 데이터에서 정책과 무관한 변화는 2019년부터 시작되도록 만들었으므로, 사전 구간만 보면 평행추세에 문제가 없어 보입니다. 사전 계수가 0이라는 것은 처치 이전까지 두 집단이 나란히 움직였다는 뜻일 뿐, 처치 이후에도 나란했을 것이라는 보장은 아닙니다. 사건연구가 검정하는 것과 DID가 필요로 하는 가정이 같지 않다는 점을 여기서 확인해 두시면 됩니다. 반사실은 관측되지 않으므로 가정 자체를 검정할 방법은 없습니다.",
  rules = list(

    list(f = function(e) inherits(e$did_es, "fixest"),
         msg = "did_es가 fixest 객체가 아닙니다. feols()의 결과를 did_es에 저장하세요."),

    # i() 항이 들어갔는가
    list(f = function(e) grepl("i\\(", .fml_txt(e$did_es)),
         msg = "i() 항이 없습니다. i(rel_year, treat, ref = -1)로 상대시간별 계수를 만드세요."),

    list(f = function(e) grepl("rel_year", .fml_txt(e$did_es)),
         msg = "i()의 첫 인수가 rel_year가 아닙니다. 처치 시점을 0으로 놓은 상대시간 변수입니다. year를 넣으면 연도별 계수가 되어 해석이 달라집니다."),

    list(f = function(e) grepl("treat", .fml_txt(e$did_es)),
         msg = "i()의 두 번째 인수가 treat이 아닙니다. 상대시간과 처치집단 여부를 곱한 항이 필요합니다."),

    list(f = function(e) grepl("ref\\s*=\\s*-\\s*1", .fml_txt(e$did_es)),
         msg = "기준 시점이 -1이 아닙니다. ref = -1로 처치 직전 시점을 기준으로 삼아야 계수를 그 시점 대비 차이로 읽을 수 있습니다."),

    # 고정효과
    list(f = function(e) {
           fx <- .get(e$did_es$fixef_vars)
           !is.null(fx) && length(fx) >= 2
         },
         msg = "고정효과가 부족합니다. 수직선 뒤에 id + year를 모두 넣으세요."),

    list(f = function(e) {
           fx <- .get(e$did_es$fixef_vars)
           is.null(fx) || setequal(fx, c("id", "year"))
         },
         msg = "고정효과가 id와 year가 아닙니다. 지역 고정효과와 연도 고정효과 두 개를 넣습니다."),

    # 공변량 통제
    list(f = function(e) grepl("post:x1|x1:post", .fml_txt(e$did_es)),
         msg = "post:x1 항이 없습니다. Task 1-1과 같은 통제를 유지해야 두 결과를 비교할 수 있습니다."),

    list(f = function(e) grepl("post:x2|x2:post", .fml_txt(e$did_es)),
         msg = "post:x2 항이 없습니다. post:x1 + post:x2를 함께 넣으세요."),

    # 군집
    list(f = function(e) {
           v <- paste(.callarg(e$did_es, "cluster"), .callarg(e$did_es, "vcov"))
           grepl("id", v) || is.na(v)
         },
         msg = "군집 표준오차가 지정되지 않았습니다. cluster = ~ id를 넣으세요."),

    # 계수가 실제로 여러 시점에 걸쳐 나왔는가
    list(f = function(e) sum(grepl("rel_year", .cfnames(e$did_es))) >= 5,
         msg = "상대시간 계수가 너무 적습니다. rel_year가 여러 값을 갖는지 확인하고, 데이터 생성 블록부터 다시 실행해 보세요."),

    list(f = function(e) !any(grepl("rel_year::-1", .cfnames(e$did_es))),
         msg = "기준 시점의 계수가 함께 추정되었습니다. ref로 지정한 시점은 계수 목록에서 빠져야 합니다.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 2-1. rdplot
# ─────────────────────────────────────────────────────────────────
"causal-03-rdplot" = list(
  need = c("rd_df"),
  call_from = NULL,
  note = "임계값 좌우에서 점들의 높이가 끊기는지, 그리고 각 구간의 관계가 직선에 가까운지 두 가지를 봅니다. 이 그림에서 점 하나는 개인이 아니라 배정변수 구간의 평균입니다. 원자료를 그대로 뿌리면 잡음에 묻혀 점프가 보이지 않기 때문에 구간 평균으로 요약한 것인데, 구간을 넓게 잡으면 없는 점프가 있어 보이거나 있는 점프가 사라질 수 있습니다. 그림은 추정 결과를 확인하는 도구이지 그 자체가 추정은 아닙니다.",
  rules = list(

    list(f = function(e) is.data.frame(e$rd_df),
         msg = "rd_df가 없습니다. 데이터 준비 블록을 먼저 실행하세요."),

    list(f = function(e) all(c("y", "run", "d") %in% names(e$rd_df)),
         msg = "rd_df에 y, run, d 열이 모두 있어야 합니다. 데이터 준비 블록을 다시 실행하세요."),

    list(f = function(e) requireNamespace("rdrobust", quietly = TRUE),
         msg = "rdrobust 패키지가 설치되지 않았습니다. 데이터 준비 블록의 패키지 설치 부분을 먼저 실행하세요."),

    list(f = function(e) {
           # 그림 자체는 객체로 남지 않으므로, 함수 호출이 성립하는지만 확인합니다
           ok <- tryCatch({
             invisible(utils::capture.output(
               rdrobust::rdplot(y = e$rd_df$y, x = e$rd_df$run, c = 0, hide = TRUE)
             ))
             TRUE
           }, error = function(err) FALSE)
           ok
         },
         msg = "rdplot()이 정상적으로 실행되지 않습니다. y에 rd_df$y, x에 rd_df$run, c에 0을 지정했는지 확인하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 2-2. RD 추정
# ─────────────────────────────────────────────────────────────────
"causal-04-rd" = list(
  need = c("rd_df", "rd_true", "rd_out"),
  call_from = NULL,
  note = "전체 표본은 2,000개이지만 추정에 쓰인 것은 임계값 주변 관측치뿐입니다. 출력의 BW est. 아래 숫자가 실제로 사용된 구간의 폭이고, Eff. Number of Obs.가 그 안에 들어온 관측치 수입니다. 여기서 추정한 값은 전체 모집단의 평균 효과가 아니라 임계값 근방에 있는 개체에 대한 효과입니다. 임계값에서 멀리 떨어진 사람에게 같은 크기의 효과가 나타나리라는 근거는 이 설계에 들어 있지 않습니다. Conventional은 점추정치를 읽을 때, Robust는 신뢰구간과 p값을 읽을 때 쓰는 것이 관례입니다.",
  rules = list(

    list(f = function(e) inherits(e$rd_out, "rdrobust"),
         msg = "rd_out이 rdrobust 객체가 아닙니다. rdrobust()의 결과를 rd_out에 저장하세요. rdplot()의 결과가 아닙니다."),

    list(f = function(e) !is.null(e$rd_out$coef) && nrow(e$rd_out$coef) >= 1,
         msg = "추정 결과가 비어 있습니다. rdrobust() 인수를 다시 확인하세요."),

    # 임계값
    list(f = function(e) {
           cc <- .get(e$rd_out$c)
           is.null(cc) || isTRUE(cc == 0)
         },
         msg = "임계값 c가 0이 아닙니다. 이 데이터는 run이 0 이상이면 처치를 받도록 만들었습니다."),

    # 차수와 미분
    list(f = function(e) {
           p <- .get(e$rd_out$p)
           is.null(p) || isTRUE(p == 1)
         },
         msg = "p가 1이 아닙니다. 국소선형회귀이므로 임계값 좌우에 직선을 적합합니다."),

    list(f = function(e) {
           dv <- .get(e$rd_out$deriv)
           is.null(dv) || isTRUE(dv == 0)
         },
         msg = "deriv가 0이 아닙니다. 수준의 점프를 재야 하므로 0입니다. 1은 기울기의 꺾임을 재는 경사형 RD입니다."),

    # 대역폭 선택
    list(f = function(e) {
           bw <- .get(e$rd_out$bwselect)
           is.null(bw) || grepl("mserd", tolower(paste(bw, collapse = "")))
         },
         msg = "bwselect가 mserd가 아닙니다. 평균제곱오차를 최소화하는 대역폭을 고르는 방법입니다."),

    # 대역폭이 실제로 잘려 있는가
    list(f = function(e) {
           bws <- .get(e$rd_out$bws)
           !is.null(bws) && is.finite(bws[1, 1]) && bws[1, 1] > 0
         },
         msg = "대역폭이 계산되지 않았습니다. 배정변수에 결측이나 무한값이 없는지 확인하세요."),

    list(f = function(e) sum(.get(e$rd_out$N_h) %||% 0) < nrow(e$rd_df),
         msg = "전체 표본이 모두 추정에 사용되었습니다. h를 직접 지정해 지나치게 넓게 잡지 않았는지 확인하세요."),

    # 결과의 타당성
    list(f = function(e) is.finite(e$rd_out$coef[1]),
         msg = "추정치가 계산되지 않았습니다. 임계값 근처에 관측치가 충분한지 확인하세요."),

    list(f = function(e) e$rd_out$coef[1] > 0,
         msg = "추정치의 부호가 음수입니다. y와 x를 바꿔 넣지 않았는지 확인하세요."),

    list(f = function(e) abs(e$rd_out$coef[1] - e$rd_true) < 1.0,
         msg = "추정치가 참값에서 크게 벗어났습니다. y에 rd_df$y, x에 rd_df$run을 넣었는지 확인하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 2-3. 조작 검정과 대역폭 민감도
# ─────────────────────────────────────────────────────────────────
"causal-05-rdbw" = list(
  need = c("rd_df", "rd_out", "dens", "h_opt"),
  call_from = NULL,
  note = "밀도 검정의 귀무가설은 배정변수의 밀도가 임계값에서 연속이라는 것입니다. p값이 크게 나오면 임계값 근처에서 값을 조정한 흔적이 없다는 뜻이고, 이 데이터는 조작을 넣지 않았으므로 그렇게 나와야 정상입니다. 검정이 보는 것은 분포의 모양이 아니라 임계값에서 끊기는지 여부입니다. 대역폭을 0.5배와 2배로 바꾼 결과도 함께 보세요. 세 값이 비슷한 자리에 머물면 결론이 대역폭 선택에 달려 있지 않다는 근거가 됩니다. 값이 크게 흔들린다면 자동 선택된 하나만 보고하는 것으로는 부족합니다. 논문에서는 자동 선택 결과를 주 결과로 두고 이 민감도 분석을 함께 싣습니다.",
  rules = list(

    list(f = function(e) inherits(e$dens, "rddensity"),
         msg = "dens가 rddensity 객체가 아닙니다. rddensity()의 결과를 dens에 저장하세요."),

    list(f = function(e) {
           cc <- .get(e$dens$c)
           is.null(cc) || isTRUE(cc == 0)
         },
         msg = "밀도 검정의 임계값이 0이 아닙니다. c = 0을 지정하세요."),

    list(f = function(e) {
           # 배정변수를 넣었는지 (결과변수를 넣는 실수 방지)
           n_used <- .get(e$dens$N$full)
           is.null(n_used) || isTRUE(n_used == nrow(e$rd_df))
         },
         msg = "밀도 검정에 넣은 변수를 확인하세요. 결과변수 y가 아니라 배정변수 rd_df$run입니다."),

    list(f = function(e) .scalar(e$h_opt) && e$h_opt > 0,
         msg = "h_opt가 양수가 아닙니다. rd_out$bws[1, 1]로 자동 선택된 대역폭을 꺼내세요."),

    list(f = function(e) .eq(e$h_opt, e$rd_out$bws[1, 1]),
         msg = "h_opt가 rd_out의 대역폭과 다릅니다. bws는 행렬이므로 [1, 1]로 첫 값을 꺼냅니다."),

    list(f = function(e) {
           # 세 배율로 실제 추정이 가능한지 확인
           ok <- tryCatch({
             vals <- vapply(c(0.5, 1, 2), function(m) {
               s <- rdrobust::rdrobust(y = e$rd_df$y, x = e$rd_df$run, c = 0,
                                       h = e$h_opt * m)
               s$coef[1]
             }, numeric(1))
             all(is.finite(vals))
           }, error = function(err) FALSE)
           ok
         },
         msg = "대역폭을 바꿔 추정하는 부분에서 오류가 납니다. h = h_opt * m으로 루프 변수 m을 곱했는지 확인하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 3-1. Wald 추정량
# ─────────────────────────────────────────────────────────────────
"causal-06-wald" = list(
  need = c("ohie_df", "fs", "rf", "first_stage", "reduced_form", "wald_est"),
  call_from = NULL,
  note = "세 숫자의 관계를 짚어 두시면 좋습니다. 1단계는 추첨이 실제 가입을 얼마나 바꿨는지이고, 축약형은 추첨이 결과를 얼마나 바꿨는지입니다. 추첨은 무작위로 배정되었으므로 축약형은 그 자체로 해석 가능한 값이며, 정책이 신청 기회를 준 것만으로 얻은 효과에 해당합니다. Wald 추정량은 이 축약형을 1단계로 나눈 값입니다. 당첨자 가운데 실제로 가입한 사람은 일부이므로, 추첨 전체에 퍼진 효과를 실제로 가입 상태가 바뀐 사람들 몫으로 되돌리는 계산입니다. 분모가 작아질수록 이 나눗셈이 불안정해지는데, 약한 도구가 문제가 되는 이유가 여기에 있습니다.",
  rules = list(

    # 1단계
    list(f = function(e) inherits(e$fs, "fixest") || inherits(e$fs, "lm"),
         msg = "fs가 회귀 객체가 아닙니다. feols()의 결과를 fs에 저장하세요."),

    list(f = function(e) grepl("^\\s*insured\\s*~", .fml_txt(e$fs)),
         msg = "1단계의 결과변수가 insured가 아닙니다. 추첨이 가입을 얼마나 바꿨는지 보는 회귀입니다."),

    list(f = function(e) "lottery" %in% .cfnames(e$fs),
         msg = "1단계에 lottery가 없습니다. insured ~ lottery로 씁니다."),

    # 축약형
    list(f = function(e) inherits(e$rf, "fixest") || inherits(e$rf, "lm"),
         msg = "rf가 회귀 객체가 아닙니다. feols()의 결과를 rf에 저장하세요."),

    list(f = function(e) grepl("^\\s*rx_num\\s*~", .fml_txt(e$rf)),
         msg = "축약형의 결과변수가 rx_num이 아닙니다. 추첨이 결과를 얼마나 바꿨는지 보는 회귀입니다."),

    list(f = function(e) "lottery" %in% .cfnames(e$rf),
         msg = "축약형에 lottery가 없습니다. rx_num ~ lottery로 씁니다."),

    list(f = function(e) !grepl("insured", .fml_txt(e$rf)),
         msg = "축약형에 insured가 들어가 있습니다. 축약형은 처치를 거치지 않고 도구가 결과에 미친 효과만 봅니다."),

    # 두 회귀가 같은 데이터인가
    list(f = function(e) {
           a <- .get(nobs(e$fs)); b <- .get(nobs(e$rf))
           is.null(a) || is.null(b) || isTRUE(a == b)
         },
         msg = "두 회귀의 관측치 수가 다릅니다. 같은 ohie_df로 추정해야 분자와 분모를 나눌 수 있습니다."),

    # 계수 추출
    list(f = function(e) .eq(e$first_stage, .cf(e$fs, "lottery")),
         msg = "first_stage가 fs의 lottery 계수와 다릅니다. coef(fs)[\"lottery\"]로 꺼내세요."),

    list(f = function(e) .eq(e$reduced_form, .cf(e$rf, "lottery")),
         msg = "reduced_form이 rf의 lottery 계수와 다릅니다. 두 회귀를 바꿔 넣지 않았는지 확인하세요."),

    # 나눗셈 방향
    list(f = function(e) .scalar(e$wald_est),
         msg = "wald_est가 숫자 하나가 아닙니다."),

    list(f = function(e) !.eq(e$wald_est, e$first_stage / e$reduced_form),
         msg = "분자와 분모가 뒤바뀌었습니다. 축약형을 1단계로 나눕니다."),

    list(f = function(e) .eq(e$wald_est, e$reduced_form / e$first_stage),
         msg = "wald_est 계산식을 확인하세요. reduced_form / first_stage 입니다."),

    # 1단계 강도
    list(f = function(e) abs(e$first_stage) > 0.05,
         msg = "1단계 효과가 지나치게 작습니다. lottery와 insured의 코딩을 확인하세요. 당첨군과 낙첨군의 가입률 차이가 0.2 안팎으로 나와야 정상입니다.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 3-2. 2SLS
# ─────────────────────────────────────────────────────────────────
"causal-07-2sls" = list(
  need = c("ohie_df", "wald_est", "iv1"),
  call_from = NULL,
  note = "2SLS 추정치가 앞에서 손으로 계산한 Wald 값과 소수점까지 같습니다. 도구가 하나이고 통제변수가 없는 경우 두 방법은 같은 계산이기 때문입니다. 함께 볼 것은 표준오차입니다. OLS로 같은 관계를 추정할 때보다 훨씬 크게 나오는데, 추첨이 만들어 낸 변이만 사용하므로 쓸 수 있는 정보가 줄어들기 때문입니다. 이것이 도구변수를 쓰는 대가입니다. 1단계 F 통계량은 도구가 처치를 얼마나 강하게 예측하는지를 나타내며, 관례적으로 10을 기준선으로 삼습니다. 이 값이 작으면 Wald 나눗셈의 분모가 작아져 추정치가 불안정해집니다.",
  rules = list(

    list(f = function(e) inherits(e$iv1, "fixest"),
         msg = "iv1이 fixest 객체가 아닙니다. feols()의 결과를 iv1에 저장하세요."),

    # IV 구조인가
    list(f = function(e) isTRUE(.get(e$iv1$iv)),
         msg = "IV 모형으로 추정되지 않았습니다. 공식에 수직선을 넣어 rx_num ~ 1 | insured ~ lottery 형태로 쓰세요."),

    list(f = function(e) "fit_insured" %in% .cfnames(e$iv1),
         msg = "내생변수가 insured가 아닙니다. 수직선 뒤의 왼쪽에 insured를 씁니다. fixest는 이 계수를 fit_insured로 표시합니다."),

    list(f = function(e) grepl("lottery", .fml_txt(e$iv1)),
         msg = "도구변수가 lottery가 아닙니다. 수직선 뒤의 오른쪽에 lottery를 씁니다."),

    list(f = function(e) grepl("^\\s*rx_num\\s*~", .fml_txt(e$iv1)),
         msg = "결과변수가 rx_num이 아닙니다."),

    # 내생변수와 도구가 뒤바뀌지 않았는가
    list(f = function(e) !("fit_lottery" %in% .cfnames(e$iv1)),
         msg = "내생변수와 도구변수가 뒤바뀌었습니다. insured ~ lottery 순서입니다. 추첨이 가입을 예측하는 것이지 그 반대가 아닙니다."),

    # 데이터
    list(f = function(e) {
           d <- .callarg(e$iv1, "data")
           is.na(d) || identical(d, "ohie_df")
         },
         msg = "data 인수가 ohie_df가 아닙니다."),

    # Wald와 일치
    list(f = function(e) is.finite(.cf(e$iv1, "fit_insured")),
         msg = "2SLS 추정치가 계산되지 않았습니다."),

    list(f = function(e) .eq(.cf(e$iv1, "fit_insured"), e$wald_est, tol = 1e-4),
         msg = "2SLS 추정치가 Wald 추정량과 다릅니다. 도구가 하나이고 통제변수가 없으면 두 값이 같아야 합니다. 통제변수를 넣지 않았는지 확인하세요."),

    # 1단계 강도
    list(f = function(e) {
           f <- .get(fixest::fitstat(e$iv1, "ivwald1", verbose = FALSE)[[1]]$stat)
           is.null(f) || f > 10
         },
         msg = "1단계 F 통계량이 10 미만입니다. 도구변수와 내생변수의 지정을 다시 확인하세요.")
  )),

# ─────────────────────────────────────────────────────────────────
# Task 3-3. 통제변수를 포함한 모형 (선택)
# ─────────────────────────────────────────────────────────────────
"causal-08-controls" = list(
  need = c("ohie_df", "iv1", "iv2"),
  call_from = NULL,
  note = "두 추정치가 크게 다르지 않고 신뢰구간도 서로 겹칩니다. 추첨이 무작위이므로 공변량은 도구변수와 상관이 없고, 따라서 통제하든 하지 않든 추정 대상이 바뀌지 않습니다. 관찰연구에서 공변량을 넣고 뺄 때 계수가 크게 흔들리는 것과 대비되는 지점입니다. 통제변수가 하는 일은 편의를 줄이는 것이 아니라 결과의 잔여 변동을 설명해 표준오차를 줄이는 것뿐입니다. 표준오차가 실제로 줄었는지 두 값을 비교해 보세요. 줄어드는 폭이 크지 않다면 이 공변량들이 결과를 설명하는 몫이 작았다는 뜻입니다.",
  rules = list(

    list(f = function(e) inherits(e$iv2, "fixest"),
         msg = "iv2가 fixest 객체가 아닙니다."),

    list(f = function(e) isTRUE(.get(e$iv2$iv)),
         msg = "iv2가 IV 모형이 아닙니다. 수직선 뒤의 insured ~ lottery는 그대로 유지해야 합니다."),

    list(f = function(e) "fit_insured" %in% .cfnames(e$iv2),
         msg = "iv2의 내생변수가 insured가 아닙니다."),

    list(f = function(e) all(c("female", "age") %in% .cfnames(e$iv2)),
         msg = "female 또는 age가 모형에 없습니다. 수직선 앞에 female + age + edu를 넣으세요."),

    list(f = function(e) "edu" %in% .cfnames(e$iv2),
         msg = "edu가 모형에 없습니다. 통제변수는 female, age, edu 세 개입니다."),

    # 도구를 통제변수로 넣는 실수
    list(f = function(e) !("lottery" %in% .cfnames(e$iv2)),
         msg = "lottery가 통제변수 자리에 들어가 있습니다. 도구변수는 수직선 뒤에만 씁니다. 앞에 넣으면 도구로 쓸 변이가 사라집니다."),

    # 처치를 통제변수로 넣는 실수
    list(f = function(e) !("insured" %in% .cfnames(e$iv2)),
         msg = "insured가 통제변수 자리에 들어가 있습니다. 내생변수는 수직선 뒤 왼쪽에만 씁니다."),

    # 결과 비교가 가능한가
    list(f = function(e) {
           a <- .get(nobs(e$iv1)); b <- .get(nobs(e$iv2))
           is.null(a) || is.null(b) || isTRUE(a == b)
         },
         msg = "두 모형의 관측치 수가 다릅니다. 표본이 달라지면 추정치 차이가 통제변수 때문인지 알 수 없습니다."),

    list(f = function(e) is.finite(.cf(e$iv2, "fit_insured")),
         msg = "iv2의 추정치가 계산되지 않았습니다."),

    list(f = function(e) .has(e, "iv_plot"),
         msg = "비교 그림을 위한 iv_plot 데이터프레임이 없습니다. 시각화 부분까지 실행하세요.")
  ))
)


# ══════════════════════════════════════════════════════════════════
#  채점 함수
# ══════════════════════════════════════════════════════════════════

check <- function(exercise_id) {

  spec <- CHECKS[[exercise_id]]
  if (is.null(spec)) {
    message("등록되지 않은 문항입니다: ", exercise_id); return(invisible())
  }

  env  <- parent.frame()
  t0   <- Sys.time()
  e    <- .snapshot(env)
  miss <- spec$need[!vapply(spec$need, function(nm) .has(e, nm), logical(1))]

  if (length(miss)) {
    last <- tryCatch(geterrmessage(), error = function(err) "")
    out  <- paste0("객체를 찾을 수 없습니다: ", paste(miss, collapse = ", "),
                   "\n   앞의 블록을 순서대로 실행했는지, 객체 이름의 철자가 맞는지 확인하세요.",
                   if (nzchar(last)) paste0("\n   [직전 오류] ", trimws(last)) else "")
    .send(exercise_id, "error", FALSE, out, NA_character_, NA_character_, t0)
    message("\u274C 실행되지 않았습니다.\n   ", out)
    return(invisible(FALSE))
  }

  hint <- NA_character_; ok <- TRUE
  for (r in spec$rules) {
    res <- tryCatch(isTRUE(r$f(e)), error = function(err) FALSE)
    if (!res) { ok <- FALSE; hint <- r$msg; break }
  }

  code <- NA_character_
  if (!is.null(spec$call_from) && .has(e, spec$call_from)) {
    code <- tryCatch(
      paste(deparse(e[[spec$call_from]]$call), collapse = " "),
      error = function(err) NA_character_)
  }

  .send(exercise_id, if (ok) "ok" else "wrong", ok,
        if (ok) "OK" else hint, hint, code, t0)

  if (ok) {
    message("\u2705 정답입니다. 잘 채우셨습니다. 다음 문항으로 넘어가세요.")
    if (!is.null(spec$caution)) {
      ct <- tryCatch(spec$caution(e), error = function(err) NULL)
      if (!is.null(ct)) message("   \U1F4DD ", ct)
    }
    if (!is.null(spec$note)) message("\n", .wrap(spec$note))
  } else {
    message("\u274C 다시 확인해 보세요.\n   \U1F4A1 ", hint)
  }

  invisible(ok)
}

# ── 전송 ──────────────────────────────────────────────────────────
.send <- function(exercise_id, status, correct, output, hint, code, t0) {
  payload <- list(
    student_id  = .tracker_env$student_id %||% "unknown",
    chapter     = CHAPTER,
    exercise_id = exercise_id,
    code        = code,
    output      = substr(output, 1, 4000),
    status      = status,
    is_correct  = correct,
    hint        = hint,
    elapsed_sec = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    session_id  = .tracker_env$session_id,
    r_version   = paste(R.version$major, R.version$minor, sep = ".")
  )
  try(
    httr2::request(paste0(SUPABASE_URL, "/rest/v1/submissions")) |>
      httr2::req_headers(apikey = SUPABASE_KEY,
                         `Content-Type` = "application/json",
                         Prefer = "return=minimal") |>
      httr2::req_body_json(payload) |>
      httr2::req_timeout(5) |>
      httr2::req_perform(),
    silent = TRUE)
}
