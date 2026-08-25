# KAPAE 2026 — ML & Causal ML Research Design Lab

This folder implements a two-step Quarto workflow for teaching students how a machine-learning or causal-inference paper should be structured before they analyse their real data.

## Student workflow

1. Open `01_student_setup.qmd`.
2. Enter only a non-identifying class ID and one design type.
3. Run the generation cell. It sources the current helper from GitHub and creates a method-specific `02_<ID>_<METHOD>_research_design.qmd` in the student's local folder.
4. In Step 2, fill in the research title, data description, outcome type/range, predictor composition, sample size, and the small number of method-specific design parameters.
5. Render and inspect the automatically generated paper-style flow.
6. Run `render_project_outputs()` for HTML, Word, PDF, and RevealJS slides.
7. Run `submit_project()` when the submission endpoint is activated.

### Supported designs

- `prediction`: random forest, XGBoost, and neural-network comparison with common resampling; algorithm-level and all-hyperparameter boxplots.
- `dml`: DML with cross-fitting, explicit OLS robustness comparison, and deliberately visible nonlinear-confounding bias in the educational simulation.
- `did`: raw treated/control trends and event-study pre-period inspection before the treatment-effect estimate.
- `iv`: first-stage relevance before 2SLS; OLS vs IV comparison.
- `matching`: propensity-score overlap before effects; Love plot and balance diagnostics in the supplement.
- `causal_forest`: CATE distribution, subgroup heterogeneity, and ATE; calibration/stability diagnostics in the supplement.
- `rd`: running-variable inspection before estimation, outcome around the cutoff, bandwidth sensitivity, and main local effect.

## Output policy

The main paper is deliberately constrained to **no more than four main tables/figures combined**; the templates use figures almost exclusively for results. Coefficient/effect plots display the marker and `estimate [95% CI]` directly. Design diagnostics that are essential to identification are placed before the main estimate rather than hidden in the supplement.

HTML and RevealJS use Plotly-converted interactive graphics. Word and PDF use the same underlying ggplot objects as static figures so rendering does not depend on HTML widgets. PDF requires a working LaTeX installation such as TinyTeX.

## Supplement automatically scaffolded

Every design includes:

- missing records by variable (`n`, `%`);
- transparent data-cleaning/sample-flow table;
- design-specific diagnostics;
- required robustness/sensitivity checklist;
- reproducibility information (`sessionInfo()`).

Method-specific supplement requirements include tuning grids/test-set evaluation for prediction; nuisance-model and fold sensitivity for DML; pre-trends/placebos for DID; weak-IV diagnostics for IV; overlap/Love plots/effective sample size for propensity scores; calibration and heterogeneity stability for causal forest; and density/bandwidth/order/placebo-cutoff checks for RD.

## Why students cannot write anonymously to GitHub directly

A public GitHub repository is publicly readable but not anonymously writable. The GitHub Contents API requires authenticated write permission. Therefore, do **not** put a personal access token in a student QMD or R file.

The recommended architecture is:

`student QMD -> submission API -> GitHub Contents API -> projects/submissions/`

The API keeps the GitHub token server-side. `submission_api/worker.js` is a minimal Cloudflare Worker example. It accepts only the seven permitted design names, sanitises the student ID, enforces a file-size limit, and writes only to `projects/submissions/<ID>_<METHOD>.qmd`. Re-submission updates the same file.

### Instructor setup for the submission API

1. Create/deploy a Cloudflare Worker and use `submission_api/worker.js` as the module entry point.
2. Create a **fine-grained GitHub token** restricted to `TheYongjinChoi/kapae2026-exercise` with repository **Contents: Read and write** permission.
3. Store the token as the Worker secret `GITHUB_TOKEN`. Never put it in `wrangler.jsonc`, a QMD, or GitHub source.
4. Optionally store a shared classroom code as `COURSE_KEY` (this prevents accidental submissions but should not be treated as a strong secret if distributed to all students).
5. Deploy the Worker and copy its HTTPS endpoint.
6. Update `projects/design_lab/config.json`:

```json
{
  "submission_endpoint": "https://YOUR-WORKER.workers.dev/",
  "course_key": "YOUR_CLASS_CODE"
}
```

Once this is on `main`, students can leave `submission_endpoint` and `course_key` blank in their QMD; `submit_project()` reads the central config automatically.

### Dropbox fallback

A normal shared Dropbox folder URL is not an anonymous upload API. To use Dropbox without student authentication, create a **Dropbox File Request** that targets the desired folder. The present templates intentionally do not automate upload to an ordinary shared-folder URL because that would require exposing a Dropbox access token. A File Request can be used as a manual fallback if the GitHub submission API is not deployed.

## Instructor gallery

`instructor_dashboard.qmd` is the instructor-facing document.

When rendered it can:

1. check that the local repository is clean and run `git pull --ff-only origin main`;
2. scan `projects/submissions/*.qmd`;
3. re-render only QMD files newer than their existing HTML output;
4. place rendered student pages in `projects/rendered/`;
5. create one list/card-style HTML gallery linking to every student project.

If there are local uncommitted changes, the automatic pull is skipped rather than risking a merge conflict.

## Files

- `01_student_setup.qmd` — blank Step 1 used by students.
- `01_student_setup_example.qmd` — Step 1 example.
- `02_student_research_design_blank.qmd` — generic Step 2 fallback; normally Step 2 is generated method-specifically.
- `02_student_research_design_example.qmd` — completed DML example.
- `R/lab_helpers.R` — generator, fake-data engine, method text, figures, rendering, submission function.
- `R/collect_projects.R` — instructor pull/scan/render/gallery helpers.
- `instructor_dashboard.qmd` — instructor gallery report.
- `submission_api/worker.js` — server-side GitHub writer.
- `submission_api/wrangler.jsonc` — Worker config skeleton.
- `config.json` — central submission endpoint configuration.
- `../submissions/` — student QMD destination.

## Recommended additions for teaching

The templates already include several safeguards worth retaining in the final course version:

- **estimand-first prompts** for causal designs, so students distinguish ATE/ATT/LATE/local RD effects from generic “causal effects”;
- **assumption gatekeeping**, where DID/RD/PS diagnostics appear before effect estimates;
- **data leakage prompts** for prediction, separating tuning/resampling from final evaluation;
- **main-output budget** to force paper-level prioritisation rather than dumping model output;
- **submission metadata** through stable pseudonymous IDs and design labels;
- **resubmission overwrite by ID/method**, avoiding dozens of duplicate files;
- **incremental instructor rendering**, so only changed projects are rebuilt before class.

A useful later extension would be an instructor rubric panel in the gallery (research question, identification, diagnostics, visual communication, reproducibility), stored separately from student submissions so feedback does not modify student files.
