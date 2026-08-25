# Install into `kapae2026-exercise`

From the repository root:

```bash
# After unzipping this package somewhere, copy the two project paths into the repo.
cp -R /path/to/kapae_design_lab/projects/design_lab projects/
mkdir -p projects/submissions
cp /path/to/kapae_design_lab/projects/submissions/.gitkeep projects/submissions/.gitkeep

# Review exactly what will be added.
git status --short projects/design_lab projects/submissions

# Stage only this lab.
git add -- projects/design_lab projects/submissions/.gitkeep

git commit -m "Add ML and causal ML research design lab"
git push origin main
```

The recommended GitHub workflow is to do this on a feature branch and merge by pull request if you want review before publishing.
