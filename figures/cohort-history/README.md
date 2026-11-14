# Generations Across China's Historical Time

R renders transparent PNGs for the homepage (no format conversion chain).

```bash
Rscript figures/cohort-history/build.R
```

Outputs:

- `static/figures/cohort-history/generations-historical-time-light.png`
- `static/figures/cohort-history/generations-historical-time-dark.png`

Requires: `ragg`, system font `PT Sans Narrow`.
