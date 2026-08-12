# Collaboration map

Institution-level co-authorship map for the Hugo site.

## Update

1. Edit `data/institutions.csv` (add a row if a new institution appears).
2. Edit `data/papers.csv` (`institution_ids` is a `;`-separated list of institution `id`s, always include `cqu`).
3. From the site repo root:

```bash
Rscript collab-map/build_collab_map.R
```

4. Commit the updated `static/collab-map/index.html` (and CSV changes), then push.

Requires R packages: `readr`, `dplyr`, `tidyr`, `leaflet`, `htmlwidgets`, `htmltools`, `geosphere`.
