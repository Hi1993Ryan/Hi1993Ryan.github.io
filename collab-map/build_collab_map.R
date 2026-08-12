#!/usr/bin/env Rscript
# Build an interactive institution-level collaboration map for the Hugo site.
# Usage (from repo root or this folder):
#   Rscript collab-map/build_collab_map.R

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(leaflet)
  library(htmlwidgets)
  library(htmltools)
  library(geosphere)
})

root <- if (file.exists("collab-map/data/institutions.csv")) {
  normalizePath(".")
} else if (file.exists("data/institutions.csv")) {
  normalizePath("..")
} else {
  stop("Run from the site repo root or collab-map/.")
}

data_dir <- file.path(root, "collab-map", "data")
out_dir <- file.path(root, "static", "collab-map")
out_file <- file.path(out_dir, "index.html")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

institutions <- read_csv(file.path(data_dir, "institutions.csv"), show_col_types = FALSE)
papers <- read_csv(file.path(data_dir, "papers.csv"), show_col_types = FALSE)

stopifnot(any(institutions$is_home))
home <- institutions %>% filter(is_home) %>% slice(1)

paper_links <- papers %>%
  mutate(ids = strsplit(institution_ids, ";", fixed = TRUE)) %>%
  tidyr::unnest(ids) %>%
  mutate(ids = trimws(ids)) %>%
  filter(ids != home$id)

counts <- paper_links %>%
  count(ids, name = "n_papers") %>%
  rename(id = ids)

partners <- institutions %>%
  filter(!is_home) %>%
  left_join(counts, by = "id") %>%
  mutate(n_papers = tidyr::replace_na(n_papers, 0L)) %>%
  filter(n_papers > 0) %>%
  arrange(desc(n_papers), name)

paper_lookup <- papers %>%
  mutate(ids = strsplit(institution_ids, ";", fixed = TRUE)) %>%
  tidyr::unnest(ids) %>%
  mutate(ids = trimws(ids)) %>%
  filter(ids != home$id) %>%
  group_by(ids) %>%
  summarise(
    paper_list = paste0(
      "<li>", year, " — ",
      ifelse(
        !is.na(doi) & doi != "",
        paste0("<a href='https://doi.org/", doi, "' target='_blank' rel='noopener noreferrer'>", htmltools::htmlEscape(title), "</a>"),
        htmltools::htmlEscape(title)
      ),
      "</li>",
      collapse = ""
    ),
    .groups = "drop"
  ) %>%
  rename(id = ids)

partners <- partners %>% left_join(paper_lookup, by = "id")

gc_line <- function(lon1, lat1, lon2, lat2, n = 80) {
  pts <- geosphere::gcIntermediate(
    c(lon1, lat1), c(lon2, lat2),
    n = n, addStartEnd = TRUE, sp = FALSE
  )
  if (is.null(dim(pts))) {
    matrix(pts, ncol = 2, byrow = FALSE)
  } else {
    pts
  }
}

# Soft academic palette (avoid purple/glow defaults)
col_home <- "#0F4C5C"
col_partner <- "#5C7A6E"
col_arc <- "#7A8B7A"
col_arc_strong <- "#0F4C5C"

m <- leaflet(options = leafletOptions(minZoom = 2, worldCopyJump = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = home$lon, lat = home$lat, zoom = 2)

for (i in seq_len(nrow(partners))) {
  p <- partners[i, ]
  arc <- gc_line(home$lon, home$lat, p$lon, p$lat)
  weight <- 1.2 + 1.4 * log1p(p$n_papers)
  m <- addPolylines(
    m,
    lng = arc[, 1],
    lat = arc[, 2],
    color = if (p$n_papers >= 2) col_arc_strong else col_arc,
    weight = weight,
    opacity = 0.55,
    group = "Links"
  )
}

partner_radius <- 6 + 4 * sqrt(partners$n_papers)
partner_popups <- paste0(
  "<div style='min-width:220px;max-width:320px;font-family:system-ui,sans-serif;'>",
  "<strong>", htmltools::htmlEscape(partners$name), "</strong>",
  ifelse(!is.na(partners$name_zh) & partners$name_zh != "",
         paste0("<br><span style='color:#555;'>", htmltools::htmlEscape(partners$name_zh), "</span>"),
         ""),
  "<br>", htmltools::htmlEscape(partners$city), ", ", htmltools::htmlEscape(partners$country),
  "<br><em>", partners$n_papers, " co-authored paper", ifelse(partners$n_papers == 1, "", "s"), "</em>",
  ifelse(!is.na(partners$url) & partners$url != "",
         paste0("<br><a href='", partners$url, "' target='_blank' rel='noopener noreferrer'>Institution site</a>"),
         ""),
  "<ul style='margin:0.4em 0 0;padding-left:1.1em;font-size:0.9em;'>",
  partners$paper_list,
  "</ul></div>"
)

m <- addCircleMarkers(
  m,
  lng = partners$lon,
  lat = partners$lat,
  radius = partner_radius,
  color = "#fff",
  weight = 1.5,
  fillColor = col_partner,
  fillOpacity = 0.9,
  popup = partner_popups,
  label = partners$name,
  group = "Institutions"
)

home_popup <- paste0(
  "<div style='min-width:200px;font-family:system-ui,sans-serif;'>",
  "<strong>", htmltools::htmlEscape(home$name), "</strong>",
  "<br><span style='color:#555;'>", htmltools::htmlEscape(home$name_zh), "</span>",
  "<br>Home institution",
  "<br><a href='", home$url, "' target='_blank' rel='noopener noreferrer'>Institution site</a>",
  "</div>"
)

m <- addCircleMarkers(
  m,
  lng = home$lon,
  lat = home$lat,
  radius = 11,
  color = "#fff",
  weight = 2,
  fillColor = col_home,
  fillOpacity = 1,
  popup = home_popup,
  label = home$name,
  group = "Home"
) %>%
  addLayersControl(
    overlayGroups = c("Home", "Institutions", "Links"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  addControl(
    html = paste0(
      "<div style='background:rgba(255,255,255,0.92);padding:8px 10px;border-radius:4px;",
      "font:12px/1.35 system-ui,sans-serif;color:#222;max-width:220px;'>",
      "<strong>Collaboration network</strong><br>",
      "Nodes = institutions<br>",
      "Arcs from ", htmltools::htmlEscape(home$name), "<br>",
      "Click a node for papers",
      "</div>"
    ),
    position = "bottomleft"
  )

saveWidget(m, out_file, selfcontained = TRUE, title = "Collaboration network")
message("Wrote ", out_file)
message("Partners: ", nrow(partners), " | Papers: ", nrow(papers))
