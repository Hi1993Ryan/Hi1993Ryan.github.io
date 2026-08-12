#!/usr/bin/env Rscript
# Build an airline-style institution collaboration map for the Hugo site.
# Usage (from site repo root):
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

# Show every partner institution in the CSV (arcs always drawn; papers optional)
partners <- institutions %>%
  filter(!is_home) %>%
  left_join(counts, by = "id") %>%
  mutate(
    n_papers = tidyr::replace_na(n_papers, 0L),
    name_zh = tidyr::replace_na(name_zh, ""),
    display_label = name
  ) %>%
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
        paste0(
          "<a href='https://doi.org/", doi,
          "' target='_blank' rel='noopener noreferrer'>",
          htmltools::htmlEscape(title), "</a>"
        ),
        htmltools::htmlEscape(title)
      ),
      "</li>",
      collapse = ""
    ),
    .groups = "drop"
  ) %>%
  rename(id = ids)

partners <- partners %>% left_join(paper_lookup, by = "id")

# Keep Western Hemisphere destinations on the Pacific side (lon + 360)
# so Asia→US routes cross the Pacific as continuous arcs, not via the Arctic
# (which breaks / looks discontinuous on a flat web map).
pacific_shift <- function(lon1, lon2) {
  if (isTRUE(lon1 > 60) && isTRUE(lon2 < -20)) lon2 + 360 else lon2
}

unwrap_lon <- function(lon) {
  if (length(lon) < 2) return(lon)
  for (i in 2:length(lon)) {
    while (lon[i] - lon[i - 1] > 180) lon[i] <- lon[i] - 360
    while (lon[i] - lon[i - 1] < -180) lon[i] <- lon[i] + 360
  }
  lon
}

# Great-circle “flight path” polyline (Pacific-preferring for Asia→Americas)
gc_line <- function(lon1, lat1, lon2, lat2, n = 140) {
  lon2 <- pacific_shift(lon1, lon2)
  pts <- geosphere::gcIntermediate(
    c(lon1, lat1), c(lon2, lat2),
    n = n, addStartEnd = TRUE, sp = FALSE
  )
  if (is.null(dim(pts))) {
    pts <- matrix(pts, ncol = 2, byrow = FALSE)
  } else {
    pts <- as.matrix(pts)
  }
  pts[, 1] <- unwrap_lon(pts[, 1])
  pts
}

partners <- partners %>%
  mutate(map_lon = vapply(lon, function(x) pacific_shift(home$lon, x), numeric(1)))

# Airline-map palette: dark basemap + sky routes
col_home <- "#F4D35E"
col_partner <- "#7EB6D9"
col_route_glow <- "#4A90A4"
col_route <- "#8FD3E8"

route_css <- htmltools::tags$style(htmltools::HTML("
  .leaflet-container { background: #0b1220; }
  .airline-legend {
    background: rgba(12, 18, 32, 0.88);
    color: #e8eef5;
    padding: 8px 10px;
    border-radius: 4px;
    font: 12px/1.4 system-ui, sans-serif;
    max-width: 230px;
    border: 1px solid rgba(143, 211, 232, 0.35);
  }
  .airline-legend strong { color: #8FD3E8; }
"))

m <- leaflet(
  options = leafletOptions(
    minZoom = 2,
    worldCopyJump = TRUE,
    zoomControl = TRUE
  )
) %>%
  addProviderTiles(
    providers$CartoDB.DarkMatter,
    options = providerTileOptions(opacity = 0.95)
  ) %>%
  # Pacific-centered: Asia left, Americas right → continuous Pacific routes
  setView(lng = 155, lat = 22, zoom = 2) %>%
  htmlwidgets::prependContent(route_css)

for (i in seq_len(nrow(partners))) {
  p <- partners[i, ]
  arc <- gc_line(home$lon, home$lat, p$lon, p$lat)
  # Visual weight: papers boost thickness; zero-paper links stay thin
  w <- 1.4 + 1.6 * log1p(max(p$n_papers, 1))
  # Soft under-glow
  m <- addPolylines(
    m,
    lng = arc[, 1],
    lat = arc[, 2],
    color = col_route_glow,
    weight = w + 2.5,
    opacity = 0.22,
    group = "Routes"
  )
  # Main dashed flight path
  m <- addPolylines(
    m,
    lng = arc[, 1],
    lat = arc[, 2],
    color = col_route,
    weight = w,
    opacity = 0.85,
    dashArray = "10 8",
    group = "Routes"
  )
}

partner_radius <- 5 + 3.2 * sqrt(pmax(partners$n_papers, 1))
zh_line <- ifelse(
  partners$name_zh != "",
  paste0("<br><span style='color:#9ab;'>", htmltools::htmlEscape(partners$name_zh), "</span>"),
  ""
)
paper_block <- ifelse(
  partners$n_papers > 0,
  paste0(
    "<br><em>", partners$n_papers, " co-authored paper",
    ifelse(partners$n_papers == 1, "", "s"), "</em>",
    "<ul style='margin:0.4em 0 0;padding-left:1.1em;font-size:0.9em;'>",
    tidyr::replace_na(partners$paper_list, ""),
    "</ul>"
  ),
  "<br><em>Collaborating institution</em>"
)

partner_popups <- paste0(
  "<div style='min-width:220px;max-width:320px;font-family:system-ui,sans-serif;'>",
  "<strong>", htmltools::htmlEscape(partners$name), "</strong>",
  zh_line,
  "<br>", htmltools::htmlEscape(partners$city), ", ",
  htmltools::htmlEscape(partners$country),
  paper_block,
  ifelse(
    !is.na(partners$url) & partners$url != "",
    paste0(
      "<br><a href='", partners$url,
      "' target='_blank' rel='noopener noreferrer'>Institution site</a>"
    ),
    ""
  ),
  "</div>"
)

m <- addCircleMarkers(
  m,
  lng = partners$map_lon,
  lat = partners$lat,
  radius = partner_radius,
  color = "#0b1220",
  weight = 2,
  fillColor = col_partner,
  fillOpacity = 0.95,
  popup = partner_popups,
  label = partners$display_label,
  group = "Institutions"
)

home_popup <- paste0(
  "<div style='min-width:200px;font-family:system-ui,sans-serif;'>",
  "<strong>", htmltools::htmlEscape(home$name), "</strong>",
  "<br><span style='color:#9ab;'>", htmltools::htmlEscape(home$name_zh), "</span>",
  "<br>Home institution / hub",
  "<br><a href='", home$url, "' target='_blank' rel='noopener noreferrer'>Institution site</a>",
  "</div>"
)

m <- addCircleMarkers(
  m,
  lng = home$lon,
  lat = home$lat,
  radius = 10,
  color = "#0b1220",
  weight = 2,
  fillColor = col_home,
  fillOpacity = 1,
  popup = home_popup,
  label = home$name,
  group = "Home"
) %>%
  addLayersControl(
    overlayGroups = c("Home", "Institutions", "Routes"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  addControl(
    html = paste0(
      "<div class='airline-legend'>",
      "<strong>Airline-style collaboration routes</strong><br>",
      "Hub: ", htmltools::htmlEscape(home$name), "<br>",
      "Dashed arcs = great-circle routes<br>",
      "Click a node for details",
      "</div>"
    ),
    position = "bottomleft"
  )

# Prefer a single file when pandoc is available; otherwise keep dependency folder
saveWidget(m, out_file, selfcontained = TRUE, title = "Collaboration network")
message("Wrote ", out_file)
message("Partners: ", nrow(partners), " | Papers: ", nrow(papers))
