#Regional map: Bosphorus, Bab el-Mandeb, Strait of Hormuz, Suez Canal and the Nile.
#Owner: Worker B (arteries-region). Run from the project root.
#
#source("R/00_config.R"); source("R/20_region.R")
#region_data <- load_region_data()
#make_region_map(region_data)
#
#Data used:
#* Natural Earth 1:50m countries (rnaturalearth, offline via rnaturalearthdata)
#* Natural Earth 1:10m rivers + lake centerlines, frozen subset in
#data/processed/region_waterways.gpkg (created once by fetch_region_data())
#* EIA 2021 chokepoint oil flows, hand-transcribed in data/raw/region_eia_2021.csv

if (!exists("PROJECT_ROOT")) source("R/00_config.R")

suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
  library(rnaturalearth)
  library(ggrepel)
})

REGION_WATERWAYS_GPKG <- project_path("data", "processed", "region_waterways.gpkg")
REGION_WATERWAYS_META <- project_path("data", "raw", "region_waterways_source.csv")
REGION_CACHE_DIR      <- project_path("data", "cache")

#---- Waterway selection ------------------------------------------------------
#Verified against the actual Natural Earth 1:10m schema (columns `name`, `name_en`,
#`featurecla`). Natural Earth names the White Nile ("Bahr el Jebel", "El Bahr el Abyad")
#and Blue Nile ("Abay", "El Bahr el Azraq") segments, so no geometry is invented.
WATERWAY_SELECTION <- data.frame(
  waterway = c("Nile", "Nile", "Nile",
               "White Nile", "Blue Nile", "Suez Canal"),
  field    = c("name", "name", "name",
               "name_en", "name_en", "name"),
  value    = c("Nile", "Damietta Branch", "Rosetta Branch",
               "White Nile", "Blue Nile", "Suez Canal")
)

#First-time download of the Natural Earth rivers layer; writes the small frozen subset.
#Needs network access once. Routine renders use the saved subset instead.
fetch_region_data <- function(cache_dir = REGION_CACHE_DIR) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(dirname(REGION_WATERWAYS_GPKG), showWarnings = FALSE, recursive = TRUE)

  rivers <- rnaturalearth::ne_download(
    scale = 10, type = "rivers_lake_centerlines", category = "physical",
    returnclass = "sf", destdir = cache_dir, load = TRUE
  )

  selected <- do.call(rbind, lapply(seq_len(nrow(WATERWAY_SELECTION)), function(i) {
    sel  <- WATERWAY_SELECTION[i, ]
    rows <- rivers[!is.na(rivers[[sel$field]]) & rivers[[sel$field]] == sel$value, ]
    if (nrow(rows) == 0) stop("No Natural Earth features found for ", sel$value, call. = FALSE)
    sf::st_sf(
      waterway   = sel$waterway,
      ne_id      = rows$ne_id,
      ne_name    = rows$name,
      ne_name_en = rows$name_en,
      featurecla = rows$featurecla,
      geometry   = sf::st_geometry(rows)
    )
  }))

  #Keep only what lies in (a small pad around) the region so the frozen file stays small.
  pad  <- 1
  bbox <- sf::st_bbox(c(xmin = REGION_XLIM[1] - pad, xmax = REGION_XLIM[2] + pad,
                        ymin = REGION_YLIM[1] - pad, ymax = REGION_YLIM[2] + pad), crs = sf::st_crs(4326))
  selected <- suppressWarnings(sf::st_crop(selected, bbox))

  unlink(REGION_WATERWAYS_GPKG)
  sf::st_write(selected, REGION_WATERWAYS_GPKG, layer = "waterways", quiet = TRUE)

  meta <- data.frame(
    layer      = "waterways",
    source     = "Natural Earth 1:10m Rivers + Lake Centerlines (physical vectors)",
    source_url = "https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-rivers-lake-centerlines/",
    retrieved  = as.character(Sys.Date()),
    ne_version = "rnaturalearth::ne_download default (ne_10m_rivers_lake_centerlines)",
    filter     = "name/name_en in: Nile, Damietta Branch, Rosetta Branch, White Nile, Blue Nile, Suez Canal; cropped to region +1 degree",
    n_features = nrow(selected)
  )
  utils::write.csv(meta, REGION_WATERWAYS_META, row.names = FALSE)
  invisible(selected)
}

#---- Data --------------------------------------------------------------------

#Three principal chokepoints: representative points only, not strait polygons.
region_important_places <- function() {
  data.frame(
    place     = c("Bosphorus", "Bab el-Mandeb", "Strait of Hormuz"),
    longitude = c(29.06, 43.33, 56.25),
    latitude  = c(41.12, 12.58, 26.57)
  )
}

#Snap hand-chosen label targets onto the verified waterway geometry so every label
#leader line starts on a real Natural Earth line.
snap_to_waterway <- function(waterways, spec) {
  pts <- lapply(seq_len(nrow(spec)), function(i) {
    line  <- sf::st_union(waterways[waterways$waterway == spec$waterway[i], ])
    target <- sf::st_sfc(sf::st_point(c(spec$target_lon[i], spec$target_lat[i])), crs = 4326)
    sf::st_cast(sf::st_nearest_points(target, line), "POINT")[2]
  })
  anchors <- do.call(c, pts)
  xy <- sf::st_coordinates(anchors)
  sf::st_sf(spec, x = xy[, 1], y = xy[, 2], geometry = anchors, crs = 4326)
}

#Load all regional layers into a named list.
load_region_data <- function() {
  if (!file.exists(REGION_WATERWAYS_GPKG)) {
    stop("Missing ", REGION_WATERWAYS_GPKG, ". Run fetch_region_data() once (needs network).",
         call. = FALSE)
  }
  if (!file.exists(REGION_EIA_CSV)) stop("Missing ", REGION_EIA_CSV, call. = FALSE)

  countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  countries <- countries[, c("name", "iso_a3")]

  important_places    <- region_important_places()
  important_places_sf <- sf::st_as_sf(important_places,
                                      coords = c("longitude", "latitude"), crs = 4326)

  rivers <- sf::st_read(REGION_WATERWAYS_GPKG, layer = "waterways", quiet = TRUE)
  eia_flows <- utils::read.csv(REGION_EIA_CSV, stringsAsFactors = FALSE)

  #Label targets are approximate; they are snapped onto the real lines. `nudge_*` is the
  #preferred label offset (degrees) from the anchor.
  label_spec <- data.frame(
    label      = c("Nile", "White Nile", "Blue Nile", "Suez Canal"),
    waterway   = c("Nile", "White Nile", "Blue Nile", "Suez Canal"),
    target_lon = c(32.7, 32.4, 34.0, 32.4),
    target_lat = c(26.0, 12.5, 12.2, 30.4),
    nudge_x    = c(-3.5, -4.0, 3.4, 3.0),
    nudge_y    = c(0.8, -0.8, 1.4, -0.2)
  )

  #Names placed by hand at fixed positions; muted context only.
  country_labels <- data.frame(
    label = c("TURKEY", "SYRIA", "IRAQ", "IRAN", "LIBYA", "EGYPT", "SAUDI ARABIA", "SUDAN",
              "SOUTH SUDAN", "ETHIOPIA", "ERITREA", "SOMALIA", "YEMEN", "OMAN"),
    lon   = c(34.0, 38.5, 43.5, 54.5, 24.0, 27.5, 44.0, 28.5, 28.2, 40.0, 38.7, 46.5, 47.0, 56.3),
    lat   = c(39.2, 35.2, 33.0, 32.5, 27.5, 25.0, 24.0, 16.5, 6.6, 8.6, 15.6, 6.0, 15.7, 20.2)
  )
  sea_labels <- data.frame(
    label = c("Black Sea", "Mediterranean Sea", "Red Sea", "Persian Gulf", "Gulf of Aden",
              "Arabian Sea"),
    lon   = c(38.5, 28.5, 38.5, 51.6, 48.0, 57.0),
    lat   = c(43.0, 33.6, 20.2, 27.0, 12.2, 14.0),
    angle = c(0, 0, -52, 30, 0, 0)
  )

  list(
    countries           = countries,
    important_places    = important_places,
    important_places_sf = important_places_sf,
    rivers              = rivers,
    nile                = rivers[rivers$waterway %in% c("Nile", "White Nile", "Blue Nile"), ],
    suez                = rivers[rivers$waterway == "Suez Canal", ],
    eia_flows           = eia_flows,
    waterway_labels     = snap_to_waterway(rivers, label_spec),
    country_labels      = country_labels,
    sea_labels          = sea_labels
  )
}

#---- Map ---------------------------------------------------------------------

#Build the regional ggplot (does not write any file).
make_region_map <- function(region_data) {
  #Attach the 2021 EIA flow and the shared color and shape to each of the three points
  ip <- merge(region_data$important_places_sf, region_data$eia_flows[, c("location", "flow_mbd")],
              by.x = "place", by.y = "location", all.x = TRUE, sort = FALSE)
  ip <- merge(ip, POINT_STYLE, by = "place", sort = FALSE)
  ip$place <- factor(ip$place, levels = POINT_STYLE$place)
  ip$label <- sprintf("%s\n%.1f million b/d (2021)", ip$place, ip$flow_mbd)
  #Preferred offsets (degrees) from each point. The Bosphorus sits 2.4 degrees below the
  #top edge, so its label goes north-west over the Balkans rather than straight up.
  nudge <- data.frame(place = POINT_STYLE$place,
                      dx = c(-4.0, 5.0, 1.5), dy = c(1.4, -4.0, 3.3))
  ip <- merge(ip, nudge, by = "place", sort = FALSE)
  ip <- ip[order(ip$place), ]
  #Plain table of label text and point coordinates for ggrepel
  ip_labels <- cbind(sf::st_drop_geometry(ip), x = sf::st_coordinates(ip)[, 1],
                     y = sf::st_coordinates(ip)[, 2])

  #Named vectors so each place keeps its own color and shape
  point_fill  <- stats::setNames(POINT_STYLE$color, POINT_STYLE$place)
  point_shape <- stats::setNames(POINT_STYLE$shape, POINT_STYLE$place)

  ggplot() +
    #Basemap: Natural Earth countries
    geom_sf(data = region_data$countries, fill = PALETTE$land, color = PALETTE$border,
            linewidth = 0.25) +
    #Waterways drawn from Natural Earth line features; Suez is slightly thicker
    geom_sf(data = region_data$nile, color = PALETTE$nile, linewidth = 0.6, lineend = "round") +
    geom_sf(data = region_data$suez, color = PALETTE$nile, linewidth = 1.0, lineend = "round") +
    #Muted country and sea names for orientation
    geom_text(data = region_data$country_labels, aes(lon, lat, label = label),
              color = PALETTE$text_muted, size = 2.7, alpha = 0.8, lineheight = 0.9) +
    geom_text(data = region_data$sea_labels, aes(lon, lat, label = label, angle = angle),
              color = "#6E8CA0", size = 3.1, fontface = "italic") +
    #Waterway names with leader lines that start on the real lines
    geom_text_repel(
      data = region_data$waterway_labels, aes(x, y, label = label),
      nudge_x = region_data$waterway_labels$nudge_x, nudge_y = region_data$waterway_labels$nudge_y,
      color = PALETTE$nile, fontface = "bold", size = 3.6, bg.color = "white", bg.r = 0.15,
      segment.color = PALETTE$nile, segment.size = 0.4, min.segment.length = 0,
      box.padding = 0.2, point.padding = 0.1, force = 0.5, max.overlaps = Inf, seed = 42
    ) +
    #The three principal points: distinct colors and shapes
    geom_sf(data = ip, aes(fill = place, shape = place), color = "white", size = 4.6, stroke = 1) +
    #Point labels with the 2021 oil flow, repelled so they do not overlap
    geom_label_repel(
      data = ip_labels, aes(x, y, label = label),
      nudge_x = ip_labels$dx, nudge_y = ip_labels$dy,
      color = PALETTE$text, fill = alpha("white", 0.9), size = 3.4, fontface = "bold",
      lineheight = 0.95, label.size = 0.3, label.padding = 0.3,
      segment.color = PALETTE$text, segment.size = 0.5, min.segment.length = 0,
      box.padding = 0.4, point.padding = 0.5, force = 0.5, max.overlaps = Inf, seed = 42
    ) +
    scale_fill_manual(name = "Principal chokepoints\n(approximate location)",
                      values = point_fill) +
    scale_shape_manual(values = point_shape, guide = "none") +
    guides(fill = guide_legend(override.aes = list(shape = unname(point_shape[levels(ip$place)]),
                                                   color = "white", size = 4.6))) +
    #Fixed regional window in longitude and latitude
    coord_sf(xlim = REGION_XLIM, ylim = REGION_YLIM, expand = FALSE, crs = 4326) +
    #Title, subtitle and caption separate the 2021 data year from the EIA publication date
    labs(
      title = "Strategic Arteries: Three Straits, the Suez Canal and the Nile",
      subtitle = paste0("Labels show 2021 oil flows through each chokepoint (EIA; crude oil and ",
                        "petroleum liquids, million barrels per day)"),
      caption = paste0(
        "Points mark representative locations, not strait boundaries. Flows are volumes transiting each chokepoint, not\n",
        "oil rents or exports. Hormuz and Bab el-Mandeb: EIA Table 1; Bosphorus: EIA Table 6 (Dardanelles is separate).\n",
        "Data year 2021; EIA page updated 2026-03-03. Basemap and waterways: Natural Earth (1:50m countries, 1:10m rivers)."
      ),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      text             = element_text(color = PALETTE$text),
      plot.title       = element_text(face = "bold", size = 16),
      plot.subtitle    = element_text(color = PALETTE$text_muted, size = 10.5, margin = margin(b = 8)),
      plot.caption     = element_text(color = PALETTE$text_muted, size = 7.8, hjust = 0, lineheight = 1.1,
                                      margin = margin(t = 8)),
      plot.caption.position = "plot",
      plot.title.position   = "plot",
      panel.background = element_rect(fill = PALETTE$sea, color = NA),
      panel.border     = element_rect(fill = NA, color = PALETTE$border),
      panel.grid       = element_line(color = alpha("white", 0.6), linewidth = 0.25),
      legend.position  = "bottom",
      legend.title     = element_text(size = 9.5, face = "bold"),
      legend.text      = element_text(size = 9.5),
      plot.margin      = margin(12, 14, 10, 14)
    )
}
