#World map: oil rents (% of GDP), 2021 (Worker A).
#
#Public functions
#load_world_data()      sf countries + oil_rents_pct_2021 + oil_rents_class (offline, from cached CSV)
#make_world_map(world)  ggplot choropleth
#fetch_world_data()     first-time WDI download that writes the cached CSV (needs network)
#world_coverage_report() ISO3 join / missingness diagnostics
#
#Indicator: World Bank WDI NY.GDP.PETR.RT.ZS = value of crude oil production minus
#production costs, as a share of GDP. It is NOT production volume, exports, or strait use.

if (!exists("PROJECT_ROOT")) source("R/00_config.R")

library(sf)
library(ggplot2)

#Class labels low -> high; "No data" is a separate class so that NA is never shown as 0.
OIL_RENT_BREAKS <- c(-Inf, 0, 1, 5, 10, 20, Inf)
OIL_RENT_LABELS <- c("0%", ">0-1%", ">1-5%", ">5-10%", ">10-20%", ">20%")
NO_DATA_LABEL   <- "No data"

#---- Data acquisition (run once; ordinary renders use the cached CSV) ---------

#Download 2021 oil rents for all WDI economies and cache them as a CSV.
#Aggregates (World, income groups, regions, ...) are kept in the file but flagged,
#so the exclusion is visible. WDI marks them with region == "Aggregates" or no region.
fetch_world_data <- function(path = WORLD_RAW_CSV) {
  wdi <- WDI::WDI(country = "all", indicator = WDI_INDICATOR,
                  start = REFERENCE_YEAR, end = REFERENCE_YEAR, extra = TRUE)
  if (nrow(wdi) == 0 || !WDI_INDICATOR %in% names(wdi)) {
    stop("WDI returned no usable data for ", WDI_INDICATOR, "; refusing to write a cache.",
         call. = FALSE)
  }
  wdi$is_aggregate <- is.na(wdi$region) | wdi$region == "Aggregates"
  wdi$retrieved_on <- as.character(Sys.Date())
  keep <- c("country", "iso2c", "iso3c", "year", WDI_INDICATOR, "region", "income",
            "is_aggregate", "lastupdated", "retrieved_on")
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(wdi[, keep], path, row.names = FALSE, na = "")
  invisible(path)
}

#---- Loading and joining -------------------------------------------------------

#Read the cached WDI CSV, keep country rows only, and return iso3c + oil rents.
read_world_oil_rents <- function(path = WORLD_RAW_CSV) {
  if (!file.exists(path)) {
    stop("Cached oil-rents file not found: ", path,
         "\nRun fetch_world_data() once (needs network) to create it.", call. = FALSE)
  }
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "")
  stopifnot(all(raw$year == REFERENCE_YEAR), WDI_INDICATOR %in% names(raw))
  countries <- raw[!raw$is_aggregate, ]
  data.frame(iso3c = countries$iso3c,
             wdi_country = countries$country,
             oil_rents_pct_2021 = as.numeric(countries[[WDI_INDICATOR]]),
             stringsAsFactors = FALSE)
}

#Natural Earth medium-scale countries with a World Bank-style ISO3 join key.
#`iso_a3` is -99 for France, Norway, Kosovo and others. `iso_a3_eh` repairs France and
#Norway, but it also gives Australia's code to two Australian dependencies (Indian Ocean
#Ter., Ashmore and Cartier Is.), which would wrongly inherit Australia's value. So the
#`_eh` code is accepted only when it equals the polygon's own `adm0_a3`.
#Kosovo has no ISO code in Natural Earth; the World Bank uses XKX.
read_world_polygons <- function() {
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  world$iso3c <- world$iso_a3
  repair <- world$iso_a3 == "-99" & world$iso_a3_eh == world$adm0_a3
  world$iso3c[repair] <- world$iso_a3_eh[repair]
  world$iso3c[world$adm0_a3 == "KOS"] <- "XKX"
  world$iso3c[world$iso3c == "-99"] <- NA_character_
  world
}

#Bin oil rents: exactly 0, (0,1], (1,5], (5,10], (10,20], >20, plus "No data" for NA.
classify_oil_rents <- function(x) {
  if (any(x < 0, na.rm = TRUE)) stop("Negative oil-rent values found; check the data.", call. = FALSE)
  cls <- cut(x, breaks = OIL_RENT_BREAKS, labels = OIL_RENT_LABELS, right = TRUE)
  cls <- factor(as.character(cls), levels = c(OIL_RENT_LABELS, NO_DATA_LABEL))
  cls[is.na(x)] <- NO_DATA_LABEL
  cls
}

#Countries as an sf object (EPSG:4326) with 2021 oil rents; NA is preserved.
load_world_data <- function() {
  world <- read_world_polygons()
  rents <- read_world_oil_rents()
  #Each iso3c is unique in both tables, so a left join cannot duplicate polygons.
  stopifnot(!anyDuplicated(rents$iso3c))
  world <- dplyr::left_join(world, rents, by = "iso3c")
  world$oil_rents_class <- classify_oil_rents(world$oil_rents_pct_2021)
  world[, c("name", "admin", "iso3c", "continent", "wdi_country",
            "oil_rents_pct_2021", "oil_rents_class")]
}

#Coverage summary: WDI rows, aggregates excluded, polygons, matches, unmatched ids.
world_coverage_report <- function(path = WORLD_RAW_CSV) {
  raw <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "")
  rents <- read_world_oil_rents(path)
  world <- read_world_polygons()
  matched <- world$iso3c %in% rents$iso3c
  list(
    wdi_rows_raw         = nrow(raw),
    wdi_aggregates_dropped = sum(raw$is_aggregate),
    wdi_country_rows     = nrow(rents),
    wdi_country_nonmissing_2021 = sum(!is.na(rents$oil_rents_pct_2021)),
    ne_polygons          = nrow(world),
    ne_matched_iso3      = sum(matched),
    ne_unmatched         = sort(world$name[!matched]),
    wdi_unmatched        = sort(rents$wdi_country[!rents$iso3c %in% world$iso3c]),
    ne_with_value        = sum(!is.na(load_world_data()$oil_rents_pct_2021))
  )
}

#---- Plot ----------------------------------------------------------------------

#Choropleth of 2021 oil rents. Antarctica is omitted from the drawing only
#(it has no data and wastes map area); load_world_data() still contains it.
make_world_map <- function(world_data) {
  stopifnot(inherits(world_data, "sf"),
            all(c("oil_rents_pct_2021", "oil_rents_class") %in% names(world_data)))
  #Drop Antarctica from the drawing only
  map_data <- world_data[world_data$name != "Antarctica", ]
  #One named color per class so the legend order matches the bins
  fill_colors <- stats::setNames(c(OIL_RENT_COLORS, PALETTE$missing),
                                 c(OIL_RENT_LABELS, NO_DATA_LABEL))

  ggplot(map_data) +
    #Fill each country by its oil-rent class; thin borders keep small countries visible
    geom_sf(aes(fill = oil_rents_class), color = PALETTE$border, linewidth = 0.1) +
    #drop = FALSE keeps empty classes in the legend
    scale_fill_manual(
      values = fill_colors, drop = FALSE,
      name = "Oil rents, % of GDP (2021)",
      guide = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5,
                           keywidth = unit(1.6, "cm"), keyheight = unit(0.5, "cm"))
    ) +
    coord_sf(crs = "ESRI:54030", expand = FALSE) +   #Robinson projection
    #Title, subtitle and source note state the variable and the year
    labs(
      title = "Which economies leaned most on oil in 2021?",
      subtitle = paste0("Oil rents as a share of GDP, 2021: the value of crude oil production minus ",
                        "extraction costs, divided by GDP.\n",
                        "This measures economic dependence, not the volume of oil produced or exported."),
      caption = paste0("Source: World Bank WDI indicator NY.GDP.PETR.RT.ZS (Oil rents, % of GDP), 2021 only; ",
                       "Natural Earth 1:50m countries.\n",
                       "Gray = no 2021 World Bank value (not zero), e.g. Kuwait, Venezuela, Turkmenistan, Syria, Yemen. ",
                       "Antarctica not shown.")
    ) +
    #Plain theme with a pale sea panel and the legend in one row at the bottom
    theme_void(base_size = 14) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = PALETTE$sea, color = NA),
      plot.title       = element_text(face = "bold", size = 22, color = PALETTE$text,
                                      margin = margin(t = 10, b = 4)),
      plot.subtitle    = element_text(size = 12.5, color = PALETTE$text, lineheight = 1.15,
                                      margin = margin(b = 10)),
      plot.caption     = element_text(size = 10, color = PALETTE$text_muted, hjust = 0,
                                      lineheight = 1.15, margin = margin(t = 10, b = 6)),
      plot.margin      = margin(10, 14, 6, 14),
      legend.position  = "bottom",
      legend.title     = element_text(size = 12, face = "bold", color = PALETTE$text),
      legend.text      = element_text(size = 11, color = PALETTE$text),
      legend.margin    = margin(t = 8)
    )
}
