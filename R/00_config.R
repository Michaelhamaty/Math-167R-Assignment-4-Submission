#Shared configuration for Arteries of Power (coordinator-owned).
#Workers read these constants; request changes through docs/logs/<role>.md.

#Walk up from the working directory until the folder that holds R/00_config.R and AGENTS.md
find_project_root <- function(start = getwd()) {
  dir <- normalizePath(start, mustWork = TRUE)
  repeat {
    if (file.exists(file.path(dir, "R", "00_config.R")) &&
        file.exists(file.path(dir, "AGENTS.md"))) {
      return(dir)
    }
    parent <- dirname(dir)
    if (identical(parent, dir)) {
      stop("Could not find project root (folder containing R/00_config.R) above: ", start)
    }
    dir <- parent
  }
}

PROJECT_ROOT <- find_project_root()

#Build a path relative to the project root so the code runs from any checkout
project_path <- function(...) file.path(PROJECT_ROOT, ...)

#---- Paths -------------------------------------------------------------------
RAW_DIR       <- project_path("data", "raw")
PROCESSED_DIR <- project_path("data", "processed")
OUTPUT_DIR    <- project_path("outputs")

WORLD_SCRIPT  <- project_path("R", "10_world.R")
REGION_SCRIPT <- project_path("R", "20_region.R")

WORLD_RAW_CSV  <- file.path(RAW_DIR, "world_oil_rents_2021.csv")
REGION_EIA_CSV <- file.path(RAW_DIR, "region_eia_2021.csv")

WORLD_MAP_PNG  <- file.path(OUTPUT_DIR, "world_map.png")
REGION_MAP_PNG <- file.path(OUTPUT_DIR, "region_map.png")

#---- Data constants ----------------------------------------------------------
REFERENCE_YEAR <- 2021L
WDI_INDICATOR  <- "NY.GDP.PETR.RT.ZS"  #Oil rents (% of GDP), not production or transit

REGION_XLIM <- c(20, 61)    #degrees east
REGION_YLIM <- c(4, 43.5)   #degrees north

#---- Export size (both maps >= 2400 px wide) ---------------------------------
EXPORT_DPI    <- 300
WORLD_WIDTH   <- 12
WORLD_HEIGHT  <- 7
REGION_WIDTH  <- 10
REGION_HEIGHT <- 10

#---- Shared palette ----------------------------------------------------------
PALETTE <- list(
  land       = "#EFE6D2",  #pale sand
  sea        = "#DCEAF2",  #pale blue
  border     = "#B9AE9A",  #subtle country outline
  nile       = "#2A8C9C",  #blue-teal waterways
  text       = "#2F3B4A",  #slate
  text_muted = "#5B6675",
  missing    = "#D4D4D0"   #neutral "No data" fill
)

#Warm copper sequential scale for the six oil-rent classes, low to high:
#exactly 0; (0,1]; (1,5]; (5,10]; (10,20]; >20.
OIL_RENT_COLORS <- c("#F6EAD7", "#EBCBA5", "#DDA673", "#C97E47", "#A85A2A", "#6E3517")

#Three principal points: distinct colors AND shapes (filled shapes 21-25).
POINT_STYLE <- data.frame(
  place = c("Bosphorus", "Bab el-Mandeb", "Strait of Hormuz"),
  color = c("#6A3D9A", "#D55E00", "#1B4F72"),
  shape = c(21L, 24L, 23L)
)

#---- Dependencies ------------------------------------------------------------
REQUIRED_PACKAGES <- c(
  "sf", "ggplot2", "rnaturalearth", "rnaturalearthdata", "dplyr",
  "WDI", "ggrepel", "geomtextpath", "scales", "testthat"
)

#Stop with a clear message if any required package is not installed
check_packages <- function(pkgs = REQUIRED_PACKAGES) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Missing R packages: ", paste(missing, collapse = ", "),
         ". Run renv::restore() from the project root.", call. = FALSE)
  }
  invisible(TRUE)
}
