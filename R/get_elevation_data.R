#' Retrieve Elevation Data Along a Route
#'
#' Generates a route between an origin and destination using Google Maps,
#' retrieves elevation values for each route point from AWS Terrain Tiles via
#' the \pkg{elevatr} package, and calculates segment and cumulative distances.
#'
#'
#' @param name Character string identifying the route (e.g. a persons name). The value is
#'   included in the output data frame.
#' @param origin Character string specifying the route starting location.
#' @param destination Character string specifying the route destination.
#'   Defaults to `"chaesalp, switzerland"`.
#' @param structure Character string passed to [ggmap::trek()] defining the
#'   route structure. Defaults to `"route"`.
#' @param mode Character string specifying the travel mode used by
#'   [ggmap::trek()]. Common options include `"driving"`, `"walking"`,
#'   `"bicycling"`, and `"transit"`. Defaults to `"bicycling"`.
#' @param cache_dir Character string specifying the directory where the elevation
#'   data will be cached.
#'
#' @details
#'
#' The function (1) retrieves route coordinates using [ggmap::trek()].
#' (2) Converts the route to an `sf` object.
#' (3) Obtains elevation values using [elevatr::get_elev_point()] with `src = "aws"`.
#' (4) Computes geodesic distances between consecutive points using [geosphere::distGeo()].
#'
#'
#' Before using this function, register your Google Maps API key.
#' Set in your environment variable as e.g. "GOOGLE_MAPS_KEY" and then register:
#'
#' \preformatted{
#' ggmap::register_google(
#'   key = Sys.getenv("GOOGLE_MAPS_KEY"),
#'   write = TRUE
#' )
#' }
#'
#' @return A data frame containing:
#' \describe{
#'   \item{name}{Route identifier supplied via `name`.}
#'   \item{lat}{Latitude coordinate of the route point.}
#'   \item{lon}{Longitude coordinate of the route point.}
#'   \item{distance}{Distance in meters from the previous route point. The
#'     first value is 0.}
#'   \item{cumulative_distance}{Cumulative distance in meters from the route
#'     origin.}
#'   \item{elevation}{Elevation in meters obtained from AWS Terrain Tiles.}
#' }
#'
#'
#'
#' @examples
#' \dontrun{
#' route_elev <- get_elevation_data(
#'   name = "Bike Tour",
#'   origin = "Utrecht, Netherlands",
#'   destination = "Chaesalp, Switzerland",
#'   mode = "bicycling"
#' )
#'
#' head(route_elev)
#' }
#'
#' @seealso [plot_elevation()]
#'
#' @export

get_elevation_data <- function(name,
                               origin,
                               destination = "chaesalp, switzerland",
                               structure = "route",
                               mode = 'bicycling',
                               cache_dir = "cache/elevation") {

  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  cache_file <- file.path(cache_dir, paste0(janitor::make_clean_names(name %||% origin), ".rds"))

  if (file.exists(cache_file)) {
    return(readRDS(cache_file)) # if data already exists in cache, return it, else -> get it!
  }

  # generate route dataframe with lon/lat points
  d.route <- tryCatch(

    ggmap::trek(origin, destination, mode = mode, structure = structure, output = "simple"),
    error = function(e) {
      stop(sprintf("trek() failed for '%s' (origin='%s', destination='%s'): %s",
                   name, origin, destination, conditionMessage(e)), call. = FALSE)
    }

  )

  # convert to sf object with lon/lat
  d.route_sf <- sf::st_as_sf(d.route, coords = c("lon", "lat"), crs = 4326)

  # get elevation data for these points
  elevation_points <- elevatr::get_elev_point(d.route_sf, src = "aws")

  # combine elevation points to a dataframe
  elevation_df <- cbind(d.route, elevation = elevation_points$elevation)

  # replace elevation for Chaesalp
  if(grepl("chaesalp|ch.salp", tolower(destination))) {
    elevation_df$elevation[nrow(elevation_df)] <- 617
  }

  distances <- geosphere::distGeo(d.route[-nrow(d.route), c("lon", "lat")], d.route[-1, c("lon", "lat")])

  # cumulative distance in meters
  cumulative_dist <- c(0, cumsum(distances))

  d.elevation <- data.frame(
    name = name,
    origin = origin,
    destination = destination,
    lat = d.route$lat,
    lon = d.route$lon,
    distance = c(0, distances),
    cumulative_distance = cumulative_dist,
    elevation = elevation_df$elevation
  )

  base::saveRDS(d.elevation, cache_file)
  return(d.elevation)

}
