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
#'   Defaults to `"utrecht, netherlands"`.
#' @param destination Character string specifying the route destination.
#'   Defaults to `"chaesalp, switzerland"`.
#' @param structure Character string passed to [ggmap::trek()] defining the
#'   route structure. Defaults to `"route"`.
#' @param mode Character string specifying the travel mode used by
#'   [ggmap::trek()]. Common options include `"driving"`, `"walking"`,
#'   `"bicycling"`, and `"transit"`. Defaults to `"bicycling"`.
#'
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

get_elevation_data <- function(name = NULL,
                               origin = "utrecht, netherlands",
                               destination = "chaesalp, switzerland",
                               structure = "route",
                               mode = 'bicycling') {

  # requires to be registered with Google via a Google Maps API key
  # -> set in your environment variable as "GOOGLE_MAPS_KEY"
  # -> register: ggmap::register_google(key = Sys.getenv("GOOGLE_MAPS_KEY"), write = TRUE)


  # generate route dataframe with lon/lat points
  d.route <- ggmap::trek(origin, destination,
                         mode = mode,
                         structure = structure, output = 'simple')

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
    lat = d.route$lat,
    lon = d.route$lon,
    distance = c(0, distances),
    cumulative_distance = cumulative_dist,
    elevation = elevation_df$elevation
  )

  return(d.elevation)

}
