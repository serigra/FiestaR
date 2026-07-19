#' Find best match and anti-match pairs based on trait similarity
#'
#' For each observation in \code{data}, finds its closest ("match") and most
#' dissimilar ("anti-match") counterpart based on Euclidean distance computed
#' over a set of numeric trait columns. Pairing is done using the Hungarian
#' algorithm (linear sum assignment) via \code{clue::solve_LSAP}, which finds
#' the globally optimal one-to-one assignment minimising (for matches) or
#' maximising (for anti-matches) total distance, rather than greedily pairing
#' each observation with its individual nearest/farthest neighbour.
#'
#' @param data A data frame containing at least a \code{name} column (used as
#'   the unique identifier for each observation) and the columns listed in
#'   \code{trait_names}.
#' @param trait_names A character vector of column names in \code{data} to be
#'   used for computing Euclidean distances between observations.
#'
#' @return A data frame with one row per observation in \code{data} (trait
#'   columns removed), augmented with the following columns:
#'   \describe{
#'     \item{match}{\code{name} of the closest paired observation.}
#'     \item{match_distance}{Euclidean distance to the matched observation.}
#'     \item{antimatch}{\code{name} of the most distant paired observation.}
#'     \item{antimatch_distance}{Euclidean distance to the anti-matched
#'       observation.}
#'   }
#'
#'
#' @examples
#' \dontrun{
#' get_match_antimatch(data = my_data, trait_names = c("trait1", "trait2"))
#' }
#'
#' @importFrom rlang .data
#' @export

get_match_antimatch <- function(data,
                                trait_names){

  scores <- as.matrix(data[, trait_names])

  # ── 2. Euclidean distance matrix ────────────────────────────────────────────
  dist_mat <- as.matrix(stats::dist(scores, method = "euclidean"))
  rownames(dist_mat) <- colnames(dist_mat) <- data$name

  # ── 3. Best MATCH pairs (Hungarian: minimise total distance) ────────────────
  # Block self-matching with large penalty
  match_cost <- dist_mat
  diag(match_cost) <- 1e9

  match_solution  <- clue::solve_LSAP(match_cost)    # linear sum assignment
  match_pairs_raw <- data.frame(
    A    = data$name,
    B    = data$name[match_solution],
    dist = dist_mat[cbind(seq_len(dim(data)[1]), match_solution)]
  )

  # Keep each pair once (A < B lexicographically)
  match_pairs <- match_pairs_raw[match_pairs_raw$A < match_pairs_raw$B, ]

  match_pairs_full <- rbind(match_pairs |> dplyr::select(.data$A, .data$B, .data$dist),
                            match_pairs |>
                              dplyr::select(.data$B, .data$A, .data$dist) |>
                              dplyr::rename(A = .data$B, B = .data$A)
                            ) |>
    dplyr::rename(name = .data$A,  match = .data$B, match_distance = .data$dist)


  # ── 4. Anti-MATCH pairs (Hungarian: maximise distance = minimise -distance) ─
  anti_solution      <- clue::solve_LSAP(dist_mat, maximum = TRUE)
  anti_pairs_raw     <- data.frame(
    A    = data$name,
    B    = data$name[anti_solution],
    dist = dist_mat[cbind(seq_len(dim(data)[1]), anti_solution)]
  )

  anti_pairs <- anti_pairs_raw[anti_pairs_raw$A < anti_pairs_raw$B, ]

  antimatch_pairs_full <- rbind(anti_pairs |> dplyr::select(.data$A, .data$B, .data$dist),
                                anti_pairs |>
                                  dplyr::select(.data$B, .data$A, .data$dist) |>
                                  dplyr::rename(A = .data$B, B = .data$A)
                                ) |>
    dplyr::rename(name = .data$A,  antimatch = .data$B, antimatch_distance = .data$dist)

 # ── 5. Results ──────────────────────────────────────────────────────────────

  d.out <-
    data |>
    dplyr::left_join(match_pairs_full, by = "name") |>
    dplyr::left_join(antimatch_pairs_full, by = "name") |>
    dplyr::select(-trait_names)

  return(d.out)

}



