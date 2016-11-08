#' Print a cache function
#'
#' @param cache a Cache object
#'
#' @return NULL
#' @export
#'
#' @importFrom hash keys
#'
#' @examples
#' cache <- create_cache()
#' print(cache)
print.Cache <- function(cache) {
  stats <- c("cache_hits", "cache_hit_time", "cache_misses", "cache_miss_time", "function_call_time", "time_saved")
  sum_stats <- numeric(length(stats))
  names(sum_stats) <- stats

  total_time_saved <- 0

  for (fun_name in keys(cache$function_cache)) {
    cat(fun_name, fill=80)
    for (stat in stats) {
      cat("  ", stat, ": ", cache$function_cache[[fun_name]][[stat]], fill=80)
      sum_stats[stat] <- sum_stats[stat] + cache$function_cache[[fun_name]][[stat]]
    }

    time_saved <- cache$function_cache[[fun_name]][["time_saved"]]

    total_time_saved <- total_time_saved + time_saved

    time_cost <- cache$function_cache[[fun_name]][["cache_hit_time"]] +
      cache$function_cache[[fun_name]][["cache_miss_time"]]

    cat("   Time cost of caching: ", time_cost, fill=80)
    cat("   Time delta (saved - cost): ", time_saved - time_cost, fill=80)


    cat("\n")
  }

  cat("Totals:", fill=80)
  for (stat in stats) {
    cat("  ", stat, ": ", sum_stats[stat], fill=80)
  }
  cat("   Time saved through caching: ", total_time_saved, fill=80)
  total_time_cost <- sum_stats["cache_hit_time"] + sum_stats["cache_miss_time"]

  cat("   Time cost of caching: ", total_time_cost, fill=80)
}

#' Hash an object
#'
#' @param object an object to hash
#' @param cache the Cache object to take the hash function from
#'
#' @return a hash of \code{object}
#' @export
#'
#' @importFrom digest digest
hash_object <- function(object, cache) {
  return(digest(object, algo=cache$hash_algo))
}
