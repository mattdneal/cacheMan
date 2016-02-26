print.Cache <- function(cache) {
  stats <- c("cache_hits", "cache_hit_time", "cache_misses", "cache_miss_time")
  sum_stats <- numeric(4)
  names(sum_stats) <- stats

  total_time_saved <- 0

  for (fun_name in keys(cache$function_cache)) {
    cat(fun_name, fill=80)
    for (stat in stats) {
      cat("  ", stat, ": ", cache$function_cache[[fun_name]][[stat]], fill=80)
      sum_stats[stat] <- sum_stats[stat] + cache$function_cache[[fun_name]][[stat]]
    }

    time_saved <- (cache$function_cache[[fun_name]][["cache_miss_time"]] /
                     cache$function_cache[[fun_name]][["cache_misses"]]
    ) * cache$function_cache[[fun_name]][["cache_hits"]] -
      cache$function_cache[[fun_name]][["cache_hit_time"]]

    total_time_saved <- total_time_saved + time_saved

    cat("   Time saved through caching: ", time_saved, fill=80)

    cat("\n")
  }

  cat("Totals:", fill=80)
  for (stat in stats) {
    cat("  ", stat, ": ", sum_stats[stat], fill=80)
  }
  cat("   Time saved through caching: ", total_time_saved, fill=80)
}
