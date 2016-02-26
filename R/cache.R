cache_class <- "Cache"

#' Create an empty cache
#'
#' @param function_cache_size number of results to store for each cached
#' function. When this limit is reached, new entries in the cache will
#' overwrite the oldest entry for the relevant function.
#' @param hash_algo the hash algorithm to use.
#'
#' @return a Cache object
#' @export
#' @importFrom hash hash
#'
#' @examples
#' cache <- create_cache()
create_cache <- function(function_cache_size=1, hash_algo="crc32") {
  cache <- new.env()
  class(cache) <- cache_class
  cache$cache_size <- function_cache_size
  cache$hash_algo <- hash_algo
  cache$function_cache <- hash()

  return(cache)
}

cache_set <- function(function_name, arg_hash, result, cache) {
  hash_slot <- cache$function_cache[[function_name]][["next_hash_slot"]]
  num_slots <- cache$cache_size

  hash_to_delete <- cache$function_cache[[function_name]][["hashes"]][hash_slot]

  if (nchar(hash_to_delete) > 0) {
    del(hash_to_delete, cache$function_cache[[function_name]])
  }

  cache$function_cache[[function_name]][["hashes"]][hash_slot] <- arg_hash
  cache$function_cache[[function_name]][[arg_hash]] <- result

  cache$function_cache[[function_name]][["next_hash_slot"]] <-
    (hash_slot %% num_slots) + 1

  return(NULL)
}

elapsed_time <- function(start_time) {
  proc.time()[[3]] - start_time
}

#' Retrieve from cache
#'
#' @param function_name function to retrieve against
#' @param function_call function call to retrieve against
#' @param cache a Cache object
#'
#' @return the result of evaluating \code{function_call} (either directly or
#' from the cache)
#'
#' @importFrom digest digest
#'
cache_get <- function(function_name, function_call, cache) {

  start_time <- proc.time()[[3]]

  function_args <-  as.list(function_call)[-1]
  if ("cache" %in% names(function_args)) {
    function_args[-which(names(function_args)=="cache")]
  }

  algo <- cache$hash_algo
  arg_hash <- digest(function_args[order(names(function_args))], algo=algo)

  if (!has.key(function_name, cache$function_cache)) {
    # Make a cache for this function
    cache$function_cache[[function_name]] <- hash()

    # We need to ape a circular linked list to cycle the cache
    cache$function_cache[[function_name]][["hashes"]] <-
      character(cache$cache_size)
    cache$function_cache[[function_name]][["next_hash_slot"]] <- 1

    # Set up some trackers
    cache$function_cache[[function_name]][["cache_hits"]] <- 0
    cache$function_cache[[function_name]][["cache_misses"]] <- 0
    cache$function_cache[[function_name]][["cache_hit_time"]] <- 0
    cache$function_cache[[function_name]][["cache_miss_time"]] <- 0

    result <- eval(function_call)

    cache_set(function_name, arg_hash, result, cache)

  } else {
    #We've got something cached for this function

    if (has.key(arg_hash, cache$function_cache[[function_name]])) {
      # Cache hit
      result <- cache$function_cache[[function_name]][[arg_hash]]

      cache$function_cache[[function_name]][["cache_hits"]] <-
        cache$function_cache[[function_name]][["cache_hits"]] + 1

      cache$function_cache[[function_name]][["cache_hit_time"]] <-
        cache$function_cache[[function_name]][["cache_hit_time"]] +
        elapsed_time(start_time)
    } else {
      # Cache miss
      result <- eval(function_call)
      cache_set(function_name, arg_hash, result, cache)

      cache$function_cache[[function_name]][["cache_misses"]] <-
        cache$function_cache[[function_name]][["cache_misses"]] + 1

      cache$function_cache[[function_name]][["cache_miss_time"]] <-
        cache$function_cache[[function_name]][["cache_miss_time"]] +
        elapsed_time(start_time)
    }
  }
  return(result)
}

#' Return the output of a function using a cache
#'
#' Check \code{cache} for the cached output of \code{fun} for the arguments
#' \code{...}. If a cache entry is found, return it. Otherwise call \code{fun}
#' with arguments \code{...} (and \code{cache} if cache is a parameter of
#' \code{fun}), cache the result and then return it.
#'
#' @param fun function to call
#' @param ... arguments to \code{fun} (excluding cache). Both named and positional
#' arguments are permitted.
#' @param cache a Cache object
#'
#' @return The output of \code{fun}
#' @export
#'
#' @examples
#' cache <- create_cache()
#'
#' result <- cached_call(mean, x=1:10, cache=cache)
#' result2 <- cached_call(mean, 1:10, cache=cache)
#'
cached_call <- function(fun, ..., cache=NULL) {
  function_name <- as.character(substitute(fun))

  if ("cache" %in% names(formals(fun))) {
    function_call <- as.call(c(list(fun), list(...), list(cache=cache)))
  } else {
    function_call <- as.call(c(list(fun), list(...)))
  }

  function_call <- match.call(fun, function_call, expand.dots=TRUE)

  if (is.null(cache)) {
    result <- eval(function_call)
  } else {
    result <- cache_get(function_name, function_call, cache=cache)
  }

  return(result)
}
