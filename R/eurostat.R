# Minimal Eurostat dissemination API client (JSON-stat 2.0 -> tidy data frame).
# Depends only on jsonlite.

eurostat_api <- "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/"

# filters: named list, e.g. list(geo = "EL", time = c("2024", "2025")).
fetch_eurostat <- function(dataset, filters = list(geo = "EL"), lang = "EN") {
  query <- unlist(Map(function(k, v) paste0(k, "=", utils::URLencode(v, reserved = TRUE)),
                      rep(names(filters), lengths(filters)), unlist(filters)))
  url <- paste0(eurostat_api, dataset, "?", paste(c(query, "format=JSON", paste0("lang=", lang)),
                                                  collapse = "&"))
  # Large tables can take well over R's default 60 s download timeout.
  old <- options(timeout = max(300, getOption("timeout")))
  on.exit(options(old))
  js <- jsonlite::fromJSON(url, simplifyVector = FALSE)
  jsonstat_to_df(js)
}

jsonstat_to_df <- function(js) {
  ids   <- unlist(js$id)
  sizes <- unlist(js$size)
  # Category codes of each dimension, in index order.
  codes <- lapply(ids, function(d) {
    idx <- unlist(js$dimension[[d]]$category$index)
    names(sort(idx))
  })
  labels <- lapply(ids, function(d) unlist(js$dimension[[d]]$category$label))
  names(codes) <- names(labels) <- ids

  n <- prod(sizes)
  grid <- expand.grid(rev(codes), stringsAsFactors = FALSE)[, rev(ids), drop = FALSE]
  flat <- seq_len(n) - 1L  # row-major, last dimension fastest (matches expand.grid on reversed dims)

  values <- rep(NA_real_, n)
  if (length(js$value)) values[as.integer(names(js$value)) + 1L] <- unlist(js$value)
  flags <- rep(NA_character_, n)
  if (length(js$status)) flags[as.integer(names(js$status)) + 1L] <- unlist(js$status)

  df <- grid
  for (d in ids) {
    if (!d %in% c("geo", "time", "freq")) df[[paste0(d, "_label")]] <- unname(labels[[d]][df[[d]]])
  }
  df$value <- values
  df$flag  <- flags
  df <- df[!is.na(df$value), setdiff(names(df), "freq"), drop = FALSE]
  if ("time" %in% names(df)) df$time <- as.integer(df$time)
  attr(df, "label")   <- js$label
  attr(df, "updated") <- js$updated
  rownames(df) <- NULL
  df
}
