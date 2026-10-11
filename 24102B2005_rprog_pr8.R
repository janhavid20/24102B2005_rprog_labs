# Laboratory Assignment 8: High-Performance Big Data Analytics Using R
# NYC TLC Yellow Taxi Trip Records

TRIP_FILES <- c("yellow_tripdata_2025-01.parquet")
ZONE_FILE <- "taxi_zone_lookup.csv"
OUTPUT_DIR <- "lab8_output"
PARALLEL_WORKERS <- max(1L, parallel::detectCores(logical = FALSE) - 1L)
BENCHMARK_REPEATS <- 3L

packages <- c("data.table", "ggplot2", "foreach", "doParallel",
              "microbenchmark", "purrr", "lubridate", "arrow")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
invisible(lapply(packages, library, character.only = TRUE))
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

missing_trip_files <- TRIP_FILES[!file.exists(TRIP_FILES)]
if (length(missing_trip_files)) stop(
  "Trip file(s) not found: ", paste(missing_trip_files, collapse = ", "),
  "\nDownload Yellow Taxi Trip Records Parquet file(s) from ",
  "https://www.nyc.gov/site/tlc/about/tlc-trip-record-data.page",
  "\nand update TRIP_FILES at the top of this script."
)
if (!file.exists(ZONE_FILE)) stop(
  "Zone lookup file not found: ", ZONE_FILE,
  "\nDownload the Taxi Zone Lookup CSV from the official TLC page and ",
  "update ZONE_FILE at the top of this script."
)

# 1. Data preparation
trips <- rbindlist(lapply(TRIP_FILES, function(f) as.data.table(arrow::read_parquet(f))), fill = TRUE)
zones <- data.table::fread(ZONE_FILE)
required <- c("tpep_pickup_datetime", "tpep_dropoff_datetime",
              "PULocationID", "DOLocationID", "passenger_count",
              "trip_distance", "fare_amount", "tip_amount",
              "tolls_amount", "total_amount", "payment_type")
absent <- setdiff(required, names(trips))
if (length(absent)) stop("Missing trip columns: ", paste(absent, collapse = ", "))
trips <- trips[, ..required]

cat("Raw dimensions:", nrow(trips), "rows x", ncol(trips), "columns\n")
capture.output(str(trips), file = file.path(OUTPUT_DIR, "data_structure.txt"))
capture.output(summary(trips), file = file.path(OUTPUT_DIR, "data_summary.txt"))
duplicate_count <- sum(duplicated(trips))
missing_before <- trips[, lapply(.SD, function(x) sum(is.na(x)))]

trips[, pickup_datetime := as.POSIXct(tpep_pickup_datetime, tz = "UTC")]
trips[, dropoff_datetime := as.POSIXct(tpep_dropoff_datetime, tz = "UTC")]
trips[, `:=`(
  PULocationID = as.integer(PULocationID),
  DOLocationID = as.integer(DOLocationID),
  passenger_count = as.numeric(passenger_count),
  trip_distance = as.numeric(trip_distance),
  fare_amount = as.numeric(fare_amount),
  tip_amount = as.numeric(tip_amount),
  tolls_amount = as.numeric(tolls_amount),
  total_amount = as.numeric(total_amount),
  payment_type = as.character(payment_type)
)]
trips <- unique(trips)
trips <- trips[
  !is.na(pickup_datetime) & !is.na(dropoff_datetime) &
    dropoff_datetime >= pickup_datetime &
    !is.na(PULocationID) & !is.na(DOLocationID) &
    !is.na(trip_distance) & trip_distance >= 0 &
    !is.na(fare_amount) & fare_amount >= 0 &
    !is.na(total_amount) & total_amount >= 0
]
trips[, `:=`(
  hour = lubridate::hour(pickup_datetime),
  day = lubridate::day(pickup_datetime),
  day_of_week = factor(
    lubridate::wday(pickup_datetime, label = TRUE, abbr = FALSE),
    levels = c("Sunday", "Monday", "Tuesday", "Wednesday",
               "Thursday", "Friday", "Saturday")
  ),
  month = lubridate::month(pickup_datetime, label = TRUE, abbr = FALSE),
  month_num = lubridate::month(pickup_datetime)
)]
if (!all(c("LocationID", "Borough", "Zone") %in% names(zones))) {
  stop("Zone lookup must contain LocationID, Borough, and Zone.")
}
zones <- unique(zones[, .(LocationID, Borough, Zone)])
pickup_zones <- copy(zones)
setnames(pickup_zones, c("LocationID", "Borough", "Zone"),
         c("PULocationID", "pickup_borough", "pickup_zone"))
dropoff_zones <- copy(zones)
setnames(dropoff_zones, c("LocationID", "Borough", "Zone"),
         c("DOLocationID", "dropoff_borough", "dropoff_zone"))
trips[pickup_zones, on = "PULocationID",
      `:=`(pickup_borough = i.pickup_borough, pickup_zone = i.pickup_zone)]
trips[dropoff_zones, on = "DOLocationID",
      `:=`(dropoff_borough = i.dropoff_borough, dropoff_zone = i.dropoff_zone)]

fwrite(missing_before, file.path(OUTPUT_DIR, "missing_values_before_cleaning.csv"))
writeLines(c(
  paste("Duplicate rows removed:", duplicate_count),
  paste("Rows after cleaning:", nrow(trips)),
  paste("Columns after preparation:", ncol(trips)),
  paste("Approximate in-memory size (MB)", round(as.numeric(object.size(trips)) / 1024^2, 2))
), file.path(OUTPUT_DIR, "cleaning_summary.txt"))

# 2. Transportation analytics with data.table
trips_by_hour <- trips[, .(trips = .N), by = hour][order(hour)]
trips_by_weekday <- trips[, .(trips = .N), by = day_of_week][order(day_of_week)]
trips_by_month <- trips[, .(trips = .N, average_fare = mean(fare_amount, na.rm = TRUE),
                            revenue = sum(total_amount, na.rm = TRUE)),
                        by = .(month_num, month)][order(month_num)]
fare_revenue_by_hour <- trips[, .(average_fare = mean(fare_amount, na.rm = TRUE),
                                  total_revenue = sum(total_amount, na.rm = TRUE)),
                              by = hour][order(hour)]
route_stats <- trips[, .(trips = .N, total_revenue = sum(total_amount, na.rm = TRUE),
                         average_fare = mean(fare_amount, na.rm = TRUE),
                         average_distance = mean(trip_distance, na.rm = TRUE)),
                     by = .(PULocationID, DOLocationID, pickup_zone, dropoff_zone)]
top_routes <- route_stats[order(-trips)][1:min(10L, .N)]
top_revenue_routes <- route_stats[order(-total_revenue)][1:min(10L, .N)]
distance_fare <- trips[trip_distance > 0 & fare_amount > 0,
  .(average_fare = mean(fare_amount), trips = .N),
  by = .(distance_band = cut(trip_distance, c(0, 1, 2, 5, 10, 20, Inf),
    labels = c("0-1", "1-2", "2-5", "5-10", "10-20", "20+")))]
distance_fare <- distance_fare[!is.na(distance_band)]
payment_by_borough <- trips[, .(trips = .N), by = .(pickup_borough, payment_type)][order(-trips)]
top_pickups <- trips[, .(trips = .N), by = .(PULocationID, pickup_zone)][order(-trips)][1:min(10L, .N)]
top_dropoffs <- trips[, .(trips = .N), by = .(DOLocationID, dropoff_zone)][order(-trips)][1:min(10L, .N)]

outputs <- list(
  trips_by_hour = trips_by_hour,
  trips_by_weekday = trips_by_weekday,
  trips_by_month = trips_by_month,
  fare_revenue_by_hour = fare_revenue_by_hour,
  top_routes = top_routes,
  top_revenue_routes = top_revenue_routes,
  distance_vs_fare = distance_fare,
  payment_by_borough = payment_by_borough,
  top_pickup_locations = top_pickups,
  top_dropoff_locations = top_dropoffs
)
for (nm in names(outputs)) fwrite(outputs[[nm]], file.path(OUTPUT_DIR, paste0(nm, ".csv")))

# 3. Functional, vectorized, and data.table implementations
hour_vectorized <- function(d) {
  z <- table(d$hour)
  data.table(hour = as.integer(names(z)), trips = as.integer(z))
}
hour_apply <- function(d) {
  tab <- table(d$hour)
  z <- apply(matrix(as.integer(tab), ncol = 1L), 1L, identity)
  data.table(hour = as.integer(names(tab)), trips = as.integer(z))
}
hour_lapply <- function(d) {
  parts <- split(d$hour, d$hour)
  data.table(hour = as.integer(names(parts)),
             trips = vapply(parts, length, integer(1)))
}
hour_purrr <- function(d) {
  parts <- split(d$hour, d$hour)
  data.table(hour = as.integer(names(parts)), trips = purrr::map_int(parts, length))
}
hour_datatable <- function(d) d[, .(trips = .N), by = hour][order(hour)]
bench_data <- trips[, .(hour)]
benchmarks <- microbenchmark(
  vectorized = hour_vectorized(bench_data),
  apply = hour_apply(bench_data),
  lapply = hour_lapply(bench_data),
  purrr_map = hour_purrr(bench_data),
  data_table = hour_datatable(bench_data),
  times = BENCHMARK_REPEATS, unit = "ms"
)
benchmark_summary <- as.data.table(summary(benchmarks))
fwrite(benchmark_summary, file.path(OUTPUT_DIR, "implementation_benchmarks.csv"))

# 4. Sequential versus parallel processing
partition_key <- if (uniqueN(trips$month_num) > 1L) "month_num" else "day_of_week"
partition_values <- unique(trips[[partition_key]])
partitions <- lapply(partition_values, function(v) {
  if (partition_key == "month_num") trips[month_num == v] else trips[day_of_week == v]
})
names(partitions) <- as.character(partition_values)
analysis_function <- function(d) {
  d[, .(trips = .N, revenue = sum(total_amount, na.rm = TRUE),
        average_fare = mean(fare_amount, na.rm = TRUE)), by = hour]
}
seq_time <- system.time({
  sequential_result <- rbindlist(lapply(partitions, analysis_function), idcol = "partition")
})
workers <- min(PARALLEL_WORKERS, length(partitions))
parallel_result <- sequential_result
parallel_time <- NA_real_
if (workers >= 2L) {
  cluster <- parallel::makeCluster(workers)
  doParallel::registerDoParallel(cluster)
  parallel_time <- system.time({
    parallel_result <- foreach::foreach(
      i = seq_along(partitions), .combine = function(...) data.table::rbindlist(list(...), fill = TRUE),
      .packages = "data.table", .export = "analysis_function"
    ) %dopar% {
      ans <- analysis_function(partitions[[i]])
      ans[, partition := names(partitions)[i]]
      ans
    }
  )[["elapsed"]]
  parallel::stopCluster(cluster)
  foreach::registerDoSEQ()
}
seq_elapsed <- unname(seq_time[["elapsed"]])
speedup <- if (!is.na(parallel_time) && parallel_time > 0) seq_elapsed / parallel_time else NA_real_
fwrite(sequential_result, file.path(OUTPUT_DIR, "sequential_partition_results.csv"))
fwrite(parallel_result, file.path(OUTPUT_DIR, "parallel_partition_results.csv"))
writeLines(c(
  paste("Partitioning variable:", partition_key),
  paste("Number of partitions:", length(partitions)),
  paste("Parallel workers:", workers),
  paste("Sequential elapsed seconds:", round(seq_elapsed, 4)),
  paste("Parallel elapsed seconds:", ifelse(is.na(parallel_time), "not run", round(parallel_time, 4))),
  paste("Speedup:", ifelse(is.na(speedup), "not available", round(speedup, 4)))
), file.path(OUTPUT_DIR, "parallel_benchmark.txt"))

# 5. Visualizations
save_plot <- function(p, filename, width = 9, height = 5) {
  ggsave(file.path(OUTPUT_DIR, filename), plot = p, width = width, height = height, dpi = 150)
}
save_plot(ggplot(trips_by_hour, aes(hour, trips)) + geom_line() + geom_point() +
  labs(title = "NYC Yellow Taxi Trips by Hour", x = "Pickup hour (0-23)", y = "Number of trips") +
  theme_minimal(), "trips_by_hour.png")
save_plot(ggplot(trips_by_weekday, aes(day_of_week, trips)) + geom_col() +
  labs(title = "Taxi Demand by Day of Week", x = "Day of week", y = "Number of trips") +
  theme_minimal(), "trips_by_weekday.png")
save_plot(ggplot(trips_by_month, aes(month, trips, group = 1)) + geom_line() + geom_point() +
  labs(title = "Monthly Taxi Demand", x = "Pickup month", y = "Number of trips") +
  theme_minimal(), "trips_by_month.png")
save_plot(ggplot(fare_revenue_by_hour, aes(hour, total_revenue)) + geom_line() + geom_point() +
  labs(title = "Taxi Revenue by Pickup Hour", x = "Pickup hour (0-23)",
       y = "Total amount collected (USD)") + theme_minimal(), "revenue_by_hour.png")
save_plot(ggplot(distance_fare, aes(distance_band, average_fare)) + geom_col() +
  labs(title = "Average Fare by Trip-Distance Band", x = "Trip distance (miles)",
       y = "Average fare (USD)") + theme_minimal(), "distance_vs_fare.png")
save_plot(ggplot(top_pickups, aes(reorder(pickup_zone, trips), trips)) + geom_col() + coord_flip() +
  labs(title = "Top Pickup Zones", x = "Pickup zone", y = "Number of trips") +
  theme_minimal(), "top_pickup_zones.png")
save_plot(ggplot(payment_by_borough, aes(payment_type, trips, fill = pickup_borough)) + geom_col() +
  labs(title = "Payment Type by Pickup Borough", x = "Payment type code",
       y = "Number of trips", fill = "Borough") + theme_minimal(), "payment_by_borough.png")

# 6. Findings and evidence-based recommendation
peak_hour <- trips_by_hour[which.max(trips), hour]
peak_day <- as.character(trips_by_weekday[which.max(trips), day_of_week])
top_route_text <- if (nrow(top_routes)) {
  paste0(top_routes$PULocationID[1], " -> ", top_routes$DOLocationID[1],
         " (", top_routes$trips[1], " trips)")
} else "Not available"
fastest_method <- benchmark_summary[which.min(benchmark_summary$median), ]
writeLines(c(
  "NYC TLC Yellow Taxi Trip Analysis",
  paste("Cleaned trip records:", format(nrow(trips), big.mark = ",")),
  paste("Peak pickup hour:", peak_hour),
  paste("Highest-demand weekday:", peak_day),
  paste("Most frequent route by location ID:", top_route_text),
  paste("Fastest measured hourly-count implementation:", as.character(fastest_method$expr)),
  paste("Median benchmark time (ms):", round(fastest_method$median, 4)),
  "",
  "Critical analysis:",
  "- data.table is well suited to large tables because grouping, joins, and updates are implemented efficiently and can avoid many intermediate copies.",
  "- Vectorized operations often avoid R-level loops; apply/lapply/purrr provide reusable functional patterns but may add conversion, splitting, or function-call overhead.",
  "- Parallel speedup depends on workload size, number of partitions, memory bandwidth, and worker startup/export overhead. Parallel processing may be slower for small jobs.",
  "- Use the measured benchmark CSV and parallel benchmark text as experimental evidence; results depend on the machine and selected dataset.",
  "- Recommendation: use data.table for large-scale cleaning, joins, grouping, and aggregation; use vectorized and functional approaches where they improve clarity or fit the operation."
), file.path(OUTPUT_DIR, "findings_and_recommendation.txt"))

cat("\nCompleted. Outputs saved in:", normalizePath(OUTPUT_DIR), "\n")
cat("Cleaned records:", format(nrow(trips), big.mark = ","), "\n")
cat("Peak hour:", peak_hour, "| Highest-demand weekday:", peak_day, "\n")
cat("Parallel speedup:", ifelse(is.na(speedup), "not available", round(speedup, 3)), "\n")
