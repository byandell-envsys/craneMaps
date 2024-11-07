# Count the observations in each ecoregion each month
# This routine is probably not needed as simple dplyr::count does the work.
# However, need to check out mean_occurrences to make sure.
get_monthly_regional_observations <- function(df, region_type, occurrence_name) {
  
  occurrence_df <- 
    dplyr::group_by(df, !!sym(region_type), month) |>
    dplyr::summarise(occurrences = n(), .groups = 'drop') |>
    dplyr::ungroup()
  
  # Get rid of rare observations (possible misidentification)
  occurrence_df <- dplyr::filter(occurrence_df, occurrences > 1)
  
  # Take the mean by region
  mean_occurrences_by_region <- 
    dplyr::group_by(occurrence_df, !!sym(region_type)) |>
    dplyr::summarise(mean_occurrences = mean(occurrences), .groups = 'drop') |>
    dplyr::ungroup()
  
  # Take the mean by month
  mean_occurrences_by_month <- 
    dplyr::group_by(occurrence_df, month) |>
    dplyr::summarise(mean_occurrences = mean(occurrences), .groups = 'drop') |>
    dplyr::ungroup()
  
  # Normalize by space and time for sampling effort
  occurrence_df <-
    dplyr::left_join(occurrence_df, mean_occurrences_by_region,
                     by = region_type) |>
    dplyr::left_join(mean_occurrences_by_month, by = "month") |>
    dplyr::mutate(norm_occurrences = occurrences / mean_occurrences.x / mean_occurrences.y)
  
  return(dplyr::select(occurrence_df, -mean_occurrences.x, -mean_occurrences.y))
}
