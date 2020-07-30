library(dplyr)
library(tidyr)
library(tidyjson)
library(tibble)

# step 1: tidy the inbound data ####
tidy_gforce <- function(json_string) {
  tmp <- json_string %>%
    tidyjson::as.tbl_json(json.column = "raw") %>%
    tidyjson::enter_object('accel') %>%
    tidyjson::gather_array() %>%
    as.data.frame() %>%
    dplyr::rename(readings = `..JSON`) %>%
    tibble::tibble() %>%
    tidyr::unnest(readings) %>%
    tidyr::unnest(readings) %>%
    dplyr::group_by(array.index) %>%
    dplyr::mutate(col = seq_along(array.index)) %>%
    tidyr::pivot_wider(
      names_from = col,
      values_from = readings
    ) %>%
    dplyr::rename(
      x = `1`,
      y = `2`,
      z = `3`,
      sample = array.index
      ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-document.id)
  
  return(tmp)
  
}

# step 2: feature engineering ####
process_gforce <- function(df) {
  
  # determine how many rows of data there are & apply the correct milliseconds
  if(nrow(df) == 200) {
    tmp <- df %>%
      mutate(
        milliseconds = sample * 75
      )
  } else if(nrow(df) == 375) {
    tmp <- df %>%
      mutate(
        milliseconds = sample * 40
      )
  }
  
  # take absolute value
  tmp <- tmp %>%
    mutate(
      x = abs(x),
      y = abs(y),
      z = abs(z),
      smv = round(sqrt(x^2 + y^2 + z^2),2)
    )
  
  return(tmp)
  
}

# step 3A: find peaks greater than or equal to threshold ####
find_peaks <- function(df, threshold) {
  
  # identify any peak that is greater than threshold
  
  peaks <- df %>%
    select(sample, milliseconds, smv) %>%
    dplyr::mutate(
      is_peak = case_when(smv >= threshold ~ T, TRUE ~ F)
    ) %>%
    dplyr::select(-smv) %>%
    identity()
  
  # find the time gap between peaks
  
  peak_gaps <- peaks %>%
    dplyr::filter(is_peak == T) %>%
    dplyr::mutate(
      time_between_peaks = ifelse(is.na(milliseconds - dplyr::lag(milliseconds)),0,milliseconds-dplyr::lag(milliseconds))
    ) %>%
    select(-milliseconds, -is_peak)
  
  # join up with the original dataframe
  final <- df %>%
    dplyr::left_join(peaks, by = c('sample','milliseconds')) %>%
    dplyr::left_join(peak_gaps, by = c('sample'))
  
  
  return(final)
  
}

# step 3B: combine peaks that are close together based on window size threshold ####
combine_peaks <- function(df, axis = 'smv', min_time) {
  
  combo <- df %>%
    dplyr::mutate(
      keep_peak = is_peak,
      gap = time_between_peaks
    )
  
  prev_peak_index = 1
  
  for(i in combo$sample) {
    
    if(combo$is_peak[i] == T & combo$gap[i] < min_time) {
      
      if(combo[[axis]][prev_peak_index] > combo[[axis]][i]) {
        combo$keep_peak[i] = F
        combo$gap[i] = 0
      } else {
        combo$keep_peak[prev_peak_index] = F
        combo$gap[i] = combo$gap[prev_peak_index] + combo$gap[i]
        combo$gap[prev_peak_index] = 0
      }
      
    }
    
    if(combo$keep_peak[i] == T) {
      prev_peak_index = i
    }
    
  }
  
  final <- combo %>%
    dplyr::filter(keep_peak == T) %>%
    dplyr::select(sample, keep_peak, milliseconds, gap) %>%
    dplyr::mutate(
      gap = milliseconds - dplyr::lag(milliseconds),
      gap = ifelse(is.na(gap), 0, gap)
    ) %>%
    dplyr::select(-milliseconds)
  
  final <- df %>%
    dplyr::left_join(final, by = 'sample') %>%
    dplyr::mutate(
      keep_peak = ifelse(is.na(keep_peak), F, keep_peak)
    )
  
  
  return(final)
  
  
}

# step 3C: rate each period/gap ####

rate_periods <- function(df, tolerance) {
  
  # only keep the peaks of interest
  
  main_peaks <- df %>%
    dplyr::filter(keep_peak == T)
  
  # find the median
  
  median_val = stats::median(main_peaks$gap, na.rm = T)
  
  # find upper limit & lower limit
  upper_limit = median_val + (median_val * tolerance)
  lower_limit = median_val - (median_val * tolerance)
  
  # rating cal
  
  final_rating <- main_peaks %>%
    dplyr::mutate(
      rating = ifelse(gap >= lower_limit & gap <= upper_limit, 1, 0),
      rating = ifelse(gap == 0, 1, rating)
    ) %>%
    identity()
  
  # need to recalculate the gap by eliminating the keep_peaks that have 0 rating
  final_rating <- final_rating %>%
    dplyr::mutate(
      gap = case_when(
        rating == 1 & dplyr::lag(rating) == 0 ~ gap + dplyr::lag(gap),
        TRUE ~ gap
      ),
      rating = dplyr::case_when(
        gap == 0 ~ 1,
        gap > upper_limit | gap < lower_limit ~ 0,
        TRUE ~ rating
      ),
      rating = dplyr::case_when(
        rating == 1 & (dplyr::lag(rating) == 0 | is.na(dplyr::lag(rating))) & (dplyr::lead(rating) == 0 | is.na(rating)) ~ 0,
        TRUE ~ rating
      )
    )
  
  ans <- list(final_rating, median_val, upper_limit, lower_limit)
  
  return(ans)
  
}

# step 4: find the repetition motion score ####
score_motion <- function(df, gforce_threshold, time_cutoff, tolerance, penalty) {
  
  # error handling for missing inputs ####
  if(missing(gforce_threshold)) {
    
    gforce_threshold = 3.0
  }
  
  if(missing(time_cutoff)) {

    
    time_cutoff <- 350
  }
  
  if(missing(tolerance)) {

    
    tolerance <- 0.2
  }
  
  if(missing(penalty)) {

    
    penalty <- 0.5
  }

  axis <- 'smv'
  
  # Acquire Data and clean it up ####
  accel_df <- df
  
  # ensure that the maximum value for the chosen axis is equal to or below the threshold set by user
  # find the max
  max_val <- accel_df %>%
    dplyr::summarize(maxval = max(.data[[axis]], na.rm = T)) %>%
    dplyr::pull(maxval)
  
  # if max_val is below user defined threshold, throw warning and lower user value to max_val (rounded down)
  if(max_val < gforce_threshold) {
    # warning(paste0("maximum g-force value for chosen axis is below the maximum threshold", '\n', 'readjusting maximum threshold to maximum found'))
    # warning('readjusting maximum threshold to maximum found')
    
    gforce_threshold <- floor(max_val)
  }
  
  if(gforce_threshold < 1.3) {
    error_val <- 0.000
    return(error_val)
  }
  
  threshold = gforce_threshold
  best_score <- 0
  i <- 1
  
  iteration <- c()
  g_force <- c()
  peaks <- c()
  
  
  # Loop to find best g-force ####
  while(threshold >= 1.29) {
    
    # first identify all peaks above the threshold value
    df_highg <- find_peaks(accel_df, threshold = threshold)
    
    # combine peaks that are close to each other based on the time cutoff
    trim_peaks <- combine_peaks(df = df_highg, min_time = time_cutoff)
    
    # score each peak to find out if it should be kept and give it a rating of 1 or 0 to indicate the gap between last peak and current is within the upper & lower limits
    scored_peaks <- rate_periods(df = trim_peaks, tolerance = tolerance)[[1]]
    ratings <- scored_peaks %>%
      dplyr::summarize(total_rating = sum(rating)) %>%
      dplyr::pull(total_rating)
    
    # log the results and move on
    iteration[i] <- i
    g_force[i] <- threshold
    peaks[i] <- ratings
    
    i <- i + 1
    threshold = threshold - 0.1
    
  }
  
  # create a tibble based on the logged results
  score_list <- tibble::tibble(iteration = iteration, gforce = g_force, num_peaks = peaks) %>%
    dplyr::mutate(
      reps = num_peaks + 1,
      makusafe_score = ifelse(num_peaks < 2, 0, num_peaks * (gforce - penalty))
    )
  
  
  # find best score
  
  # determine the gforce that yields the highest makusafe score & classify it
  best_score <- score_list %>%
    dplyr::slice(which.max(makusafe_score)) %>%
    dplyr::mutate(
      makusafe_classification = dplyr::case_when(
        makusafe_score < 5 ~ 'Not Repetitive Motion',
        makusafe_score < 8 ~ 'Some Repetitive Motion Detected',
        makusafe_score < 12 ~ 'Repetitive Motion Detected',
        TRUE ~ 'Excessive Repetitive Motion'
      )
    )
  
  # rerun to get final output ####
  
  # rerun to get a final output dataframe that we want based on best score
  
  df_highg <- find_peaks(accel_df, threshold = best_score$gforce)
  trim_peaks <- combine_peaks(df = df_highg, min_time = time_cutoff)
  scored_peaks <- rate_periods(df = trim_peaks, tolerance = tolerance)[[1]]

  
  class = best_score$makusafe_classification
  
  
  # final list of outputs that will be returned by function if user demands it
  ans <- list(accel_df, score_list, best_score, scored_peaks)
  
  return(ans)
  
}

# create hypertuning grid ####

# hypertune the data
hypertune <- function(df2) {
  
  tune_window <- seq(from = 350, to = 500, by = 50)
  tune_tolerance <- seq(from = 0.2, to = 0.4, by = 0.1)
  tune_penalty <- seq(from = 0.5, to = 0.75, by = 0.25)
  
  tune_grid <- tidyr::crossing(tune_window, tune_tolerance, tune_penalty)
  
  # check to make sure that the highest smv value when floored is still greater than 1.3
  max_val <- floor(max(df2$smv))
  
  if(max_val < 1.3) {
    return(0)
  }
  
  
  results <- tune_grid %>%
    purrr::pmap_dfr(function(...) {
      current <- tibble(...)
      tmp <- score_motion(df = df2,
                           time_cutoff = current$tune_window,
                           tolerance = current$tune_tolerance,
                           penalty = current$tune_penalty)
      
      mk_score <- tmp[[3]]$makusafe_score
      mk_class <- tmp[[3]]$makusafe_classification
      mk_gforce <- tmp[[3]]$gforce
      
      current %>%
        dplyr::mutate(
          score = mk_score,
          gforce = mk_gforce,
          classification = mk_class
        ) %>%
        dplyr::select(tune_window, tune_tolerance, tune_penalty, score, gforce, classification)
      
      
    }
    )
  
  # find the highest score
  high_score <- results %>%
    slice(which.max(score)) %>%
    pull(score)
  
  # find the gforce
  gf <- results %>%
    slice(which.max(score)) %>%
    pull(gforce)
  
  return(high_score)
  
  
}


get_score <- function(input) {
  
  stp1 <- tidy_gforce(input)
  stp2 <- process_gforce(stp1)
  
  stp3 <- hypertune(df2 = stp2)
  
  return(stp3)
  
}

