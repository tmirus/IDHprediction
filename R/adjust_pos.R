#' Adjust measurement positions in NMR dataset
#'
#' Adjust measurements for magnetic field strength, bandwidth and number of
#' measured data points with respect to a reference dataset.
#' @param data matrix of NMR signals to be adjusted. Each row contains one spectrum.
#' @param B_ref numeric, magnetic field strength of reference
#' @param B_new numeric, magnetic field strength of new data
#' @param bw_ref numeric, bandwidth of reference
#' @param bw_new numeric, bandwidth of new data
#' @param dp_ref numeric, number of data points measured for reference
#' @param dp_new numeric, number of data points measured for new data
#' @return new_data, matrix, nrow(data) x dp_ref
#' @export

adjust_positions <- function(
  data,
  B_ref, B_new,
  bw_ref, bw_new,
  dp_ref, dp_new
) {
  # adjust spectra for differences in bandwidth and magnetic field strength
  freq_scale <- B_ref / B_new
  bw_scale <- bw_new / bw_ref
  
  # first, interpolate to get to the same number of data points
  
  if ((dp_ref / dp_new) %% 1 != 0 && (dp_new / dp_ref) %% 1 != 0)
  {
    stop("Number of datapoints needs to be a divisor or multiple of 1024.")
  }

  # for each index in new data, calculate its reference position
  positions <- round(
    seq(-((dp_new/2)-1), dp_new/2, by = 1) * bw_scale * freq_scale * (dp_ref / dp_new) + (dp_ref/2), digits = 0
  )
  
  
  pos_dup <- duplicated(positions)

  new_data <- matrix(NA, nrow = nrow(data), ncol = dp_ref)
  
  # data is spread out (more data points in reference)
  if (!any(pos_dup))
  {
    new_data[, positions[which(positions > 0 & positions < dp_ref)]] <- data[
      ,
      which(positions > 0 & positions < dp_ref)
    ]
  } else {
    # data is compressed (less data points in reference)
    for (i in 1:length(positions))
    {
      if (positions[i] <= 0 || positions[i] >= dp_ref)
        next
      if (pos_dup[i]) {
        new_data[, positions[i]] <- rowMeans(data[, which(positions == positions[i]), drop = FALSE])
      } else {
        new_data[, positions[i]] <- data[, i]
      }
    }
  }
  for (i in seq_len(nrow(new_data))) {
    if (is.na(new_data[i, 1])) {
      new_data[i,1] <- 0
    }
    if (is.na(new_data[i, ncol(new_data)])) {
      new_data[i, ncol(new_data)] <- 0
    }
    for (j in seq_len(ncol(new_data)-1)) {
      if (is.na(new_data[i, j])) {
        if (j == 1) {
            next
        }
        empty_idx <- 1
        while (is.na(new_data[i, j+empty_idx])) {
              empty_idx <- empty_idx + 1
        }
        new_data[i, (j-1):(j+empty_idx)] <- seq(new_data[i, j-1], new_data[i, j+empty_idx], length.out = empty_idx+2)
      }
    }
  }
  return(new_data)
}
