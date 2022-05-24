#' binning of features
#'
#' Take a matrix and reduce the number of features by using bins
#' @param spectra matrix containing NMR spectra in rows
#' @param bin_size numeric, number of features to combine in one bin (by averaging)
#' @return binned_data
#' @export

bin_spectra <- function(spectra, bin_size){
    # binning of features in a matrix
    n_bin <- ncol(spectra) %/% bin_size
    rest <- ncol(spectra) %% bin_size

    if(rest > 0){
        n_bin <- n_bin + 1
    }

    binned_data <- matrix(0, ncol = n_bin, nrow = nrow(spectra))
    rownames(binned_data) <- rownames(spectra)
    colnames(binned_data) <- 1:ncol(binned_data)

    for(i in 1:n_bin){
        binned_data[, i] <- rowMeans(
            spectra[, ((i-1)*bin_size+1):min(i*bin_size, ncol(spectra)), drop = FALSE]
        )
    }
    return(binned_data)
}
