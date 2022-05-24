#' align spectra
#'
#' align all spectra in a data set to the highest peak in a region.
#' In this package, that is used to align all spectra to the choline peak.
#' @param spectra matrix containing NMR spectra in rows
#' @param region numeric vector containing indices of a consecutive region of features, e.g. 1:100
#' @param ref_pos optional, numeric, the new index of the highest peak in the specified region.
#' If not supplied, the minimum index of the peaks in all samples in the set is used.
#' @return matrix containing the shifted spectra
#' @export

align_spectra <- function(spectra, region, ref_pos = NULL){
    # align spectra in a given matrix to the maximum signal in a given region

    # find maximum signal in the given region
    reference_positions <- apply(spectra, 1, function(x, positions = region){
        (positions[1]-1)+which.max(x[positions])
    })

    if (is.null(ref_pos))
    {
      # choose the minimum as new position and calculate shifts
      ref_pos <- min(reference_positions)
    }
    shifts <- reference_positions - ref_pos

    # new matrix containing the shifted spectra
    shifted_spec <- matrix(0, nrow = nrow(spectra), ncol = ncol(spectra) - max(shifts))
    default_pos <- max(shifts) + 1

    for(i in 1:nrow(shifted_spec)){
        start = shifts[i] + 1
        end = start + ncol(shifted_spec) - 1

        shifted_spec[i,] <- spectra[i, start:end]
    }
    rownames(shifted_spec) <- rownames(spectra)
    colnames(shifted_spec) <- 1:ncol(shifted_spec)
    return(shifted_spec)
}
