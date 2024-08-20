library(magrittr)
library(rgl)


#' edgelist_from_he_ds
#' Produce an extended edgelist from the information in a
#' half-edge data structure
#'
#' @param he_ds The half-edge data structure
#'
#' @return A 9 column matrix with information about the two endpoints
#' of of each edge, the labels of the endpoints, and an indication of
#' whether the edge is 'cut' or not.
#' @export
#'
#' @examples
#'
edgelist_from_he_ds <- function(he_ds) {
  pairs <- Reduce(rbind, lapply(he_ds$half_edges, function(e) {
    lf <- e$vert$ix
    rt <- e$pair$vert$ix
    if(lf < rt) {
      c(e$vert$point, e$pair$vert$point, lf, rt, e$cut)
    }
  }))
  pairs %>% `rownames<-`(NULL)
}

#' coedgelist_from_he_ds
#' Produce an extended edgelist from the information in a
#' half-edge data structure, but using the 'virtual' edges between faces
#' instead of the actual edges between vertices.
#'
#' @param he_ds The half-edge data structure
#'
#' @return A 9 column matrix with information about the two endpoints
#' of of each edge, the labels of the endpoints, and an indication of
#' whether the edge is 'cut' or not.
#' @export
#'
#' @examples
#'
coedgelist_from_he_ds <- function(he_ds) {
  pairs <- Reduce(rbind, lapply(he_ds$half_edges, function(e) {
    lf <- e$face$ix
    rt <- e$pair$face$ix
    if(lf < rt) {
      c(
        e$face$center,
        e$pair$face$center,
        lf, rt, e$cut)
    }
  }))
  pairs %>% `rownames<-`(NULL)
}

#' get_display_info
#' Process an edgelist to assemple the information into a
#' format amenible to rgl.
#'
#' @param info Either an extended edgelist or a coedgelist
#'
#' @return A list of the extracted info
#' @export
#'
#' @examples
#'
get_display_info <- function(info) {
  ct <- which(info[,9] == 1)
  fd <- which(info[,9] == 0)
  vsc <- info[ct,1:6] %>% t() %>% as.numeric() %>%
    matrix(byrow = TRUE, ncol = 3)
  vsf <- info[fd,1:6] %>% t() %>% as.numeric() %>%
    matrix(byrow = TRUE, ncol = 3)
  vs <- info[,1:6] %>% t() %>% as.numeric() %>%
    matrix(byrow = TRUE, ncol = 3)
  ts <- info[,7:8] %>% t() %>% as.numeric() %>%
    matrix(byrow = TRUE, ncol = 1)
  list(vsc = vsc, vsf = vsf, vs = vs, ts = ts)
}
