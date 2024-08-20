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

#' display_hes
#' Display half-edge data structures with rgl.
#'
#' @param hes A list of half-edge data structures
#' @param rc The number of rows and columns in the displayl r * c = length(hes)
#' @param shared Whether the rgl mouse interactions are shared or not
#' @param special a boolean indicating whether to show simplified view,
#' without cut and fold information.
#'
#' @return Nothing is returned from this function.
#' @export
#'
#' @examples
#'
display_hes <- function(hes, rc, shared = FALSE, special = FALSE) {
  open3d(windowRect = c(10, 100, 750, 350 * rc[1]))
  view3d(20, 10, zoom = 0.85)
  par3d(font = 2, FOV = 10) #0 is isomorphic view
  material3d(color = 'white', alpha = 0.1)
  mfrow3d(rc[1], rc[2], sharedMouse = shared)
  for(i in 1:length(hes)) {
    he <- hes[[i]]
    einfo <- he %>% edgelist_from_he_ds() %>% get_display_info()
    cinfo <- he %>% coedgelist_from_he_ds() %>% get_display_info()
    next3d()   # won't advance the first time, since it is empty
    bgplot3d({
      plot.new()
    })
    if(special) {
      segments3d(einfo$vs, col="black", lwd=1)
      segments3d(cinfo$vs, col="red", lwd=1)
    } else {
      segments3d(einfo$vsc, col="black", lwd=1)
      segments3d(einfo$vsf, col="yellow", lwd=1)
      segments3d(cinfo$vsf, col="red", lwd=1)
      segments3d(cinfo$vsc, col="lightblue", lwd=1)
    }
    text3d(einfo$vs, texts = str_c('v', einfo$ts), adj = 1.1)
    text3d(cinfo$vs, texts = str_c('f', cinfo$ts), col="red", adj = 1.1)
  }
  highlevel(integer()) # To trigger display as rglwidget
}

