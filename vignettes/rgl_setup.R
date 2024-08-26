options(rgl.useNULL=FALSE)
suppressPackageStartupMessages(library(rgl))
options(rgl.useNULL=TRUE)
options(rgl.printRglwidget=FALSE)

#' get_display_info
#' Process an edgelist to assemple the information into a
#' format amenible to rgl.
#'
#' @param info Either an extended edgelist or a coedgelist
#'
#' @return A list of the extracted info
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
#'
display_hes <- function(hes, rc, shared = FALSE, special = FALSE) {
  open3d(windowRect = c(10, 100, 2000, 500 * rc[1]))
  view3d(20, 10, zoom = 0.85)
  par3d(font = 2, FOV = 10) #0 is isomorphic view
  material3d(color = 'white', alpha = 0.1)
  mfrow3d(rc[1], rc[2], sharedMouse = shared)
  for(i in 1:length(hes)) {
    he <- hes[[i]]
    einfo <- he %>% edgelist_from_he_ds() %>% get_display_info()
    cinfo <- he %>% coedgelist_from_he_ds() %>% get_display_info()
    next3d()   # won't advance the first time, since it is empty
    if(special) {
      segments3d(einfo$vs, col="black", lwd=1)
      segments3d(cinfo$vs, col="red", lwd=1)
    } else {
      segments3d(einfo$vsc, col="black", lwd=1)
      segments3d(einfo$vsf, col="yellow", lwd=1)
      segments3d(cinfo$vsf, col="red", lwd=1)
      segments3d(cinfo$vsc, col="skyblue", lwd=1)
    }
    text3d(einfo$vs, texts = stringr::str_c('v', einfo$ts), adj = 1.1)
    text3d(cinfo$vs, texts = stringr::str_c('f', cinfo$ts), col="red", adj = 1.1)
  }
  highlevel(integer()) # To trigger display as rglwidget
}
