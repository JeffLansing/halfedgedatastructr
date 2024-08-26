library(magrittr)

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
#' edgelist_from_he_ds(list(half_edges = c()))
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
#' coedgelist_from_he_ds(list(half_edges = c()))
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

#' edgelist_to_adjmat
#' Convert an edgelist to an adjacency matrix.
#' The edgelist is treated as an index array for the
#' desired matrix.
#'
#' @param edgelist An n X 2 matrix of edges of a graph
#' @param dim Optional help for deciding the dimension of
#' the result; defaults to the largest value in the edgelist
#'
#' @return The adjacency matrix of the graph
#' @export
#'
#' @examples
#' elist <- rbind(c(1,4), c(2,4), c(3,4))
#' edgelist_to_adjmat(elist)
#'
edgelist_to_adjmat <- function(edgelist, dim = NULL) {
  if(is.null(dim)) {
    dim <- max(edgelist)
  }
  mx <- matrix(0,dim,dim)
  mx[edgelist] <- 1
  mx[edgelist[,c(2,1)]] <- 1
  mx
}

#' adjmats_from_info
#'
#' @param info Either an edgeinfo or a coedgeinfo data structure
#'
#' @return Two adjacency matrices that are extracted from the info
#' structure, one for the cut edges and one for the fold edges.
#' @export
#'
#' @examples
#' info <- rbind(c(0,0,0,1,1,1,1,2,1),c(1,1,1,0,0,0,2,1,0))
#' adjmats_from_info(info)
#'
adjmats_from_info <- function(info) {
  ct <- which(info[,9] == 1)
  fd <- which(info[,9] == 0)
  vsc <- info[ct,7:8] %>% t() %>% as.numeric() %>%
    matrix(byrow = TRUE, ncol = 2)
  vsf <- info[fd,7:8] %>% t() %>% as.numeric() %>%
    matrix(byrow = TRUE, ncol = 2)
  amc <- vsc %>% edgelist_to_adjmat()
  amf <- vsf %>% edgelist_to_adjmat()
  list(amc = amc, amf = amf)
}

#' hull_plus_g6
#' Treat the half-edge data structure as an arithmetic operator on
#' the indices of the spanning trees
#'
#' @param g6 A spanning tree in g6 encoding
#' @param hull A hull structure for a polyhedron
#'
#' @return Vector of 4 adjacency matrices in g6 encoding.
#' Only 2 of them will be trees, and one of those trees will be
#' identical to the input tree.
#' @export
#'
hull_plus_g6 <- function(g6, hull) {
  am <- rgraph6::adjacency_from_graph6(g6)[[1]]
  he <- HalfEdgeDataStructure$new(hull)
  # Match the sizes of the 2 input objects, and apply cut if
  # they are the same, otherwise apply fold
  if(nrow(am) == length(he$vertices)) {
    he$cut <- am
  } else {
    he$fold <- am
  }
  # Return the results as a vector of named graph6 strings
  el <- edgelist_from_he_ds(he) %>% adjmats_from_info() %>%
    rgraph6::as_graph6()
  coel <- coedgelist_from_he_ds(he) %>% adjmats_from_info() %>%
    rgraph6::as_graph6()
  c(el = el, coel = coel)
}




