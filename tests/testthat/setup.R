library(magrittr)
library(rgraph6)
library(stringr)

iterate_around_face <- function(face, spanning = FALSE) {
  hes <- list()
  he <- face$edge
  rot <- face$rot
  for(i in seq_len(rot)) {
    he <- he$succ
  }
  ix <- he$ix
  for(i in 1:24) {
    if(!spanning | he$cut) {
      # print(c(he$ix, he$face$ix))
      hes[[length(hes) + 1]] <- he
    }
    he <- he$succ
    if(he$ix == ix) break
  }
  hes
}

iterate_around_vertex <- function(vert, spanning = FALSE) {
  hes <- list()
  he <- vert$edge
  ix <- he$ix
  for(i in 1:24) {
    if(!spanning | he$cut) {
      # print(c(he$ix, he$face$ix))
      hes[[length(hes) + 1]] <- he
    }
    he <- he$pair$succ
    if(he$ix == ix) break
  }
  hes
}

iterate_around_tree <- function(vert, spanning = TRUE) {
  seen <- c()
  twigs <- iterate_around_vertex(vert, spanning)
  twig <- twigs[[1]]
  path <- list(twig)
  seen <- c(seen, twig$ix)
  for(i in 1:24) {
    twigs <- iterate_around_vertex(twig$pair$vert, spanning)
    twixs <- get_ixs(twigs)
    if(twig$succ$ix %in% twixs) { #try to follow the successor first
      twig <- twig$succ
    } else {
      preseen <- setdiff(twixs, twig$pair$ix) #don't turn around if unnecessary
      if(length(preseen) < 1) {
        twig <- twig$pair
      } else {
        twix <- setdiff(preseen, seen)
        id <- which(twixs == twix[1])
        if(length(id) == 1) {
          twig <- twigs[[id]]
        } else {
          return(path)
        }
      }
    }
    if(twig$ix %in% seen) {
      return(path)
    } else {
      path[[length(path) + 1]] <- twig
    }
    seen <- c(seen, twig$ix)
  }
  path
}

get_ixs <- function(x) {
  if(is.null(x) || length(x) < 1) return(NULL)
  lapply(1:length(x), function(k) {
    x[[k]]$ix
  }) %>% unlist()
}

get_vixs <- function(x) {
  lapply(1:length(x), function(k) {
    x[[k]]$vert$ix
  }) %>% unlist()
}

tet <- list(
  vertices = list(
    list(id = 1, point = c(1,1,1)),
    list(id = 2, point = c(1,-1,-1)),
    list(id = 3, point = c(-1,1,-1)),
    list(id = 4, point = c(-1,-1,1))
  ),
  ridges = list(
    list(id = 1, vertices = c(1,3)), list(id = 2, vertices = c(2,3)),
    list(id = 3, vertices = c(1,2)), list(id = 4, vertices = c(3,4)),
    list(id = 5, vertices = c(1,4)), list(id = 6, vertices = c(2,4))
  ),
  facets = list(
    list(normal = c(sqrt(3)/3,sqrt(3)/3,-sqrt(3)/3), center = c(1/3,1/3,-1/3),
         orientation = -1, vertices = c(2,1,3)),
    list(normal = c(-sqrt(3)/3,sqrt(3)/3,sqrt(3)/3), center = c(-1/3,1/3,1/3),
         orientation = 1, vertices = c(4,1,3)),
    list(normal = c(-sqrt(3)/3,-sqrt(3)/3,-sqrt(3)/3), center = c(-1/3,-1/3,-1/3),
         orientation = -1, vertices = c(4,2,3)),
    list(normal = c(sqrt(3)/3,-sqrt(3)/3,sqrt(3)/3), center = c(1/3,-1/3,1/3),
         orientation = 1, vertices = c(4,2,1))
  )
)

cube <- list(
  vertices = list(
    list(id = 1, point = c(0,0,0)),
    list(id = 2, point = c(0,0,1)),
    list(id = 3, point = c(0,1,0)),
    list(id = 4, point = c(0,1,1)),
    list(id = 5, point = c(1,0,0)),
    list(id = 6, point = c(1,0,1)),
    list(id = 7, point = c(1,1,0)),
    list(id = 8, point = c(1,1,1))
  ),
  ridges = list(
    list(id = 1, vertices = c(1,3)), list(id = 2, vertices = c(1,5)),
    list(id = 3, vertices = c(5,7)), list(id = 4, vertices = c(3,7)),
    list(id = 5, vertices = c(1,2)), list(id = 6, vertices = c(5,6)),
    list(id = 7, vertices = c(2,6)), list(id = 8, vertices = c(7,8)),
    list(id = 9, vertices = c(6,8)), list(id = 10, vertices = c(3,4)),
    list(id = 11, vertices = c(2,4)), list(id = 12, vertices = c(4,8))
  ),
  facets = list(
    list(normal = c(0,0,-1), center = c(1/2,1/2,0),
         orientation = 1, vertices = c(7,3,5,1)),
    list(normal = c(0,-1,1), center = c(1/2,0,1/2),
         orientation = -1, vertices = c(6,2,5,1)),
    list(normal = c(1,0,0), center = c(1,1/2,1/2),
         orientation = 1, vertices = c(6,7,8,5)),
    list(normal = c(-1,0,0), center = c(0,1/2,1/2),
         orientation = 1, vertices = c(4,2,3,1)),
    list(normal = c(0,1,0), center = c(1/2,1,1/2),
         orientation = -1, vertices = c(4,7,8,3)),
    list(normal = c(0,0,1), center = c(1/2,1/2,1),
         orientation = 1, vertices = c(4,6,8,2))
  )
)


tet_spanning_trees <- c('CF', 'CL', 'CM', 'CR', 'CX', 'CY', 'CU', 'C[',
                        'Cb', 'Ch', 'Ci', 'Cd', 'Ck', 'Cp', 'Cq', 'Cs') %>%
  adjacency_from_graph6()

# 3 of 384 spanning trees for cube, and for octahedron, that "go together".
cube_spanning_trees <- c("GP`GO_", "GQ`GO_", "GR_GOG") %>% adjacency_from_graph6()
oct_spanning_trees <- c("EAgo", "EAk_", "EAhG") %>% adjacency_from_graph6()



