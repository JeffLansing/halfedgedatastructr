library(R6)
library(magrittr)

#' HalfEdge R6 Class
#'
HalfEdge <- R6Class("HalfEdge", list(
  #' @field ix Index of this half-edge.
  ix = NULL,
  #' @field dir Direction this half-edge points
  dir = NULL,
  #' @field vert Vertex at the end of the half-edge
  vert = NULL,
  #' @field pair Oppositely oriented adjacent half-edge
  pair = NULL,
  #' @field face Face that the half-edge borders
  face = NULL,
  #' @field succ Next half-edge around the face
  succ = NULL,
  #' @field cut Whether this half-edge is cut or not
  cut = FALSE,
  #' print
  #' render an identifier for this object
  #' @param ...
  #'
  #' @return invisible
  #' @export
  #'
  print = function(...) {
    cat(str_c("e", self$vert$ix, ".", self$succ$vert$ix), "\n", sep = "")
    invisible(self)
  }
))

#' Vertex R6 Class
Vertex <- R6Class("Vertex", list(
  #' @field ix Index of this vertex.
  ix = NULL,
  #' @field point The spatial location of this vertex: x,y,z coordinates.
  point = NULL,
  #' @field edge The half-edge that starts at this vertex.
  edge = NULL,
  #' print
  #' render an identifier for this object
  #' @param ...
  #'
  #' @return invisible
  #' @export
  #'
  print = function(...) {
    cat(str_c("v", self$ix), "\n", sep = "")
    invisible(self)
  }
))

#' Face R6 Class
Face <- R6Class("Face", list(
  #' @field ix Index of this face.
  ix = NULL,
  #' @field rot Rotation of this face:
  #' 0 = 0 = 2*pi, 1 = pi/2, 2 = pi, 3 = 3*pi/2.
  rot = 0,
  #' @field normal The normal to this face: x,y,z coordinates.
  normal = NULL,
  #' @field edge The starting edge for traversing the perimeter of this face.
  edge = NULL,
  #' print
  #' render an identifier for this object
  #' @param ...
  #'
  #' @return invisible
  #' @export
  #'
  print = function(...) {
    cat(str_c("f", self$ix), "\n", sep = "")
    invisible(self)
  }
))

#' HalfEdgeDataStructure R6 Class
#' @import magrittr
#' @importFrom pracma cross
#'
HalfEdgeDataStructure <- R6Class("HalfEdgeDataStructure",
   private = list(
     .faces = NULL,
     .vertices = NULL,
     .half_edges = NULL,
     .he_index = NULL,
     .he_face_index = NULL
   ),
   public = list(
     #' @field BIG A constant used to encode a half-edge as an integer
     BIG = 100,

    #' rotate
    #' rotate normal vector onto axis plane
    #'
    #' If you have a plane, you have a normal vector and an origin.
    #' Let's call your plane's normal vector the new z axis.
    #' You can generate the new y axis by crossing the old x axis
    #' with the new z axis (your plane's normal).
    #' Generate the new x axis by crossing the new z with the new y.
    #' Make all your new axis vectors into unit vectors (length 1).
    #' For every point you have, create a vector that goes from your new origin
    #' to the point (vector subtraction of point - plane_origin).
    #' Just dot with the new x and new y unit vectors and you get a pair (x,y).
    #'
    #' @param points The points to rotate
    #' @param normal The normal to a face
    #' @param around The center of the face
    #'
    #' @return The rotated points
    #'

   rotate = function(points, normal, around) {
     initial <- c(1,0,0)
     if(identical(abs(normal), initial)) {
       initial <- c(0,1,0)
     }
     old_x_axis = initial
     z_axis = normal
     y_axis = pracma::cross(old_x_axis, z_axis)
     x_axis = pracma::cross(z_axis, y_axis)
     axis = rbind(x_axis, y_axis, z_axis)
     sweep(points, 2, around) %*% t(axis)
   },

    #' get_angle
    #'
    #' @param begin A beginning point (x,y vector)
    #' @param finish An ending point (x,y vector,
    #'
    #' @return an angle in radians from the x-axis
    #'

    get_angle = function(begin, finish) {
      deltaX = finish[1] - begin[1]
      deltaY = finish[2] - begin[2]
      ang <- atan2(deltaY,deltaX) #counter-clockwise
      ang
    },

    #' adjmat_to_edgelist
    #'
    #' Convert an adjacency matrix to an edgelist
    #'
    #' @param adjmat An adjacency matrix for a graph
    #'
    #' @return The corresponding edgelist for the same graph
    #'
    #'
    adjmat_to_edgelist = function(adjmat) {
    dim <- nrow(adjmat)
    edgelist <- matrix(0, dim - 1, 2)
    k <- 1
    for ( i in 1:dim){
      for ( j in i:dim) {
        if(adjmat[i,j] == 0) next
        edgelist[k,] <- c(i, j) %>% as.numeric()
        k <-  k + 1
     }
    }
    edgelist
    },

    #' initialize
    #'
    #' Initialize a half-edge data structure using information
    #' in a "hull" data structure.
    #'
    #' @param hull A data structure which is compatible with
    #' the output of the cxhull::cxhull() function.
    #'
    #' @return An initalized half-edge data structure
    #' @export
    #'
    #' @examples
    #' tet <- list(
    #'   vertices = list(
    #'     list(id = 1, point = c(1,1,1)),
    #'     list(id = 2, point = c(1,-1,-1)),
    #'     list(id = 3, point = c(-1,1,-1)),
    #'     list(id = 4, point = c(-1,-1,1))
    #'   ),
    #'   ridges = list(
    #'     list(id = 1, vertices = c(1,3)), list(id = 2, vertices = c(2,3)),
    #'     list(id = 3, vertices = c(1,2)), list(id = 4, vertices = c(3,4)),
    #'     list(id = 5, vertices = c(1,4)), list(id = 6, vertices = c(2,4))
    #'   ),
    #'   facets = list(
    #'     list(id = 1, normal = c(sqrt(3)/3,sqrt(3)/3,-sqrt(3)/3), center = c(1/3,1/3,-1/3),
    #'          orientation = -1, vertices = c(2,1,3)),
    #'     list(id = 2, normal = c(-sqrt(3)/3,sqrt(3)/3,sqrt(3)/3), center = c(-1/3,1/3,1/3),
    #'          orientation = 1, vertices = c(4,1,3)),
    #'     list(id = 3, normal = c(-sqrt(3)/3,-sqrt(3)/3,-sqrt(3)/3), center = c(-1/3,-1/3,-1/3),
    #'          orientation = -1, vertices = c(4,2,3)),
    #'     list(id = 4, normal = c(sqrt(3)/3,-sqrt(3)/3,sqrt(3)/3), center = c(1/3,-1/3,1/3),
    #'          orientation = 1, vertices = c(4,2,1))
    #'   )
    #' )
    #' he_ds <- HalfEdgeDataStructure$new(tet)
    #'
    initialize = function(hull) {
      NVS <- length(hull$vertices)
      NRS <- length(hull$ridges)
      NFS <- length(hull$facets)

      private$.vertices = lapply(1:NVS, function(k) {
        vert <- Vertex$new()
        vert$ix <- k
        vert$point <- hull$vertices[[k]]$point
        vert
      })

      private$.half_edges = list()
      private$.he_index = matrix(NA, NVS, NVS) # index of he's by vertex id
      for(k in 1:NRS) {
        vix <- hull$ridges[[k]]$vertices
        v <- vix[1]
        s <- vix[2]
        vs <- HalfEdge$new()
        vs$ix <- self$BIG * v + s
        vs$vert <- private$.vertices[[v]]
        private$.vertices[[v]]$edge <- vs
        sv <- HalfEdge$new()
        sv$ix <- self$BIG * s + v
        sv$vert <- private$.vertices[[s]]
        private$.vertices[[s]]$edge <- sv
        vs$pair <- sv # create pairing
        sv$pair <- vs # create pairing
        len <- length(private$.half_edges) + 1
        private$.half_edges[[len]] <- vs
        private$.half_edges[[len + 1]] <- sv
        private$.he_index[v,s] <- len
        private$.he_index[s,v] <- len + 1
      }

      private$.faces = lapply(1:NFS, function(k) {
        face <- Face$new()
        face$ix <- k
        facet <- hull$facets[[k]]
        Face$normal <- facet$normal
        NFVS <- length(facet$vertices)
        points <- Reduce(rbind,lapply(1:NFVS, function(k) {
          hull$vertices[[facet$vertices[k]]]$point
        }))
        rotated <- self$rotate(points, facet$normal, facet$center) %>% zapsmall()
        angles <- c()
        center <- colMeans(rotated)
        for(i in 1:NFVS) {
          point <- rotated[i,]
          angle <- self$get_angle(center, point)
          angles <- c(angles, angle)
        }
        ixs <- facet$vertices[order(angles)]
        jxs <- c(tail(ixs, -1), ixs[1]) #rotate clockwise 90
        kxs <- c(tail(jxs, -1), jxs[1])
        hes <- list()
        for(i in 1:NFVS) {
          j <- jxs[i]
          k <- kxs[i]
          eix <- private$.he_index[j,k]
          he <- private$.half_edges[[eix]]
          he$dir <- i
          he$face <- face
          len <- length(hes) + 1
          hes[[len]] <- he
          if(len > 1) {
            hes[[len - 1]]$succ <- he
          }
        }
        hes[[NFVS]]$succ <- hes[[1]]
        face$edge <- hes[[1]]
        face
      })

      private$.he_face_index = matrix(NA, NFS, NFS) # index of he's by face id
      for(i in 1:length(private$.half_edges)) {
        he <- private$.half_edges[[i]]
        other_face <- he$pair$face
        if(is.null(other_face)) {
          warning(sprintf("Other face is NULL for half-edge %d", he$ix))
          next
        }
        private$.he_face_index[he$face$ix,other_face$ix] <- i
      }
    }
   ),
    active = list(
      #' @field faces
      #' A list of faces of the input hull.
      #'
      faces = function(value) {
        if (missing(value)) {
          private$.faces
        } else {
          stop("'$faces' is read only", call. = FALSE)
        }
      },
      #' @field vertices
      #' A list of vertices of the input hull.
      #'
      vertices = function(value) {
        if (missing(value)) {
          private$.vertices
        } else {
          stop("'$vertices' is read only", call. = FALSE)
        }
      },
      #' @field half_edges
      #' A list of half_edges of the input hull.
      #'
      half_edges = function(value) {
        if (missing(value)) {
          private$.half_edges
        } else {
          stop("'$half_edges' is read only", call. = FALSE)
        }
      },
      #' @field he_index
      #' An index of the half-edges by pairs of vertex ids (as a matrix).
      #'
      he_index = function(value) {
        if (missing(value)) {
          private$.he_index
        } else {
          stop("'$he_index' is read only", call. = FALSE)
        }
      },
      #' @field he_face_index
      #' An index of the half-edges by pairs of face ids (as a matrix).
      #'
      he_face_index = function(value) {
        if (missing(value)) {
          private$.he_face_index
        } else {
          stop("'$he_face_index' is read only", call. = FALSE)
        }
      },
      #' @field cut
      #' Requires a spanning tree of the hull in adjacency list form.
      #'
      cut = function(tree) {
        if (missing(tree)) {
          stop("argument '$cut' is missing, with no default", call. = FALSE)
        }
        elist <- tree %>% self$adjmat_to_edgelist()
        for(i in 1:nrow(elist)) {
          edge <- elist[i,]
          heix <- private$.he_index[edge[1], edge[2]]
          he <- private$.half_edges[[heix]]
          he$pair$cut <- TRUE
          he$cut <- TRUE
        }
      },
      #' @field fold
      #' Requires a spanning tree of the dual of hull in adjacency list form.
      #'
      fold = function(tree) {
        if (missing(tree)) {
          stop("argument '$fold' is missing, with no default", call. = FALSE)
        }
        elist <- tree %>% self$adjmat_to_edgelist()
        for(i in 1:length(private$.half_edges)) {
          private$.half_edges[[i]]$cut <- TRUE
        }
        for(i in 1:nrow(elist)) {
          edge <- elist[i,]
          heix <- private$.he_face_index[edge[1], edge[2]]
          he <- private$.half_edges[[heix]]
          he$pair$cut <- FALSE
          he$cut <- FALSE
        }
      }
    )
)


