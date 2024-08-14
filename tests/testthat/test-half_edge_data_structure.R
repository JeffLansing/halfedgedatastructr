test_that("initialize works", {
  he_ds <- HalfEdgeDataStructure$new(tet)
  vert <- he_ds$vertices[[1]]
  ixs <- iterate_around_vertex(vert) %>% get_ixs()
  expect_equal(ixs, c(104, 102, 103))
})

test_that("cut works", {
  he_ds <- HalfEdgeDataStructure$new(tet)
  he_ds$cut <- tet_spanning_trees[[1]]
  vert <- he_ds$vertices[[1]]
  vixs <- iterate_around_tree(vert) %>% get_vixs()
  expect_equal(vixs, c(1, 4, 3, 4, 2, 4))
})

test_that("fold works", {
  he_ds <- HalfEdgeDataStructure$new(tet)
  he_ds$fold <- tet_spanning_trees[[1]]
  vert <- he_ds$vertices[[1]]
  vixs <- iterate_around_tree(vert) %>% get_vixs()
  expect_equal(vixs, c(1, 3, 2, 3, 4, 3))
})

test_that("face index works", {
  he_ds <- HalfEdgeDataStructure$new(tet)
  hes <- he_ds$half_edges
  ixs <- c()
  for(i in 1:length(hes)) {
    he <- hes[[i]]
    fa <- he$face$ix
    fb <- he$pair$face$ix
    ix <- he_ds$he_face_index[fa, fb]
    ixs <- c(ixs, ix)
  }
  expect_equal(ixs, 1:length(hes))
})

test_that("cut works for cube", {
  he_ds <- HalfEdgeDataStructure$new(cube)
  he_ds$cut <- cube_spanning_trees[[1]]
  vert <- he_ds$vertices[[8]]
  vixs <- iterate_around_tree(vert) %>% get_vixs()
  expect_equal(vixs, c(8, 4, 3, 1, 5, 7, 5, 6, 2, 6, 5, 1, 3, 4))
})

test_that("fold works for cube", {
  he_ds <- HalfEdgeDataStructure$new(cube)
  he_ds$fold <- oct_spanning_trees[[1]]
  vert <- he_ds$vertices[[8]]
  vixs <- iterate_around_tree(vert) %>% get_vixs()
  expect_equal(vixs, c(8, 4, 3, 1, 5, 7, 5, 6, 2, 6, 5, 1, 3, 4))
})

test_that("face center has content", {
  he_ds <- HalfEdgeDataStructure$new(cube)
  tbl <- Reduce(rbind, lapply(1:6, function(k) {
    face <- he_ds$faces[[k]]
    face$center
  }))
  expect_equal(ncol(tbl), 3)
})

test_that("iterate_around_face works", {
  he_ds <- HalfEdgeDataStructure$new(cube)
  tbl <- Reduce(rbind, lapply(1:6, function(k) {
    face <- he_ds$faces[[k]]
    iterate_around_face(face) %>% get_vixs()
  })) %>% `rownames<-`(c(1:6))
  expect_equal(colSums(tbl), c(37, 37, 17, 17))
})

test_that("cut and fold are symmetric", {
  paths <- lapply(1:16, function(k) {
    he_ds1 <- HalfEdgeDataStructure$new(tet)
    he_ds1$cut <- tet_spanning_trees[[k]]
    vert <- he_ds1$vertices[[1]]
    iterate_around_tree(vert) %>% get_vixs() %>%
      str_c(collapse = '') %>% as.numeric()
  }) %>% unlist()

  paths2 <- lapply(1:16, function(k) {
    he_ds1 <- HalfEdgeDataStructure$new(tet)
    he_ds1$fold <- tet_spanning_trees[[k]]
    vert <- he_ds1$vertices[[1]]
    iterate_around_tree(vert) %>% get_vixs() %>%
      str_c(collapse = '') %>% as.numeric()
  }) %>% unlist()

  map <- lapply(1:16, function(k) {which(paths2 == paths[k])}) %>% unlist()
  expect_equal(map, c(16, 12, 13, 7, 1, 3, 8, 2, 15, 9, 11, 14, 10, 4, 6, 5))
})



