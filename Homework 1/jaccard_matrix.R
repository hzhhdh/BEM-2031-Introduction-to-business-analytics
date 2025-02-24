#' Jaccard Similarity / Distance Matrix Function
#'
#' @param d Data with a character column of individual unit cases, and a character column of items to be compared
#' @param unit_var The name of the variable of the ID's of the cases
#' @param item_var The name of the variable of the items
#' @param distance Boolean, if TRUE will return a jaccard distance instead of similarity
#' @return A sparse Matrix of similarities or distances
#' @examples 
#' 
#' w <- data.frame(id = sample(1:10, size = 100, replace = T),
#'                 item = sample(letters,size = 100, replace = T))
#' jaccard_matrix(w, 'id', 'item')
jaccard_matrix <- function(d, unit_var, item_var, distance = F) {
  require(magrittr)
  require(tidyverse)
  
  # list of all the unique unit id's
  u_unit <- sort(unique(d[[unit_var]]))
  
  # create a list of all the pairs of u_unit
  all_pairs_list <- combn(u_unit, 2, simplify = F)
  
  # for each pair, pull the items, compare them
  if(distance)
    cat('Computing Jaccard distance for all pairs...\n')
  else
    cat('Computing Jaccard similarity for all pairs...\n')
  similarity_value <- unlist(pblapply(all_pairs_list, function(r) {
    i1 <- d[which(d[[unit_var]] == r[1]),][[item_var]]
    i2 <- d[which(d[[unit_var]] == r[2]),][[item_var]]
    length(intersect(i1, i2)) / length(union(i1, i2))
  }))
  
  all_pairs <- do.call(rbind, all_pairs_list) %>% 
    `colnames<-`(c('u1', 'u2')) %>% 
    as_tibble() %>% 
    mutate(value = similarity_value) %>% 
    mutate(value = case_when(distance ~ 1 - value, T ~ value)) %>% 
    filter(value != 0)
  
  i1 <- match(all_pairs$u1, u_unit)
  i2 <- match(all_pairs$u2, u_unit)
  m <- sparseMatrix(i = i1, j = i2, 
                    x = all_pairs$value, 
                    symmetric = T, 
                    dimnames = list(u_unit, u_unit))
  
  return(m)
}


#' Jaccard Similarity / Distance Matrix Function (fast). This is a faster version that uses data.table instead of just the tidyverse.
#'
#' @param d Data with a character column of individual unit cases, and a character column of items to be compared
#' @param unit_var The name of the variable of the ID's of the cases
#' @param item_var The name of the variable of the items
#' @param distance Boolean, if TRUE will return a jaccard distance instead of similarity
#' @return A sparse Matrix of similarities or distances
#' @examples 
#' 
#' w <- data.frame(id = sample(1:10, size = 100, replace = T),
#'                 item = sample(letters,size = 100, replace = T))
#' jaccard_matrix_fast(w, 'id', 'item')
jaccard_matrix_fast <- function(d, unit_var, item_var, distance = F) {
  require(magrittr)
  require(tidyverse)
  require(data.table)
  
  # list of all the unique unit id's
  u_unit <- sort(unique(d[[unit_var]]))
  
  # for each pair, pull the items, compare them
  if(distance)
    cat('Computing Jaccard distance for all pairs...\n')
  else
    cat('Computing Jaccard similarity for all pairs...\n')
  
  d <- data.table(d)
  setnames(d, old = c(unit_var, item_var), new = c('uvar', 'ivar'))
  
  all_sims <- pblapply(unique(d$uvar), function(i1) {
    di <- d[uvar == i1][,ivar]
    d$divar <- d$ivar %in% di
    disize <- d[,.(isize = .N), by = uvar]
    isects <- d[uvar != i1,.(isect = sum(divar)), by = uvar]
    unions <- merge(isects, disize, by = 'uvar')
    unions$uisize <- length(di)
    unions[,union := isize + uisize - isect]
    unions[,sim := isect / (isize + uisize - isect)]
    setnames(unions, 'uvar', 'u2')
    unions$u1 <- i1
    return(unions)
  }) %>% rbindlist
  
  if(distance)
    all_sims$sim <- 1 - all_sims$sim
  
  i1 <- match(all_sims$u1, u_unit)
  i2 <- match(all_sims$u2, u_unit)
  m <- sparseMatrix(i = i1, j = i2, 
                    x = all_sims$sim, 
                    symmetric = F, 
                    dimnames = list(u_unit, u_unit))
  
  return(m)
}


