library(tidyverse)
library(gravitas)
library(philentropy)

harmony_data <- smart_meter10 %>% harmony(
  ugran = "month",
  filter_out = c("hhour", "fortnight")
) 

 data_new <- smart_meter10 %>% 
   create_gran(harmony_data[1,1]$facet_variable) %>%
   create_gran(harmony_data[1,2]$x_variable)
 0
.data_new$hour_day = as.numeric(data_new$hour_day)
#density(data_new$hour_day)
data_new$day_week = as.numeric(data_new$day_week)
#density(data_new$day_week)
# 
# data_new[-c(1:2)]  %>% 
#   pivot_wider(names_from = day_week,
#               values_from = general_supply_kwh,
#               values_fn = list(general_supply_kwh = list)) %>%
#   map(function(x) density(unlist(x)))


step1 <- data_new[-c(1:2)]  %>% 
  pivot_wider(names_from = day_week,
              values_from = general_supply_kwh,
              values_fn = list(general_supply_kwh = list))


density_extractx <- function(x)
{
  density(x)$y
}

colNms <- colnames(step1)[2:8]

step2 <- NULL

for (i in 1:length(colNms)) {
  step2[[i]] <- lapply(step1[[colNms[i]]], density_extractx)
}
#names(step2) <- d$hour_day
str(step2)


# mat1 <- matrix(1:6, nrow=2)
# mat2 <- matrix(2:7, nrow=2)
sumDiff_mat <- function (mat1, mat2) {
  sum(mat2 - mat1) 
}

compute_JSD <- function(x, y)
{
  mat <- rbind(x, y)
  return(JSD(mat))
}


rowTibb <- step1[, 1]
step3 <- rep(list(diag(nrow(rowTibb))), length(colNms))
step4 <- matrix(NA, ncol = nrow(rowTibb), nrow = length(colNms))
## Logic
# for each of the list 7 DOW
#__ find the stepped sum difference of density vector elements
for (k in 1:length(colNms)){
  
  dist <- matrix(NA, 
                 nrow = nrow(rowTibb),
                 ncol = nrow(rowTibb)) ## Matrix
  row_of_col_max <- NULL
  for(i in 1:nrow(rowTibb))
  {
    for (j in 1:nrow(rowTibb))
    {
      m1 <- step2[[k]][[i]]
      m2 <- step2[[k]][[j]]
      message(paste0("K:",k," I:",i," J:", j))
      dist[i, j] <- compute_JSD(m1, m2)
      row_of_col_max[j] <- max(dist[, j])
    }
  }
  step3[[k]] <- dist
  step4[k, ] <- row_of_col_max
}
str(step3)



########
# dim(step2)
#   nest(everything()) %>% 
#   mutate(models = lapply(data, density_extractx(data))

# compute JSD for two such coordinates of points
compute_JSD <- function(x, y)
{
  mat <- rbind(x, y)
  return(JSD(mat))
}

# compute pairwise JSD matrix for n vectors (coordinates of points)
