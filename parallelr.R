# ---------------------------------------------------------------------------- #
# This script examplifies how to run parallel processes in R and compares      #
# parallel to serial performance in a simple counting scenario                 #
# Author Leon Eyrich Jessen March 2017                                         #
# https://github.com/leonjessen/ParallelR/blob/master/parallelr.R              #
# ---------------------------------------------------------------------------- #



# Clear workspace
# ------------------------------------------------------------------------------
rm(list=ls())

# Load libraries
# ------------------------------------------------------------------------------
library('foreach')
library('doParallel')
library('tidyverse')
library('ggsci')

# Define functions
# ------------------------------------------------------------------------------
i_can_count_to = function(n){
  i = 0
  while( i < n ){ i = i + 1 }
  return(i)
}

# Time serial and parallel counting
# ------------------------------------------------------------------------------

# Setup parallel processing
cores = detectCores()
cl    = makeCluster( cores - 1 )
registerDoParallel(cl)

# Set count limits and number of repeats
ns = seq(100,2500,by=100)
r  = 10000

# Prebuild output matrix
out = matrix(nrow=length(ns),ncol=3)
for( i in 1:length(ns) ){

  # Do serial counting
  start_clock = Sys.time()
  for( j in 1:r ){ i_can_count_to(n=ns[i]) }
  stop_clock  = Sys.time()
  time_a      = stop_clock - start_clock

  # Do parallel counting
  start_clock = Sys.time()
  counts      = foreach( j = 1:r ) %dopar% { i_can_count_to(n=ns[i]) }
  stop_clock  = Sys.time()
  time_b      = stop_clock - start_clock
  
  # Store results
  out[i,] = c(ns[i],time_a,time_b)
  
}
# Done, stop cluster
stopCluster(cl)

# Convert to long format tibble
d = tibble(count_to = out[,1], serial = out[,2], parallel = out[,3]) %>%
  gather(key='proc_type',value='secs',-count_to) %>% 
  mutate(proc_type = factor(proc_type))

# Plot the results
# ------------------------------------------------------------------------------
d %>% ggplot(aes(x=count_to,y=secs,colour=proc_type)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_colour_npg() +
  labs(x = paste0("count to 'x' ",r," times")) +
  theme_bw()
ggsave('parallelr_results.png')
