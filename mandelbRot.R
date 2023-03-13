#
#
#

if (!require("tidyverse", quietly = TRUE))
  install.packages("tidyverse")

# Estimate the Madelbrot Set ----

calculate_mandelbrot_set = function(
  real_lim = c(-2, 1),
  imag_lim = c(-1.2, 1.2),
  increment = 0.002,
  itts = 50
  ){
  
  reals = seq(real_lim[1], real_lim[2], increment)
  imaginaries = seq(imag_lim[1], imag_lim[2], increment)

  z_matrix <- matrix(
    NA,
    ncol=length(reals),
    nrow=length(imaginaries)
  )
  
  j <- NULL
  for(j in 1:length(imaginaries)){
    i <- NULL
    for(i in 1:length(reals)){
      #		print(j)
      z <- complex(
        real=reals[i],
        imaginary=imaginaries[j]
      )
      z0 <- z
      k <- NULL
      for(k in 1:itts){
        z <- (z ^ 2) + z0
        if(is.nan(as.numeric(Re(z)))){
          next
        }
        
        if(Re(z) > 2){
          z_matrix[j,i] <- k
          break
        }
      }
      
    }
  }
  return(z_matrix)
}


# Plot the Mandelbrot Set ----



# Logistic map function ----

calculate_logistic_map = function(
  r_min = 0,
  r_max = 4,
  r_inc = 0.01,
  x_start = 0.5,
  itr = 40,
  ignore_itr = 0
  ){
  res = NULL
  for(r in seq(r_min, r_max, by = r_inc)){
    x = x_start
    i = 1
    if(as.numeric(ignore_itr) >= 1){
      for(i in 1:ignore_itr){
        x = r * x * (1 - x)
      }
    }
    
    for(j in i:itr){
      x = r * x * (1 - x)
      res = rbind(
        res,
        data.frame(
          itr = j,
          r = r,
          x = x
        )
      )
    }
  }
  return(res)
}


# plot logistic map data ----

plot_logistic_map = function(
  lmap,
  r_range = c(0, 4),
  x_range = c(0, 1),
  click_h_min = -0.59,
  click_h_max = 0.82,
  click_v_min = 0.75,
  click_v_max = 1.26,
  zoom_per_click = 2
){
  
  click_h_diff = click_h_max - click_h_min
  click_v_diff = click_v_max - click_v_min
  for(i in 1:5){
    
    p = lmap %>% 
      filter(r >= r_range[1] & r <= r_range[2] & x >= x_range[1] & x <= x_range[2]) %>% 
      ggplot(aes(r, x, colour = itr)) +
      geom_point(size = 0.1, alpha = 0.5) +
      theme_classic()
    print(p)
    click = locator(n=1)
    
    # Want to test zero margins 
    #  +
    # theme(
    #   plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
    # )
    
    
    # Debugging
    print(paste0("x click is: ", click$x))
    print(paste0("y click is: ", click$y))
    
    click_h_propn = (click$x - click_h_min) / click_h_diff
    click_h_actual = click_h_propn * (r_range[2] - r_range[1])
    
    click_v_propn = (click$y - click_v_min) / click_v_diff
    click_v_actual = click_v_propn * (x_range[2] - x_range[1])
    
    r_range_value = r_range[2] - r_range[1]
    x_range_value = x_range[2] - x_range[1]
    
    # define new ranges for r_range and x_range by dividing the r/x_range_value
    # by four (to halve the range and take half to add +/- the clicked value...)
    
    r_range = c(
      (click_h_actual - (r_range_value / 2) / zoom_per_click),
      (click_h_actual + (r_range_value / 2) / zoom_per_click)
      )
    x_range = c(
      (click_v_actual - (x_range_value / 2) / zoom_per_click),
      (click_v_actual + (x_range_value / 2) / zoom_per_click)
      )
  }
}

