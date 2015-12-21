#CLT Demo

??


demoCLM <- function(dist="norm", smp_size=5, num_smp=100, 
                    dist_mean=0, dist_sd=1, 
                    lambda=0, 
                    chi_df=0, chi_ncp=0)
  {
    par(mfrow=(c(1,2)))
    draw_means <- c()
    draw_obss  <- c()
    for(draw_nmb in 1:num_smp){
      #draw a sample
      if(dist=="norm"){
        draw <- rnorm(smp_size,dist_mean,dist_sd)
      }
      if(dist=="pois"){
        draw <- rpois(smp_size, lambda)
      }
      if(dist=="chi"){
        draw <- rchisq(smp_size, chi_df, chi_ncp)
      }
      
      #get sample's mean
        draw_obs   <- draw
        draw_mean  <- mean(draw)
        
      #put draw in compiled sample
        draw_obss   <- c(draw_obss, draw_obs)
        draw_means <- c(draw_means, draw_mean)
        
      #Draw Histograms
        hist(draw_obss, main=paste("Total Obs:",draw_nmb*smp_size))
        hist(draw_means, main=paste("Number of Means:",draw_nmb,"\nSample Size: ",smp_size))
        a <- readline("Any key (q to quit)   ")
        if(a=='q'){break()}
    }
    return(draw_means)
    par(mfrow=(c(1,1)))
}