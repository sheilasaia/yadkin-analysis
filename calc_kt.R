# calculate log-Person type III frequency factor (kt)

# purpose: for flood frequency and low-flow frequency analysis of streamflow data
# last updated: 20170912
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

calc_kt=function(my_probability_list,cskew) {
  # my_probability_list ranges from 0 to 1
  # represents exceedance or non-exceedance probability (depending on whether you're running flood or low-flow frequency analysis)
  # example: my_probability_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
  # cskew represents the coefficient of skew for data of interest
  
  # find w term
  w_term1=1/(my_probability_list^2)
  w=sqrt(log(w_term1))
  
  # find standard normal z statistic
  z_term1=2.515517+0.802853*w+0.010328*(w^2)
  z_term2=1+1.432788*w+0.189269*(w^2)+0.001308*(w^3)
  z=w-(z_term1/z_term2)
  
  # find kt
  k=cskew/6
  kt_term1=((z^2)-1)*k
  kt_term2=(1/3)*((z^3)-6*z)*(k^2)
  kt_term3=((z^2)-1)*(k^3)
  kt_term4=z*(k^4)
  kt_term5=(1/3)*(k^5)
  kt=z+kt_term1+kt_term2-kt_term3+kt_term4+kt_term5
  
  return(kt)
}