# rescale -1 to 1 function

# purpose: rescale input from -1 to 1
# last updated: 20171019
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

rescale_minus1to1=function(my_list) {
  my_rescaled_list={}
  for (i in 1:length(my_list)) {
    if (my_list[i]<0){
      my_rescaled_list[i]=-(my_list[i]/min(my_list))
    }
    else if (my_list[i]==0){
      my_rescaled_list[i]=0
    }
    else {
      my_rescaled_list[i]=my_list[i]/max(my_list)
    }
  }
  return(my_rescaled_list)
}
  