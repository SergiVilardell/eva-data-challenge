score <- function(prediction_matrix, train_data, variable = NULL){
  if(variable == "CNT"){
    cnt_sum <- 0
    for(i in 1:ncol(prediction_matrix)){
      cnt_sum <- cnt_sum + weights_cnt[i]*sum((ifelse(u_cnt[i] <= train_data, 1, 0) - prediction_matrix[,i])^2)
    }
    return(cnt_sum)
  }
  
  if(variable == "BA"){
    ba_sum <- 0
    for(i in 1:ncol(prediction_matrix)){
      ba_sum <- ba_sum + weights_ba[i]*sum((ifelse(u_ba[i] <= train_data, 1, 0) - prediction_matrix[,i])^2)
    }
    return(ba_sum)
  }
}

