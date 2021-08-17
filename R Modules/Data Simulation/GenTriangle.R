#TRIANGLE GENERATING FUNCTION
#GenTriangle function takes the seed as input. It assumes the claim environment has already been loaded. 


GenTriangle = function(seed, runoff){

  set.seed(as.integer(seed))
  
  n_vector <- claim_frequency(I, E, lambda)
  occurrence_times <- claim_occurrence(n_vector)
  # Module 2: Claim size
  claim_sizes <- claim_size(n_vector, S_df, type = "p", range = c(0, 1e24))
  # Module 3: Claim notification
  notidel <- claim_notification(n_vector, claim_sizes, paramfun = notidel_param)
  # Module 4: Claim settlement
  setldel <- claim_closure(n_vector, claim_sizes, paramfun = setldel_param)
  # Module 5: Claim payment count
  no_payments <- claim_payment_no(n_vector, claim_sizes, rfun = rmixed_payment_no,
                                  claim_size_benchmark_1 = 0.0375 * ref_claim,
                                  claim_size_benchmark_2 = 0.075 * ref_claim)
  # Module 6: Claim payment size
  payment_sizes <- claim_payment_size(n_vector, claim_sizes, no_payments, 
                                      rfun = rmixed_payment_size)
  # Module 7: Claim payment time
  payment_delays <- claim_payment_delay(n_vector, claim_sizes, no_payments, setldel,
                                        rfun = r_pmtdel, paramfun = param_pmtdel,
                                        occurrence_period = rep(1:I, times = n_vector))
  payment_times <- claim_payment_time(n_vector, occurrence_times, notidel, payment_delays)
  # Module 8: Claim inflation
  payment_inflated <- claim_payment_inflation(
    n_vector, payment_sizes, payment_times, occurrence_times,
    claim_sizes, base_inflation_vector, SI_occurrence, SI_payment)
  
  transaction_dataset <- generate_transaction_dataset(
    claims(
      frequency_vector = n_vector,
      occurrence_list = occurrence_times,
      claim_size_list = claim_sizes,
      notification_list = notidel,
      settlement_list = setldel,
      no_payments_list = no_payments,
      payment_size_list = payment_sizes,
      payment_delay_list = payment_delays,
      payment_time_list = payment_times,
      payment_inflated_list = payment_inflated),
    # adjust = FALSE to retain the original simulated times
    adjust = FALSE)
  
  incremental <- matrix(0,nrow = 40, ncol = 40)
  
  for (i in 1:40){
    for (j in 1:40){
      tr1 <- transaction_dataset[transaction_dataset$occurrence_period == i,]
      if (j == 40 && runoff == 1){
        tr2 <- tr1[tr1$payment_period >= j + i - 1,]
      } else {
        tr2 <- tr1[tr1$payment_period == j + i - 1,]
      }
      
      incremental[i,j] = sum(tr2$payment_inflated)
    }
    
  }
  
  
  
  
  incremental <- as.data.table(incremental)
  
  
  
  Data = copy(incremental)
  Data = as.data.frame(Data)
  AQ <- c(1:40)
  Data <- cbind(AQ, Data)
  colnames(Data)<- c("AQ",1:40)
  return(Data)
}


