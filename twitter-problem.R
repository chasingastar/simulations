library(dplyr)

# GLOBAL variables

STRING_LENGTH <- 100
ITERATIONS <- 100000


#' Title
#'
#' @param n 
#'
#' @return a string of length n, consisting of H and T only
#' @export
#'
#' @examples
generate_string <- function(n) {
  # Generate a random sequence of 'H's and 'T's
  symbols <- c('H', 'T')
  string <- paste(sample(symbols, n, replace = TRUE), collapse = '')
  return(string)
}


#' Title
#'
#' @param k 
#'
#' @return
#' @export
#'
#' @examples
generate_df <- function(k){
  df <- data.frame(
    UID = numeric(0),
    main_string = character(0)
  ) 
  for(i in 1:k)
  {
    temp <- data.frame(
        UID = i,
        main_string = generate_string(STRING_LENGTH)
    )
    df <- rbind(df,temp)
  }
return(df)  
}





count_HH <- function(main_str){
  tot <- 0
  #print(main_str)
  for (i in 1:(nchar(main_str)-1))
  {
    #print(paste(i,substr(main_str,i,i+1)))
   if(substr(main_str,i,i+1) == "HH")
   {
     tot <- tot + 1
   }
  }
  return(tot)
}

count_HT <- function(main_str){
  tot <- 0
  #print(main_str)
  for (i in 1:(nchar(main_str)-1))
  {
    #print(paste(i,substr(main_str,i,i+1)))
    if(substr(main_str,i,i+1) == "HT")
    {
      tot <- tot + 1
    }
  }
  return(tot)
}


print(Sys.time())
main_df <- generate_df(ITERATIONS)

main_df <- main_df %>% 
  mutate(HH_score = sapply(main_string, count_HH)) # apply the count_HH functions to each instance of main_string in the dataframe

main_df <- main_df %>% 
  mutate(HT_score = sapply(main_string, count_HT)) # apply the count_HH functions to each instance of main_string in the dataframe

main_df <- main_df %>% 
  mutate(HH_win = ifelse(HH_score> HT_score,1,0))

main_df <- main_df %>% 
  mutate(HT_win = ifelse(HT_score> HH_score,1,0))

main_df <- main_df %>%
  mutate(DRAW = ifelse(HH_score== HT_score,1,0))

main_df <- main_df %>%
  mutate(delta = HT_score-HH_score)

results_df <- main_df %>% summarize(HT = sum(HT_win), HH = sum(HH_win), DRAW = sum(DRAW) )
print(Sys.time())

results_by_difference <- main_df %>% group_by(delta) %>% summarize(freq = n())

# Create bar plot using ggplot2
distribution_graph  = ggplot(results_by_difference, aes(x = delta, y = freq)) +
          geom_bar(stat = "identity", fill = "skyblue") +
          labs(x = "Delta", y = "Frequency", title = "Bar Chart of (HT_score - HH_score) vs Frequency")

print(distribution_graph)
