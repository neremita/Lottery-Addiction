---
  title: "Mission 649 Solutions"
---
  # Developing A Mobile App For Alleviating Lottery Addiction
'''  
  The RMarkdown file is intended to build a understanding of what is to go in our lottery addiction appilcation.
'''
  # Core Functions

factorial <- function(n) {
  final_product <- 1
  for (n in 1:n) {
    final_product = n * final_product
  }
  return(final_product)
}
permutation <- function(n, k) {
  #n has to be larger than k
  if (n >= k) {
  return(factorial(n)/factorial(n-k))
  } else {
    print('n is less than k')
  }
}
combination <- function(n, k) {
  if (n >= k) {
    return(permutation(n, k)/factorial(k))
  } else {
    print('n is less than k')
  }
}
'''
  #One-ticket Probability

'''
one_ticket_probability <- function(x) {
  total_combos <- combination(49, 6)
  if (typeof(x) == 'double' & length(x) == 6) {
    for (i in length(x)) {
      if (duplicated(x)[[i]] == TRUE) {
        return("You have a 0.00% change of winning big")
      } else {
        prob <- 1 / total_combos * 100
        pretty_prob <- sprintf("%1.9f", prob)
        return(paste("You have a ", pretty_prob, "% chance of winning big.", sep = ""))
      }
    }
  } else {
    return('Error: wrong type or length')
  }
}

  
a <- c(6, 7, 8, 9 , 10, 11)
one_ticket_probability(a)

'''
  We created a function to determine the probability of winning the big 6/49 prize with a single outcome.


  #Historical Data Check for Canada Lottery
'''
library(readr)

df <- read_csv("~/Google Drive/Work/R programming/Guided Projects/Lottery Addiction/649.csv")

print(dim(df))
head(df, 3)
tail(df, 3)

  #A New Data Structure

data1 <- c(1, 3, 5)
data2 <- c(2, 4, 6)
data3 <- c(8, 9, 7)

unnamed_list <- list(data1, data2, data3)
first_vector <- unnamed_list[[1]]
named_list <- list(first = data1, second = data2, third = data3)
first_item_sum <- sum(named_list$first)

'''

  #Using pmap
  
'''
library(purrr)

data_list <- list(data1, data2, data3)

averages <- pmap(data_list, function(x, y, z) {(x+y+z)/3})
first_average <- unlist(averages[1])                 
                 
'''

  #Function for Historical Data
  
'''
library(dplyr)

  #create a seperate df for the winning combos
winning_numbers <- df %>%
  select('NUMBER DRAWN 1',
         'NUMBER DRAWN 2',
         'NUMBER DRAWN 3',
         'NUMBER DRAWN 4',
         'NUMBER DRAWN 5',
         'NUMBER DRAWN 6')

  #delete column names so pmap can use new df
colnames(winning_numbers)  <- NULL

list <- pmap(winning_numbers, function(x,y,z,a,b,c) {c(x,y,z,a,b,c)})

check_historical_occurence <- function(test, historical) {
  vector <- c()
  for (i in 1:length(list)) {
    vector <- c(vector, setequal(a, list[[i]]))
  }
  return(paste("Your combination has appeared ", sum(vector), " time(s) in the past. ", one_ticket_probability(test), sep = ""))
}
a <- c(1,2,3,4,5,6)
check_historical_occurence(a, list)
'''
  We created a function that matches an inputted vector of 6, unique numbers between 1 and 49 to any winning combinations in the past, along with the probability of winning  the grand prize.
'''

'''

  #Multi-ticket Probability
  
'''

multi_ticket_probability <- function(x) {
  total_combos <- combination(49, 6)
  if (x > 0) {
    prob <- x / total_combos * 100
    pretty_prob <- sprintf("%1.9f", prob)
    return(paste("You have a ", pretty_prob, "% chance of winning big with ", x, " ticket(s).", sep = ""))
  } else {
    return("You can't play without at ticket.")
  }
}

multi_ticket_probability(13983816)

'''
  We created this function to simulate the chance of winning the grand prize per number of tickets purchased.
'''

'''
  #Less Winning Numbers - Function
'''

probability_less_6 <- function(x) {
  success_outcomes <- combination(6, x) * combination(49 - x, 6 - x)
  total_combos <- combination(49, 6)
  if (x >= 3 & x<= 5) {
    prob <- success_outcomes / total_combos * 100
    pretty_prob <- sprintf("%1.9f", prob)
    return(paste("You have a ", pretty_prob, "% chance of winning a smaller prize from ", x, " numbers.", sep = ""))
  } else {
    return("Error: The input must be between 3 and 5")
  }
}

probability_less_6(5)

'''
  We created this function to simulate the chance of winning a smaller prize based off of 3, 4, and 5 inputs.
'''



