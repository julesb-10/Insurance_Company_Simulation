
#PART a)
a0 = 25000 #Initial Capital
a_t = c(a0)
A = c() #Initialize vector of Bernoullis indicating whether capital stays positive throughout all 365 days
J = c() #Initializing J vector
R = c() #Time until first ruin vector
for(j in 1:1000){ #Using 1000 simulations to estimate P(A)
  for(i in 2:366){ #index starting at 2 to be able to add to a_t vector without overwriting initial capital
    
    num_claims = floor(rexp(1, rate = 1/10)) #claims on day i, using floor since number claims has to be an integer
    daily_claim_amounts = rexp(num_claims, rate = 1/1000) # amount of each claim made on day i
    total_daily_claim = sum(daily_claim_amounts) #total in claims made on day i
    net_change_in_capital = 11000 - total_daily_claim #How much company loses or makes on day i
    a_t[i] = a_t[i-1] + net_change_in_capital 
  }
  for(k in 1:length(a_t)){  # Loop to update R vector indicating time until ruin IF ruin occurs
    if(a_t[k]<0){
      R[j] = k
      break
    }
  }
  A[j] = all(a_t > 0) + 0 #Adding to vector of Bernoullis indicating of capital stays positive throughout the year
}
mean(A) #Estimate for P(A)

#PART b)

J = 1000 - sum(A) #Number simulations out of 1000 where ruin occurs
R_not_NA = R[!is.na(R)] #Sub-vector of R without NA Values, ie time until ruin given ruin occurred (See part a for construction of R vector)

#Estimate for E[R | A^c]:
estimate = sum(R_not_NA) / J
estimate