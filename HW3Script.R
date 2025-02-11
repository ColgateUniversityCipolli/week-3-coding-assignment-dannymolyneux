prime_factors = function(n){
  primefactors = c() #stores prime factors of n
  i = 2 #initialize i to 2 because everything is divisible by 1, and we don't care about that
  while(n>1){
    while(n %% i == 0){ #if i is a factor
      n = n/i #divides by factor to potentially get other factors of n
      primefactors = append(primefactors, i) #add i to vector of prime factors
    }
    i++ #find next factor
    if(i*i>n){ #checks that n has no more prime divisors
      if(n!=1){ #if n=1, we can break
        primefactors = append(primefactors, n) #append the prime n to vector of prime factors
      }
      break #if n=1, we are done
    }
  }
  return primefactors #return vector of all prime factors (includes duplicates)
}