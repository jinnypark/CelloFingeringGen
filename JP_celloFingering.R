#Cello fingering_1st position  #Jinny Park 
#--modified from pianoFingering notes from Christopher Raphael
#intended for use alongside Suzuki Cello Book 1, 1st position chapter
# example pieces and preferences for fingering drawn from Suzuki Cello book.

#This program seeks minimal finger movement while keeping 1st position
#where if two notes are related by mod7, they will be played by same finger.
#fingerings here are processed as
#open String = 1; 1st finger = 2, 2nd finger = 3, 3rd finger = 4, pinky =5
# these are re-processed with converting function and
# plotted as open String = 0; 1st finger = 1; and so on.

cost = function(m1,m2,f1,f2) {
  if (m2 < m1) { temp = m1; m1 = m2; m2 = temp; temp = f1; f1 = f2; f2 = temp}  # assume m2 > m1
  m1mod7 = m1 %% 7;
  m2mod7 = m2 %% 7;
  
  
  # there are 4 cases: f2 != 1, f2 > f1, f2 = f1, f2 < f1
  openString = c(0,7,14,21);
  
  #how to tell if two notes are on the same string?
  #no cost for crossover unless two notes are on the same string
  #This function ensures that the program gives more cost for shifting on the same string
  # which allows the program to prefer staying in one hand position
  onString <- function(m_1, m_2) {
    if(m2 < 7) {
      if (m1 < 7) {return (TRUE)}
      else {return(FALSE)}
    } else if (m2 <14) {
      if (m1 <14) {return(TRUE)}
      else {return(FALSE)}
    } else if (m2 <21) {
      if(m1 <21) {return(TRUE)}
      else {return(FALSE)}
    }
  }
  # - - - -- - -
  # Extension function 
  extension <- function(m1_,m2_, f1_,f2_) {
    if((m2_ - m1_) > (f2_ - f1_)) {
      return(TRUE)
    } else {return(FALSE)}
  }
  
  
  #case 0: open string
  if (f2 != 1) {
    if (m2 %in% openString)
    {return (1000)} #"1" is actually fingering "0" but R thinks it's 1. 
    
  } else { #if f2 == 1 and m2 is openString. 
    if(!(m2 %in% openString)) {
      return(1000)
    }
  }
  

  # case 1: f2 > f1
  if (f2 > f1) { # if no crossover
    if (m2mod7 == m1mod7) {return(100)}
    ## gotta make sure that same finger position notes are played by same finger
    if(m1 %in% openString && (m2-m1 == 2)) {
      if (f2 != 2) {return (100)}
    }
   #     if (f2 != 2) {return(100)}
   #   }
   # }
    
   # else if (extension(m1,m2,f1,f2)) {
  #    return(100)
   # }
    
    else{ return(abs((f2-f1) - (m2-m1)))}
    
  }
  
  # case 1: f2 = f1
  if (f2 == f1) { 	 # if same finger
    if (m1mod7 == m2mod7) { return(0); } 
    
    else { return(100); }   # very high cost since we really want to avoid this.  
  }
  
  # case 3: f2 < f1 (crossover)  (remember we are always moving *upward* in pitch)	

  return(3);
  
  #costs for shifting: if two notes are on the same string, Penalize.
  if(onString(m1,m2)) { #if they are both on the same String and crossing over
    return(100)
  } else  if (m2mod7 == m1mod7) {return(100)}
  else {return(abs((f2-f1) - (m2-m1)))}

}
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#Test pieces
###Integers represent pitches, taking account of register.
### 0 represents middle C (C4).
### 1, 13, 25 represents C# or Db in different octaves.

#twinkle twinkle
#piece = c(14,14,21,21,23,23,21,19,19,18,18,16,16,14,21,21,19,19,18,18,16,21,21,19,19,18,18,16,14,14,21,21,23,23,21,19,19,18,18,16,16,14);
#French Folk song
#piece = c(26, 26, 26, 25, 25, 25, 23, 25, 26, 21, 19, 19, 19, 18, 18, 18, 16, 16, 16, 14, 14, 16, 18, 14, 16, 18, 14, 16, 18, 19, 16, 18, 19,16, 18, 19,16, 18, 19, 21,26, 25, 23, 21, 19, 18, 16, 14, 16, 14);  
#C Major scale and arpeggiation
#piece = c(0, 2, 4, 5, 7, 9, 11, 12, 19, 12, 7, 0)
#testing for same fingering on mod7 spaces, while keeping *first position*
#piece = c(0,7,14,21,2,9,16,23, 3, 10, 17, 24)

#2nd position
#piece = c(0,2,4,5,7,21,23,25,27,29,30)
#piece = c(0, 21, 27, 29, 31)
#piece = c(7, 21,27, 29, 31) #G A D E F#
piece = c(21, 23, 25, 26, 28, 30, 31, 30, 28, 26, 25, 23, 21) #A B C# D E F# G
#piece = c(21, 23, 24, 26, 28, 30, 31, 26, 24, 23, 21)
#A B C D

N = length(piece);
L = 5;  # the number of states = number of fingers


# the dynamic programming algorithm ....

xhat = rep(0,N);   # this will be the optimal state sequence (finger sequence)
optscore = matrix(1000000,N,L)   # optscore[n,s] is the best score to trellis level (note) n ending with state (finger) s
# these are the numbers we wrote inside the circles
optpred = matrix(0,N,L)          # optpred[n,s] is the best preceding state of n,s
# these are the states pointed to by the arrows we drew
for (s in 1:L) optscore[1,s] = 0;  # can begin with any finger at no cost
for (n in 2:N) {     # for each note
  for (s in 1:L) {   # for each possible finger
    for (r in 1:L) { # for each possible predecessor finger
      z = optscore[n-1,r]+ cost(piece[n-1],piece[n],r,s);  # cost of going from state r to state s
      if (z < optscore[n,s]) {   # if best so far, store these values
        optscore[n,s] = z; 
        optpred[n,s] = r; 
      }
    }
  }
}

xhat[N] = which.min(optscore[N,])  # best final finger

for (n in (N-1):1) xhat[n] = optpred[n+1,xhat[n+1]];  # follow pointers back

#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# a function to convert fingering number to conventions where
#open string = 0. 
translate <- function(fingering) {
  return (fingering - 1);
}

for (g in 1:length(xhat)) {
  xhat[g] = translate(xhat[g])
}

plot(1:N,piece,type="n");
lines(1:N,piece);
text(1:N,piece,label=xhat,cex=2);



