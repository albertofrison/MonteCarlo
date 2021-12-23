# A set of examples that use the Monte Carlo Simulation to estimate results (as well as compute exact result trough statistics or formulas)
# These exercises are inspired by:
# the Probability Module of the EdX - Harvard Professional Certificate in Data Science go and check them out: https://www.edx.org/professional-certificate/harvardx-data-science
# the Italian Wikipedia Page on the Monte Carlo Method: https://it.wikipedia.org/wiki/Metodo_Monte_Carlo
# I also try to discover how big needs a Monte Carlo simulation be? OR, how many times do we need to make a Montecarlo Simulation to estimate the right results? 
# Sometimes Monte Carlo simulations are too heavy computationally speaking and the number of experiments need to be carefully determinated
# You understand that this code is made by a beginner programmer :)

#####
# Needed Libraries
library (gtools)
library (ggplot2)
library (ggforce)

##### 
# 01 BLACK JACK AND NATURAL 21
suits <- c("Diamond", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid (number = numbers, suit = suits) #combines lists
deck <- paste (deck$number, deck$suit)

# Estimation of getting a Natural 21: An ACE Plus a Facecard (OR viceversa)
aces <- paste ("Ace", suits)
facecards <- c("Ten", "Jack", "Queen", "King")
facecard <- expand.grid(number = facecards, suit = suits)
facecard <- paste (facecard$number, facecard$suit)
hands <- combinations(52,2, v = deck)

# We can calculate the probability to have an ace first and then a facecard OR a facecard first and an ACEas second card
prob_nat_21 <-  mean ((hands[,1] %in% aces & hands[,2] %in% facecard) | (hands[,1] %in% facecard & hands[,2] %in% aces))

# Let's try to find the answer by a Montecarlo Simulation (that's means to play a large number of hands and see the probability of a Natural 21)
# This function returns the probability of gettinf a natural 21  - simulating B Experiments, B hands
monte_carlo_nat_21 <-  function (B) {
  return(mean (replicate(B, {
    hand <- sample (deck, 2)
    (hand[1] %in% aces & hand [2] %in% facecard) | (hand[1] %in% facecard & hand[2] %in% aces)
  })))
}

# We have made the function that makes the B experiments and retunrs A probability, now we can prepare a set of experiments testing the accuracy of the model
B <- 10^seq(1,6, len = 100)               # a list of number of Experiments from 10^1 to 10^6 - 100 rows (10^1 plays 10 hands, 10^6 plays a million hands)
prob <- sapply (B, monte_carlo_nat_21)    # prob will contain 100 rows, with a ever increasing guess on the probability of a natural 21

black_jack_df <- data.frame(B = B, Result = prob)   # I rather use ggplot with a data.frame

# The following chart, tests both the model (graphically) AND plots the increasing stability of the process (as B increases)
ggplot (data = black_jack_df, aes(x=log10(B), y = Result)) +
  geom_line (color  = "blue") +
  geom_hline(aes(yintercept = prob_nat_21), color = "red" , linetype ="dashed") +
  labs (x = "Log10 of # of Number of Experiments", y= "Simulation Result (Probability of a Natural 21 in Black Jack)", title = "Right size of a Monte Carlo simulation [Natural 21 in Black Jack]", subtitle ="As the number of experiments increases, the Result stabilizes approximating the correct probability", caption = "https://github.com/albertofrison/MonteCarlo") +
  theme_bw()

#Saves the Plot
ggsave(filename = "1. Monte Carlo Experiment Black Jack.png", device = "png")
################################################################################################################################################# END


#####
# 02 BIRTHDAY PROBLEM or,
# What's the probability to have at least two students having the birtdday on the same day in a class is composed by 50 students?

# BIRTHDAY PROBLEM (Statistical Theory)
birthdays_exact_prob <- function (n) {
  vect <- seq (365, 365-n+1)/365  #theory: with one guy the probability of NOT having a "double birthday is 0, with two guys, the second has only 364 days out of 365 to choose from, the third 363 and so on. All these events must happen simoultainously hence you can multiply them
  1 - prod(vect)                  # one minus, stands for 100% - the event of having a day with two birthdays
} 

# BIRTHDAY PROBLEM (Monte Carlo Simulation)
birthdays_computed_prob <- function (n, B = 10000) {
  same_day <- replicate (B, {
    bdays <- sample (1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean (same_day)
}

# Plotting the Results and Testing the correctness of the Simulations
index <- c(1:55) # class size
b_days_computed <- sapply (index, birthdays_computed_prob) #computation through Monte Carlo
b_days_exact <- sapply (index, birthdays_exact_prob) #computation through Statistical Theory

birthdays_df <- data.frame (index, b_days_computed, b_days_exact) 

# This plot is very sexy in IMHO - demonstrates graphically how the MC simulations approximates the statistical reality
ggplot (data = birthdays_df) +
  geom_point (aes(x = index, y = b_days_computed), color = "blue",  size = 1.5) +
  geom_line (aes(x = index, y = b_days_exact), color = "red",  size = 1) +
  scale_y_continuous(labels = scales::percent) 

ggsave(filename = "2. Monte Carlo Experiment Birthday Problem - Identity 1.png", device = "png")

# Answer to the question: what happens with a group of 50 people? It is almost impossible to have NO double birthdays!!! Very counter intuitive!
ggplot (data = birthdays_df) +
  geom_point (aes(x = index, y = b_days_exact), color = "red") +
  labs (x = "Group Size", y= "Probability of a Double Birthday", title = "Probability of a Double Birthday in a group of friends", subtitle ="In a group of 50 people, it is very unlikely that there is no double birthay along the year") +
  scale_y_continuous(labels = scales::percent) +
  geom_hline(aes(yintercept = birthdays_exact_prob(50)), color = "blue" , linetype ="dashed") +
  geom_vline(aes(xintercept = 50), color = "blue" , linetype ="dashed")

ggsave(filename = "2. Monte Carlo Experiment Birthday Problem - Group of 50.png", device = "png")


birthdays_computed_prob_B <- function (B, n = 28) {    # modified to have 28 as a fixed constant and B as a variable
  same_day <- replicate (B, {
    bdays <- sample (1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  return (mean (same_day))
}


B <- 10^seq(1,6, len = 100)
b_days_computed_B <- sapply (B, birthdays_computed_prob_B) # each simulation has a different number of Bs as Bs increases

# Correct Plotting og the Stabilization of the result as B increases
birthdays_df_2 <- data.frame (B, b_days_computed_B) 

ggplot (data = birthdays_df_2, aes(x=log10(B), y = b_days_computed_B)) +
  geom_line (color = "blue") +
  geom_hline(aes(yintercept = birthdays_exact_prob(28)), color = "red" , linetype ="dashed") +
  labs (x = "Log10 of # of number of Experiments", y= "Simulation Result (Birthday Problem with group of 28 people)", title = "How to determine the [right] size of a Monte Carlo simulation?", subtitle ="As number of simulations increases, the result stabilizes approximating the red line", caption = "https://github.com/albertofrison/MonteCarlo") +
  theme_bw() 

ggsave(filename = "2. Monte Carlo Experiment Birthday Problem.png", device = "png")


######
# 3. The approximation of the value of Pi
#Sia M un punto di coordinate (x,y) con 0<x<1 e 0<y<1 e scegliamo casualmente i valori di x e y.
#Se {\displaystyle x^{2}+y^{2}<1}x^{2}+y^{2}<1 allora il punto M appartiene al settore di disco (un quarto del cerchio a cui appartiene) di centro (0,0) di raggio 1.
#L'area di tale disco ? il raggio elevato al quadrato per PI_GRECO diviso 4. Nell'esempio il raggio ? pari a uno e quindi l'area di interesse ? 1*PI_GRECO/4 = PI_GRECO/4. Il punto pu? cadere quindi o nel cerchio o nel quadrato circoscritto al cerchio. La probabilit? che il punto cada all'interno del settore di disco ? quindi pari al rapporto tra l'area del settore PI_GRECO/4 e l'area del quadrato circoscritto al settore di disco che ? 1; quindi la probabilit? ? PI_GRECO/4.
#Facendo il rapporto del numero dei punti che cadono nel settore di disco con il numero dei tiri effettuati si ottiene un'approssimazione del numero PI_GRECO/4 se il numero dei tiri ? grande.
#Eseguendo numericamente l'esempio si ottiene un andamento percentuale dell'errore mostrato nel grafico sottostante.


circles <- data.frame(x=0,y=0,r=1)      #dummy df to plot a circle...
theme_set(theme_void())                 #clear all space

# Real Algorithm starts here
B <- 10^1                               # size of experiment (number of points in the square / circle)
x <- sapply (B, runif)                  # vector of x positions
y <- sapply (B, runif)                  # vector of y positions
x <- ifelse(runif(B)<0.5,x,-x)          # 50% possibility to have a positive or negative x (to have a whole circle)
y <- ifelse(runif(B)<0.5,y,-y)          # 50% possibility to have a positive or negative y (to have a whole circle)
ext_pi <- sum((x^2 + y^2) <1)/B*4       # estimation of pi


d_frame <- data.frame (x=x, y=y)        #contains data for plot

ggplot () +
  geom_circle(data = circles, aes(x0 = x, y0 = y, r = r), fill= "coral", color = "red")+            # a beautiful circle centered in x0, y0, r radius
  geom_point (data = d_frame, aes(x=x, y = y), color = "maroon", size = 0.1) +                      # the dataframe with the points
  coord_fixed()+                                                                                    # keeps proportions of x vs y
  geom_hline(aes(yintercept = 0), color = "red" , linetype ="dashed") +                             # the axis
  geom_vline(aes(xintercept = 0), color = "red" , linetype ="dashed") +
  ggtitle(paste("Number of points: ",B," - Estimation of PI = ",ext_pi, sep="")) +
  theme(panel.background = element_rect(fill = "lightblue", colour = "lightblue"))
  
ggsave(filename = paste("3. Monte Carlo Experiment Birthday Problem - B ", B, ".png", sep=""), device = "png")


######
#### Trying to estimate a good B for the PI estimation

estimate_pi <- function (B) {
  x <- sapply (B, runif)                      # vector of x positions
  y <- sapply (B, runif)                      # vector of y positions
  return (sum((x^2 + y^2) <1)/B*4)            # estimation of pi
}

B <- 10^seq(1,7, len = 500)                   # B (number of experiments) goes from 10^1 to 10^7 with 500 intermediate values 
estimated_pi <- sapply (B, estimate_pi)       # let's make the vector of results of simulations as B increases

pi_data_frame <- data.frame (B, estimated_pi) # dataframe ready for plot - x = B, y = the estimated value of pi based on the number of B points randomly launched in the circle/square area

#plot of the stabilization chart
ggplot (data = pi_data_frame, aes(x=log10(B), y = estimated_pi)) +
  geom_line (color = "blue") +
  geom_hline(aes(yintercept = pi), color = "red" , linetype ="dashed") +
  labs (x = "Log10 of # of number of Experiments", y= "Simulation Result (PI approximation through the Circle Method)", title = "How to determine the [right] size of a Monte Carlo simulation?", subtitle ="As number of simulations increases, the result stabilizes approximating to 3.14", caption = "https://github.com/albertofrison") +
  theme_bw()
 
ggsave(filename = "3. Monte Carlo Experiment Approximation of PI.png", device = "png")
