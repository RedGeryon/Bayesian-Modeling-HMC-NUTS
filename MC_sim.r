num_weeks <- 1e4
position <- rep(0,num_weeks)
current <- 10
for (i in 1:num_weeks) {
	#pos
	position[i] = current

	#proposal
	proposal <- current + sample( c(-1,1), size = 1)
	if (proposal < 1 ) proposal <- 10
	if (proposal > 10) proposal <- 1

	prob_move <- proposal / current
	current <-  ifelse( runif(1) < prob_move, proposal, current)
}

d <- data.frame(position, seq(1:1e4))