# this function takes one argument, x, appends the value of that argument
# to a greeting, and then prints the whole greeting
say_hi <- function(x) {
	hi <- paste("Greetings, ", x, "!", sep = "")
	# the paste command allows string concatenation
	return(hi)
}
