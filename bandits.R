library(ggplot2)
library(reshape2)


num.of.pulls <- 1000    # How many times to pull the lever?
num.of.bandits <- 10    # How many bandis are there to choose from?

# Generate means and standard deviations for each bandit.
bandit.means <- rnorm(num.of.bandits, mean=0, sd=1)
bandit.sds <- rep(1, num.of.bandits)

# Select the bandit with the largest mean.
best.bandit <- which.max(bandit.means)

# First try the sample-average method that always chooses the bandit whose
# average of sample rewards gotten so far is the largest. Then try
# \epsilon-greedy action-value methods that with \epsilon probability uniformly
# choose a random bandit instead of the most promising one. This introduces
# some exploration instead of just exploiting all the time.

epsilons <- c(0, 1/100, 1/10, 1/3, 1/2, 2/3)

bandit.estimated.means <- matrix(0, nrow=length(epsilons), ncol=num.of.bandits)
bandit.times.pulled <- matrix(0, nrow=length(epsilons), ncol=num.of.bandits)
best.mean.history <- matrix(0, nrow=length(epsilons), ncol=num.of.pulls)
reward.history <- matrix(0, nrow=length(epsilons), ncol=num.of.pulls)
pull.history <- matrix(0, nrow=length(epsilons), ncol=num.of.pulls)


for (j in 1:length(epsilons)) {
    for (i in 1:num.of.pulls) {

        # Determine which bandit to pull.
        if (runif(1) < epsilons[j]) {
            bandit <- sample(1:num.of.bandits, size=1)
        } else {
            bandit <- which.max(bandit.estimated.means[j, ])
        }

        # Generate a reward.
        reward <- rnorm(n=1, mean=bandit.means[bandit], sd=bandit.sds[bandit])

        # Bookkeeping for nice plots.
        reward.history[j, i] <- reward
        pull.history[j, i] <- bandit
        best.mean.history[j, i] <- max(bandit.estimated.means[j, ])

        # Increment sample average.
        bandit.times.pulled[j, bandit] <- bandit.times.pulled[j, bandit] + 1
        avg <- bandit.estimated.means[j, bandit]
        bandit.estimated.means[j, bandit] <-
            avg + (reward-avg) / bandit.times.pulled[j, bandit]

    }
}

# Plot mean reward per pull history for different epsilon choices.

avg.rewards <- apply(reward.history, 1, function(row) {
    cumsum(row) / 1:num.of.pulls
})

EpsilonToColumnName <- function (epsilon) {
    paste("VA, eps = ", sprintf("%.3f", epsilon))
}

avg.reward.per.pull <- as.data.frame(avg.rewards)
colnames(avg.reward.per.pull) <- sapply(epsilons, EpsilonToColumnName)
avg.reward.per.pull$pull <- 1:num.of.pulls

colors <- rainbow(length(epsilons))

df <- melt(avg.reward.per.pull, id="pull")

p <- ggplot(df, aes(x=pull, y=value, color=variable)) +
    geom_line(size=1.3) +
    xlab("Number of pulls") +
    ylab("Average reward") +
    theme(text = element_text(size=18))
print(p)

readline("Press <ENTER> for more... ")

# For each bandit plot the distribution of strategies that pulled them.
df <- as.data.frame(t(pull.history))
colnames(df) <- sapply(epsilons, EpsilonToColumnName)
df$pull <- 1:num.of.pulls
df <- melt(df, id="pull")
df$value <- as.factor(df$value)

p <- ggplot(df, aes(variable, fill=variable)) +
     geom_bar() +
     facet_wrap(~ value)
print(p)
