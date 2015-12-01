################################################
## Stat 186 Final Project
## Andy Shi and Diana Zhu
## Spring 2015
################################################

## Source of data:
# http://www.icpsr.umich.edu/icpsrweb/DSDR/studies/34241

library(data.table)
library(car)

#################################
## DATA CLEANING
#################################

# helper function
mergena <- function(datat, columns) {
    require(data.table)
    # merges two columns of a data.table datat based on NAs. Assumes that the
    # NAs occur in distinct positions in each column. Returns a new vector of
    # the merged information. 
    # datat: data table
    # columns: vector of columns to merge, as strings
    len <- length(columns)
    stopifnot(len > 1)

    merged <- as.character(datat[,get(columns[1])])
    for (colind in 2:len) {
        nas <- is.na(merged)
        nextcol <- datat[, get(columns[colind])]
        # check that the not-NAs are in different places
        stopifnot(!any(!nas & !is.na(nextcol)))
        merged[nas] <- as.character(nextcol)[nas]
    }

    return(as.factor(merged))
}

# http://stackoverflow.com/questions/15272916/how-to-wait-for-a-keypress-in-r
readkey <- function() {
    # requires user to press enter to see the next plot
    cat("Press [enter] to continue")
    line <- readline()
}

# load data into a data table
load('../ICPSR_34241/DS0001/34241-0001-Data.rda')

# cleaning up 
dataAll <- data.table(da34241.0001)
rm(da34241.0001)

colnames(dataAll)

# select columns
educIncome <- dataAll[, list(PPP, COUNTRY, WAVE, BIRTHCOHORT, GENDER, CIVIL,
                             SECONDARY, HHSIZE, BORN, RURALFIN, POORCHEALTH,
                             MOTHEREDUC_GEN, MOTHEREDUC_WLS)]

# make an ID for merging purposes
ID <- 1:nrow(educIncome)
educIncome[, ID:=ID]
summary(educIncome)

# combine maternal education, combine levels, drop unnecessary columns
mothered <- mergena(educIncome, c("MOTHEREDUC_GEN", "MOTHEREDUC_WLS"))
educIncome[, MOTHEREDUC:=mothered]
levels(educIncome$MOTHEREDUC) <- c('(1) no school', '(2) primary', 
                                   '(2) primary', '(3) secondary', 
                                   '(3) secondary', '(4) secondary+', 
                                   '(4) secondary+')

educIncome[, MOTHEREDUC_GEN:=NULL]
educIncome[, MOTHEREDUC_WLS:=NULL]


# wave depends on the country. we can discard it from our model
no.na <- na.exclude(educIncome)
no.na <- droplevels(no.na)
# ignore warnings for this
alias(SECONDARY ~ ., data=no.na)
educIncome[, WAVE:=NULL]

#a <- educIncome[!is.na(educIncome[, POORCHEALTH]),]
#poorchealth <- educIncome[, POORCHEALTH]
#educIncome[, POORCHEALTH:=NULL]


summary(educIncome)

# remove the outcome
outcome <- educIncome[, ID, PPP]
educIncome[, PPP:=NULL]


#################################
## LOOKING AT MISSINGNESS
#################################
plotMulti <- function(data, condition, condname, fnameBase, width=6, height=8) {
    for (column in colnames(data)) {
        tab1 <- table(data[!condition, get(column)])
        tab0 <- table(data[condition, get(column)])

        las <- 0
        mar <- c(5.1, 4.1, 4.1, 2.1)
        cex.lab <- 1
        # set vertical plot labels
        if (column == "ID") {
            next
        }
        if (column == "COUNTRY" || column == "BIRTHCOHORT") {
            las = 3
            mar <- c(8, 4.1, 4.1, 2.1)
            cex.lab <- 0.5
        }
        fname <- paste0('../figure/', fnameBase, '-', column, '.pdf')
        print(fname)
        pdf(fname, width=width, height=height)
        par(mfrow=c(2,1), mar=mar, cex.lab=cex.lab)
        barplot(tab1/sum(tab1), main=paste0(column, ', Not ', condname), las=las,
                cex.names=cex.lab)
        barplot(tab0/sum(tab0), main=paste(column, ',', condname), las=las,
                cex.names=cex.lab)
        dev.off()
    }
}

plotMulti(educIncome, is.na(outcome$PPP), 'Missing PPP', 'missing-ppp')


#################################
## ESTIMATING PROPENSITY SCORE
#################################

# histogram of propensity scores
plotProp <- function(modelObj, fname, data, width=6, height=8) {
    prop.scores <- fitted(modelObj)
    pdf(paste0("../figure/", fname, ".pdf"), width=width, height=height)
    par(mfrow=c(2,1), oma=c(0,0,3,0))
    hist(prop.scores[data[, SECONDARY=='(0) no']], xlab="Propensity Score",
        main="No Secondary Education")
    hist(prop.scores[data[, SECONDARY=='(1) yes']], xlab="Propensity Score",
        main="Yes Secondary Education")
    mtext("Histogram of Propensity Scores", outer=TRUE, cex=1.5)
    dev.off()
}

prop.scores.fit1 <- glm(SECONDARY ~ . - ID, data=educIncome,
                        family=binomial, na.action=na.exclude)
summary(prop.scores.fit1)
vif(prop.scores.fit1)
prop.scores1 <- fitted(prop.scores.fit1)
plotProp(prop.scores.fit1, 'propscores-noimpute', educIncome)


# trying dropping POORCHEALTH and MOTHEREDUC
educIncome.red <- educIncome[, list(COUNTRY, BIRTHCOHORT, GENDER, CIVIL,
                                    SECONDARY, HHSIZE, BORN, RURALFIN, ID)]
prop.scores.fit2 <- glm(SECONDARY ~ . - ID, data=educIncome.red,
                        family=binomial, na.action=na.exclude)
summary(prop.scores.fit2)
plotProp(prop.scores.fit2, 'propscores-dropping', educIncome)

# cannot drop birthcohort
prop.scores.fit2b <- glm(SECONDARY ~ COUNTRY + GENDER + CIVIL + HHSIZE + BORN +
                         RURALFIN, data=educIncome.red, family=binomial,
                         na.action=na.exclude)
summary(prop.scores.fit2b)
anova(prop.scores.fit2, prop.scores.fit2b, test='Chi')

# add interaction
prop.scores.fit3 <- glm(SECONDARY ~ COUNTRY * RURALFIN + BIRTHCOHORT + GENDER +
                        CIVIL + HHSIZE + BORN + GENDER:COUNTRY,
                        data=educIncome.red, family=binomial,
                        na.action=na.exclude)
summary(prop.scores.fit3)
anova(prop.scores.fit2, prop.scores.fit3, test='Chi')

# three-way interaction
prop.scores.fit3b <- glm(SECONDARY ~ COUNTRY * RURALFIN * GENDER + BIRTHCOHORT +
                        CIVIL + HHSIZE + BORN, data=educIncome.red,
                        family=binomial, na.action=na.exclude)
summary(prop.scores.fit3b)
anova(prop.scores.fit3, prop.scores.fit3b, test='Chi')

plotProp(prop.scores.fit3b, 'propscores-interact', educIncome.red)

# propensity score model for now
prop.scores.mod <- prop.scores.fit3b
myeducIncome <- educIncome.red

#################################
## SUBCLASSIFICATION
#################################

# drop mismatched controls
myeducIncome[, propensity:=fitted(prop.scores.mod)]
myeducIncome <- na.exclude(myeducIncome)
setkey(myeducIncome, SECONDARY)

su <- summary(myeducIncome['(1) yes', propensity])
psMin <- su['Min.']
psMax <- su['Max.']

control <- myeducIncome['(0) no']
mask <- (control$propensity) > psMin & (control$propensity < psMax)
print(sprintf("Proportion of controls dropped: %f", 1 -
              (sum(mask)/length(mask))))
control <- control[mask]
myeducIncome.remain <- rbind(control, myeducIncome['(1) yes'])

# cutting into subclasses based on quantiles
quant.even <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
breaks.even <- quantile(myeducIncome.remain[, propensity], quant.even)
cuts.even <- cut(myeducIncome.remain[, propensity], breaks=breaks.even,
                 include.lowest=TRUE, labels=c(1,2,3,4,5))

myeducIncome.remain[, even:=as.factor(cuts.even)]
xt.even <- xtabs(~even + SECONDARY, data=myeducIncome.remain)
print(xt.even)
weight.even <- xt.even[, 2]

# try two different sets of quantiles
quant.uneven <- c(0, 0.75, 0.85, 0.90, 0.95, 1)
breaks.uneven <- quantile(myeducIncome.remain[, propensity], quant.uneven)
cuts.uneven <- cut(myeducIncome.remain[, propensity], breaks=breaks.uneven,
                 include.lowest=TRUE, labels=c(1,2,3,4,5))

myeducIncome.remain[, uneven:=as.factor(cuts.uneven)]
xt.uneven <- xtabs(~uneven + SECONDARY, data=myeducIncome.remain)
print(xt.uneven)
weight.uneven <- xt.uneven[, 2]

# covariate balance
# function to plot covariate balance
plotBalance <- function(data, strat, treat, fnameBase, width=6, height=8) {
    stratLevels <- levels(strat)
    nstrats <- length(stratLevels)
    treatLevels <- levels(treat)
    ntreats <- length(treatLevels)
    for (column in colnames(data)) {
        las <- 0
        mar <- c(5.1, 4.1, 4.1, 2.1)
        cex.lab <- 1
        # set vertical plot labels
        if (column == "ID") {
            next
        }
        if (column == "COUNTRY" || column == "BIRTHCOHORT") {
            las = 3
            mar <- c(8, 4.1, 4.1, 2.1)
            cex.lab <- 0.5
        }
        fname <- paste0('../figure/', fnameBase, '-', column, '.pdf')
        print(fname)

        pdf(fname, width=width, height=height)
        par(mfrow=c(nstrats,ntreats), mar=mar, cex.lab=cex.lab)
        for (l in stratLevels) {
            for (tr in treatLevels) {
                if (column == 'propensity') {
                    hist(data[strat == l & treat == tr, get(column)], 
                         main=paste0(column, ', Subclass, ', l, '\n', 
                                     'Treatment ', tr), xlab='Propensity Score')
                }
                else {
                    tab <- table(data[strat == l & treat == tr, get(column)])
                    barplot(tab/sum(tab), 
                            main=paste0(column, ', Subclass, ', l, '\n', 
                                        'Treatment ', tr), 
                            las=las, cex.names=cex.lab)
                }
            }
        }
        dev.off()
    }
}

plotBalance(myeducIncome.remain, myeducIncome.remain$even,
            myeducIncome.remain$SECONDARY, 'cov-balance-even', height=12,
            width=8)
plotBalance(myeducIncome.remain, myeducIncome.remain$uneven,
            myeducIncome.remain$SECONDARY, 'cov-balance-uneven', height=12,
            width=8)

#################################
## ANALYSIS
#################################

# Naive Neyman

myeducIncome.out <- na.exclude(merge(myeducIncome.remain, outcome, by='ID'))
colnames(myeducIncome.out)

t.test(PPP ~ SECONDARY, data=myeducIncome.out)


pdf('../figure/log-ppp.pdf')
hist(myeducIncome.out[, log(PPP + 1)], main='Histogram of PPP', xlab='log(PPP + 1)')
dev.off()
# Naive regression

form <- as.formula('log(PPP + 1) ~ COUNTRY*GENDER*RURALFIN + BIRTHCOHORT + CIVIL + SECONDARY + HHSIZE')
out.lm <- lm(form, data=myeducIncome.out)
summary(out.lm)


# why naive regression is bad
par(mfrow=c(1,2))
plot(out.lm, which=1)
plot(out.lm, which=2)

# Regression within each subclass

subReg <- function(formula, data, strat, treat) {
    stratLevels <- levels(data[, get(strat)])
    nstrats <- length(stratLevels)
    effect <- rep(NA, nstrats)
    ses <- rep(NA, nstrats)
    mods <- as.list(rep(NA, nstrats))
    for (i in 1:nstrats) {
        l <- stratLevels[i]
        subs <- data[as.logical(data[, get(strat)] == l), ]
        mod <- lm(formula, data=subs)
        mods[[i]] <- mod
        effect[i] <- mod$coefficients[treat]
        ses[i] <- (summary(mod))$coefficients[treat, 'Std. Error']
    }
    return( list(effect=effect, ses=ses, models=mods) )
}
#lapply(list(1,2,3,4,5), function(x) { summary(myeducIncome.out[even==x]) })
# need to drop BORN---very few individuals.
res.even <- subReg(form, myeducIncome.out, 'even', 'SECONDARY(1) yes')
lapply(res.even$models, summary)

# regression diagnostics
par(mfrow=c(2, 3))
for (i in 1:5) {
    mod <- res.even$models[[i]]
    ids <- myeducIncome.out[even==i, ID]
    cook <- cooks.distance(mod)
    id <- which(cook > 5)
    print(ids[id])
    print(ids[which(is.na(cook))])
    print(summary(cook))
    plot(cook, type='h')
}

res.uneven <- subReg(form, myeducIncome.out, 'uneven', 'SECONDARY(1) yes')
lapply(res.uneven$models, summary)

# regression diagnostics
par(mfrow=c(2, 3))
for (i in 1:5) {
    mod <- res.uneven$models[[i]]
    ids <- myeducIncome.out[uneven==i, ID]
    cook <- cooks.distance(mod)
    id <- which(cook > 5)
    print(ids[id])
    print(ids[which(is.na(cook))])
    cook <- cooks.distance(mod)
    print(summary(cook))
    #plot(cook, type='h')
}



# drop influential observations and refit
myeducIncome.new <- myeducIncome.out[!(ID == 84547 | ID == 16157 | ID == 85690 | ID == 34417),]
nrow(myeducIncome.new)

res.even <- subReg(form, myeducIncome.new, 'even', 'SECONDARY(1) yes')
lapply(res.even$models, summary)

# regression diagnostics
par(mfrow=c(2,3))
for (mod in res.even$models) {
    plot(mod, which=1)
}
par(mfrow=c(2, 3))
for (mod in res.even$models) {
    plot(mod, which=2)
}
par(mfrow=c(2, 3))
for (i in 1:5) {
    mod <- res.even$models[[i]]
    ids <- myeducIncome.out[even==i, ID]
    cook <- cooks.distance(mod)
    id <- which(cook > 5)
    print(ids[id])
    print(ids[which(is.na(cook))])
    cook <- cooks.distance(mod)
    print(summary(cook))
    plot(cook, type='h')
}

res.uneven <- subReg(form, myeducIncome.new, 'uneven', 'SECONDARY(1) yes')
lapply(res.uneven$models, summary)

# regression diagnostics
par(mfrow=c(2, 3))
for (mod in res.uneven$models) {
    plot(mod, which=1)
}
par(mfrow=c(2, 3))
for (mod in res.uneven$models) {
    plot(mod, which=2)
}
par(mfrow=c(2, 3))
for (i in 1:5) {
    mod <- res.uneven$models[[i]]
    ids <- myeducIncome.out[uneven==i, ID]
    cook <- cooks.distance(mod)
    id <- which(cook > 5)
    print(ids[id])
    print(ids[which(is.na(cook))])
    cook <- cooks.distance(mod)
    print(summary(cook))
    plot(cook, type='h')
}


#################################
## Final fits
#################################
myeducIncome.new <- myeducIncome.new[!(ID == 3833),]
res.even <- subReg(form, myeducIncome.new, 'even', 'SECONDARY(1) yes')
lapply(res.even$models, summary)

# regression diagnostics
par(mfrow=c(2,3))
for (mod in res.even$models) {
    plot(mod, which=1)
}
par(mfrow=c(2, 3))
for (mod in res.even$models) {
    plot(mod, which=2)
}
par(mfrow=c(2, 3))
for (i in 1:5) {
    mod <- res.even$models[[i]]
    cook <- cooks.distance(mod)
    plot(cook, type='h')
}

res.uneven <- subReg(form, myeducIncome.new, 'uneven', 'SECONDARY(1) yes')
lapply(res.uneven$models, summary)

# regression diagnostics
par(mfrow=c(2, 3))
for (mod in res.uneven$models) {
    plot(mod, which=1)
}
par(mfrow=c(2, 3))
for (mod in res.uneven$models) {
    plot(mod, which=2)
}
par(mfrow=c(2, 3))
for (i in 1:5) {
    mod <- res.uneven$models[[i]]
    cook <- cooks.distance(mod)
    plot(cook, type='h')
}


#################################
## WEIGHTED ESTIMATES
#################################

wdif.even <- sum(weight.even * res.even$effect) / sum(weight.even)
wdif.uneven <- sum(weight.uneven * res.uneven$effect) / sum(weight.uneven)


weight.even.var <- ((weight.even / sum(weight.even)))^2
sd.even <- sqrt( sum(weight.even.var * (res.even$ses)^2) / sum(weight.even.var) )

weight.uneven.var <- ((weight.uneven / sum(weight.uneven)))^2
sd.uneven <- sqrt( sum(weight.uneven.var * (res.uneven$ses)^2) /
                  sum(weight.uneven.var) )


print("even breaks:")
print(c(wdif.even - 1.96 * sd.even, wdif.even + 1.96 * sd.even))
print(exp(c(wdif.even - 1.96 * sd.even, wdif.even + 1.96 * sd.even)))

print("uneven breaks:")
print(c(wdif.uneven - 1.96 * sd.uneven, wdif.uneven + 1.96 * sd.uneven))
print(exp(c(wdif.uneven - 1.96 * sd.uneven, wdif.uneven + 1.96 * sd.uneven)))

s <- as.numeric(summary(out.lm)$coefficients['SECONDARY(1) yes',])
print(c(s[1] - 1.96 * s[2], s[1] + 1.96 * s[2]))
print(exp(c(s[1] - 1.96 * s[2], s[1] + 1.96 * s[2])))
