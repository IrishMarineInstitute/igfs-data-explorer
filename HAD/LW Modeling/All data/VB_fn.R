VB_fn=function (start.list, data, binding, maxiter.em = 1000, reltol = 1e-08, 
          plot.fit = FALSE, verbose = TRUE, optim.method = "BFGS", 
          estimate.mixprop = TRUE, distribution) 
{
  if (!"mixprop" %in% names(start.list[["par"]])) {
    stop("No starting value for mixing proportion provided, specify 'mixprop = value' in start.list list")
  }
  if (max(binding) != length(start.list[["par"]][["growth.par"]])) {
    stop("Mismatch in the length of growth.par and that specified by binding.")
  }
  ollike <- rep(NA, maxiter.em)
  if (plot.fit) {
    blue2red <- colorRampPalette(c("blue", "red"))
    col.vec <- blue2red(100)
    breaks <- seq(-0.1, 1.1, length = 100)
    data$jitter.age <- jitter(data$age)
  }
  classified.data <- data[data$obs.sex %in% c("female", "male"), 
                          ]
  unclassified.data <- data[data$obs.sex == "unclassified", 
                            ]
  female_growth_fit <- function(x) {
    linfF * (1 - exp(-kF * (x - t0F)))
  }
  male_growth_fit <- function(x) {
    linfM * (1 - exp(-kM * (x - t0M)))
  }
  for (i in 1:maxiter.em) {
    if (i == 1) {
      par <- start.list[["par"]]
    }
    growth.par <- par[["growth.par"]]
    linfF <- exp(growth.par[binding["lnlinf", "female"]])
    linfM <- exp(growth.par[binding["lnlinf", "male"]])
    kF <- exp(growth.par[binding["lnk", "female"]])
    kM <- exp(growth.par[binding["lnk", "male"]])
    t0F <- -exp(growth.par[binding["lnnt0", "female"]])
    t0M <- -exp(growth.par[binding["lnnt0", "male"]])
    sigmaF <- exp(growth.par[binding["lnsigma", "female"]])
    sigmaM <- exp(growth.par[binding["lnsigma", "male"]])
    mixprop <- par[["mixprop"]]
    muF.unclass <- female_growth_fit(unclassified.data$age)
    muM.unclass <- male_growth_fit(unclassified.data$age)
    muF.class <- female_growth_fit(classified.data$age)
    muM.class <- male_growth_fit(classified.data$age)
    classified.data$tau <- ifelse(classified.data$obs.sex == 
                                    "female", 1, ifelse(classified.data$obs.sex == "male", 
                                                        0, NA))
    unclassified.data$tau <- get_growth_post_prob(mixprop = mixprop, 
                                                  muF = muF.unclass, muM = muM.unclass, sigmaF = sigmaF, 
                                                  sigmaM = sigmaM, data = unclassified.data, distribution = distribution)
    complete.data <- rbind(classified.data, unclassified.data)
    if (distribution == "normal") {
      ll.F.class <- sum(classified.data$obs.sex == "female") * 
        log(mixprop) + sum(dnorm(classified.data$length, 
                                 mean = muF.class, sd = sigmaF, log = TRUE)[classified.data$obs.sex == 
                                                                              "female"])
      ll.M.class <- sum(classified.data$obs.sex == "male") * 
        log(1 - mixprop) + sum(dnorm(classified.data$length, 
                                     mean = muM.class, sd = sigmaM, log = TRUE)[classified.data$obs.sex == 
                                                                                  "male"])
      ll.miss <- sum(log(mixprop * dnorm(unclassified.data$length, 
                                         mean = muF.unclass, sd = sigmaF) + (1 - mixprop) * 
                           dnorm(unclassified.data$length, mean = muM.unclass, 
                                 sd = sigmaM)))
    }
    if (distribution == "lognormal") {
      ll.F.class <- sum(classified.data$obs.sex == "female") * 
        log(mixprop) + sum(dlnorm(classified.data$length, 
                                  meanlog = log(muF.class) - sigmaF^2/2, sdlog = sigmaF, 
                                  log = TRUE)[classified.data$obs.sex == "female"])
      ll.M.class <- sum(classified.data$obs.sex == "male") * 
        log(1 - mixprop) + sum(dlnorm(classified.data$length, 
                                      meanlog = log(muM.class) - sigmaM^2/2, sdlog = sigmaM, 
                                      log = TRUE)[classified.data$obs.sex == "male"])
      ll.miss <- sum(log(mixprop * dlnorm(unclassified.data$length, 
                                          meanlog = log(muF.unclass) - sigmaF^2/2, sdlog = sigmaF) + 
                           (1 - mixprop) * dlnorm(unclassified.data$length, 
                                                  meanlog = log(muM.unclass) - sigmaM^2/2, sdlog = sigmaM)))
    }
    ollike[i] <- ll.F.class + ll.M.class + ll.miss
    if (plot.fit) {
      tau.col <- col.vec[cut(complete.data$tau, breaks)]
      par(mfrow = c(1, 1), mar = c(2, 2, 1, 1), oma = c(2, 
                                                        2, 1, 1))
      age.pred <- seq(min(complete.data$jitter.age), max(complete.data$jitter.age), 
                      length = 50)
      plot(complete.data$jitter.age, complete.data$length, 
           pch = ifelse(complete.data$obs.sex == "unclassified", 
                        17, 19), col = paste(tau.col, 40, sep = ""), 
           ylim = c(0, max(complete.data$length)), xlim = c(0, 
                                                            max(complete.data$jitter.age)), xlab = "", 
           ylab = "")
      lines(age.pred, female_growth_fit(age.pred), col = "red")
      lines(age.pred, male_growth_fit(age.pred), col = "blue")
      mtext(side = 2, line = 2.5, text = "Length")
    }
    if (estimate.mixprop) {
      mixprop <- sum(complete.data$tau)/length(complete.data$tau)
    }
    par[["mixprop"]] <- mixprop
    complete.data$weights <- complete.data$tau
    vb_fit <- optim(vb_bind_nll, par = growth.par, gr = vb_bind_gr, 
                    binding = binding, data = complete.data, method = optim.method, 
                    distribution = distribution)
    par[["growth.par"]] <- vb_fit$par
    if (verbose) {
      cat(paste("EM iteration:", i, "|", "Observed data log-likelihood: ", 
                ollike[i], "\n"))
    }
    if (i > 2) {
      if (abs(ollike[i] - ollike[i - 1]) < abs(ollike[i - 
                                                      1] * reltol) | i == maxiter.em) {
        oll <- function(theta, estimate.mixprop, distribution) {
          linfF <- exp(theta[binding["lnlinf", "female"]])
          linfM <- exp(theta[binding["lnlinf", "male"]])
          kF <- exp(theta[binding["lnk", "female"]])
          kM <- exp(theta[binding["lnk", "male"]])
          t0F <- -exp(theta[binding["lnnt0", "female"]])
          t0M <- -exp(theta[binding["lnnt0", "male"]])
          sigmaF <- exp(theta[binding["lnsigma", "female"]])
          sigmaM <- exp(theta[binding["lnsigma", "male"]])
          if (estimate.mixprop) {
            mixprop <- plogis(theta[max(binding) + 1])
          }
          else {
            mixprop <- mixprop
          }
          muF.unclass <- linfF * (1 - exp(-kF * (unclassified.data$age - 
                                                   t0F)))
          muM.unclass <- linfM * (1 - exp(-kM * (unclassified.data$age - 
                                                   t0M)))
          muF.class <- linfF * (1 - exp(-kF * (classified.data$age - 
                                                 t0F)))
          muM.class <- linfM * (1 - exp(-kM * (classified.data$age - 
                                                 t0M)))
          if (distribution == "normal") {
            ll.F.class <- sum(classified.data$obs.sex == 
                                "female") * log(mixprop) + sum(dnorm(classified.data$length, 
                                                                     mean = muF.class, sd = sigmaF, log = TRUE)[classified.data$obs.sex == 
                                                                                                                  "female"])
            ll.M.class <- sum(classified.data$obs.sex == 
                                "male") * log(1 - mixprop) + sum(dnorm(classified.data$length, 
                                                                       mean = muM.class, sd = sigmaM, log = TRUE)[classified.data$obs.sex == 
                                                                                                                    "male"])
            ll.miss <- sum(log(mixprop * dnorm(unclassified.data$length, 
                                               mean = muF.unclass, sd = sigmaF) + (1 - 
                                                                                     mixprop) * dnorm(unclassified.data$length, 
                                                                                                      mean = muM.unclass, sd = sigmaM)))
          }
          if (distribution == "lognormal") {
            ll.F.class <- sum(classified.data$obs.sex == 
                                "female") * log(mixprop) + sum(dlnorm(classified.data$length, 
                                                                      meanlog = log(muF.class) - sigmaF^2/2, 
                                                                      sdlog = sigmaF, log = TRUE)[classified.data$obs.sex == 
                                                                                                    "female"])
            ll.M.class <- sum(classified.data$obs.sex == 
                                "male") * log(1 - mixprop) + sum(dlnorm(classified.data$length, 
                                                                        meanlog = log(muM.class) - sigmaM^2/2, 
                                                                        sdlog = sigmaM, log = TRUE)[classified.data$obs.sex == 
                                                                                                      "male"])
            ll.miss <- sum(log(mixprop * dlnorm(unclassified.data$length, 
                                                meanlog = log(muF.unclass) - sigmaF^2/2, 
                                                sdlog = sigmaF) + (1 - mixprop) * dlnorm(unclassified.data$length, 
                                                                                         meanlog = log(muM.unclass) - sigmaM^2/2, 
                                                                                         sdlog = sigmaM)))
          }
          oll <- ll.F.class + ll.M.class + ll.miss
          return(-oll)
        }
        if (estimate.mixprop) {
          oll.fit <- optim(fn = oll, par = c(par[["growth.par"]], 
                                             qlogis(mixprop)), hessian = TRUE, control = list(maxit = 10000), 
                           estimate.mixprop = TRUE, distribution = distribution, 
                           method = optim.method)
        }
        else {
          oll.fit <- optim(fn = oll, par = c(par[["growth.par"]]), 
                           hessian = TRUE, control = list(maxit = 10000), 
                           estimate.mixprop = FALSE, distribution = distribution, 
                           method = optim.method)
        }
        if (!(round(-oll.fit$value/ollike[i], 4) == 1)) {
          warning(paste("EM solution and optim solution differ by ", 
                        -oll.fit$value - ollike[i], ", final parameter values may differ from final EM values.", 
                        sep = ""))
        }
        theta <- oll.fit$par
#        par.vcov <- solve(oll.fit$hessian)
#        par.se <- sqrt(diag(par.vcov))
        female.pars <- c(oll.fit$par[binding[, "female"]], 
                         oll.fit$par[max(binding) + 1])
#        female.se <- c(par.se[binding[, "female"]], par.se[max(binding) + 1])
        male.pars <- c(oll.fit$par[binding[, "male"]], 
                       -oll.fit$par[max(binding) + 1])
#        male.se <- c(par.se[binding[, "male"]], par.se[max(binding) + 1])
        theta.df <- data.frame(Parameter = c(rownames(binding), 
                                             "logitpi"), Female = female.pars, #Female.Std.Error = female.se, 
                               Male = male.pars)#, Male.Std.Error = male.se)
        res <- list()
        res$logLik.vec <- ollike[1:i]
        res$logLik <- ollike[i]
        res$complete.data <- complete.data
        res$coefficients <- theta.df
        #res$vcov <- par.vcov
        res$convergence <- ifelse(i == maxiter.em, 1, 
                                  0)
        return(res)
      }
    }
    rm(list = ls()[!ls() %in% c("classified.data", "unclassified.data", 
                                "maxiter.em", "par", "ollike", "reltol", "plot.fit", 
                                "col.vec", "breaks", "verbose", "vb_bind_nll", "binding", 
                                "optim.method", "estimate.mixprop", "distribution", 
                                "female_growth_fit", "male_growth_fit")])
  }
}