
library(MASS)
library(corpcor)

## code to prepare `hospital` dataset goes here

dmat <- function(i) {
  j <- length(i)
  n <- sum(i)
  index <- cbind(start = cumsum(c(1, i[-j])), stop = cumsum(i))
  H <- matrix(0, nrow = n, ncol = j)
  for (i in 1:j) {
    H[index[i, 1]:index[i, 2], i] <- 1L
  }
  return(H)
}

r <- function(n, mu, sigma, data) {
  dmat(n) %*% rnorm(length(n), mu, sigma) * data
}

logit <- function(xb) 1/(1 + exp(-xb))

mycut <- function(x, p) {
  cut(x = x, breaks = quantile(x, probs = p), labels = FALSE, include.lowest = TRUE)
}

## seed for simulation parameters
set.seed(1)

# total number of hospitals
k <- 35

# number of doctors within each hospital
n <- sample(8:15, size = k, TRUE)
# total number of doctors
j <- sum(n)


# number of patients within each doctor
N <- sample(2:40, size = j, TRUE)
# total number of patients
i <- sum(N)

mu <- list(
  int = 0,
  experience = 18,
  cont = c(
    Age = 5.1,
    Married = 0,
    FamilyHx = 0,
    SmokingHx = 0,
    Sex = 0,
    CancerStage = 0,
    LengthofStay = 6,
    WBC = 6000,
    RBC = 5
  ),
  bounded = c(BMI = 5.5, IL6 = 4, CRP = 5)
)

R <- diag(9)

rownames(R) <- names(mu$cont)

R[1, 2] <- 0.3
R[1, 4] <- 0.3
R[1, 6] <- 0.5
R[1, 7] <- 0.5
R[2, 6] <- -0.2
R[2, 7] <- -0.4
R[2, 8] <- 0.25
R[3, 4] <- -0.5
R[3, 6] <- 0.3
R[3, 7] <- 0.3
R[4, 5] <- 0.3
R[4, 6] <- 0.7
R[4, 7] <- 0.5
R[6, 7] <- 0.5
R[7, 8] <- -0.3
R[8, 9] <- -0.1
R[lower.tri(R)] <- t(R)[lower.tri(t(R))]

(R <- cov2cor(make.positive.definite(R)))

p <- list(
  school = 0.25,
  sex = 0.4,
  married = 0.6,
  familyhx = 0.2,
  smokehx = c(0.2, 0.2, 0.6),
  stage = c(0.3, 0.4, 0.2, 0.1)
)

## hospital variables
b <-
  cbind(
    HID = 1:k,
    Hint = rnorm(k, mean = mu$int, sd = 1),
    Medicaid = runif(k, min = 0.1, max = 0.85)
  )

H <- dmat(N) %*% dmat(n) %*% b

## View the first few rows
head(H)

b <-
  cbind(
    DID = 1:j,
    Dint = rnorm(j, mean = mu$int, sd = 1),
    Experience = experience <- floor(rnorm(j, mean = mu$experience, sd = 4)),
    School = school <- rbinom(j, 1, prob = p$school),
    Lawsuits = rpois(j, pmax(experience - 8 * school, 0) / 8)
  )

D <- dmat(N) %*% b

## View the first few rows
head(D)

## continuous variables
Xc <- as.data.frame(
  cbind(
    mvrnorm(n = i, mu = rep(0, 9), Sigma = R),
    sapply(mu$bounded, function(k) rchisq(n = i, df = k))
  )
)

Xc <- within(Xc, {
  Age <- ((Age / 1.6) + mu$cont["Age"]) * 10
  Married <- mycut(Married, c(0, 1 - p$married, 1)) - 1
  FamilyHx <- mycut(FamilyHx, c(0, 1 - p$familyhx, 1)) - 1
  SmokingHx <- factor(mycut(SmokingHx, c(0, cumsum(p$smokehx))))
  Sex <- mycut(Sex, c(0, 1 - p$sex, 1)) - 1
  CancerStage <- factor(mycut(CancerStage, c(0, cumsum(p$stage))))
  LengthofStay <- floor(LengthofStay + mu$cont["LengthofStay"])
  WBC <- ((WBC / .001) + mu$cont["WBC"])
  RBC <- ((RBC / 3.5) + mu$cont["RBC"])
  BMI <- pmin(BMI * 2 + 18, 58)
})

## create dummies and drop the intercept
Xmdummy <- model.matrix(~ 1 + SmokingHx + CancerStage, data = Xc)[, -1]

X <- cbind(Xc[, -c(4,6)], Xmdummy,
           "Sex:Married" = Xc[, "Sex"] * Xc[, "Married"],
           "IL6:CRP" = Xc[, "IL6"] * Xc[, "CRP"],
           "BMI:FamilyHx" = Xc[, "BMI"] * Xc[, "FamilyHx"],
           "SmokingHx2:FamilyHx" = Xmdummy[, "SmokingHx2"] * Xc[, "FamilyHx"],
           "SmokingHx3:FamilyHx" = Xmdummy[, "SmokingHx3"] * Xc[, "FamilyHx"],
           "SmokingHx2:Age" = Xmdummy[, "SmokingHx2"] * Xc[, "Age"],
           "SmokingHx3:Age" = Xmdummy[, "SmokingHx3"] * Xc[, "Age"],
           "Experience:CancerStage2" = D[, "Experience"] * Xmdummy[, "CancerStage2"],
           "Experience:CancerStage3" = D[, "Experience"] * Xmdummy[, "CancerStage3"],
           "Experience:CancerStage4" = D[, "Experience"] * Xmdummy[, "CancerStage4"])

## Final data
mldat <- data.frame(Xc, D, H)
mldat <- mldat[, -which(colnames(mldat) %in% c("Dint", "Hint"))]

mldat <- within(mldat, {
  Sex <- factor(Sex, labels = c("female", "male"))
  FamilyHx <- factor(FamilyHx, labels = c("no", "yes"))
  SmokingHx <- factor(SmokingHx, labels = c("current", "former", "never"))
  CancerStage <- factor(CancerStage, labels = c("I", "II", "III", "IV"))
  School <- factor(School, labels = c("average", "top"))
  DID <- factor(DID)
  HID <- factor(HID)
})

## Final for simulation
dat <- cbind(X, D, H)
dat <- as.matrix(dat[, -which(colnames(dat) %in% c("DID", "HID"))])

b <- as.data.frame(
  rbind(
    'Age' = c(1, 1, 0, 0, 1, 8, .8, -1, -1, 0),
    'Married' = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0),
    'FamilyHx' = c(1, 1, 0, 0, 0, -8, 0, 0, -5, 0),
    'Sex' = c(0, 0, -1, 1, 0, 0, 0, -1, 0, 0),
    'LengthofStay' = c(0, 0, 0, 0, 0, 0, 0, .9, 0, 0),
    'WBC' = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    'RBC' = c(0, 0, 0, 0, 0, 0, 0, 0, 0, -2),
    'BMI' = c(0, 0, 0, 0, 1, 1, 0, 0, 0, 0),
    'IL6' = c(0, 0, 1, 0, 0, 3.2, 0, 0, -1, 0),
    'CRP' = c(0, 0, 1, 0, 0, 5, 0, 0, -.8, 0),
    'SmokingHx2' = c(-1, -1, 0, 0, 0, -2, -2, 0, 0, 0),
    'SmokingHx3' = c(-2, -2, 0, 0, 0, -10, -4, 0, 0, 0),
    'CancerStage2' = c(1, 1, 0, 0, 0, .5, 2, 1, -1, 0),
    'CancerStage3' = c(1.5, 1.5, 0, 0, 0, 1, 4, 2, -3, 0),
    'CancerStage4' = c(2, 2, 0, 0, 0, 2, 6, 3, -6, 0),
    'Sex:Married' = c(0, 0, 0, 4, 0, 0, 0, 0, 0, 0),
    'IL6:CRP' = c(0, 0, 3, 0, 0, 0, 0, 0, 0, 5),
    'BMI:FamilyHx' = c(0, 0, 0, 0, 0, 60, 0, 0, 0, 0),
    'SmokingHx2:FamilyHx' = c(0, 0, 0, 0, 0, 0, 0, 0, 0, -.5),
    'SmokingHx3:FamilyHx' = c(0, 0, 0, 0, 0, 0, 0, 0, 0, -5),
    'SmokingHx2:Age' = c(-4, -4, 0, 0, 0, 0, 0, 0, 0, 0),
    'SmokingHx3:Age' = c(-8, -8, 0, 0, 0, 0, 0, 0, 0, 0),
    'Experience:CancerStage2' = c(0, 0, 0, 0, 0, 4, 0, 0, 0, 0),
    'Experience:CancerStage3' = c(0, 0, 0, 0, 0, 16, 0, 0, 0, 0),
    'Experience:CancerStage4' = c(0, 0, 0, 0, 0, 40, 0, 0, 0, 0),
    'Dint' = c(3, 3, 3, 3, 3, 20, 8, 10, 5, 10),
    'Experience' = c(0, 0, 0, -5, -3, 0, 0, 0, 3, 2),
    'School' = c(0, 0, 0, 0, 2, 0, 0, 0, 0, 1),
    'Lawsuits' = c(0, 0, 0, 0, -2, 0, 0, 0, 0, 0),
    'Hint' = c(0, 0, 0, 4, 5, 40, 0, 0, 6, 0),
    'Medicaid' = c(0, 0, 0, 0, 3, 0, 0, 0, 0, 0)
  )
)

b <- b / apply(dat, 2, sd)

colnames(b) <-
  c(
    "tumor",
    "co2",
    "pain",
    "wound",
    "mobility",
    "ntumors",
    "zeroinflation",
    "nmorphine",
    "remission",
    "lungcapacity"
  )


## Tumor Size
outcome <-
  data.frame(
    tumorsize = rnorm(
      n = i,
      mean = (dat %*% b$tumor) + r(N, 0.8, 0.8, dat[, "LengthofStay"]) + 70,
      sd = 8
    )
  )

## CO2
outcome$co2 <- abs(
  rnorm(
    n = i,
    mean = ((dat %*% b$co2) + r(N, 0.8, 1, dat[, "LengthofStay"]))/100,
    sd = 0.08
  ) + 1.6
)

## Pain
outcome$pain <- rnorm(n = i, mean = (dat %*% b$pain), sd = 6)

outcome$pain <- with(outcome, pmin(pain, quantile(pain, 0.99)))

outcome$pain <- with(
  outcome,
  cut(pain,
      breaks = seq(from = min(pain) - 0.1, max(pain) + 0.1, length.out = 10),
      labels = FALSE)
)


## Cancer in remission

tmp <- dat %*% b$remission + r(N, -1.5, 3, dat[, "LengthofStay"])

outcome$remission <- as.numeric(
  rnorm(n = i, mean = tmp, sd = 15) > quantile(tmp, probs = .75)
)

hospital <- cbind(outcome, mldat)

head(hospital)

usethis::use_data(hospital, overwrite = TRUE)
