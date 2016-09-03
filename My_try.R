
#===============================================================================================================
#                                               *A First Guide* 
#                                                I - The Basics
#===============================================================================================================

setwd("~/Documents/Stats/Guide1data/data")

#*********************************************** 1) Working with R *****************************************----

# 1.3 POTATO YIELD----------------------------------------------------------------------------------------------

#Undersøger forholdet mellem udbytte af potatoes og brugen af fosfor (DU HAR SELV LAVET DATA..).

potato <- read.table(file = "potato.txt", header = TRUE, dec = ".")

#HUSK: "file" (argument name) kunne være undladt da den er på den "rigtige plads".
#     Når argument name er specificeret er det ikke vigtigt hvor i kommandoen argument value står.
#     dec = "." kunne være undladt da dette er defualt - i modsætning til header = TRUE
#     Se s. 17-18 eller ?read.table for mere info.

str(potato)

dim(potato)

head(potato)

plot(x=potato$P82, y=potato$yield)

#Linær reg

lm(yield~P82, data = potato)

# 1.4 NAMES

x <- 1.8^2
y <- exp(0.5)
z <- x*y - sqrt(2.4)

z
# Eller:
print(z)



#*********************************************** 2) Getting data in R ****************************************----

# 2.1 DATA VIA CLIPBOARD------------------------------------------------------------------------------------------

# Markér txt. eller felter fra regne ark. Copy or kør her efter understående kommando (eks.)

potato1 <- read.table(file = "clipboard", header = TRUE, dec = ".")

# 2.2 SAVE AND READ DATA IN CSV.----------------------------------------------------------------------------------

potato2 <- read.csv(file = "potato.csv", sep = ",", dec = ".")

# Check evt. din data i et text program for om sep ="," el. sep=";"
# Notér at Header = TRUE her er default og kan undlades. 

# 2.3 READ EXCEL (.xlsx)------------------------------------------------------------------------------------------

# potato2 <- read.xlsx(file = "potato.xlsx", sheetIndex = 1)

# MEN package'en er der ikke lige nu.

# 2.4 MAKE R FIND THE FILE----------------------------------------------------------------------------------------

# Det her åbner blot for et vindue så man lettere kan finde den rigtige wd

potato3 <- read.csv(file = file.choose(), sep = ",", dec=".")

# 2.5 DATA FRAMEs INFO-------------------------------------------------------------------------------------------

dim(potato)
# Giver dig antal rows/culm.

names(potato)
#Giver dig alle var. navne.

head(potato)
# Viser alle var. og et kort udsnit af de første værdier.

mean(potato$yield)
# FInder gennemsnit (mean) for var. (culm./vector) 'yield' i dataframen 'potato'

#2.5.1 THE FUNTIONS 'attach', 'detach' AND 'width'--------------------------------------------------------------

attach(potato)
# Nu behøver jeg ikke specifisere dataframe mere. 
# Det er lidt riski men måske brugbart hvis du kun arbejder i en dataframe..

# eks:
mean(yield)

detach(potato)
# Nu er den 'frakoblet' igen

with(potato, plot(P82,yield))
# Her specificere du for en enkelt kommando, hvilket er hurtiger end understående:

plot(potato$P82,potato$yield)
# Her bruger du så at siger hele navnet/adressen på værdien i stedet.



#****************************************** 3) Types And Structures of Data in R ***********************************

# 3.2 VECTORS IN R ---------------------------------------------------------------------------

# Undersøg vector (var./culm.) yield i dataframe potato
potato$yield

# Vectore kan lægges sammen, ganges ect.:

potato$P81

potato$P82

potato$P81 + potato$P82

# Alt.:

with(potato, P81 + P82)

# Ligeledes med funkt., eks:

sqrt(potato$yield)

# DU kan skabe din egen vector med kommandoen 'c()' - (c : concatenation)

x <- c(2,5,6,9,17)
# du har nu skabt vectoren (var./culm.) x
x
# See. Her har du lagt flere vectore sammen til en lang vector. 
# Denne kan du forlænge med en anden:

y <- c(21,29)
c(x,y)
# See..

# Andre smarte funktioner:

w <- rep(2, times=6)
w
# En vector med 6 gange værdien 2

u = rep(c(1,2,3), each=4)
u
# En vector med 4 gange værdierne 1, 2 og 3

z <- seq(from=1, to=15, by=3)
z
# Giver en vector fra 1 til 15 med spring af 3 (narturlige tal).

y <- 1:5
y
# En vector med alle værdier fra 1 til 5 (naturlige tal).

(1:25)^2

# 3.2.1 HOW TO SELECT ELEMENTS OF A VECTOR ---------------------------------------------------------------------

attach(potato)
yield
yield[1]
# Har valgt værdien i første row

yield[2:4]
# Rækkerne 2 til 4

i <- c(1,4,7)
yield[i]
# Samler row's 1,4 og 7 under betegnelsen 'i', hvorefter de kaldes frem.

yield[-1]
yield[-i]
# Her ser vi hvordan vi undelader specifikke row's

detach(potato)

# 3.3 LOGICAL VALUES-------------------------------------------------------------------------------------------

attach(potato)
yield
yield>400
# Logisk analyse ved FLASE/TRUE

cut400 <- (yield>400)
yield[cut400]
# Bruger en logisk funktion til at samler alle row's med værdi > 400
# Nu findes en sub-vector kan fremhæves

# Dette kan også gøres med mere direkte indexsering:
i <- c(3,6)
yield[i]
# Men en logisk indexering er ofte mere passende.

# Eks:
yield[P82 == 0]
# Har ses udbyttet på de plots uden gødning i året 1982

which (cut400)
# Viser hvil cases (rows) hvor de logiske betingelser i cut400 er opfyldt

!cut400
# '!' skal generelt forståes som 'not' og vender i dette tilfælde om på TRUE/FALSE
# De andre logiske operationer er: '==', '<', '<=', '>', '=>' and '!=' (not equel)

# Logiske operationer kan samles via '&' (and) eller '|' (or).
#Eks.
P81
P82
(P81>0) & (P82>0)
# Viser de tilfælde hvor udsagnet er sandt i begge vectore.

(P81>0) | (P82>0)
# Viser de tildfælde hvor udsagnet er sandt i mindst en af vectorene

detach(potato)

# 3.4 MISSING VALUES -----------------------------------------------------------------------------

potatoNA <- read.table(file = "potatoNA.txt", header = TRUE, dec = ".")

potatoNA$yield

potatoNA$yield[1] + potatoNA$yield[2]
# Lægger vi værdierne sammen for vi NA, de den ene er = NA

sum(potatoNA$yield)
# Vi kan heller ikke få vectorens samlede sum.

sum(potatoNA$yield, na.rm = TRUE)
# Således køre vi uden NA. Notér at na.rm = FALSE er defuelt

# ALt. kommandoen is.na:
is.na(potatoNA$yield)
sum(potatoNA$yield[!is.na(potatoNA$yield)])

# 3.5 MATRICES ----------------------------------------------------------------------------------------

A <- matrix(1:6, nrow = 2, ncol = 3)
A
B <- matrix(1:6, nrow = 3, ncol = 2)
B
# Giver sig selv. 
# De kan nu ganges:

A%*%B

# De overstående er pr. defualt fyldt colm. by colm. Efter ønske kan man sætter byrow=TRUE
A <- matrix(1:6, nrow = 2, ncol = 3, byrow = TRUE)
A
# See...

# Kommandoen 'det()' kan bruges til at finde determinaten
# Kommandoen 'solve()' kan bruges til at udregne den inverse matrix.

# 3.5.1 HOW TO SELECT ROWS; COLUMS -----------------------------------------------------------------------

x <- matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE)
x

# Hvis du gerne vil vælge en værdi bruges der - ligesom ved vectore - klammer.
# Nu blot med to tal; '[row,colm.]'

x[2,3]

# VIdere kan du vælge og fra vælge specifikke værdier.
# Eks's

x[1:2, c(2,4)]

x[2, -3]

x[2:3,]
# Notér at fraværet af en værdi efter kommaet betyder 'all'

#Du kan videre bruge logisk indeksering - se s.31 -32



#*********************************************** 4) Data Handling In R **************************************----

# 3.1 HOW TO SELECT PART OF A DATAFRAME ---------------------------------------------------------------------------

# Vi laver nu et sub-set i vore potato data:

data0 <- subset(potato, P81==0)
data0

# Alt.:

data0 <- potato[potato$P81 == 0,]
data0

# Således alle de opservationer hvor der ikke blev gødet i 1981
# Notér at du ved 'subset()' functionen ikke behøver specificerer 'potato$' da dette er givet.

# 3.2 COMBINING DATA ---------------------------------------------------------------------------------------------- 
# Antag at opservaringer til dataframen 'potato' kom fra tre dataset: dataO, data30 og data60.
# Først skaber vi dem for øvelsens skyld:

data0 <- potato[1:3,]
data0

data30 <- potato[4:6,]
data30

data60 <- potato[7:9,]
data60

# Vi samler det med 'rbind()' (rowbind)

potatoSAM <- rbind(data0,data30,data60)
potatoSAM

# Ligeledes med 'cbind()' (columbind)

data81X2 <- potato[1]*2
data81X2
# vi har nu colm. med værdierne  '81'*2

potato81X2<- cbind(potato, data81X2) 
potato81X2

# Du kan også selv skabe nye var:

potato$in.yield <- log(potato$yield)
head(potato, n=4)

# Videre kan functione 'merge()' bruges. Se s. 35 eller '?merge'

# 4.3 ANALYSIS OF DATA SPLIT INTO SUBGROUPS -------------------------------------------------------------------------

by(potato$yield, INDICES = list(factor(potato$P81)), FUN = mean)
#Super lækker funktion - den giver dig gennemsnittet af 'yield' i sub-grupperne (0 fosfor, 30 fosfor og 60 fosfor.)

by(potato$yield, INDICES = list(factor(potato$P81)), FUN = median)
# Samme ide, men her medianen istedet for mean

# Alt.:
a1 <- aggregate(potato$yield, by=list(factor(potato$P81)), FUN=mean)
a1
# See..

# 4.5 HOW TO WORK ON A DATA FRAME -----------------------------------------------------------------------------------

#For at undgå lange navne på var bla$bla iog stadig undgå 'attach()' kan man gøre tre ting.

# 1) Som vist tidligere:

lm1 <- lm(yield~P82, data = potato)
summary(lm1)

# 2) Med en hit-and-run attach/detach

# 3) Med with kommandoen:

lm2 <- with(potato, lm(yield~P82))
summary (lm2)

lm3 <- with(potato, lm(yield~P82 + P81))
summary(lm3)

# Vil du lave ændringer i dit dataframe kan du bruge 'transform()'

potatoT <- transform(potato, ln.yield=log(yield), sqrt.yield=sqrt(yield))
potatoT
# NICE! - let måde at indsætte nye variabler/vectore


#===============================================================================================================
#                                         II - For Statistical Analysis ----
#===============================================================================================================

#*************************************** 5) R Functions For Data Presentation ******************************----

load("milkyield.rda")
str(milkyield)
head(milkyield)

# 5.1 FUNCTIONS LISTING DATA OR DATA SUMMARIES-------------------------------------------------------------------

data1 <- subset(milkyield, Group == 1)
str(data1)
# Noter dig at i 'milkyield' er var. Group med både 1 og 2. nu er disse sorteret.

y1 <- data1$Yield
# Nu har du udtrykket værdierne

length(y1)
# Columen (vectoren) er altså 32c værdier 'lang'

#Følgende er en række kommandoer til brug på en sådan vector

sum(y1)
# Summe af alle værdierne i vectoren

mean(y1)
# Gennemsnittet af værdierne

sd(y1)
# Standart deviation

var(y1)
# Correlation, Variance an Covariance... (?)

median(y1)
# medianen

min(y1)
max(y1)
# Min og max værdierne

which.min(y1)
which.max(y1)
# Nummeret på min og max opservationen

# Vi undersøger nu et dataset nærmere:
load("hydrolysis.rda")

head(hydrolysis)
str(hydrolysis)
# See...

table(hydrolysis$feed)
# 'feed' er en factor med fem cat.- her ser vi hvor mange obs. der er i hver cat..

table(hydrolysis$feed, hydrolysis$hour)
# er ser du en tabel mellem 'hour' (8,16,24...) og 'feed' (barley, fish...) 
# og hvormange gange en given kombination optræder (spoiler; 2 gange.. alle sammen..). 

# 5.2 GRAPHICAL FUNCTIONS ----------------------------------------------------------------------------

plot(potato$yield, potato$P81)
# 'plot(x,y)' : Scatterplot for to lige lange vectore

points(potato$yield, potato$P81)
# adds points to current plot.. (de er der allerede på et scatterplot)

lines(potato$yield, potato$P81)
# adds lines

boxplot(potato$yield)
# Boxplot af værdierne i x

hist(potato$yield)
# Histogram af værdierne i x

pairs(potato)
# Matrix-scatterplot af par i dataframe 'potato'.

qqnorm(potato$yield)
# qq-plot for at se om data er normaltfordelt.

# For 'interaktionplot' se s. 43 etc.
# Kommandoen 'par()' skulle vist også være god....


#5.2.1 THE PLOT FUNCTION ---------------------------------------------------------------------------------------

load("hydrolysis.rda")
head(hydrolysis)


hydro1 <- subset(hydrolysis, feed == "barley")
# Subset for alle opservationer der blev fodret "barley"

plot(hydro1$hour, hydro1$serine)
# Af observationerne i subsætte (feed == 'barley') 
# visualisere vi nu evt. sammen hæng mellem værdierne i var. 'hour' og 'serine'.

plot(hydro1$hour, hydro1$serine, 
     main = "Serine",          # Hoved title
     xlab = "Time (h)",        # Label på x-aksen
     ylab = "Concentration",   # Label på y-aksen
     xlim = c(0,80),           # Øvre grænse for x-aksen
     ylim = c(3,4.5))          # Øvre grænse for y-akse

# Muligheder:
plot(hydro1$hour, hydro1$serine, 
     main = "Serine",          # Hoved title
     xlab = "Time (h)",        # Label på x-aksen
     ylab = "Concentration",   # Label på y-aksen
     xlim = c(0,80),           # grænser for x-aksen (nedre,øvre)
     ylim = c(3,4.5),          # grænser for y-aksen (nedre,øvre)
     pch = 16,                 # 'Plotting character' nummeret korrespondere med en type 'punkt'. se ?pch for muligheder.
     col = "brown",            # Farven på pch typen
     cex = 1.3,                # Størrelsen på pch punkterne
     cex.lab=1.3,              # Størrelsen på xlab og ylab
     cex.axis=1.3)             # Størrelsen på v værdierne på x-aksen og y-aksen             
     
# Med funktionen 'points' kan vi indsætte punkter i det plot vi arbejder i.
# Vi laver et nyt subset_

hydro2 <- subset(hydrolysis, feed == "fish")

#Og nu et nye points

points(hydro2$hour, hydro2$serine, 
       col = "blue",
       pch = 1)

# Flotte nye points :)

# Kan også leger med typer af plot:

# Muligheder:
plot(hydro1$hour, hydro1$serine, 
     type = "l",              # Type af plot. l = lines. se +plot for oversigt
     main = "Serine",
     xlab = "Time (h)",
     ylab = "Concentration",
     xlim = c(0,80),
     ylim = c(3,4.5),
     pch = 16,
     col = "brown", 
     cex = 1.3, 
     cex.lab=1.3, 
     cex.axis=1.3)             


# Ligesom det var tilfældet med points, kan du også indsætte linjer:

lines(hydro2$hour, hydro2$serine, 
      lty = 2,                # Giver en stiplede linje. 1 (defuelt) = solid. 3 = punkter
      col = "blue",
      pch = 1)

#Vi kan tilføje en linje (y=a+bx) til det nuværende plot. 'a' = skæringspunkt 'b' = hældning

abline(a="3.5", b="0.1")

# 'h' giver en horisontal linje og 'v' en vertilkal. se evt '?abline'

# 5.2.2 HOW TO ADD A LEGEND --------------------------------------------------------------------

# Vi konstruere nu et plot med alle typer af obs på.
# Hvorfor vi først genere subset'en

hydro1 <- subset(hydrolysis, feed == "barley")
hydro2 <- subset(hydrolysis, feed == "fish")
hydro3 <- subset(hydrolysis, feed == "mais")
hydro4 <- subset(hydrolysis, feed == "meat")
hydro5 <- subset(hydrolysis, feed == "soy")

plot(hydro1$hour, hydro1$serine,
     main="Serine",
     xlab = "Time (h)",
     ylab = "Consentration",
     ylim = c(3,6),
     pch = 16,
     col= "brown",
     cex = 1.3, cex.lab = 1.3, cex.axis = 1.3) 

# Vores plot. Nu indsætter vi de andre subset:

points(hydro2$hour, hydro2$serine, 
       col = "blue",
       pch = 1)

points(hydro3$hour, hydro3$serine,
       col = "red",
       pch = 2)

points(hydro4$hour, hydro4$serine,
       col = "black",
       pch = 3)

points(hydro5$hour, hydro5$serine,
       col = "brown",
       pch = 4)

# Lækkert - nu tilføjer vi legends (forklaringer)

legend(x=60, y=6,
       legend = c("barley","fish","mais","meat","soy"),
       col = c("brown","blue","red","black","brown"),
       pch = c(16,12,3,4))

# Notér dig at x=60, y=6 giver kordinator for forklaringsboksen.
# Du kan også bruge 'locator kommandoen'. se s. 46-47


# 5.2.3 HOW TO SAVE THE PLOTS ------------------------------------------------------------------

savePlot("plot1", "png")
savePlot("plot1", "pdf")
# Virker vist kun på Windows??? -> heldigt der er en 'export' knap..


# Husk du kan åbne et ekstra plot vindue vedd følgende kommando:

x11()

# 5.2.4 FUTHER GRAPHICAL UTILITIES --------------------------------------------------------------

# Til indentifikation af eks. outlier:

identify(hydro1$hour, hydro1$serine, labels = "patient")
# Lækkert. Husk dog bruge det rigtige x (eks; hydro1 og ikke 2) 
# og at trykke 'Esc' el. 'finish' for at få label'en frem.

# Pakken 'lattic' skulle havde endnu mere sjov. 

#*************************************** 6) Basic Statistical Functions ***********************************----

# 6.1 THE NORMAL DISTRIBUTION ---------------------------------------------------------------------------------

# Vi har nu x med 'mean' = 4200 og 'sd' = 420. 
# vi undersøger nu sandsynligheden for at få værdien 4500 inde for denne fordeling:

pnorm(4500, mean = 4200, sd = 420)
# Sandsynligheden for at få værdien 4500 er således 76.24% :)

# Sandsynigheden for ikke at få 4500 findes ved:

1-pnorm(4500, mean = 4200, sd = 420)

# Alt.:

pnorm(4500, mean = 4200, sd = 420, lower=FALSE)

# Altså 23.75 i begge tilfælde
# Obs: pr. defuelt er 'mean' = 0 og 'sd' = 1

# 'Quantiles' (egenligt 25% og 75% men alle percentiler kan specificeres) værdierne kan findes ved 'qnorm'

qnorm(0.5, mean = 4200, sd = 420)
# Ikke overraskende er 50% = 4200

qnorm(0.75, mean = 4200, sd = 420)
# 75% = 4483.29

# 'pnorm' og 'qnorm' er således inverse i forhold til hinanden

# random værdier fra en given normal distrubution kan gives ved:

rnorm(n=6, mean = 15, sd=2)
# 'n' = antal værdier du ønsker. 'mean' og 'sd' definere, som før, distrubutionen

# Sidte funktion er 'dnorm' som giver distrubutionens densitet

# Vi har her at gøre med z-distrubutionen. Ønsker man en t-distrubution bruges 'pt', 'qt', 'rt' og 'dt'. 
# Her skal nogen andre parametre dog specificeres. her i blandt 'df' (degress og freedom = n-1).

pt(4500, df = 0.120)
# Nope - fatter det ikke helt... Er heller ikke vigtig for n>100

# Vi kan undersøge antagelsen om normal fordeling med et Q-Q plot.
# VI laver et subset af 'milkyield'

load("milkyield.rda")

data1 <- subset(milkyield, Group == 1)

qqnorm(data1$Yield) # Laver plottet
qqline(data1$Yield) # TIlføjer linje

# Lækkert.

# Kolmogorov-Smirnov's test for normal fordeling:
ks.test(data1$Yield, "pnorm", mean = 4708, sd = 648)

# Hypotese test. Forkastes nul hypotesen er der ikke tale om en normalfordeling
# Da p > 0.05 er der tale om en normalfordeling
#'mean' og 'sd' er dog ikke sande? - brøver med de rigtige værdier:

mean(milkyield$Yield)
sd(milkyield$Yield)
ks.test(data1$Yield, "pnorm", mean = 5017, sd = 703.6939)
# Så kan vi pludseligt forkaste nul-hypotesen?
# Vi tjekker visualisering:
hist(milkyield$Yield)

# Ser tvivlsomt ud...

# Shapiro-Wilks' test:

shapiro.test(data1$Yield)
# Virker super fint. Nul-hypotese test. Da p>0.05 forkaster vi alternativ hypotesen. Det er en normalfordeling.
# Så hvad er foreskellen mellem de to test?
# Det er noget med at Shapiro er mere sensitiv, men kun til egentlige normal fordelinger (z)
# Kolmogorov er mindre sensitiv, men kan bruge på flere distruputions typer (eks. t formoder jeg)
# Er dog stadig i tvivl om hvorfor overstående resultater bliver så forskellige...
# Brug 'shapiro.tast()' indtil vidre.

# 6.2 PARAMETRIC TWO-SAMPEL TEST --------------------------------------------------------------------

# paired t-tset. Vi bruger dataframen 'niacin'

load("niacin.rda")
niacin

# Vi undersøger nu om der er foreskel på 'pre' og 'after'.
t.test(niacin$pre, niacin$after, paired = TRUE, conf.level = 0.95)

# vi forkaster således nul-ypotesen og acceptere alternativ hypotesen.
# Samme resultat kan vi få "i hånden" - vi bruger 't' og 'df' fra t-testen :

2*(1-pt(2.6558, df=7))

# Alt.:

2*pt(2.6558, df=7,lower.tail = FALSE)

# Vi får samme resultat.

# Overstående var en paired test; før og efter. En 'indepoendent sample test' ser således ud:

load("milkyield.rda")

data1 <- subset(milkyield, Group == 1)
data2 <- subset(milkyield, Group == 2)

t.test(data1$Yield, data2$Yield, var.equal = TRUE)
# Forkaster nul-hypotese
# Vi har her antaget ENS VARIANS ('var.equal = TRUE'). Pr. defualt er 'var.equal = FALSE'
# Eks.:

t.test(data1$Yield, data2$Yield)
# See...
# Ønsker vi at undersøge om der er tale om ENS VARIANS bruger vi Bartlett test:

bartlett.test(Yield~Group, data = milkyield)
# Med p > 0.05 (0.8675) kan vi godt antage ENS VARIANS.

# 6.3 DISCRETE DISTRIBUTION (CAT. VAR.)--------------------------------------------------------------

# Binominal fordeling. Til catagoriske var og lille n. Fortalle noget med sandsynligheden for SUCCES
# Vi antager nu at vi har n = 5 (eks. 5 forsøg på at "plat" i plat og krone) og p = 1/2 
# Vi undersøger nu sansynligheden for at får ikke at få plat i nogen af vores førsøg.
# VI bruger kommandoen 'dbinom()'

dbinom(0, size = 5, prob = 1/2)
# Der er 3% sandsynlighed for at få plat 0 gange ( 0 succes)

# Nu med 1/3:

dbinom(0, size = 5, prob = 1/3)
# 13% sandsynlighed for 0 succes

dbinom(0:5, size = 5, prob = 1/3)
# Her ser vi sandsynlighederne 0,1,2,3,4 og 5 succeser

# Vi kan agrragere sandsynligehederne med sum kommandoen:

sum(dbinom(0:5, size = 5, prob = 1/3))
# Sandsynligheden for et få enten 0,1,2,3,4 eller 5 er self. 100%
# Alt. 'pbinom()' funktionen

pbinom(5, size = 5, prob = 1/3)
# Samme resultat :)

# Vi kan også genere 20 (eks.) random observationer med kommandoen 'rbinom':

rbinom(20, size = 5, prob = 1/3)
# See..

# Vi kan genere en ator sampel der kan give os et teoretisk mean, som burde være tæt på det empiriske:

x <- rbinom(20000, size = 5, prob = 1/3)
mean(x)

# vi tjekker:

5/3
# Close enough

# Og naturligvis qbinom til at finde 'Quantiles' (percentiler)

# Nu kikker vi på 'MULTINOMAL DISTRIBUTION'. Her kan outkommet være mere en blot 1/0
# Lad os antage en situation med 4 mulige udfald pr. obs.
# Her er de sande sandsynligheds parametre 0.49, 0.23, 0.04, 0.23.
# Vi generere en sample fra denne multinominelle distrubution med n=142:

rmultinom(1, 142, prob = c(0.49, 0.23, 0.04, 0,23))

# Og også udregne 'density function' (??) i et givent punkt.
# Eks. X1 = 70, X2 = 33, X3 = 6 og X4 = 33

dmultinom(c(70, 33, 6, 33), size = 142, prob = c(0.49, 0.23, 0.04, 0.23)) 
# Virker, men skal nok lige arbejde med den her lidt længere....

# Nu 'THE PISSION DISTRUBUTION'

# vI SPRINGE LIGE FRA S. 57 - 67 :)

#*************************************** 7) Linear Normal Models ********************************************----

# 7.1.1 ONE-WAY ANOVA ---------------------------------------------------------------------------------------------------
# Har er alle x factore

load("chlorophyll.rda")
plot(chloro~treat, data = chlorophyll) #Scatter
boxplot(chloro~treat, data = chlorophyll) #Box

str(chlorophyll)
# Vi ser at treat er int. Den skal i sagens natur (ANOVA være en factor:

chlorophyll$treat <- factor(chlorophyll$treat)
str(chlorophyll)
# Cool - vi laver nu vores model:

model1 <- lm(chloro~treat, data = chlorophyll)

# Du kunne også havde lavet 'factor' on the go: 'lm(chloro~factor(treat), data=chlorophyll)'

# Vi bruger nu (one-way) ANOVA. Her er y en met. var. og x en cat. var..
# ANOVA kan fortælle os om gennemsnittet i mindste to af katagorierne er sig. forskelllige fra hinanden.
# Vi analysere nu med anova:

anova(model1)

# Med p < 0.001 er der sig. forskel på gennemsnitte i mindst to grupper.
# Vi ved dog ikke hvilke eller hvor mange grupper der er forskel på.
# Og vi ved heller ikke noget om hvile forskelle der er tale om.

# vi kan også sammenligne en model uden treatment med en med treatment:

model2 <- lm(chloro~1, data = chlorophyll) # treat = 1 = no treatment

anova(model2,model1)
# Tjaaa, er lidt i tvivl om havd der foregå her..

# PARAMETRE ESTIMATERNE findes ved kommandoen 'summary':

summary(model1)
# Her er der tale om en god gammal lin. reg med cat. var.
# Ref. cat = treat1.
# Du har Intercept det er li''constant' i stata. estimat der er li' coef. osv.

# Du kan omgå 'intercept' og således også får alle de egenlige værdier 
# uden at skulle lægge dem sammen med intercept først.

model1a <- lm(chloro~treat-1, data = chlorophyll)
summary(model1a)

# Her skal det dog noteres at p-værdien må angive foreskellen fra 0
# da der ikke er en ref. cat..

# CONFIDENTS INTERVALLER:

confint(model1)
# See

# Igen kan man også intercept og regnestykkerne over:
confint(model1a)

# Ændre REF. CAT.
# Ikke ligesså let som i stata, men det er self. flere måder at gøre det på.
# R Tager pr. defuelt den katagorie der kommer først - nummerisk eller alph.
# Vi kikke på to løsninger. I begge tilfælde skal vi konstruere en nu model.
# Den første metode ændre ikke på data. 
# Her benyttes følgende funktion på x-var's plads: "C('var',contr.treatment(n, base = 'ref.cat.'))"

model1b <- lm( chloro~C(treat,contr.treatment(3, base=2)), data=chlorophyll )
summary(model1b)

# Alt. kan du skabe en ny var i dataframen, hvor ref. cat pr. defuelt er en anden ('relevel'):

chlorophyll$newtreat <- relevel(factor(chlorophyll$treat), ref = 2) 
model1c <- lm(chloro~newtreat, data = chlorophyll)
summary(model1c)

# Samme resultat.. Ved ikke hvad jeg lige synes er hottest..

# 7.1.2 TWO-WAY ANOVA. MULTI-WAY ANOVA. -------------------------------------------------------------------------
# Interaktion (!!)

load("organic.rda")

model1 <- lm(matter~vet+tim+vet:tim, data = organic)
anova (model1)
summary (model1)

#Alt.:

model1a <- lm(matter~vet*tim, data = organic)
anova(model1a)
summary(model1a)
# Same..

# Du kan også nøjes med leddet:

model1b <- lm(matter~vet:tim, data = organic)
anova(model1b)
summary(model1b)

# Alt. kunne man også havde sammen lignet to modeller. En med interaktion og en uden:

model2 <- lm(matter~vet + tim, data = organic)
anova(model1, model2)
# Altså ingen sig. foreskel på (gennemsnittet i) de to modeller.
# Tolkningen er nok at den at Model 1 bruges som ref. for Model 2.
# Notér dig at de lablen "Model 1" og "Model 2" i outputtet er uafhængigt af de navne du selv har valgt.

# Vi kan vidre også sammenligne med en model med færre var.:

model3 <- lm(matter~vet, data = organic)
anova(model3, model2)

# Begge var. et altså isg. og interessante.
# Lækkert værktøj. Notér dig at jo midre RSS (Residual Sum of Squares) self. skal være så lille som muligt. 
# P-testen må skulle forståes således at der er sig. forskel på modellerne
# men det er vel mig der skal aflæse RSS for at se hvilken en der bedst?

# Paralle imod 'Succesiv'e tests:

# I overstående blev modellerne hold op i mod den additive model: model2.
# På den måde blev modellerne teste parallet op mod hinanden.
# VI kan også gøre det 'successivt' dvs. drope eller adde en var af gangen.
# Dette gøred med hendholdsvis 'add1' og 'drop1' kommandoen. EKs:

drop1(model2, test = "F")
# Det burde være simpelt, men er sgu lidt i tvivl om tolkningen..
# Skulle efter sigene være godt til interaktion..

# INTERAKTION PLOTS.

# Man kan her med fordel bruge 'attach', men for sportens skyld gøres dette ikke her.

interaction.plot(x.factor = organic$tim, trace.factor = organic$vet, response = organic$matter)
# 'x.factor' = x-aksen. 'trace.factor' = grafer. 'respons.factor' = y-akser.


interaction.plot(x.factor = organic$vet, trace.factor = organic$tim, response = organic$matter)
# See.Respons vil altid være den afhængige variable, men 'trace' og 'x' kan man jo se på.

# 7.2.1 SIMEL LINEAR REGRESSION -------------------------------------------------------------------------------

# Vi konsturer to variabler x(acid) og y(fat). 
# (Noget med intagelse af fedt, og en bestemt slags syre..)

x <- c(29.8,30.3,22.6,18.7,14.8,4.1,4.4,2.8,3.8)
y <- c(67.5,70.6,72.0,78.2,87,89.9,91.2,93.1,96.7)

model1 <- lm(y~x)

anova(model1)
# Ved ikke om det giver meget mening når der er tale om metriake var?

summary(model1)
# Fint og flot som vi kender det.

# Alt. kune man havde lavet en model med og uden x og sammenlignet dem:

model2 <- lm(y~1)

anova(model2, model1)
# Kan godt lide ideen om at sammenligne modeller, men skal lige fatte det helt.

# 7.2.2 MULTIPLE LINEAR REGRESSION ------------------------------------------------------------------------------

# Vi indsætter flere x blot ved '+'. (eget) eks:

nyvar <- c(1,2,3,4,5,6,7,8,9)

model3 <- lm(x~y + nyvar)

summary(model3)

# Et special eksempel er regression hvor vi ønske at kvadrer et led 
# (kvadratisk transformation)

# Vi ser på højden af træer som en funktion af diameteren:

h <- c(32,31,30,29,29,28,25,23,20,18,17,17,16,16,15,13,11,11)
d <- c(22.7,22.7,22.6,22.6,21.9,21.9,21.8,21.0,20.4,18.6,19.2,18.9,18.5,18.1,17.7,17.2,16.5,15.5)
d2 <- d^2
plot(d,h)

# Vi teste nu en kvadredet model mod en 'normel':

model1 <- lm(h~d+d2)
model2 <- lm(h~d)
anova(model2,model1)
# Her konkludere din bog at sammenhængen ikke er linear.
# Det giver også fint mening hise det handler om at de skal være sig. forskellige
# og RSS skla være mindst... Men jeg er stadig ikke sikker på den tolkning.

summary(model1)
summary(model2)

# Her ser vi tydeligt (og på en måde vi forstår) at sammen hængen ikke er linær,
# Men altså 'kurvet', da det kvadrede led er signifikant.

# Jeg tænker vi kan gøre det samme med en logarikmisk transformation:

dlog <- log(d)

model3 <- lm(h~d+dlog)
anova(model2,model3)
# Virker fint. men resultatet ligner jo lidt det vi så før..

summary(model3)
# Her ser vi jo også at leddet som sig, om end mindre end d2.. nok om det.

# 7.2.3 TEST FOR LINEARITY -------------------------------------------------------------------------------------

# VI bruger her datasættet 'hydrolysis':

load("hydrolysis.rda")
head(hydrolysis)
str(hydrolysis)

# Vores y (serine) giver vi en log. trans.
# Og vi laver en ekstra var. hvor hour er en factor:

hydrolysis$logserine <- log10(hydrolysis$serine)
hydrolysis$hourfac <- factor(hydrolysis$hour) 

head(hydrolysis)
str(hydrolysis)
# Således.

# Først to modeller med hour som at. var.
# Med med interaktion og en uden.

# Fordi dette her ellers bliver meget langt, sætter vi attach til hydrolysis...

attach(hydrolysis)

model1 <- lm(logserine~feed:hourfac)
model2 <- lm(logserine~feed+hourfac)

# Igen ANOVA for at se hvilken model der passer bedst
# (det gør de hele tiden så du bliver nød til at fatte hvorfor...)

anova(model2,model1)
# Pas

# Nu prøver vi med tid som en metrisk variabel

model3 <- lm(logserine~feed+hour)
model4 <- lm(logserine~feed+hour+feed*hour)

anova(model3,model2)
# Gør som bogen siger, alt ser rigtigt ud. men er sgu lidt i tvivl om tolkningen.

# ESTIMATION:
# model3 viser sig (åbenbart) at være den bedste så den bruges nu:
# Vi ønsker at ser de fulde estimater - altså ikke i forhold til 'intercept'
# Vi bruger derfor '-1' funktionen:

model3a <- lm(logserine~feed+hour -1)
summary(model3a)

# Conf. findes med 'confit' og den par-vise estimation vises i 7.4 (estimable)

detach(hydrolysis)

# 7.3 MODEL VALIDATION -----------------------------------------------------------------------------------------

load("hydrolysis.rda")
attach(hydrolysis)

# Vi konstruere model3 fra før.

logserine <- log10(serine)

model3 <- lm(logserine~feed+hour)

model3a <- lm(logserine~feed+hour-1) #Uden ref. cat.

summary(model3)
summary(model3a) # Bare for sjov og øvelse :)

# 7.3.1 ANALYSIS OF RESIDUALS (Normalfordelte fejlled og Heteroskedasticitet)------------------------------------

pred3 <- predict(model3) # Predicted values
res3 <- residuals(model3) # Raw residuals
sres3 <- rstandard(model3) # Standardized residuals

# Disse objekter kan nu bruges til residual plots ('plot()') og QQ-plots ('qqnorm()') 

plot(pred3, res3) # TJEKKER FOR VARIANSHOMOGENITET/HETEROSKEDASTICITET 
abline(h=0) # Giver en horisontal linje til sammenligning

plot(pred3, sres3) # TJEKKER FOR VARIANSHOMOGENITET/HETEROSKEDASTICITET 
abline(h=0) # Giver en horisontal linje til sammenligning

qqnorm(sres3) # TJEKKER FOR NORMALFORDELTE FEJLLED
abline(a=0,b=1) # GIver dig en 45* linje at sammenligne med

# Her ser alt godt ud; Residualerne falder fint omkring 0
# Og i QQ-plottet ligger de fint på linjen

# 7.3.2 TRANSFORMATION. BOX-COX ANALYSE -------------------------------------------------------------------------

# Antag nu at der var problemer med model specifiketionenerne.
# Vi konstruere nu ne model hvor 'serine' ikke er log. trans:

model3.orig <- lm(serine~feed+hour)
plot(predict(model3.orig), rstandard(model3.orig)) # On the fly..
abline(h=0)
# Vi ser at der er et problem med varianhomogeniteten
# positive værdier i enderne, negative værider i midten.

# Vi kan bruge 'boxcox()' til at undersøge hvilken model transformation der er passende
# Først:

library(MASS)

boxcox(serine~feed+hour) 

# Da den optimale værdi (midten) er meget tæt på λ = 0 er en log. trans den korrekte
# Ved λ = 1 går ikke du fra en transformation er nødvendig
# Ved λ = 2 tænker jeg at en kvadratisk transformation er på sin plads
# Er dog ikke helt sikke.

# Test for λ = 1

y <- c(1,2,3,4,5,6,7,8,9)
x <- c(2,4,6,8,10,12,14,16,18)

modelxy <- lm(y~x)

boxcox(modelxy)

# Den optimale værdi lægger som forventet på λ = 1

# Test for λ = 2

x2 <- c(1,4,9,16,25,36,49,64,81)

modelxy2 <- lm(y~x2)

boxcox(modelxy2)

# Den optimale værdi lægger som forventet på λ = 2

# MEGA LÆKKERT!

detach(hydrolysis)

# 7.4 ESTIMATION OF CONTRAST -----------------------------------------------------------------------------------

load("hydrolysis.rda")

attach(hydrolysis)

# Vi så i 7.1.1 hvordan vi kunne skifte ref. cat. med relevel.
# I tilfældet med med cat. var. 'feed' skal vi så lave fire moddeler da der er 5 cat.
# Vi kan dog også bruge funktionen 'estimable' fra pakker 'gmodels'.

library(gmodels)

# Først laver vi en model uden med ref. cat.
# (Og stadig med y = log(serine))

model3a <- lm(log10(serine)~feed + hour)

summary(model3a)

# For en bedre foklaring af hvad der her sker, se s. 87. se evt '?estimable'
# Men: 0 og 1'erne korrespondere til de cat du gerne vil undersøge.
# rbind (row bind) konsturer en matirx som vi kalder 'contr'
# Disse sætter vi ind i estimable funktionen og ber' om et conf. int. på 0.95

contr <- rbind('fish-mais' = c(0,-1,1,0,0,0))
estimable(model3a, contr, conf.int = 0.95)

# Virker fint og med p<0 går jeg ud fra at jeg kan forkaste nul-hypotesen (??)

# Vi kan også undersøge forskellen på 'barley' og 'meat' i den 16'ne time således:

barley.est <- c(1,0,0,0,0,16)
meat.est <- c(0,0,0,1,0,16)
est <- rbind(barley.est, meat.est)
estimable(model3a, est, conf.int = 0.95)

# Kikker vi på conf. int. tyder det på at vi faktisk her tester for nulhypotese mellem de to...
# Bogen siger nu noget med en log-transformation - jeg går ud fra det er logserine
# og bekymre mig ikke mere herom...

# Prøver lige noget:

fish.est <- c(0,1,0,0,0,0)
mais.est <- c(0,0,1,0,0,0)
soy.est <- c(0,0,0,0,1,0)
est <- rbind(barley.est, fish.est, mais.est, meat.est, soy.est)
estimable(model3a, est, conf.int = 0.95)

# Du får i hvet fald conf. int. der tyder på at du kan forkaste nul-hypotesen
# mellem hver cat. men er stadig ikke sikker på du kan tolke p-værdien således...

# Adjustet Means With 'estimable'
# Adjusted means el. least squares mean el. bare LS-means..

# Er ikke sikker på hvad det her kan så vender tilbage til det når 
# det bliver nødvedigt. dvs. springer s. 89 over..

#*************************************** 8) Models With Random Effects ***********************************----

# 8.2 ANALYSIS OF MIXED LINEAR MODELS (lme, lmer) ------------------------------------------------------------

# 8.2.1 Analysing a singel random factor

# Vi bruger datasættet 'porkers' der omhandler mørheden af svinekød... (!)
# Og pakken 'nlme'

load("porkers.rda")
library(nlme)
str(porkers)

model1 <- lme(tenderness~chilling+ph+ph:chilling, random =~ 1|porker, method = "REML", data = porkers)
# Noter dig her at datasættet hedder 'porkers', man var. hedder 'porker'....

# undersøger antagelrser:

plot(model1)
# Hvor er qq-plottet?

summary(model1)

# VI bruger ANOVA til at undersøge om moddelen med interaktion er bedre end den uden
# (Nested model)
# Husk her at bruge 'ML' og ikke 'REML'

model1.ML <- lme(tenderness~chilling+ph+ph:chilling, random =~ 1|porker, method = "ML", data = porkers)
model2.ML <- lme(tenderness~chilling+ph, random =~ 1|porker, method = "ML", data = porkers)

anova(model1.ML,model2.ML)

# Forskellen på 'loglik' er li' 'L.Ratio' hvilket holdt op i mod chi-square dist. og df giver p=0.66
# Her ser det altså ikke ud til at interaktionen er nødvendig (vel?)

# Vi gør vidre med 'model2.lm' og tjekker nu om 'chilling' gør en forskel:

model3.ML <- lme(tenderness~ph, random =~ 1|porker, method = "ML", data = porkers)
anova(model2.ML,model3.ML)

# Da vi ikke kan forkaste nul-hypotesen går vi vidre med den mindste model (3)
# Vi holder der nu op mod en model hvor vi kun tjekker for random foreskelle (4)

model4.ML <- lme(tenderness~ 1, random =~ 1|porker, method = "ML", data = porkers)
anova(model3.ML,model4.ML)

# Da p < 0.05 okan du nu ikke bekræte null-hypotesen. Vi køre således vidre med 'model3.ML'

# Computation of p- value with parametric bootstrap:
# Vi ser på testen mellem 'model2.ML' og 'model3.ML'
# Her simulere vi nu 1000 dataset fra nul-modellen (3)
# og sammenligner den max. LR med den fra den virkelige data (2.319):

sim <- simulate.lme(model3.ML, m2=model2.ML,nsim = 1000, method = "ML")
lrsim <- 2*(sim$alt$ML - sim$null$ML)
psim <- sum(lrsim>2.319)/1000
psim

# Således stiger vores p-værdi fra 0.1278 -> 0.13
# Vi benytter model3 som den færdige model, og køre den nu med "REML" 
# (som giver mere præcise resultater end "ML" men åbenbart ikke fungerede før..)

model3 <- lme(tenderness~ph, random =~ 1|porker, method = "REML", data = porkers)
summary(model3)

# Lækkert. Vi kan udføre de samme krumspring som ved lm,
# dvs. relevel, estimable osv.

# 8.2.2 TWO OR MORE RANDOM FACTORS -----------------------------------------------------------------------------

# Vi bruger datafram 'vitE' (E vitamin i kød). Vi bruger kvadratroden af 'vitE' som respons (y). 
# Ligeledes har vi var. 'lab' og 'sampel'. som skal være faktore:

load("vitE.rda")
library(nlme)

# Her skal du være forsigtig med at 'attach'e. Du for en advarsel fordi både datasæt og var. hedder 'vitE'
# Du prøver nogle ting, blandt andet 'rename' med pakken 'plyr'. det virker egenglig fint men, der er for meget ballad
# Så du køre uden attach.

vitE$sqrtEvit <- sqrt(vitE$vitE)

L <- factor(vitE$lab)
S <- factor(vitE$sample)

# Poduktet af L og S (L:S="LS") er  dan random effekt.
# I lme's random effekter kan vi ikke sætte et produkt in 'on the fly'. vi konstruere der herfor før.
# Notér dig at LS er en subdivision af L. Dette gives ved L/LS  

LS <- L:S
model1 <- lme(sqrtEvit~S, random =~ 1|L/LS)

# Og vi fitter modellen uden intercept (ref. cat.)

model1a <- lme(sqrtEvit~S - 1, random =~ 1|L/LS)

summary(model1a)

# Er lidt i tvivl om tolkningen, men bogen siger jeg skal notere mig SD for L og L:S 
# hvilke hendholdsvis er 0.245 og 0.134. Disse skal ses i forhold ti residualet på 0.6774 (?)

# Generelt om lmer: 
# lmer har nogle fordele og ulemper i forhold til lme.:
# Random factors kan være non-nested
# Der kan være mere end two random factors (det kan lme ogdå, men det er kompliceret..)
# Dog fungere lmer ikke med 'simulate' eller 'estimable'.

# Vi bruger nu dataframet 'chocolate'. (sweetness score)

load("chocolate.rda")

attach(chocolate)

# Vi tranformere nogel var. (spørg ikke hvad der foregår ved y - de skriver 'stabilize'..?)

y <- asin(sqrt(score/15))

A <- factor(assessor)
P <- factor(product)
S <-factor(session)

# Vi laver nu en model hvor P er fixed, og A,S,A*P,P*S OG A*S er random

library(lme4)

model1 <- lmer(y~P + (1|A)+(1|S)+(1|A:P)+(1|P:S)+(1|A:S), REML=TRUE)
summary(model1)
# Lidt anderledes ind i bogen, men alt er fint.

# Da Variance=~0 for S og P:S laver vi en model uden disse.
# Så sammenligner vi modeller, hvorfor vi igen bruger lm og ikke lme:

model2 <- lmer(y~P+(1|A)+(1|A:P)+(1|A:S), REML = TRUE)

model2.ML <- lmer(y~P+(1|A)+(1|A:P)+(1|A:S), REML = FALSE)
model3.ML <- lmer(y~1+(1|A)+(1|A:P)+(1|A:S), REML = FALSE)

anova(model3.ML,model2.ML)
# Resultaterne er lidt anderlese, men ikke eget. Kommandoen også lidt anderledes men pyt.
# Det vigtige er at vi ikke kan bekræfe nul-hypotesen, hvorfor vi bruger 'model2' (REML self.)

summary(model2)

# Without intercept:

model2a <- lmer(y~P-1+(1|A)+(1|A:P)+(1|A:S), REML = TRUE)

summary(model2a)

# Det virker super smart det hele, men er stadig lidt presset af tolkningen..
# Men det er i hverfald noget med at der ikke er nogle p-værdier så man må nøjes med t-dist.. se s. 100


#****************************************** 9) Repeated Measurements ***********************************----

# 9.1.1 WIDE FORM AND LONG FORM (Preliminaries) ----------------------------------------------------------------

load("goats.rda")

# Vi har 28 geder; 4 slags fodre og gedernes vægt målt over 5 gange (1 pre test ofc.).

head(goats)

# Det vi ser her kaldes "wide form": en linje pr. ged.
# Vi skal dog bruge "long form". Dvs.: en linje pr. observeret vægt (post treatment)
# Vi omformer data med 'reshape' kommandoen:

? reshape
long.g <- reshape(goats, idvar="goat", 
                  varying = list(4:7), # Vi bruger var. 4-7: post treat vægtene 
                  times=c(26,45,61,91), # Uge numrene
                  v.names="weight", # Navne på variabler i 'long' korrosponderende med variablerne i 'wide'.
                  direction = "long") # Går fra 'wide' til 'long'. 


long.g <- long.g[order(long.g$goat),] # Sotere data efter ged...

head(long.g)

# Noter dig at baseline 'w0' nu har sin egen var. Og det samme har 'time' og 'weight'. 
# Data viser nu en obs. pr. obs (so to say.) 

# 9.1.2 PROFILE PLOTS ------------------------------------------------------------------------------------------

# Vi konstruere nogel plots.
# Hendholdsvis pr. subjekt (ged) = 'subjekt profiles'. Giver os et billede af den typiske 'time respons'  
# og pr. treatment (fodder) = 'average profiles'. Giver os et billede af forholdet mellem tid og treat.

# SUBJECT PROFILES:
# Her vil vi gerne havde baseline med, Hvorfor:

long.gb <- reshape(goats, idvar="goat", 
                  varying = list(3:7), # Vi har 3 (baseline) med.
                  times=c(0,26,45,61,91), # Hvorfor det også afspejles her med 'times = 0'
                  v.names="weight",
                  direction = "long")
library(lattice)
? xyplot

xyplot(weight~time|feed, groups = goat, 
       xlab="Time (days)",
       type="l",
       data=long.gb)

# AVERAGE TREATMENT PROFILES

?with
?tapply
avedata <- with(long.gb,tapply(weight, list(time,feed), mean))
avedata

obsTimes <- c(0,26,45,61,91)

plot(obsTimes, avedata[,1],
     ylim = range(avedata),
     xlab = "Time (days)",
     ylab = "Weight (kg)",
     type = "b")                           # Feed type 1

for (i in 2:4) lines(obsTimes,avedata[,i],
                     col=i,
                     type = "b")          # Add feed type 2-4
                     
# 9.2 ANALYSIS OF SUMMERY MEASURES -----------------------------------------------------------------------------

# Vi kikker nu på vægt forøgelsen (incremental) fra day 26 til 91 og antager 'independent'.
# Vi bruger 'lm' på 'wide' data'en

mIncr <- lm(w91-w26~w0+factor(feed), data = goats)
summary(mIncr)
# Ref. cat. = treat 1. 
# Og vi ser blandt andet at treat 4 giver en signifikant mindre vægtforøgning end trat.1

# 9.3 THE RANDOM INTERCEPT MODEL -------------------------------------------------------------------------------
# Vi sætter nu 'ged' som randomfactor (bruger lme) og laver interaktione mellem 'feed' og 'time'

library(nlme)
?lme

mRanInt <- lme(weight~w0+factor(feed)*factor(time), random =~1|goat, data = long.g)
summary(mRanInt)

# Lækkert, men tolkningen tho... feed1 er ref cet...

# 9.4 INVESTIGATION OF THE CORRELATION STUCTURE ----------------------------------------------------------------

# Det her bliver lidt lang håret..
# I 'random intercept models' (overstående) er det antaget at hver fodertype (eller hver ged?)
# har samme variance (correlation) på tværs af tid. Denne antagelse er sjældent rimlig.
# Vi undersøger derfor variancen på tværs af tid manuelt:

lmfit0 <- lm(weight~w0+factor(feed)*factor(time),data = long.g)

# Vi har har her 'long' men skal bruge 'wide'. 
# Vi kunne bruge 'reshape', men også gøre følgende:


lmfit <- lm(data.matrix(goats[,4:7]) ~ goats$w0 + factor(goats$feed))

lmfit

# Objektet 'data.matrix(goats[,4:7]' skabe en matrix 28*4 som giver obs dagene som colm.
# Når dette objekt køres gennem 'lm' skabes en lin. reg. for colm. (dag) af gangen.
# Foskellie coef. fra forskellig dage findes. 
# Disse korrospondere med interaktionen 'factor(feed)*factor(time)'
# Forskellige coef. er tilladt for 'baseline' (intercept).
# I dette tilfælde ser vi kun en meget lille forskel på tværs af dagene. (se evt. s 107)

# Vi henter residualerne:

lmFitRes <- residuals(lmfit)
head(lmFitRes)

# Følgende kommandoer udregner en 'variance-covariance' matrix
# og den corresponderende corrolations matrix:

var(lmFitRes) # Varianc-covariance matrix

cor(lmFitRes) # Correlation matrix

# Er i tvivl om hvorledes man skal tolke på 'var()',
# Men 'cor()' er lige til. Vi ser her (diagonalt) at correlationen
# Er rimlig konsistent for dagene w26,w45 og w61 (ca. 0.7-0.8)
# Der i mod lægger w91 generelt lidt lavere (0.6-0.7)
# Forskellen er stor nok til at bogen problematisere det (manglende variance homogenitet).
# I section 9.6 ser vi på en model der tillader variance heterogenitet

# Cor. matirxen viser også at correlationen generelt bliver mindre over tid.
# Følgende plot illustrere:

corMat <- cor(lmFitRes) # Correlation matrix
corVec <- corMat[upper.tri(corMat)] # Vector med off-diagonale elementer (?)
corVec

timeDist <- c(45,61,61,91,91,91) - c(26,26,45,26,45,61)
timeDist

plot(timeDist, corVec) # 'plot(x,y)'
# Lækker og overskueligt

# Vi kan også et 'pairwise' scatterplot over residualerne:

pairs(lmFitRes)
# Fint. og kan også bruges til at identificere outliers.

# 9.5 SERIAL CORRELATION AND VARIANCE HOMOGENEITY --------------------------------------------------------------

# Se s. 110 for en forklaring herom + skema over mugligheder.

# 9.5.1 FITTING MODELS WITH SERIAL CORRELATION STUCTURE --------------------------------------------------------
# Først 'the Diggel model' (mGausNugget).

mGausNugget <- lme(weight~w0+factor(feed)*factor(time),
                   random =~ 1|goat,
                   corr=corGaus(form =~ time|goat,nugget = TRUE), # Se nedestående
                   data=long.g)

# Random og fixed elementer er specificeret som sædvanligt.
# 'corr=corGuas' specificere at correlationen svækkes som en 'Guassian density'
# 'time' pecificere at tid er målt med variablen 'time'
# Og 'nugget = TRUE' specificere at iid. fejl (målefejl?) er indkluderet.

# Følgende er eks på de andre moddeler nævnt på s. 110-112:

mGausNoNugget <- lme(weight~w0+factor(feed)*factor(time),
                   random =~ 1|goat,
                   corr=corGaus(form =~ time|goat,nugget = FALSE), # uden indragelsen af målefejl
                   data=long.g)

mExpNugget <- lme(weight~w0+factor(feed)*factor(time),
                     random =~ 1|goat,
                     corr=corExp(form =~ time|goat,nugget = TRUE), # Exponensiel frem for guassisk ændring
                     data=long.g)

mExpNoNugget <- lme(weight~w0+factor(feed)*factor(time),
                  random =~ 1|goat,
                  corr=corExp(form =~ time|goat,nugget = FALSE), # Exponensiel frem for guassisk + ingen måle fejl
                  data=long.g)

mUnrestrict <- lme(weight~w0+factor(feed)*factor(time),
                    random =~ 1|goat,
                    corr=corSymm(form =~ 1|goat), # Kan alle slags ændringer
                    data=long.g)

# Understående er modellerne uden 'random effects':

mGausNoRi <- gls(weight~w0+factor(feed)*factor(time), # Notér dig 'gls' frem for 'lme'
                 corr=corGaus(form =~ time|goat), # guassisk uden 'random effects'
                 data=long.g)

mExpNoRi <- gls(weight~w0+factor(feed)*factor(time), # Notér dig 'gls' frem for 'lme'
                 corr=corExp(form =~ time|goat), # Eksponensiel uden 'random effects'
                 data=long.g)


# 9.5.2 EXTRACTING THE ESTIMATES ------------------------------------------------------------------------------

# Vi ser på moddelerne:

summary(mGausNugget)
# Se en nogenlunde forklaring på s. 113

getVarCov(mGausNugget, individuals = 1, type = "marginal")
# Igen; se bog - s. 113-114

# 9.5.3 HYPOTHESIS TEST IN FIXED PART OF THE MODEL ------------------------------------------------------------

# ligesom tidligere; vi fitter med 'ML' og tester i ANOVA:

mGausNuggetML <- lme(weight~w0+factor(feed)*factor(time), # time som factor
                     random =~ 1|goat,
                     corr=corGaus(form = ~time|goat, nugget = T),
                     data = long.g, method = "ML")

mGausNuggetLinML <- lme(weight~w0+factor(feed)*time, # tid som mnumerisk
                        random =~ 1|goat,
                        corr=corGaus(form = ~time|goat, nugget = T),
                        data = long.g, method = "ML")

anova(mGausNuggetLinML,mGausNuggetML)
# Vi kan ikke afvise hypotesen, hvorfor det giver mening at reducere den yderligere

# 9.5.4 MODEL VALIDATION ---------------------------------------------------------------------------------------
# Vi kikker på variance homogenitet og corroelations strukturen

plot(mGausNugget)

# vi kan også tilføje lidt farve:

plot(mGausNugget, col=factor(long.g$time)) # Farver i tids katagorier
plot(mGausNugget, col=long.g$feed) # Farver i foder katagorier
plot(mGausNugget, col=long.g$goat) # Farver i ged (...) katagorier

# Ser vi på plottet med farvet tids katagorier kan vi notere os at
# variationen er lidt større for dag 91 (blå), 
# hvilket stemmer overens med tidligere resultater.

plot(mGausNugget$fitted[,2],mGausNugget$residuals[,2]) #Kan også det her uden helt at vide hvorfor...

# CORRELATION PLOT:
# Det bliver lidt lang håret, men se evt. s. 116-117 for en ok forklaring.

# Vi sammenligner med vores empiriske observationer vedrørende correlation fra 'lmFit'

corFctGauseNugget <- function(h){
     (0.1598+0.0603*exp(-(h/36.31)^2))/
     (0.1598+0.0603+0.0313)}
# Estimere corr. funktionen (...)

plot(timeDist, corVec, xlim = c(0,100), # Noteŕ dig at kommandoen 'timeMax' ikke findes..
     ylim = c(0.6,1), ylab = "correlation")
# The points (...)

plot(corFctGauseNugget, 0, 100,
     add=T, col=2)
# Corr fkt. i rød
# Det ser fint ud - men der var også meget tryl i første kommando..

# 9.5.5 COMPARISON OF DIFFERENT CORRELATIONS STRUKTURES --------------------------------------------------------
# Ofte sammenlignes forskellige modeller med 'AIC' (Akaike's Information Criterion)
# ANOVA raportere 'AIC' såfremt vi indputter flere modeller:

anova(mGausNugget,mGausNoNugget,mGausNoRi,mExpNugget,mExpNoNugget,mExpNoRi,mRanInt,mUnrestrict)

# Her ser vi på hvilke modeller der har lavest 'AIC'
# Umiddelbart ser 'mGausNoNugget', 'mExpNoRi' og 'mExpNoNugget' udtil at fungere bedst.
# Samme konk. får vi hvis vi kikker på 'BIC' (Baysian Information Criterion).
# p-værdierne skal man vist ikke (endnu) ligge for meget i.

# 9.6 MODELS WITH VARIANCE INHOMOGENEITY (heteroskedatisitet) --------------------------------------------------
# Vi kan bruge funktionen 'weights' til at tage højde for at foreskel i obs.

# Diggel Model With Time-depending Variance:
# Eks. så vi tidlige at variationen så ud til at være større for dag 91 end for resten.
# Vi forlænger vores 'mGausNugget' model:

mDiffVarGaus <- lme(weight~w0+factor(feed)*factor(time),
                    random = ~1|goat,
                    corr=corGaus(form = ~time|goat,nugget=T),
                    weights = varIdent(form = ~1|time),
                    data = long.g)

mDiffVarGaus

# Nu har residualets varians mulighed for at varierer mellem dage.
# Som det ses nederest i outputtet er dag '26' ref. cat
# Vi har nugget: 0.4387794 og parameter est: 1.0000
# Så residualernes varians er 0.4387794^2 * 1
# for dag 45, 61 og 91 er det så hendholdsvis:
# 0.4387794^2 * 1.0780679, 0.4387794^2 * 0.9633398 og 0.4387794^2 * 0.1.5030405 
# Som forventet er det dag 91 der slår ud.
# Notér dig at 'nugget' effekten antages ens for alle dage.


# Unrestricted Model With Time-depending Variance:
# Alt. metode: tillader forskellig variance på forskellige tidspunkter osv. (se s.120)
# Vi bruger her 'gls', hvorfor vi først omformer vores data 
# således at dagene heder 1-4 istedet for 26,45,61,91:

long.g1 <- transform(long.g, obsNo=as.numeric(factor(time)))
head(long.g1)

# Lækkert

mDiffVarSymm <- gls(weight~w0+factor(feed)*factor(time),
                    corr=corSymm(form = ~obsNo|goat),
                    weights = varIdent(form = ~1|obsNo),
                    data = long.g1)

mDiffVarSymm

# Vi kan træffe samme konk vedrørende dag 91.
# Notér dig at 'within-goat correlation' findes under 'correlation structure'.

# Vi undersøger vores moddeler med anova:

anova(mGausNugget, mDiffVarGaus, mDiffVarSymm)

# AIC og p-værdier tyder på at der ikke er nogen grund til at tage højde for heteroskedasticitet.

# Unrestricted Models With Group-depending Variance.
# Variantion kan variere mellem subjekter frem for eks. tidspunkter.

# Eks.:
# Homonet 'renin' blev målt 18 paienter + 6 kontrol under 7 forskellige udfordringer:

load("renin.rda")

# Udregner gennemsnit, sd,s og obs for hver kombinatione af tid og gruppe:

avedata <- with(renin, tapply(l_renin, list(time,grp), mean))

sddata <- with(renin, tapply(l_renin, list(time,grp), sd))

n_data <- with(renin, tapply(l_renin, list(time,grp), length))

# Langhåret; se s. 121-122..
# Det handler om at du får en en fin graf med 95% conf. int. (hvorfor; 1,96)
# 1:7 fordi der er 7 udfordringer/dage/obs
# Først kontrol gruppen: aveData[,1], sddata[,1] og (n_data[,1])
 

par(mfrow=c(1,2)) # ? Er det her jeg specificere at jeg vil havde to plots på samme side?

plot(1:7,avedata[,1], xlim = c(0,8),ylim = c(2,6),
     xlab = "Measurement times",ylab = "",type = "n")
# Som vi kender den

lines(1:7,avedata[,1] + 1.96*sddata[,1]/sqrt(n_data[,1])) # Linje for øvre konf. int.
lines(1:7,avedata[,1] - 1.96*sddata[,1]/sqrt(n_data[,1])) # Linje for nedre konf. int.

cord.x <- c(1:7,7:1)
# Simpel da den blot fortæller hvor grafen starter og hvor den slutter.

cord.y <- c(avedata[,1] -
              1.96*sddata[,1]/sqrt(n_data[,1]),
              avedata[7:1,1]+
              1.96*sddata[7:1,1]/sqrt(n_data[7:1,1]))
# Kompleks da denne fortæller om hele forløbet på grafen


polygon(cord.x,cord.y,col = 'skyblue') 
# En figur på baggrund af konstruerede kordinator.

lines(1:7,avedata[,1],type = "b",pch=16) 
# Giver den egentlige gennemsnits linje, aka; vores bedste bud.


# ligeledes for patinent gruppen men nu med aveData[,2], sddata[,2] og (n_data[,2]):

plot(1:7,avedata[,2], xlim = c(0,8),ylim = c(2,6),
     xlab = "Measurement times",ylab = "",type = "n")

lines(1:7,avedata[,2] + 1.96*sddata[,2]/sqrt(n_data[,2]))
lines(1:7,avedata[,2] - 1.96*sddata[,2]/sqrt(n_data[,2]))

cord.x <- c(1:7,7:1)
cord.y <- c(avedata[,2] -
              1.96*sddata[,2]/sqrt(n_data[,2]),
            avedata[7:1,2]+
              1.96*sddata[7:1,2]/sqrt(n_data[7:1,2]))

polygon(cord.x,cord.y,col = 'skyblue')

lines(1:7,avedata[,2],type = "b",pch=16)

# Fuk det er smukt..
# Jeg kunne også havde kørt på samme plot:

plot(1:7,avedata[,2], xlim = c(0,8),ylim = c(2,6),
     xlab = "Measurement times",ylab = "",type = "n")

lines(1:7,avedata[,2] + 1.96*sddata[,2]/sqrt(n_data[,2]))
lines(1:7,avedata[,2] - 1.96*sddata[,2]/sqrt(n_data[,2]))
lines(1:7,avedata[,1] + 1.96*sddata[,1]/sqrt(n_data[,1]))
lines(1:7,avedata[,1] - 1.96*sddata[,1]/sqrt(n_data[,1]))

cord.xk <- c(1:7,7:1)
cord.yk <- c(avedata[,1] -
              1.96*sddata[,1]/sqrt(n_data[,1]),
            avedata[7:1,1]+
              1.96*sddata[7:1,1]/sqrt(n_data[7:1,1]))

polygon(cord.xk,cord.yk,col = 'skyblue')

lines(1:7,avedata[,1],type = "b",pch=16)

cord.xp <- c(1:7,7:1)
cord.yp <- c(avedata[,2] -
              1.96*sddata[,2]/sqrt(n_data[,2]),
            avedata[7:1,2]+
              1.96*sddata[7:1,2]/sqrt(n_data[7:1,2]))

polygon(cord.xp,cord.yp,col = 'brown')

lines(1:7,avedata[,2],type = "b",pch=16)

# Jep - det fandt du lige selv på ;)


# Det ser ud til at mængden af homonet renin svinger mere i patient gruppen.
# Dette kan der tages højde for med 'gls':

m1 <- gls(l_renin~factor(grp)+factor(time),
          weights = varIdent(form = ~1|grp),
          corr = corSymm(form = ~time|id),
          data = renin)

summary(m1)
# Denne model tillader ikke variation inde for grupperne
# Se ellers s. 123-124 for en ok fortolkning:
#Vi kan også lave en model der tillader variantion inde for grupperne:

m2 <- gls(l_renin~factor(grp)+factor(time),
          corr = corSymm(form = ~time|id),
          data = renin)

summary(m2)

# Et hurtigt kik på 'AIC' viser at m1 er mest passende.

# 9.7 MULTIPLE SERIES FOR EACH SUBJECT -------------------------------------------------------------------------
# Det er noget med at der her er to slags korrelationer man her skal tage højde for
# Correlationen indefor en (tids)serie og et subjekt subjekter
# Og correlationen på tværs af mål/(tids)serier indefor et subjekt

# Her kommer et eks, jeg måske ikke helt forstår:

library(nlme)
load("glucose.rda")
head(glucose)

glucose<-subset(glucose,infusion!="C") # Laver et eller andet subset i dataframet.
long.gluc <- reshape(glucose,idvar = "series", 
                     varying = list(7:18),times = 30*(1:12),
                     v.names = "gluc", direction = "long")

head(long.gluc) # Vi ser at tidsserien er gået fre 'wide' til 'long'

# Nu mere tryl og jazz -> se s. 125-126 for forklaringe:

mMultiSerGaus <- lme(gluc~factor(time)*infusion*group,
                     random = ~1|subject/period,
                     corr=corGaus(form = ~time|subject/period,
                     nugget = T), data = long.gluc,
                     subset=time>200)

summary(mMultiSerGaus)
# Og bla bla smart på s. 126-127

#****************************************** 10) Generalized Linear Models *********************************----
# 10.1 LOGISTIC REGRESSION ------------------------------------------------------------------------------------
# binary response. Dead or alive

load("tickdata.rda")
# Vi udsætter tæger for en svamp og ser hvor mange der overlever.
# Hver eksperiment er et bur med 10 tæger af en given art, der så får en given mængde svamp

# Vi laver to modeller; en med og en uden interaktion mellem dosis og art:

fit0 <- glm(cbind(surv,dead)~factor(specie)*factor(dose),
            data = tickdata,binomial)

# Notér dig det fine trick med 'cind()'

fit1 <- glm(cbind(surv,dead)~factor(specie)+factor(dose),
            data = tickdata,binomial)

anova(fit1,fit0, test = "Chisq") #Chisq = chi-squared dist.
# Da p>0.05 (0.38) ser interaktionen ikke udtil at være sig.
# VI bruger fit1

summary(fit1)
# Dosis og art ser ud til at havde sig. eff.
# Vi sammenligner nu med en lang model - er konstrueret. (ved ikke helt hvorfor..)

load("ticklong.rda")
head(ticklong)

fit1.long <- glm(Y~factor(speciel)+factor(dosel),data = ticklong,binomial)
summary(fit1.long)
# Vi for de samme resultater (men der er en forskel i AIC, DF og Deviance residuals??)

# 10.1.1 ODDS-RATIO --------------------------------------------------------------------------------------------

# 'glm' giver os log.odds og log.conf. 
# for at får odds-ratio + conf. int. kan følgende gøres:

exp(coef(fit1))
# Her regner du bare fra log med exp - simpelt

exp(confint(fit1))
# Igen - omregner fra log med exp.

# Man kan også specificere sine conf. int. med pakken gmodels:

library(gmodels)

cm <- rbind('6 vs 0 dose' = c(0,0,1,0,0,0),
            '9 vs 8 dose'= c(0,0,0,0,-1,1),
            '0i vs 0m'= c(0,-1,0,0,0,0))
# pas.. Men det er vigtigt...

exp(estimable(fit1,cm,conf.int = 0.95))[,c(1,6,7)]
# virker fint og lækkert med både estimat (odds-r) og conf. iint,
# men er lidt i tvivl om hvad der skete med cm...

# Se en ok tolkning på s 133

# Alt. kunne man bruge mfx-pakken

library(mfx)

logitor(fit1,data = tickdata)
# Meget lettere og finere -> nu med p-værdi
logitmfx(fit1,data = tickdata)
# TRYLLLLLLL! Marginale effekter -> her givet af katagorierne.

# 10.1.2 CORRELATED DATA ---------------------------------------------------------------------------------------

# Grundet det eksperimentielle design - 10 tæger pr bur -
# kan der være andet variantion end den dosis og art kan forklare
# Dette kan vi tage højde for med funktionen 'quasibinomial'

fit.cl <- glm(cbind(surv,dead)~factor(specie)+factor(dose), data = tickdata, quasibinomial)
summary(fit.cl)

# Coef. er de samme, men std.e og herved p er højere.
# Her ændre det dog ikke meget ved konk.
# Overdispantations parametre er 2.359031 og den nye std.e er netop fundet ved std.e*sqrt(2.359031)

# Vi kan igen hente odds-ratio og konf. int.:

exp(estimable(fit.cl, cm,conf.int=0.95))[,c(1,6,7)]
# conf. int. er nu lidt bredere

# Alt. mfx:
logitor(fit.cl,data = tickdata) # odds.ratio
logitmfx(fit.cl,data = tickdata) # Marginale effekter -< giver sig selv ved kat var.
# Multi lækkert

# Alt. kan man også benytte tilgangen 'genealized estimating equation'(GEE)

library(gee)

fit.gee <- gee(Y~factor(speciel)+factor(dosel)
               ,data = ticklong,
               id=experiment,
               family = binomial,
               corstr = "independence")

summary(fit.gee)

# Her får du nu både 'naive' og 'robuste' std.e
# De naive korrospondere ca med dem fra 'fit1' og de robuste ca med dem fra fit.cl

# 10.1.3 NATURAL RESPONSE: CALCULATING IT YOURSELF! ---------------------------------------------------------

# Nogen tæger dør naturligt under eksperimentet - det har R lidt svært ved.
# Under stående er ikke desto mindre et forsøg på at i mødekomme denne præmis
# (se s. 135-136 for jazz)

dose <- c(0,10^6,10^7,10^8,10^9) # the data (?)
numb <- c(60,30,60,60,60)
numbd <- c(40,22,51,56,59)

# og nu 'minus log-likelihood functionen':

logl <- function(c,al,be) {
  res=log(choose(numb[1],numbd[1]))+
    numbd[1]*log(c)+(numb[1]-numbd[1])*log(1-c)
                     for (i in 2:5) {
                       p=c+(1-c)*exp(al+be*log10(dose[i]))/
                         (1?exp(al+be*log10(dose[i])))
                       res=res+
                         log(choose(numb[i],numbd[i]))+
                         numbd[i]+log(p)+(numb[i]-numbd[i])*log(1-p)
                       
}
 return(-res)
}

# Virker det??
# Vi henter pakken bbmle (mle2):

library(bbmle)

guess <- list(c=0.2,al=0,be=0) # Starting values
opt <- mle2(logl,start=guess,method="Nelder-Mead")

summary(opt)
# Her sker ikke så meget, men debuggen vist sig fra sin fineste side.
# Herfor springer vi resten af det her afsnit over. se evt. s136-137.

# 10.2 PROPORTIONAL ODDS MODEL -------------------------------------------------------------------------------


















