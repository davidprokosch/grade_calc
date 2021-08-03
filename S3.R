### "object oriented programming"
# --> programming centered on 'objects'
# --> 'objects' are instances of 'classes',
#     in the sense that '2' is an instance of 'numeric',
#     'iris' is an instance of 'data.frame' etc.
# --> what exactly makes an 'object' or a 'class' differs between languages

irishead <- head(iris)

print(irishead[1])
# vs.
idt <- as.data.table(irishead)
print(idt[1])

### OOP in R
# There are multiple OOP-systems in R:
# - S3: introduced in "S" (predecessor of R) version 3
#   (There is no S2 or S1)
# - S4: introduced later, a bit more complicated, more strict; seems popular
#   in bioinformatics.
# - Reference classes: uses 'reference semantics', differs quite a bit
#   from the above.
# - R6: Simpler version of reference classes.
#
# We will cover S3, R6.

###################################################
### S3

# Many languages write:
# <object> = <constructor>()
# <object>.<member> = 10
# <object>.<function>(), e.g.
# object.print()
# We write
# <function>(<object>, ...)
print(idt)
print(irishead)

# What gets called:

print

# UseMethod: inspects class() of object
class(irishead)
class(idt)

# irishead -->
# <functionname>.<classname>
print.data.frame

# idt -->
data.table:::print.data.table

# Simplest S3-objects: lists with a 'class'
# let's make our own object

h1 <- list(age = 22, name = "Hubert", color.hair = "black")
class(h1) <- "Human"


# print method for objects of class 'Human'
print.Human <- function(x, ...) {
  cat(sprintf("A human named %s age %s\n", x$name, x$age))
}

# normal function
talk <- function(x) {
  if (x$age > 2) "hello" else "aaaa"
}


# What if we want to have a different 'talk' for different classes?
h2 <- list(age = 1, name = "Ernie", color.hair = "blond")
class(h2) <- "Human"

c1 <- list(age = 12, name = "Agnes")
class(c1) <- "Cat"

print.Cat <- function(x, ...) {
  cat(sprintf("A cat named %s age %s\n", x$name, x$age))
}


talk <- function(x) {
  # UseMethod: call the relevant method for object depending on their class
  # (the function should only call UseMethod() and do nothing else.)
  UseMethod("talk")
}

# 'talk' methods for classes 'Cat' and 'Human'
talk.Cat <- function(x) {
  "meow"
}
talk.Human <- function(x) {
  if (x$age > 2) "hello" else "aaaa"
}

# 'default' method is called whenever 'UseMethod()' doesn't find
# a class-specific method.
talk.default <- function(x) {
  stop(sprintf("Object of class %s does not talk.", class(x)[1]))
}

# generic that returns a different constant for each
legs <- function(x) UseMethod("legs")
legs.Cat <- function(x) 4
legs.Human <- function(x) 2
legs.default <- function(x) 0

legs(c1)
talk(h1)
talk(h2)
talk(c1)
talk(3)

## Inheritance: "is-a relationship"

class(h1)  ## only 'Human' so far

# multiple classes: listed from 'specific' to 'general'
# - specific class is used first, then more general class
class(h1) <- c("Human", "Creature")
class(h2) <- c("Human", "Creature")
class(c1) <- c("Cat", "Creature")

print.Creature <- function(x, ...) {
  cat(sprintf("A Creature (%s) named %s age %s\n", class(x)[[1]], x$name, x$age))
}
print.Human  # this method still exists.
print(h1)  # calls print.Human, not print.Creature, because it is more specific
rm(print.Human)  # remove the function so that print.Creature gets called
rm(print.Cat)
h1  # now printing "A Creature..."
c1

talk.Creature <- function(x, ...) {
  "..."
}

f1 <- list(age = 3, name = "Konrad")
class(f1) <- c("Fish", "Creature")

talk(f1)  # doesn't find talk.Fish, so calls talk.Creature
talk(3)

yearOfBirth <- function(x) {
  UseMethod("yearOfBirth")
}

yearOfBirth.Creature <- function(x) {
  year(Sys.time()) - x$age
}

yearOfBirth(c1)


talk(f1)
yearOfBirth(f1)

## Chaining Classes, NextMethod

c2 <- list(age = 5, name = "Larissa, the Talking Cat")
class(c2) <- c("TalkingCat", "Cat", "Creature")

c3 <- list(age = 2, name = "Nadine, the Talking Cat")
class(c3) <- c("TalkingCat", "Cat", "Creature")

talk.TalkingCat <- function(x) {
  if (x$age > 3) {
    "Hello, meow are you?"
  } else {
    NextMethod()  # goes to the next class.
  }
}

legs(c2)  # uses legs.Cat
talk(c2)  # uses talk.TalkingCat

talk(c3)  # talk.TalkingCat --[NextMethod()]--> talk.Cat
talk.TalkingCat(c3)  # Error


## Inheritance does not always need to be the same
# (but it is usually a good idea to be consistent)

hc <- list(age = 1, name = "Simurgh, the Result of a Questionably Ethical Experiment")
# functions with this object use the 'TalkingCat' function first; if not
# found (or when NextMethod() is called) they go on to the 'Human' function etc.
class(hc) <- c("TalkingCat", "Human", "Creature")

talk(hc)
hc$age <- 3
talk(hc)
hc$age <- 5
talk(hc)


xx <- list(age = 1, name = "testobject")
class(xx) <- c("Human", "Cat", "Creature")
legs(xx)

## Creating S3-Objects

# use the 'structure()' function to set the 'class' attribute
# (see further below on attributes)
h3 <- structure(
  list(age = 10, name = "Balthasar", color.hair = "brown"),
  class = c("Human", "Creature")
)


# Aside: can also set names with the 'structure()' function
vec <- c(1, 5, 2)
names(vec) <- c("a", "b", "c")

vec2 <- structure(
  c(1, 5, 2),
  names = c("a", "b", "c")
)

broken <- structure(
  list(age = 10, nam = "Balthasar", color.hair = "brown"),
  class = c("Human", "Creature")
)
broken


### Good Practice: Constructor functions
# - upper case, same as class name
# - perform checks
# - construct the object
# - for each class there should ideally be only one
#   (or a small number) of constructors


## Ok, but not great: create the whole class at once
Human <- function(name, age, color.hair) {
  assertString(name)
  assertNumber(age, lower = 0)
  assertChoice(color.hair, c("brown", "black", "blond", "ginger", "grey"))
  structure(list(age = age, name = name, color.hair = color.hair), class = c("Human", "Creature"))
}

## Better: Make use of inheritance
Creature <- function(name, age) {
  assertString(name)
  assertNumber(age, lower = 0)
  structure(list(age = age, name = name), class = "Creature")
}

Human <- function(name, age, color.hair) {
  assertChoice(color.hair, c("brown", "black", "blond", "ginger", "grey"))
  obj <- Creature(name, age)  # !!
  obj$color.hair <- color.hair
  class(obj) <- c("Human", class(obj))  # !!
  obj
}

Cat <- function(name, age) {
  assertNumber(age, upper = 35)
  obj <- Creature(name, age)
  class(obj) <- c("Cat", class(obj))
  obj
}

Cat("Minka", 4)

talk(c1)

getRandomHuman <- function() {
  Human(sample(c("Agathe", "Balthasar", "Caesar"), 1), rgeom(1, .01), sample(c("brown", "black", "blond", "ginger", "grey"), 1))
}


## We can now write our code and assume that classes look the way they are supposed to look
ageInTenYears <- function(creat) {
  ### We don't need these:
  # assertList(creat)
  # assertNumber(creat$age, lower)
  ### Instead we trust that the class object obeys its contract
  ### and only check assertClass()
  assertClass(creat, "Creature")
  # don't need to assert that creat$age exists, it is part of the class definition
  creat$age + 10
}

ageInTenYears(h1)

h1$age <- NULL
ageInTenYears(h1)
ageInTenYears(structure(3, class = "Creature"))

#### Example of S3 as it is used in R:
### lm()

model <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)

str(coefficients(model))
plot(model)  # press 'enter' to page through plots
summary(model)

str(model)  # this is what the class looks like internally
stats:::print.lm  # This function gets called for print(<object of class lm>)
print(model, digits = 1)
stats:::coef.default  # This function is called by coefficients(). The name is not optimal....
model$coefficients
coefficients(model)

predict(model, newdata = data.frame(Sepal.Width = 3, Species = "setosa"))

### compare lm() with ranger():

library("ranger")

rfmodel <- ranger(Sepal.Length ~ Sepal.Width + Species, data = iris)

str(rfmodel)  # big object...

prediction <- predict(rfmodel, data = data.frame(Sepal.Width = 3, Species = "setosa"))
str(prediction)  # prediction is an S3 object itself
# the behaviour is (unfortunately!) different from  predict() for lm.
# this is not well designed.
prediction$predictions

###################################################
### Addendum: attributes

x <- 1

x

# access (get and set) attributes using
# 'attributes(<obj>)$<attr name>' or 'attr(<obj>, <attr name>)'.

attributes(x)
attributes(x) <- list(yattr = 100, testattr = "hi")
x

y <- 2
attributes(y) <- list(yattr = 200, newattr = "hello")

x + y


class(x) <- "testclass"

x
attributes(x)

attributes(x)$testattr
attr(x, "testattr")

attr(x, "testattr") <- "bye"
attributes(x)$testattr <- "bye bye"


class(x) <- "test2"
attr(x, "class") <- "test3"


attr(x, "names") <- "xname"
x
names(x)


z <- structure(3, testattr = "xyz", names = "xxx")
str(z)

str(x)

# c() erases all attributes except 'names':
str(c(x))

# this is because individual members of vectors do not have attributes.
# attributes are always for the whole vector.
xx <- c(1, 2)
attr(xx[[1]], "test") <- "xx"  # this does not change xx!
str(xx)

str(x)
str(y)
str(c(x, y))

c(x, y)

# this *does* work for lists:
xx <- list(1, 2)
attr(xx[[1]], "test") <- "xx"
str(xx)