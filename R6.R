
### R6
# - more similar to OOP in many other languages, such as Java
# - define a class using R6::R6class
# - create objects using <class>$new()
# - call functions using <object>$<function>()
# - access member varaibles using <object>$<value> (this is the same as in S3)

Creature <- R6::R6Class("Creature",
                        # the 'public' list contains functions and member variables that can be
                        # accessed from 'outside'
                        public = list(
                          age = NULL,
                          name = NULL,
                          # special method 'initialize': gets called when <object>$new() constructor is called.
                          # (and should not be called directly from outside)
                          initialize = function(name, age) {
                            # typical pattern: assert and initialize member variables in initialize():
                            self$age <- assertNumeric(age, lower = 0)
                            self$name <- assertString(name)
                          },
                          talk = function() {
                            "..."
                          }
                        ),
                        # the 'active' list contains functions that look like member variables
                        # from the outside
                        active = list(
                          legs = function() rgeom(1, .01)  # accessing $legs will give a random value
                        )
)

# create object of R6-class Creature
cr1 <- Creature$new("Shoggoth", 363236)
class(cr1)  # also has S3 classes, so we *could* also do S3 with this object

cr1$age
cr1$name

cr1$legs  # active binding: gives different values for each call
cr1$legs
cr1$legs

# The following gives an error: active bindings can not be assigned when
# the functions don't have an argument. See 'legs' active binding of Cat class below.
cr1$legs <- 100

cr1$nae <- "Shoggi"  # error because slot does not exist
cr1$name <- "Shoggi"
cr1$nae  # NULL; no check, unfortunately.

# reference semantics:
cr2 <- cr1
cr2$name <- "Shoggo"
# cr1 was also changed:
cr1$name

# - use 'clone()' to create a *copy*.
# - use 'clone(deep = TRUE)' to create a "deep" copy (if your R6 object contains
#   further R6 objects in itself and should be copied recursively.
# - you should probably always use 'clone(deep = TRUE)' because you, the user of
#   the object, don't know whether recursive cloning is necessary or not.
# - the fact that 'deep = TRUE' is not the default is ridiculously bad design.


## Inheritance
# - create a class that contains all the things from the base class
# - may overwrite some functions
# - this is similar to S3 inheritance: first try to use the more specific
#   class's method, then the more general method if not found.
# - may call the base class's functions using 'super$<function>()' -- this is similar
#   to NextMethod()
Cat <- R6::R6Class("Cat",
                   inherit = Creature,
                   public = list(
                     furcolor = NULL,
                     initialize = function(name, age, furcolor) {
                       # initialize() should usually call super$initialize(),
                       # because the inheriting class shouldn't worry about how the superclass
                       # gets initialized.
                       # If the 'Creature' class internals change, then you don't need to change
                       # the 'Cat' class, because super$initialize() does all the necessary things.
                       super$initialize(name, age)
                       self$furcolor <- furcolor
                       
                       # we can access the 'private' member variables only from inside this class.
                       private$.legs <- 4
                     },
                     talk = function() {
                       "meow"
                     }
                   ),
                   active = list(
                     # active binding with argument: this is given whenever the value is assigned
                     legs = function(x) {
                       # check whether we do assignment using 'missing()': if the value is
                       # missing, then we are only 'reading' this active binding; if the value
                       # is not missing, we are doing assignment
                       if (!missing(x)) {
                         # good practice for assignment: check validity
                         assertInt(x, lower = 0, upper = private$.legs)
                         private$.legs <- x
                       }
                       private$.legs
                     }
                   ),
                   # 'private' member variables and methods can only be accessed / called from
                   # inside this class (or from an inheriting class!)
                   private = list(
                     .legs = NULL
                   )
)

cx <- Cat$new("Miezi", 10, "white")
cx$furcolor
cx$legs
cx$legs <- 3
cx$legs
cx$legs <- 10 # active binding checks (and rejects) this value: number of legs can only decrease.
# (Sorry for this disturbing example, I obviously haven't thought this through...)

cx$talk()
cx$furcolor <- "brown"
cx$legs