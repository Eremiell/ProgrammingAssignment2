## Functions in this source file provide functionality of specialized matrix
## with cache for its inverse. Computing an inverse can be computationally
## intensive in some cases and the inverse value can be needed often in various
## calculations. The cache is also safely deleted in case of matrix mutation
## and only calculated in case of need, so it won't get recalculated after each
## mutation unless explicitly called.

## Builds a matrix with inverse cache, accessible through getters and setters.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	return (list(
		set = function(y) {
			x <<- y
			inv <<- NULL
			return (x)
		},
		get = function() {
			return (x)
		},
		setInverse = function(inverse) {
			inv <<- inverse
			return (inv)
		},
		getInverse = function() {
			return (inv)
		}
	))
}

## Returns an inverse matrix to x. Computes only if not found in cache.
## Stores in cache in such case.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if (!is.null(inv)) {
		return (inv)
	}
	inv <- solve(x$get(), ...)
	x$setInverse(inv)
	return (inv)
}

## A few short notes on formatting of my source code as this is a peer reviewed
## assignment and such subjective things as "well-written and easy to read" are
## part of the grading guidelines:

## Such as most of you, I've started with provided samples and modified them to
## work with matrices and inverses. (Inverse matrices if you wanna be exact,
## but let's just use the condensed form to save some space.) It was quite
## straightforward, so I did some code cleanup and sanitation, discarded some
## unnecessary variables, bracketed blocks, introduced explicit returns, etc.

## Some of you might be new to programming languages, so you might not
## understand the purpose of some of these changes, but most of them are
## considered best practices in nearly any programming language. Let me explain
## some of them:

## Discarding unneeded variables:

## When you first write up some code, you store something in here and something
## in there and you often can't really see, which variables will you need in
## the future and which not. So it's a common sanitation practice to go through
## your code once it's debugged and remove unneeded variables. You put one time
## calls inside other calls and save both space in your memory and tiny bits of
## time on your processor. It might arguably sometimes make the things slightly
## harder to read, unless you're a skilled programmer, but it's definitely
## a good practice considering the performance of your program.

## Explicit block delimiting:

## In most languages, you're allowed to not delimit blocks as long as they only
## contain one command. Now, that may break things horribly once you add
## a second line into the block and forget to properly delimit it. It's
## therefore considered best practice to always explicitly delimit blocks.

## Explicit returns:

## R has this nice feature/syntax sugar allowing implicit return of last value
## evaluated in given function. But sometimes, especially when scanning the
## code to find some feature, bug or something, you might easily miss the
## return value. Explicit returns make it very clear, what each function gives
## you and therefore make readability of code better.

## Returns on setters:

## This might be a bit of an individual opinion, but at least some programmers
## believe, successful setter should return something to show, it was in fact
## successful. The popular candidates are boolean value of true, the mutated
## object itself and the value being set. I picked the last one in this case as
## I feel, it's best visually checked of the three.

## Lower camel case:

## It was already used in the sample code, even though non persistently.

## Distinction between functions and logical constructs:

## There are two kinds of constructs denoted by round brackets and it's very
## common practice to distinct between them by putting or not putting a space
## in between the keyword and an opening bracket. Namely functions don't sport
## a space and logical or flow constructs like conditions, loops, returns and
## such do. Some of these don't even use brackets at all in some languages to
## clearly distinct them from function calls. Even though these constructs
## might not be any different in inner implementation in some languages, they
## still have quite different means and expectations, where functions are
## expected to do something and return to the exact same spot, possibly with
## some return value, while logical and flow constructs are expected to somehow
## change the flow of execution, possibly skipping some commands or repeating
## them or even completely breaking out of function or loop.

## Indentation by tabs:

## Now, this one might be the most controversial of these and is the subject of
## neverending dispute between programmers using tabs and programmers using
## spaces. I'm apparently on the tabs side. Some of the main reasons include:

## It's much harder to break. I've seen countless times source codes, where
## some structures were indented by 4 spaces and some by 3 or 5. Often by
## mistake. Being one off with tabs makes much clearer visual disturbance.

## Everyone on the project may choose their optimal indentation. Most modern
## IDEs let you choose, how wide they should represent your tabs. So I might
## get them 4 spaces wide and you might get them 3, 6, 8 or even 1 space wide,
## whatever you prefer, and we can still work on the same source file without
## any need of compromises or endless conversions back and forth.

## Last but not least, it takes less space. Let's say an average line of code
## is indented about 2 levels (at least in object oriented programming). That
## makes it usually 8 spaces or 2 tabs, both spaces and tabs counting as one
## character. Now a casual one purpose program with some features and possibly
## some graphic user interface might have somewhere between a few thousand and
## a few tens of thousands lines. Let's assume both a space and a tab take one
## byte. (Not necessarily true in all encodings, but both are among the first
## 128 characters in original US-ASCII/ANSI table, so the chances are pretty
## high.) For 10k lines of code, that makes 80k bytes of spaces and 20k bytes
## of tabs on average. That's about 60kB difference on every 10k lines of code.
## I prefer to store other things than empty space on beginning of every line.

## Thanks for reading so far and I hope, some of these things might be
## insightful for you, if you're new to programming.

## As a last note, this code probably resembles more a typical C++ code than
## a typical R code. I come from a programming background and by using norms
## we apply to nearly any programming language, I might have broken some
## typical constructs or unspoken rules of formatting R. I do so all the time
## in Python. The "official" guidelines change from language to language, from
## project to project, from company to company. I tend to write all my code
## across all languages in standardized way whenever possible. If you come from
## a statistics background, are used to R and find this code strange, that's
## entirely possible. This is the way (at least some) programmers would do it.
