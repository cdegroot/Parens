This is an experiment based on
http://www.softwarepreservation.org/projects/LISP/book/LISP%201.5%20Programmers%20Manual.pdf - the Lisp 1.5 manual.

I never really learned Lisp, and the only times I sorta used it was
in Emacs and in the SGMLtools project (where DSSL was the style
sheet language). It seems sorta fun to implement the code from the
book, as faithfully as possible, requiring an Mexp and an Sexp
parsers.

As I also suck in parsers and interpreters/compilers, I'm using
Scala because Scala's Parser Combinators make the process at least
halfway bearable :).

This is on Github for my convenience, it is meant purely as an
academic exercise for my own pleasure. If you can use the code,
congrats, but at the moment do not expect support or something.

OBTW - I decided to use the original typography. In the macos/ directory,
there is a keyboard layout that maps alt-l to the lambda and alt-a to the
right arrow. 

OBTW2 - No, I don't know where this is going to. I'm just trying to 
make tests pass here, where the input of the tests is all the various
examples in the manual :)

