This is an experiment based on
http://www.softwarepreservation.org/projects/LISP/book/LISP%201.5%20Programmers%20Manual.pdf - the Lisp 1.5 manual.

I never really learned Lisp, and the only times I sorta used it was
in Emacs and in the SGMLtools project (where DSSL was the style
sheet language). It seemed like a fun idea to implement the code
from the book, as faithfully as possible, requiring Mexp and Sexp
parsers. So I decided to just go through all the code and examples
in the book, write them down as unit tests, and make them pass. 

I've decided to use Scala because Scala's Parser Combinators make
the process at least halfway bearable :). Really, they're the most
fun I ever had when writing parsers. 

This is on Github for my convenience, but it is meant purely as an
academic exercise for my own pleasure (I mean, hey, if you want a
Lisp environment on the JVM, your biggest problem is to pick which
one to use). If you can use the code, congrats, but at the moment
do not expect support or something.

OBTW - I decided to use the original typography. In the macos/ directory,
there is a keyboard layout that maps alt-l to the lambda and alt-a to the
right arrow. 

OBTW2 - No, I don't know where this is going to. I'm just trying to 
make tests pass here, where the input of the tests is all the various
examples in the manual :)

