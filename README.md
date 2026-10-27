# parscomb
A parser combinator library in Fortran 2018. Additionally, a full SML parser is included as an example usage of the library.

The SML parser likely contains bugs and produces exceptionally poor error messages, but does seem to work on fairly complex examples

This implementation of parser combinators is not the pinnacle of efficiency, which is partially at the fault of the Fortran type system.
Memoization is not implemented

The `src` directory contains the library itself. When ran using fpm, this program runs the SML parser on the first command line argument
and prints the result.

The SML parser can be found in `src/examplse/sml_parser_mod.f90`

# compilation
Use fpm to compile, run and install this program

# usage
Types:
The fortran type system is far more limited than a typical functional programming language.
As such, features like typeclasses, variants, lambdas, and generics have to be emulated with inheritance and unlimited polymorphic variables.

`parser`:

All parsers contain a `run` type bound procedure, which takes a stream (`class(*), dimension(:)`), and returns a parse result

The stream should usually be of type `character` but is left polymorphic to allow tokenization if desired

All parsers are a subtype of this abstract type

`parse_result`:
Abstract type for `parse_success` and `parse_failure`

`parse_success`:
Contains a `class(*)` representing the parsed data, a pointer to the rest of the stream to be processed, and the number of elements consume
Note that an unallocated `class(*)` is a valid result

`parse_failure`:
Contains an error string and how many elements in the error occurred

Base parsers:

`char_parser(c)`: a parser that accepts only 'c'

`option_parser(str)`: a parser that accepts any character in str

`str_parser(str)`:  a parser that accepts the string str (for the most part this is a superset of `char_parser`)

`predicate_parer(p)`: accepts any value that satisfies 

`succeed_parser()`: parser that always suceeds and consumes nothing

`stub_parser(parser)`: parser that wraps the input parser (useful for making cyclical relationships)

Combinators:

Any chain of 2+ results is represented by a linked list formed by pairs (right associative tree)

`parser ** parser`: Parsers the first value followed by the second, returns a pair

`int * parser`: Repeats a parser n times

`parser .or. parser`: Does the second parser if the first fails

`.rep.parser`: Repeats parser 0+ times

`.oneplus.parser`: Repeats parser 1+ times

`parser > parser`: like `**` but returns the result of the right parser (instead of a pair)

`parser < parser`: like `**` but returns the result of the left parser

`parser .sepby. parser`: parses 1+ copies of first parser that are separated by the second parser (like a list)

`.opt.parser`: either parser or empty

`parser .error. str`: replaces the error mesage of the parser

`parser .annotate. str`: annotates the error message of the parser

`.no.parser`: Inverts the result of the parser

`.nonempty.parser`: Fails if the result is null

`parser .ifempty. str`: Fails if the result is null with the string as the error

`.ref.parser`: Forms a reference to the parser (instead of copy), parser must be target

`parser .tag. str`: On success, wraps the result in a tag type with the given string

`.has.parser`: Runs the parser but doesn't consume input

`.lassoc.parser`: Runs the parser and then reforms the pair tree into a left associative tree

`parser .lassoc. str`: Runs the parser and then reforms the pair tree into a left associative tree, where each node is tagged

`map_parser(parser, func)`: Applys the function to the parse result on success
