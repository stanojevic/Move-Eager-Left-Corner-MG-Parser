Eager-Move Left-Corner Minimalist Grammar Parser
================================================

This is implementation of left-corner MG parser that predicts move
operation as soon as possible. It also has a more standard (not move-eager
but still arc-eager) MG left-corner parser.

This parser in its current form is way to slow to parse any non-trivial sentence
but it is fast enough to verify parsing transitions and to do interactive parsing
similar to interactive theorem proving.

How to use the code
-------------------

First install Simple Build Tool and optionally Graphviz if you want your trees visualized.
Enter the project directory (the one containing this file) and type:

         sbt run

That will run the default example with the default grammar on the default sentence with eager-move parser.

For interactive parsing type:

        sbt console

That will start Scala REPL where you first need to import relevant functions with:

        import ed.ac.uk.leftcorner.Interactive._

Then you can load your favorite grammar with:

        loadGrammar("src/main/scala/ed/ac/uk/leftcorner/grammars/G1.txt")

If you want eager-move (the default one) type `useEagerMove` otherwise `useStandardMove`.

Now you enter a sentence you want to parse interactively:

        resetParseState(List("Bowie", "likes", "what", "Iggy", "sings"))

To print the current stack state with the last transition taken just type:

        state

To print possible transitions to take type:

        options

To take any of the possible transitions call function takeAction by proving the id of the action, let's say:

        takeAction(2)

As a result you will get the new stack content.
        
If you have arrived to the final state the tree will be printed as text but you
can also get a pdf drawing of it with:

        state.tree.visualize()
        
If you want to see example run of eager move (same one you got earlier with `sbt run`) type:

        ed.ac.uk.leftcorner.Main.exampleEagerMove
        
If you want to see example run of non-eager move type:

        ed.ac.uk.leftcorner.Main.exampleNonEagerMove
        
It may happen that you get a StackOverflowError because miniKanren goes very deep in recursion.
In that case you will need to increase JVM's heap size -Xss10M

How is SMC implemented
----------------------

Most MG implementations represent movers as a list that could potentially be empty.
Checking for SMC consists only of checking of there are two movers with the same initial feature.
Preventing duplicates in this representation requires an ``extra-logical'' operation (cut and var in Prolog).
This makes it difficult to do eager move when movers are not instantiated.

Instead in this parser a different representation is used that uses only normal logical operations.
Movers are represented as a list that is always of the same length as the number of mover types (licensees).
If the mover of type, let's say, wh is missing the entry for wh in the movers list will say NO_MOVER.
Otherwise it will contain the info about the mover (span and features).

All operations over mover lists do not change the length of the list but only it's content.
SMC is implemented by preventing merge and move that combines two movers lists with
non-empty slots for the same feature.

Code guide
----------

This algorithm uses a lot of unification machinery.
Prolog seems a natural fit for implementing this but I decided to do it Scala so that I could
 do logical part with miniKanren and the non-logical part in typed functional language.
 
 MiniKanren is an idea how to implement a small logical programming language within a 
 functional programming language and it's described in this book 
https://mitpress.mit.edu/books/reasoned-schemer-second-edition .
My implementation of miniKanren is in the kanren package and usage of miniKanren to implement logical
part of MG parsing is in MGlogic object.

MiniKanren is a beautiful idea and I learned a lot about logical programming while implementing it
but in retrospect I should have done it in Swi-Prolog as a subprocess instead of miniKanren because 
miniKanren is too slow and doesn't have neat syntax as Prolog.

Parsing (that is too slow to be used for any non-trivial sentence) is done by backtracking
search implemented using the old idea of Philip Wadler described in ``How to replace failure by a list of successes''.
Wadler in that paper explain how to do depth-first search with backtracking in a functional language.
I have extended that using the idea of Kanren's interleaved streams to implement breadth-first search.
Advantage of breadth-first search is that it won't enter infinite recursion with empty strings because it will find
the shortest solution first. The disadvantage is that it will take exponentially more memory than DFS and if there
is no parse it will search forever. That's why I also added a parameter to specify the longest sequence of transitions.

Parsing is slow because of:
1. backtracking search
2. miniKanren unification is slow
3. no prevention of prediction of impossible categories
4. no oracle

Reason 1 could be addressed with a greedy probabilistic model.
Reason 2 with some good Prolog (or Mercury) implementation instead of miniKanren.
Reason 3 with pre-computing all possible categories and constraining prediction only to them.
Reason 4 is much more complicated. One partial solution is present in the previous Prolog implementation but it's not implemented here.

Because we don't restrict impossible categories (reason 3) the prediction+application of eager move might enter into infinite
prediction of cyclic movement by generating and applying movement on category like +wh +wh +wh +wh +wh +wh ....

To prevent this I hardcoded that only one eager move can be applied. This hack is not a necessity.

Contact
------

If you have any problems using it shout at:

Miloš Stanojević        \
m.stanojevic@ed.ac.uk   \
University of Edinburgh 
