# group-theory

This file defines a few functions that allow us to exhaustively search for finite groups of any given order n.

The strategy is to systemically generate latin squares (which are normalized to avoid generating too many isomorphic groups),
and filter for those which are the regular representation of finite groups. 

The regular representation (RR for short) has special qualities that make it suited for the task at hand. Specifically, the RR has a double meaning. It is simultaneously:

  (1) the set of elements of the group, with each element represented by its action (which permutes the group)
  
  (2) the rows of the group multiplication table
 
Concretely, consider the left regular representation (LRR) of the group with 4 elements, and all non-identity elements of 
order 2:

                                    M = [[1,2,3,4],
                                         [2,1,4,3],
                                         [3,4,1,2],
                                         [4,3,2,1]]
 
Here, the first row [1,2,3,4], and the numeral 1 appearing throughout the table, refer to the same element, the identity. 
In general, the numeral 'n' and the n'th row of the table are two different ways of representating the n'th element of 
the group. 

The advantage of the RR is that the elements of the group are representated in such a way as to allow us to compose them.
If I tell you 'a' and 'b' are the elements of a group, and nothing more, it is meaningless to ask you to compute the product
'ab'. If instead I tell you [2,1,4,3], [3,4,1,2] are the elements of a group in the LRR, we can readily calculate the product
with the rules for composing permutations. 

Insisting on the RR saves us from the computationally heavy task of, for instance, verifying that a multiplication table is
associative, which we would be forced to do if we allowed our multiplication tables to take any form. Instead we can verify:

                                      https://latex.codecogs.com/gif.latex?M_i&space;\times&space;M_j&space;=&space;$M_{M_{ij}}$

where M_i_j is the number in the i'th row and j'th column.

We only consider latin squares where the first element of the n'th row is n, and where the first row is the identity (that is,
the identity permutation [1,2,3,...,n]. We also construct our latin squares to have the identity elements distributed to be
symmetric about the main diagonal. That is, if M_i_j = 1, then also M_j_i = 1. 

As we build our latin square, we check that each row, considered as a permutation, has order dividing the order of the group.                                               
We also construct each row to have order (considered as a permutation) at least as great as the last row. This is to avoid
constructing too many isomorphic tables, but is likely making it too difficult to find a single group of order 10. 

We return each group, together with the orders of it's elements as an ordered pair. Thus, running 
                                  
                                              fastGroup 4
will return 
                                               
                               [
                                ([1,2,4,4], [[1,2,3,4], [2,1,4,3], [3,4,2,1], [4,3,1,2]]),
                                ([1,2,2,2], [[1,2,3,4], [2,1,4,3], [3,4,1,2], [4,3,2,1]])
                               ]
               
                                           
The first element [1,2,4,4] of the first tuple gives the respective orders of each of the rows 
                              
                              [[1,2,3,4], [2,1,4,3], [3,4,2,1], [4,3,1,2]]
                       
The second tuple is of course the group M defined above with its signature [1,2,2,2]. These are the two groups of order 4.

The function fastGroup takes a positive integer n and will lazily (and exhaustively) find all the groups of order n.
Though we impose many normalizing conditions, the number of latin squares grows very fast with the number of rows. After 
compilation, our function can find all the groups of order 8 in almost instantly (exhausting all the latin squares is a
different matter). 
