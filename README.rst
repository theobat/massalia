Massalia
------------------

Massalia is a framework for designing APIs on top of 
`Morpheus-graphql <https://github.com/morpheusgraphql/morpheus-graphql>`_ and 
`Hasql <https://hackage.haskell.org/package/hasql>`_.

It draws most of its inspiration primarily from `join-monster <https://github.com/join-monster/join-monster/tree/master>`_,
and a little bit from `hasura <https://hasura.io/>`_.

The main ideas and opinions are:

- A GraphQL query/subscription/mutation contains enough information to build an SQL query if needed.
- The data should be normalized on the input and denormalized on the output.
- The SQL queries should maximize the filtering joins/where and minimize the data-fetching ones.
  That is, the data shaping should happen at the select stage as much as possible.

What's the difference with join-monster and hasura then ?

  - Join monster uses a join-heavy approach where data fetching is mixed with data shaping (joins are used for both) which is
    not needed. Besides join monster lacks a number of important features such as parametrized queries and support for user-defined types
    at the sql level.
  - Hasura is made/optimized towards JSON rendering in the database. Massalia is geared towards postgresql rows/arrays rendering in the 
    the datatabase. The result is that massalia is more haskell oriented, whereas hasura really shines 
    any situation where you want to write business logic separatly from the query engine. Under heavy loads, using hasura is probably
    a better bet.

.. warning::

  It's in alpha **alpha** stage and not on hackage yet. If you want to use it in its early form
  please refer to the github version. It won't be on hackage before several month.

