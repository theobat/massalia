===============
Massalia
===============

Massalia is a framework for designing `GraphQL <https://graphql.org/>`_ APIs on top of 
`Morpheus-graphql <https://github.com/morpheusgraphql/morpheus-graphql>`_ and 
`Hasql <https://hackage.haskell.org/package/hasql>`_.

It draws most of its inspiration from `join-monster <https://github.com/join-monster/join-monster/tree/master>`_,
and -to a lesser extent- from `hasura <https://hasura.io/>`_.


Main ideas and opinions
-------------------------------

- A GraphQL query/subscription/mutation contains enough information to build an SQL query if needed.
- The data should be normalized on the input and denormalized on the output.
- The SQL queries should maximize the filtering joins/where and minimize the data-fetching ones.
  That is, the data shaping should happen at the select stage as much as possible.

What's the difference with join-monster and hasura then ?

- **Join monster** uses a join-heavy approach where data fetching is mixed with data shaping (joins are used for both) which is not needed. Besides join monster lacks a number of important features such as parametrized queries and support for user-defined types at the sql level.
- **Hasura** is made/optimized towards JSON rendering in the database. Massalia is geared towards postgresql rows/arrays rendering in the the datatabase. The result is that massalia is more haskell oriented, whereas hasura really shines any situation where you want to write business logic separatly from the query engine. Under heavy loads, using hasura along with a distributed architecture, is probably a better bet.

Is it an ORM or Type-Relational mapping ?

Yes and no. It could be used as an ORM or as a Type-Relational mapping, no doubt.
But it lacks a good definition for "this field is not needed", we rely on defining a default value for now.
(id est, we define default values for all the records we map to the database).
Which is pretty shitty, but it's mostly a non concern when using massalia with GraphQL,
because we constructively use the fields sent in the graphQL query, and all the others are ignored at the
graphQL level.
For a more widespread ORM/TRM, the issue is not at the library level, it lies in Haskell's lack of a proper
extensible record system. Thus, for now (and almost certainly until haskell gets proper dependent types and/or extensible records),
the library is not meant for out-of-graphQL usage.

Quick exemple
-------------------------------

The main function of massalia is to implement this workflow:

- assuming you defined this:

.. code-block:: haskell

  data Plant = Plant {
    id :: UUID,
    name :: Text
  } deriving (Show, Generic, GQLType, SQLRecord ())

  instance SQLSelect () Plant where
    toSelectQuery = basicQueryAndDecoder (basicEntityNoFilter "plant")

- assuming it receives this graphql query:

.. code-block:: graphql

  query { plantList { id name } }

- it should generate this query (along with its `hasql decoder <hackage.haskell.org/package/hasql-1.4.2/docs/Hasql-Decoders.html>`_)

.. code-block:: postgres

  SELECT row(plant.name, plant.id)
  FROM plant

- then hasql decode it into this value

.. code-block:: haskell

  resultValue = [Plant {id="d2630f2b-fa27-4d3e-9d99-2c3c66c98483", name="Some example plant name"}]

- and eventually (if nothing else is done at the haskell level) into this JSON:

.. code-block:: JSON

    { plantList:
        [
            { "name": "okokok"
            , "id": "d2630f2b-fa27-4d3e-9d99-2c3c66c98483"
            }
        ]
    }


It also handles nesting collection of types (through the collection combinator).


.. warning::

  It's in alpha **alpha** stage and not on hackage yet. If you want to use it in its early form
  please refer to the github version. It won't be on hackage before several month.

