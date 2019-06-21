# Excersises day II

1. Write an implementation for `getMigrationsToRun` to yield the migrations
   to run based on the current `Rev` from the database. What cases can't this
   function handle at this point?

1. Does the type for `getMigrationsToRun` function make sense? If not, what
   could it be instead?

1. Does the type for `Migration` still make sense? If not, what could
   it be instead?

1. If you want, change the types and fix the compile errors. You can also wait
   with this if you're unsure what/if there is a problem with these types.

1. Think of a CLI. Implement it. Keep the implementation simple. Try to see if
   you can use [`System.Environment.getArgs`][getargs] and a hand written
   parser.

1. File parsing time! See if you can read (a simple version of) that index
   file! Take a look at [`Data.Attoparsec.ByteString.Char8`][attoparsec] for
   parsing things.

   It's fine if your initial parser breaks on whitespace or comments.

1. Get your program to run the migrations from the `schemactl-index` file.

1. What information do you need if you want to support Downgrades? Change the
   program to support it. Hint: you probably need to change the types of
   `Migration`, `markActiveRevision`, and possibly your parser.

1. Add support for reading the connection settings from a config file. Can you
   use JSON for this? Hint: a small solution can include `Data.Aeson`,
   `GHC.Generics`, and `DeriveGeneric`.

1. Add support for whitespace and/or comments in your `indexFileP` parser.

[getargs]:https://www.stackage.org/haddock/lts-13.26/base-4.12.0.0/System-Environment.html#v:getArgs
[attoparsec]:https://www.stackage.org/haddock/lts-13.26/attoparsec-0.13.2.2/Data-Attoparsec-ByteString-Char8.html
