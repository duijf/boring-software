# Excersises day II and waay beyond

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

   What is the difference between the types `parseArgs :: IO (Maybe EventType)`
   and `parseArgs :: [String] -> Maybe EventType`? How do they differ in their
   use?

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

1. Add support for specifying the amount of migrations to run on during an
   upgrade or downgrade. Make people pass it on the CLI.

1. Add more validation logic to the `getMigrationsToRun` function (for
   interesting mistakes humans could make). Add a `--strict` mode to toggle
   these validations.

1. Your CLI code is probably getting a bit convoluted at this point, especially
   if you want to handle common idioms wherer the order does not matter. See if
   you can change your hand-written parser to one based on
   [`optparse-applicative`][optparse].

1. Are you happy with your error handling code? (Yes is fine; trust your
   instinct). If not, research alternatives and try them out until you're
   happy.

1. Add support for making connections to multiple databases. Add these to your
   config file. Allow the user to specify which database to run migrations
   against in the config file.

1. Read passwords/secrets from another source like environment variables or
   maybe some encrypted file? (Hint for the latter: See if you can use
   [`fernet`][fernet])

1. Add support for making connections over SSH tunnels. Research your own libs
   and write an implementation. Add this as an option in the config file.

1. Since we now have SSH tunnels: add support for locking before running
   migrations. Add a `break-lock` command to your CLI.

And so on and so on... At this point (and probably also before), you will have
a pretty great idea on how to write useful tooling in Haskell.

[getargs]:https://www.stackage.org/haddock/lts-13.26/base-4.12.0.0/System-Environment.html#v:getArgs
[attoparsec]:https://www.stackage.org/haddock/lts-13.26/attoparsec-0.13.2.2/Data-Attoparsec-ByteString-Char8.html
[optparse]:https://www.stackage.org/lts-13.26/package/optparse-applicative-0.14.3.0
[fernet]:https://github.com/fernet/spec
