## Setup

1. Get Postgres

```
# Ubuntu/Debian
$ sudo apt install postgresql
$ sudo systemctl start postgresql@10-main.service

# Arch
$ sudo pacman -Syu postgresql
$ sudo systemctl start postgresql.service
$ sudo mkdir -p /var/lib/postgres/data
$ sudo chown postgres:postgres -R /var/lib/postgres/data
$ sudo -u postgres initdb /var/lib/postgres/data
$ sudo systemctl start postgresql.service
```

1. Get comfortable

```
git clone https://github.com/duijf/boring-software.git
cd boring-software/code
stack setup
stack build
```


1. Set up the cluster roles and permissions

```
$ sudo -u postgres psql
postgres=# create role "test" with login password 'test';
postgres=# create database "test";
postgres=# grant all privileges on database "test" to "test";
postgres=# \q
$ psql -U test test
password:
test=> create table foo (id bigserial primary key);
test=> drop table foo;
```

## First steps

1. Find the docs for `ConnectInfo`. Try to make a value of this type.

1. Try to connect to Postgres from Haskell using you rown `ConnectInfo`. Use
   the credentials from the DB setup.

1. Find out what functions from `postgresql-simple` allow you to operate on a
   `Connection`. Read the sigs for `query` and `execute`.

1. Find out how to get a `Query` type.

1. Find out what function has type `Bytestring -> Query`

1. Write a function `executeSqlFile :: Connection -> FilePath -> IO ()`

1. Think of how we'd want to represent the structure of the `schemactl` tables
   in Haskell types things in the Database.  Find them in
   `db/000_bootstrap.sql.up`.

1. Create Haskell types for these.

1. See if you can get `FromRow` and `ToRow` instances for your custom types.

1. Write a function `markActiveRevision :: Pg.Connection -> ? -> IO ()` which
   marks a revision as active in the DB. You might need to create a type for
   this.

1. Write a function `getActiveRevision :: Pg.Connection -> IO ?` which marks a
   revision as active in the DB. You might need to re-use your type from the
   previous section.

## Minimally useful!

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

## Other essentials

1. What information do you need if you want to support Downgrades? Change the
   program to support it. Hint: you probably need to change the types of
   `Migration`, `markActiveRevision`, and possibly your parser.

1. Add support for reading the connection settings from a config file. Can you
   use JSON for this? Hint: a small solution can include `Data.Aeson`,
   `GHC.Generics`, and `DeriveGeneric`.

1. Add support for specifying the amount of migrations to run on during an
   upgrade or downgrade. Make people pass it on the CLI.

## Productionize, enhance

1. Add support for whitespace and/or comments in your index file parser.

1. Add more validation logic to the `getMigrationsToRun` function (for
   interesting mistakes humans could make). Add a `--strict` mode to toggle
   these validations.

   Some ideas: duplicate migrations, warn/error on changes to migration files
   after they have been applied the fact. Warn/error when users change the
   order of migrations in the file based on the `schemactl_events` table.

1. Your CLI code is probably getting a bit convoluted at this point, especially
   if you want to handle common idioms wherer the order does not matter. See if
   you can change your hand-written parser to one based on
   [`optparse-applicative`][optparse].

1. Are you happy with your error handling code? ('Yes' is fine; trust your
   instinct). If not, research alternatives and try them out until you're
   happy.

1. What other things aren't you happy with? See if you can find solutions to
   the things that bother you.

## Competitive solution

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

[getargs]:https://www.stackage.org/haddock/lts-13.26/base-4.12.0.0/System-Environment.html#v:getArgs
[attoparsec]:https://www.stackage.org/haddock/lts-13.26/attoparsec-0.13.2.2/Data-Attoparsec-ByteString-Char8.html
[optparse]:https://www.stackage.org/lts-13.26/package/optparse-applicative-0.14.3.0
[fernet]:https://github.com/fernet/spec
