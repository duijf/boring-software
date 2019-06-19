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

## First domain things

1. Find the docs for `ConnectInfo`. Try to make a value of this type.

1. Try to connect to Postgres from Haskell using you rown `ConnectInfo`. Use
the credentials from the DB setup.

1. Find out what functions from `postgresql-simple` allow you to operate on a
`Connection`. Read the sigs for `query` and `execute`.

1. Find out how to get a `Query` type.

1. Find out what function has type `Bytestring -> Query`

1. Write a function `executeSqlFile :: Connection -> FilePath -> IO ()`

## Domain model

1. Think of how we'd want to represent the structure of the `schemactl` tables
   in Haskell types things in the Database.  Find them in
   `db/000_bootstrap.sql.up`.

1. Create Haskell types for these.

1. Find out from the docs how `query` and `execute` work.

1. See if you can get `FromRow` and `ToRow` instances for your custom types.

1. Write a function `markActiveRevision :: Pg.Connection -> ? -> IO ()` which
marks a revision as active in the DB. You might need to create a type for this.

1. Write a function `getActiveRevision :: Pg.Connection -> IO ?` which marks a
revision as active in the DB. You might need to re-use your type from the
previous section.
