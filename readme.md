# Postgres Paillier UDFs

Postgres UDF (usign R / prl) to do floating point paillier encryption operations

# Build

```
docker build -t "postgres-paillier:latest" .
```

# Start Postgres & R

```
docker run --name my-paillier  -e POSTGRES_PASSWORD=mysecret -p 5432:5432 -d postgres-paillier
```

You can now connect PgpAdmin or similar to you localhost running paillier postgres (port 5432). Try the example below

# Using R directly
```
docker exec -i -t my-paillier /bin/bash
...
R
```

# Example
```
drop table if exists raw;
drop table if exists enc;
drop table if exists additions;
drop table if exists decrypted;

CREATE TABLE raw (id text, a numeric, b numeric);
INSERT INTO raw VALUES ('a',13.56,1.54);
INSERT INTO raw VALUES ('b',13.56,1.54);
INSERT INTO raw VALUES ('c', 35.7, 54.06);
INSERT INTO raw VALUES ('d', 0.7, -54.06);
INSERT INTO raw VALUES ('e', 3565.7, -54.06);
INSERT INTO raw VALUES ('f', 345.7, 324.06);
INSERT INTO raw VALUES ('g', 0.7, 154.06);
INSERT INTO raw VALUES ('h', 332.7, 235.06);
INSERT INTO raw VALUES ('i', 15.7, -56.06);
INSERT INTO raw VALUES ('j', 355.7, 568.06);
INSERT INTO raw VALUES ('k', 25.7, 1.06);

create table enc as (select id,encrypt(a) as a, encrypt(b) as b from raw);
create table additions as (select id, add_enc(a,b) as c from enc);
create table decrypted as (select id, decrypt(c) from additions);
select * from decrypted;


```


