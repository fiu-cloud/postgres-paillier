# Postgres Paillier UDFs

Postgres UDF (using R / prl) to do floating point paillier encryption operations

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
drop table if exists encrypted_tbl;
drop table if exists calcs_tbl;
drop table if exists decrypted_tbl;

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

create table encrypted_tbl as (select id,encrypt(a) as a_enc, encrypt(b) as b_enc, a,b from raw);
create table calcs_tbl as (select id, a, b, add_enc(a_enc,b_enc) as c, sub_enc(a_enc,b_enc) as d, smult_enc(a_enc,b) as e from encrypted_tbl);
create table decrypted_tbl as (select id, a,b,decrypt(c) as addition, decrypt(d) as subtraction,decrypt(e) as multiply from calcs_tbl);
select * from decrypted_tbl;

```


