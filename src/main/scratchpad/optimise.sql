DROP TABLE IF EXISTS products;
drop table if exists a;
CREATE TABLE products (
 product_id BIGSERIAL,
 product_name VARCHAR (255) NOT NULL
);

INSERT INTO products(product_name) VALUES ('a');
INSERT INTO products(product_name) VALUES ('b');
INSERT INTO products(product_name) VALUES ('c');
INSERT INTO products(product_name) VALUES ('d');
INSERT INTO products(product_name) VALUES ('e');
INSERT INTO products(product_name) VALUES ('f');
INSERT INTO products(product_name) VALUES ('g');
INSERT INTO products(product_name) VALUES ('h');
CREATE TABLE a as select product_id, product_id%5 as group_id, product_name from products;
select * from  a;


select product_id,product_name, group_id, ROW_NUMBER() OVER (PARTITION BY group_id) from a;


-- create test table
CREATE TABLE test_data (
fyear integer,
firm float8,
eps float8
);
-- insert randomly pertubated data for test
INSERT INTO test_data
SELECT
    (b.f + 1) % 10 + 2000 AS fyear,
    floor((b.f+1)/10) + 50 AS firm,
    f::float8/100 + random()/10 AS eps
FROM
    generate_series(-500,499,1) b(f);

CREATE OR REPLACE
FUNCTION r_regr_slope(float8, float8)
RETURNS float8 AS
$BODY$
slope <- NA
y <- farg1
x <- farg2
if (fnumrows==9) try (slope <- lm(y ~ x)$coefficients[2])
return(slope)
$BODY$
LANGUAGE plr WINDOW;

SELECT *, r_regr_slope(eps, lag_eps) OVER w AS slope_R
FROM (SELECT firm, fyear, eps,
lag(eps) OVER (ORDER BY firm, fyear) AS lag_eps
FROM test_data) AS a
WHERE eps IS NOT NULL
WINDOW w AS (ORDER BY firm, fyear ROWS 8 PRECEDING);
In