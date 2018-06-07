drop table if exists grouped;
DROP TABLE IF EXISTS products;
-- create test table
drop table if exists test_data;
CREATE TABLE test_data (
row_id bigserial,
fyear integer,
firm float8,
eps float8
);
-- insert randomly pertubated data for test
INSERT INTO test_data(fyear,firm,eps)
SELECT
    (b.f + 1) % 10 + 2000 AS a,
    floor((b.f+1)/10) + 50 AS b,
    f::float8/100 + random()/10 AS c
FROM
    generate_series(-5000000,5000000,1) b(f);

CREATE TABLE grouped as select row_id, row_id%1000 as group_id,fyear,firm,eps from test_data;
CREATE TABLE grouped_1 as select group_id, array_agg(grouped.firm) from grouped group by group_id ;
SELECT sum(2) from grouped_1 limit 10


select group_id, array_agg(grouped.firm) from grouped group by group_id limit 100;





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
