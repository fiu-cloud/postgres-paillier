CREATE EXTENSION plr;
CREATE OR REPLACE FUNCTION encrypt (float) RETURNS bytea AS 'return(encrypt(arg1))' LANGUAGE 'plr';
CREATE OR REPLACE FUNCTION decrypt (bytea) RETURNS float AS 'return(decrypt(arg1))' LANGUAGE 'plr';
CREATE OR REPLACE FUNCTION add_enc (bytea,bytea) RETURNS bytea AS 'return(addenc(arg1,arg2))' LANGUAGE 'plr';
