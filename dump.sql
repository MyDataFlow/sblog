--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner:
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner:
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: article; Type: TABLE; Schema: public; Owner: hblog; Tablespace:
--

CREATE TABLE  IF NOT EXISTS  articles (
    id bigserial PRIMARY KEY NOT NULL,
    title character varying(200) NOT NULL,
    summery character varying(600) NOT NULL,
    body text NOT NULL,
    hold boolean DEFAULT false,
    created_at timestamp DEFAULT now()
);

CREATE TABLE IF NOT EXISTS tags (
  id bigserial PRIMARY KEY NOT NULL,
  name text NOT NULL
);
CREATE UNIQUE INDEX tags_name ON tags (name);

CREATE TABLE IF NOT EXISTS tags_and_artiles(
  id bigserial PRIMARY KEY NOT NULL,
  tag_id bigint NOT NULL,
  article_id bigint NOT NULL
);
CREATE UNIQUE INDEX tags_and_artiles_unique ON tags_and_artiles (tag_id,article_id);
