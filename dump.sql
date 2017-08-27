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

CREATE TABLE  IF NOT EXISTS entries (
    id bigserial PRIMARY KEY NOT NULL,
    title text NOT NULL,
    url text DEFAULT "",
    summary text DEFAULT "",
    body text NOT NULL,
    markdown text NOT NULL,
    published boolean DEFAULT false,
    created_at timestamp without time zone NOT NULL DEFAULT now(),
    updated_at timestamp without time zone NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS tags (
  id bigserial PRIMARY KEY NOT NULL,
  name text NOT NULL
);
CREATE UNIQUE INDEX tags_unique ON tags (name);

CREATE TABLE IF NOT EXISTS taggings(
  id bigserial PRIMARY KEY NOT NULL,
  tag_id bigint NOT NULL,
  entry_id bigint NOT NULL
);
CREATE UNIQUE INDEX taggings_unique ON taggings (tag_id,entry_id);

CREATE TABLE IF NOT EXISTS series (
  entry_id bigint NOT NULL,
  name   VARCHAR(255) NOT NULL,
  index    INTEGER NOT NULL,

  PRIMARY KEY (entry_id, series),
  FOREIGN KEY (entry_id) REFERENCES entries (id) ON DELETE CASCADE
);
