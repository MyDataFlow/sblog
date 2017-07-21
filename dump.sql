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
    title character varying NOT NULL,
    summary character varying NOT NULL,
    body text NOT NULL,
    published boolean DEFAULT false,
    created_at timestamp without time zone NOT NULL DEFAULT now(),
    updated_at timestamp without time zone NOT NULL DEFAULT now()
);
CREATE TABLE bookmarks (
    id bigserial PRIMARY KEY NOT NULL,
    title character varying NOT NULL,
    summary character varying NOT NULL,
    url character varying NOT NULL,
    created_at timestamp without time zone NOT NULL DEFAULT now(),
    updated_at timestamp without time zone NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX bookmarks_unique ON bookmarks (url);

CREATE TABLE IF NOT EXISTS tags (
  id bigserial PRIMARY KEY NOT NULL,
  name text NOT NULL
);
CREATE UNIQUE INDEX tags_unique ON tags (name);

CREATE TABLE IF NOT EXISTS taggings(
  id bigserial PRIMARY KEY NOT NULL,
  tag_id bigint NOT NULL,
  related_type int DEFAULT 1,
  related_id bigint NOT NULL
);
CREATE UNIQUE INDEX taggings_unique ON taggings (tag_id,related_type,related_id);
