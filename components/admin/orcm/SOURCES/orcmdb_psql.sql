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
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: -
--

-- CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: -
--

-- COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

--
-- Name: add_data_sample(character varying, character varying, character varying, timestamp without time zone, double precision, character varying, character varying); Type: FUNCTION; Schema: public; Owner: -
--

CREATE FUNCTION add_data_sample(p_hostname character varying, p_data_group character varying, p_data_item character varying, p_time_stamp timestamp without time zone, p_value_num double precision, p_value_str character varying, p_units character varying) RETURNS void
    LANGUAGE plpgsql
    AS $$
DECLARE
    v_node_id numeric;
    v_data_item_id numeric;
    v_data_item_name varchar(250);
    v_data_type numeric;

BEGIN
    -- Initialize variables
    v_node_id := 0;
    v_data_item_id := 0;
    v_data_item_name := p_data_group || '_';
    v_data_item_name := v_data_item_name || p_data_item;

    -- Determine the value type
    if (p_value_num is not null and p_value_str is null) then
        v_data_type = 1; -- Numeric data type
    elseif (p_value_str is not null and p_value_num is null) then
        v_data_type = 2;
    else
        raise exception 'No value passed or ambiguous value defined';
        raise SQLSTATE '45000';
    end if;

    -- Get the node ID
    select node_id into v_node_id
    from node
    where hostname = p_hostname;

    -- Add the node if it doesn't exist
    if (v_node_id = 0 or v_node_id is null) then
        insert into node(
            hostname,
            node_type,
            status,
            num_cpus,
            num_sockets,
            cores_per_socket,
            threads_per_core,
            memory)
        values(
            p_hostname,
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            NULL,
            NULL);

        select node_id into v_node_id
        from node
        where hostname = p_hostname;
    end if;

    -- Get the data item ID
    select data_item_id into v_data_item_id
    from data_item
    where name = v_data_item_name;

    -- Add the data item if it doesn't exist
    if (v_data_item_id = 0 or v_data_item_id is null) then
        insert into data_item(
            name,
            data_type)
        values(
            v_data_item_name,
            v_data_type);

        select data_item_id into v_data_item_id
        from data_item
        where name = v_data_item_name;
    end if;

    -- Add the data sample
    insert into data_sample(
        node_id,
        data_item_id,
        time_stamp,
        value_num,
        value_str,
        units)
    values(
        v_node_id,
        v_data_item_id,
        p_time_stamp,
        p_value_num,
        p_value_str,
        p_units);

    return;
END;
$$;


SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: data_item; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE data_item (
    data_item_id integer NOT NULL,
    name character varying(250) NOT NULL,
    data_type integer DEFAULT 1 NOT NULL,
    CONSTRAINT data_type CHECK (((data_type = 1) OR (data_type = 2)))
);


--
-- Name: data_item_data_item_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE data_item_data_item_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: data_item_data_item_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE data_item_data_item_id_seq OWNED BY data_item.data_item_id;


--
-- Name: data_sample; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE data_sample (
    node_id integer NOT NULL,
    data_item_id integer NOT NULL,
    time_stamp timestamp without time zone NOT NULL,
    value_num double precision,
    value_str character varying(50),
    units character varying(50)
);


--
-- Name: node; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE node (
    node_id integer NOT NULL,
    hostname character varying NOT NULL,
    node_type smallint,
    status smallint,
    num_cpus smallint,
    num_sockets smallint,
    cores_per_socket smallint,
    threads_per_core smallint,
    memory smallint
);


--
-- Name: data_samples_view; Type: VIEW; Schema: public; Owner: -
--

CREATE VIEW data_samples_view AS
 SELECT ( SELECT node.hostname
           FROM node) AS hostname,
    ( SELECT data_item.name
           FROM data_item) AS name,
    ( SELECT data_item.data_type
           FROM data_item) AS data_type,
    ( SELECT data_sample.value_num
           FROM data_sample) AS value_num,
    ( SELECT data_sample.units
           FROM ((data_sample
      JOIN node ON ((( SELECT node_1.node_id
              FROM node node_1) = ( SELECT data_sample_1.node_id
              FROM data_sample data_sample_1))))
   JOIN data_item ON ((( SELECT data_item_1.data_item_id
         FROM data_item data_item_1) = ( SELECT data_sample_1.data_item_id
         FROM data_sample data_sample_1))))) AS units;


--
-- Name: event; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE event (
    node_id integer NOT NULL,
    event_type_id integer NOT NULL,
    time_stamp timestamp without time zone NOT NULL,
    description text NOT NULL
);


--
-- Name: event_type; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE event_type (
    event_type_id integer NOT NULL,
    name character varying(250)
);


--
-- Name: event_type_event_type_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE event_type_event_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: event_type_event_type_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE event_type_event_type_id_seq OWNED BY event_type.event_type_id;


--
-- Name: fru; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE fru (
    node_id integer NOT NULL,
    fru_type_id integer NOT NULL,
    fru_id integer NOT NULL,
    serial_number character varying(50) NOT NULL
);


--
-- Name: fru_fru_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE fru_fru_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: fru_fru_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE fru_fru_id_seq OWNED BY fru.fru_id;


--
-- Name: fru_type; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE fru_type (
    fru_type_id integer NOT NULL,
    name character varying(250) NOT NULL
);


--
-- Name: fru_type_fru_type_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE fru_type_fru_type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: fru_type_fru_type_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE fru_type_fru_type_id_seq OWNED BY fru_type.fru_type_id;


--
-- Name: job; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE job (
    job_id integer NOT NULL,
    name character varying(100),
    num_nodes integer NOT NULL,
    username character varying(75) NOT NULL,
    time_submitted timestamp without time zone NOT NULL,
    start_time timestamp without time zone,
    end_time timestamp without time zone,
    energy_usage real,
    exit_status integer
);


--
-- Name: job_job_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE job_job_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: job_job_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE job_job_id_seq OWNED BY job.job_id;


--
-- Name: job_node; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE job_node (
    job_id integer NOT NULL,
    node_id integer NOT NULL
);


--
-- Name: maintenance_record; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE maintenance_record (
    node_id integer NOT NULL,
    fru_type_id integer NOT NULL,
    fru_id integer NOT NULL,
    replacement_date date NOT NULL,
    old_serial_number character varying(50) NOT NULL,
    new_serial_number character varying(50) NOT NULL
);


--
-- Name: node_node_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE node_node_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


--
-- Name: node_node_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE node_node_id_seq OWNED BY node.node_id;


--
-- Name: data_item_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY data_item ALTER COLUMN data_item_id SET DEFAULT nextval('data_item_data_item_id_seq'::regclass);


--
-- Name: event_type_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY event_type ALTER COLUMN event_type_id SET DEFAULT nextval('event_type_event_type_id_seq'::regclass);


--
-- Name: fru_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY fru ALTER COLUMN fru_id SET DEFAULT nextval('fru_fru_id_seq'::regclass);


--
-- Name: fru_type_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY fru_type ALTER COLUMN fru_type_id SET DEFAULT nextval('fru_type_fru_type_id_seq'::regclass);


--
-- Name: job_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY job ALTER COLUMN job_id SET DEFAULT nextval('job_job_id_seq'::regclass);


--
-- Name: node_id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE ONLY node ALTER COLUMN node_id SET DEFAULT nextval('node_node_id_seq'::regclass);


--
-- Name: data_item_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY data_item
    ADD CONSTRAINT data_item_pkey PRIMARY KEY (data_item_id);


--
-- Name: data_sample_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY data_sample
    ADD CONSTRAINT data_sample_pkey PRIMARY KEY (node_id, data_item_id, time_stamp);


--
-- Name: event_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY event
    ADD CONSTRAINT event_pkey PRIMARY KEY (node_id, event_type_id, time_stamp);


--
-- Name: event_type_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY event_type
    ADD CONSTRAINT event_type_pkey PRIMARY KEY (event_type_id);


--
-- Name: fru_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY fru
    ADD CONSTRAINT fru_pkey PRIMARY KEY (node_id, fru_type_id, fru_id);


--
-- Name: fru_type_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY fru_type
    ADD CONSTRAINT fru_type_pkey PRIMARY KEY (fru_type_id);


--
-- Name: job_node_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY job_node
    ADD CONSTRAINT job_node_pkey PRIMARY KEY (job_id, node_id);


--
-- Name: job_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY job
    ADD CONSTRAINT job_pkey PRIMARY KEY (job_id);


--
-- Name: maintenance_record_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY maintenance_record
    ADD CONSTRAINT maintenance_record_pkey PRIMARY KEY (node_id, fru_type_id, fru_id, replacement_date);


--
-- Name: node_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY node
    ADD CONSTRAINT node_pkey PRIMARY KEY (node_id);


--
-- Name: unique_host; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY node
    ADD CONSTRAINT unique_host UNIQUE (hostname);


--
-- Name: unique_name; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY data_item
    ADD CONSTRAINT unique_name UNIQUE (name);


--
-- Name: data_sample_data_item_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY data_sample
    ADD CONSTRAINT data_sample_data_item_id_fkey FOREIGN KEY (data_item_id) REFERENCES data_item(data_item_id);


--
-- Name: data_sample_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY data_sample
    ADD CONSTRAINT data_sample_node_id_fkey FOREIGN KEY (node_id) REFERENCES node(node_id);


--
-- Name: event_event_type_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY event
    ADD CONSTRAINT event_event_type_id_fkey FOREIGN KEY (event_type_id) REFERENCES event_type(event_type_id);


--
-- Name: event_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY event
    ADD CONSTRAINT event_node_id_fkey FOREIGN KEY (node_id) REFERENCES node(node_id);


--
-- Name: fru_fru_type_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY fru
    ADD CONSTRAINT fru_fru_type_id_fkey FOREIGN KEY (fru_type_id) REFERENCES fru_type(fru_type_id);


--
-- Name: fru_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY fru
    ADD CONSTRAINT fru_node_id_fkey FOREIGN KEY (node_id) REFERENCES node(node_id);


--
-- Name: job_node_job_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY job_node
    ADD CONSTRAINT job_node_job_id_fkey FOREIGN KEY (job_id) REFERENCES job(job_id);


--
-- Name: job_node_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY job_node
    ADD CONSTRAINT job_node_node_id_fkey FOREIGN KEY (node_id) REFERENCES node(node_id);


--
-- Name: maintenance_record_node_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: -
--

ALTER TABLE ONLY maintenance_record
    ADD CONSTRAINT maintenance_record_node_id_fkey FOREIGN KEY (node_id, fru_type_id, fru_id) REFERENCES fru(node_id, fru_type_id, fru_id);


--
-- PostgreSQL database dump complete
--

