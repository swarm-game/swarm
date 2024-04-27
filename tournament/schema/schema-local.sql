--
-- PostgreSQL database dump
--

-- Dumped from database version 14.11 (Ubuntu 14.11-0ubuntu0.22.04.1)
-- Dumped by pg_dump version 14.11 (Ubuntu 14.11-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: swarm; Type: DATABASE; Schema: -; Owner: postgres
--

CREATE DATABASE swarm WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.UTF-8';


ALTER DATABASE swarm OWNER TO postgres;

\connect swarm

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: evaluated_solution; Type: TABLE; Schema: public; Owner: kostmo
--

CREATE TABLE public.evaluated_solution (
    id integer NOT NULL,
    evaluated_at timestamp with time zone DEFAULT now() NOT NULL,
    scenario character varying(40) NOT NULL,
    seed bigint NOT NULL,
    wall_time_seconds double precision NOT NULL,
    ticks bigint,
    char_count integer,
    ast_size integer,
    builtin boolean NOT NULL
);


ALTER TABLE public.evaluated_solution OWNER TO kostmo;

--
-- Name: scenarios; Type: TABLE; Schema: public; Owner: kostmo
--

CREATE TABLE public.scenarios (
    content_sha1 character varying(40) NOT NULL,
    uploader integer NOT NULL,
    original_filename text,
    swarm_git_sha1 character varying(40),
    uploaded_at timestamp with time zone DEFAULT now() NOT NULL,
    content text NOT NULL
);


ALTER TABLE public.scenarios OWNER TO kostmo;

--
-- Name: solution_id_seq; Type: SEQUENCE; Schema: public; Owner: kostmo
--

CREATE SEQUENCE public.solution_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.solution_id_seq OWNER TO kostmo;

--
-- Name: solution_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: kostmo
--

ALTER SEQUENCE public.solution_id_seq OWNED BY public.evaluated_solution.id;


--
-- Name: solution_submission; Type: TABLE; Schema: public; Owner: kostmo
--

CREATE TABLE public.solution_submission (
    content_sha1 character varying(40) NOT NULL,
    uploader integer NOT NULL,
    uploaded_at timestamp with time zone DEFAULT now() NOT NULL,
    solution_evaluation integer
);


ALTER TABLE public.solution_submission OWNER TO kostmo;

--
-- Name: users; Type: TABLE; Schema: public; Owner: kostmo
--

CREATE TABLE public.users (
    id integer NOT NULL,
    alias text NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.users OWNER TO kostmo;

--
-- Name: submissions; Type: VIEW; Schema: public; Owner: kostmo
--

CREATE VIEW public.submissions AS
 SELECT scenarios.original_filename,
    scenarios.content_sha1 AS scenario,
    scenarios.uploaded_at AS scenario_uploaded_at,
    COALESCE(foo.submission_count, (0)::bigint) AS submission_count,
    users.alias AS scenario_uploader,
    scenarios.swarm_git_sha1
   FROM ((public.scenarios
     LEFT JOIN ( SELECT evaluated_solution.scenario,
            count(*) AS submission_count
           FROM public.evaluated_solution
          WHERE (NOT evaluated_solution.builtin)
          GROUP BY evaluated_solution.scenario) foo ON (((scenarios.content_sha1)::text = (foo.scenario)::text)))
     JOIN public.users ON ((scenarios.uploader = users.id)));


ALTER TABLE public.submissions OWNER TO kostmo;

--
-- Name: users_id_seq; Type: SEQUENCE; Schema: public; Owner: kostmo
--

CREATE SEQUENCE public.users_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.users_id_seq OWNER TO kostmo;

--
-- Name: users_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: kostmo
--

ALTER SEQUENCE public.users_id_seq OWNED BY public.users.id;


--
-- Name: evaluated_solution id; Type: DEFAULT; Schema: public; Owner: kostmo
--

ALTER TABLE ONLY public.evaluated_solution ALTER COLUMN id SET DEFAULT nextval('public.solution_id_seq'::regclass);


--
-- Name: users id; Type: DEFAULT; Schema: public; Owner: kostmo
--

ALTER TABLE ONLY public.users ALTER COLUMN id SET DEFAULT nextval('public.users_id_seq'::regclass);


--
-- Name: scenarios scenarios_pkey; Type: CONSTRAINT; Schema: public; Owner: kostmo
--

ALTER TABLE ONLY public.scenarios
    ADD CONSTRAINT scenarios_pkey PRIMARY KEY (content_sha1);


--
-- Name: solution_submission solution_file_pkey; Type: CONSTRAINT; Schema: public; Owner: kostmo
--

ALTER TABLE ONLY public.solution_submission
    ADD CONSTRAINT solution_file_pkey PRIMARY KEY (content_sha1);


--
-- Name: evaluated_solution solution_pkey; Type: CONSTRAINT; Schema: public; Owner: kostmo
--

ALTER TABLE ONLY public.evaluated_solution
    ADD CONSTRAINT solution_pkey PRIMARY KEY (id);


--
-- Name: users users_pkey; Type: CONSTRAINT; Schema: public; Owner: kostmo
--

ALTER TABLE ONLY public.users
    ADD CONSTRAINT users_pkey PRIMARY KEY (id);


--
-- Name: fki_solution_file_solution; Type: INDEX; Schema: public; Owner: kostmo
--

CREATE INDEX fki_solution_file_solution ON public.solution_submission USING btree (solution_evaluation);


--
-- Name: fki_solution_scenario; Type: INDEX; Schema: public; Owner: kostmo
--

CREATE INDEX fki_solution_scenario ON public.evaluated_solution USING btree (scenario);


--
-- Name: scenario_uploader; Type: INDEX; Schema: public; Owner: kostmo
--

CREATE INDEX scenario_uploader ON public.scenarios USING btree (uploader);


--
-- Name: scenarios scenarios_uploader_fkey; Type: FK CONSTRAINT; Schema: public; Owner: kostmo
--

ALTER TABLE ONLY public.scenarios
    ADD CONSTRAINT scenarios_uploader_fkey FOREIGN KEY (uploader) REFERENCES public.users(id) NOT VALID;


--
-- Name: solution_submission solution_file_solution; Type: FK CONSTRAINT; Schema: public; Owner: kostmo
--

ALTER TABLE ONLY public.solution_submission
    ADD CONSTRAINT solution_file_solution FOREIGN KEY (solution_evaluation) REFERENCES public.evaluated_solution(id) NOT VALID;


--
-- Name: evaluated_solution solution_scenario; Type: FK CONSTRAINT; Schema: public; Owner: kostmo
--

ALTER TABLE ONLY public.evaluated_solution
    ADD CONSTRAINT solution_scenario FOREIGN KEY (scenario) REFERENCES public.scenarios(content_sha1) NOT VALID;


--
-- PostgreSQL database dump complete
--

