BEGIN TRANSACTION;
CREATE TABLE IF NOT EXISTS "users" (
	"id"	INTEGER NOT NULL UNIQUE,
	"alias"	TEXT NOT NULL,
	"created_at"	DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
	PRIMARY KEY("id" AUTOINCREMENT)
);
CREATE TABLE IF NOT EXISTS "scenarios" (
	"content_sha1"	TEXT NOT NULL UNIQUE,
	"uploader"	INTEGER NOT NULL,
	"original_filename"	TEXT,
	"swarm_git_sha1"	TEXT,
	"uploaded_at"	DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"content"	TEXT NOT NULL,
	PRIMARY KEY("content_sha1"),
	FOREIGN KEY(uploader) REFERENCES users(id)
);
CREATE TABLE IF NOT EXISTS "evaluated_solution" (
	"id"	INTEGER NOT NULL UNIQUE,
	"evaluated_at"	DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"scenario"	TEXT NOT NULL,
	"seed"	INTEGER NOT NULL,
	"wall_time_seconds"	REAL NOT NULL,
	"ticks"	INTEGER,
	"char_count"	INTEGER,
	"ast_size"	INTEGER,
	"builtin"	BOOLEAN,
	PRIMARY KEY("id" AUTOINCREMENT),
	FOREIGN KEY(scenario) REFERENCES scenarios(content_sha1)
);
CREATE TABLE IF NOT EXISTS "solution_submission" (
	"content_sha1"	TEXT NOT NULL,
	"uploader"	INTEGER NOT NULL,
	"uploaded_at"	DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"solution_evaluation"	INTEGER,
	PRIMARY KEY("content_sha1"),
	FOREIGN KEY(solution_evaluation) REFERENCES evaluated_solution(id)
);

CREATE VIEW submissions AS
 SELECT scenarios.original_filename,
    scenarios.content_sha1 AS scenario,
    scenarios.uploaded_at AS scenario_uploaded_at,
    COALESCE(foo.submission_count, 0) AS submission_count,
    users.alias AS scenario_uploader,
    scenarios.swarm_git_sha1
   FROM ((scenarios
     LEFT JOIN ( SELECT evaluated_solution.scenario,
            count(*) AS submission_count
           FROM evaluated_solution
          WHERE (NOT evaluated_solution.builtin)
          GROUP BY evaluated_solution.scenario) foo ON (scenarios.content_sha1 = foo.scenario))
     JOIN users ON (scenarios.uploader = users.id));
COMMIT;
