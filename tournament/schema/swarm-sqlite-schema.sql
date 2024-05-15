PRAGMA foreign_keys = ON;

BEGIN TRANSACTION;

CREATE TABLE IF NOT EXISTS "users" (
	"alias"	TEXT NOT NULL,
	"cookie" TEXT NOT NULL UNIQUE DEFAULT (lower(hex(randomblob(16)))),
	"github_access_token"	TEXT NOT NULL,
	"github_access_token_expires_at"	DATETIME NOT NULL,
	"github_refresh_token"	TEXT NOT NULL,
	"github_refresh_token_expires_at"	DATETIME NOT NULL,
	PRIMARY KEY("alias")
);

CREATE TABLE IF NOT EXISTS "scenarios" (
	"content_sha1"	TEXT NOT NULL UNIQUE,
	"uploader"	TEXT NOT NULL,
	"original_filename"	TEXT,
	"title"	TEXT,
	"swarm_git_sha1"	TEXT,
	"uploaded_at"	DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"content"	TEXT NOT NULL,
	PRIMARY KEY("content_sha1"),
	FOREIGN KEY(uploader) REFERENCES users(alias)
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
	"uploader"	TEXT NOT NULL,
	"uploaded_at"	DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"solution_evaluation"	INTEGER,
	"content"	TEXT NOT NULL,
	PRIMARY KEY("content_sha1"),
	FOREIGN KEY(uploader) REFERENCES users(alias),
	FOREIGN KEY(solution_evaluation) REFERENCES evaluated_solution(id)
);

CREATE VIEW agg_scenario_submissions AS
 SELECT scenarios.original_filename,
    scenarios.content_sha1 AS scenario,
    scenarios.uploaded_at AS scenario_uploaded_at,
    COALESCE(foo.submission_count, 0) AS submission_count,
    scenarios.uploader AS scenario_uploader,
    scenarios.swarm_git_sha1,
	scenarios.title
   FROM ((scenarios
     LEFT JOIN ( SELECT evaluated_solution.scenario,
            count(*) AS submission_count
           FROM evaluated_solution
          WHERE (NOT evaluated_solution.builtin)
          GROUP BY evaluated_solution.scenario) foo ON (scenarios.content_sha1 = foo.scenario))
     );


CREATE VIEW all_solution_submissions AS
SELECT
	evaluated_solution.scenario,
    solution_submission.uploaded_at,
	evaluated_solution.seed,
	evaluated_solution.wall_time_seconds,
	evaluated_solution.ticks,
	evaluated_solution.char_count,
	evaluated_solution.ast_size,
    solution_submission.uploader AS solution_submitter,
    solution_submission.content_sha1 AS solution_sha1
   FROM solution_submission
	 JOIN evaluated_solution ON evaluated_solution.id = solution_submission.solution_evaluation
   WHERE NOT evaluated_solution.builtin;

COMMIT;
