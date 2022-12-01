DROP TABLE IF EXISTS projectworkers;
DROP TABLE IF EXISTS task;
DROP TABLE IF EXISTS project;
DROP TABLE IF EXISTS employee;

CREATE TABLE employee (
	name		varchar(255) NOT NULL,
	description	varchar(255) NOT NULL,
	PRIMARY KEY(name)
) ENGINE=InnoDB;

CREATE TABLE project (
	projectNr	int NOT NULL auto_increment,
	description	varchar(255) NOT NULL,
	parent		int NULL,
	PRIMARY KEY(projectNr),
	FOREIGN KEY(parent) REFERENCES project (projectNr)
) ENGINE=InnoDB;

CREATE TABLE projectworkers (
	employee	varchar(255) NOT NULL,
	project		int NOT NULL,
	PRIMARY KEY(employee,project),
	FOREIGN KEY(employee) REFERENCES employee (name),
	FOREIGN KEY(project) REFERENCES project (projectNr)
) ENGINE=InnoDB;

CREATE TABLE task (
	taskNr		int NOT NULL auto_increment,
	project		int NOT NULL,
	description	varchar(255) NOT NULL,
	done		tinyint NOT NULL,
	PRIMARY KEY(taskNr),
	FOREIGN KEY(project) REFERENCES project (projectNr)
) ENGINE=InnoDB;
