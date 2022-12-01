DROP TABLE IF EXISTS contacts;

CREATE TABLE contacts (
	contactId	int unsigned NOT NULL auto_increment,
	name		varchar(255) NOT NULL,
	phoneNr		varchar(16),
	
	PRIMARY KEY(contactId)
);