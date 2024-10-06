create table person(
    "personId" uuid PRIMARY KEY,
    "givenName" varchar(127),
    "middleNames" varchar(255),
    "lastName" varchar(127),
    "email" varchar(255),
    "updatedAt" timestamp
);
create table application(
    "applicationId" uuid PRIMARY KEY,
    "applicationSecret" varchar(255),
    "name" varchar(127),
    "redirectUris" text
);
create table session(
    sessionId uuid PRIMARY KEY,
    personId uuid REFERENCES person,
    startedAt timestamp,
    expireAt timestamp 
);
create table log(
    logId uuid PRIMARY KEY,
    logType varchar(127),
    jsonData jsonb
);
insert into person("personId", "givenName", "middleNames", "lastName", "email", "updatedAt")
values ('2731e589-b931-40e9-ae8f-a204baccdc61', 'Yoan', 'middle', 'Poulmarck', 'ypoulmarck@gmail.com', CURRENT_TIMESTAMP)
--psql -h localhost -p 5432 -d LambdaAuth -U user
