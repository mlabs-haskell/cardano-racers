CREATE TYPE RARITY AS ENUM ('common', 'rare', 'epic');
CREATE TYPE ASSET_TYPE AS ENUM ('car', 'driver');

CREATE TABLE RACE (
  race_hash varchar not null,
  nitro_fee bigint not null,
  -- race_type varchar not null; free-roll, h2h, etc.
  -- conditions ...
  PRIMARY KEY (race_hash, nitro_fee)
);

CREATE TABLE ASSET_OPTION (
  name varchar not null,
  rarity RARITY not null,
  asset_type ASSET_TYPE not null,
  image_url varchar not null,
  description varchar not null,
  nitro_amount bigint not null,
  PRIMARY KEY (rarity)
);

CREATE TABLE CAR (
  name varchar not null,
  token_name varchar not null,
  rarity RARITY not null,
  image_url varchar not null,
  description varchar not null,
  attributes jsonb not null,
  PRIMARY KEY (token_name)
);

CREATE TABLE DRIVER (
  name varchar not null,
  token_name varchar not null,
  rarity RARITY not null,
  image_url varchar not null,
  description varchar not null,
  attributes jsonb not null,
  PRIMARY KEY (token_name)
);

CREATE TABLE RACE_PARTICIPANTS (
  race_hash varchar not null,
  nitro_fee bigint not null,
  car_token_name varchar not null,
  driver_token_name varchar not null,
  payout_address varchar not null,
  CONSTRAINT car_token_name_constr FOREIGN KEY (car_token_name) REFERENCES CAR(token_name),
  CONSTRAINT driver_token_name_constr FOREIGN KEY (driver_token_name) REFERENCES DRIVER(token_name),
  CONSTRAINT race_constr FOREIGN KEY (race_hash, nitro_fee) REFERENCES RACE(race_hash, nitro_fee)
);

CREATE TABLE RACE_REGISTRATIONS (
  race_hash varchar not null,
  nitro_fee bigint not null,
  pubkey_hash varchar not null,
  FOREIGN KEY (race_hash, nitro_fee) REFERENCES RACE(race_hash, nitro_fee)
);
