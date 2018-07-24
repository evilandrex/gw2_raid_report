# gw2_raid_report
Andrexia's raid report!

## Database setup
```SQL
CREATE DATABASE raid_report;
USE raid_report;

CREATE TABLE encounter_data (
  fight_id VARCHAR(50) PRIMARY KEY,
  log_link VARCHAR(50),
  team VARCHAR(50),
  boss VARCHAR(50),
  success INT,
  date INT,
  duration INT,
  team_dps INT,
  damage_done INT,
  player1 VARCHAR(50),
  player2 VARCHAR(50),
  player3 VARCHAR(50),
  player4 VARCHAR(50),
  player5 VARCHAR(50),
  player6 VARCHAR(50),
  player7 VARCHAR(50),
  player8 VARCHAR(50),
  player9 VARCHAR(50),
  player10 VARCHAR(50),
  missing_players VARCHAR(100),
  boss_damage FLOAT,
  bleeding FLOAT,
  burning FLOAT,
  confusion FLOAT,
  poison FLOAT,
  torment FLOAT,
  blind FLOAT,
  chilled FLOAT,
  crippled FLOAT,
  fear FLOAT,
  immobile FLOAT,
  slow FLOAT,
  taunt FLOAT,
  weakness FLOAT,
  vulnerability FLOAT,
  aegis FLOAT,
  alacrity FLOAT,
  fury FLOAT,
  might FLOAT,
  protection FLOAT,
  quickness FLOAT,
  regeneration FLOAT,
  resistance FLOAT,
  retaliation FLOAT,
  stability FLOAT,
  swiftness FLOAT,
  vigor FLOAT
);

CREATE TABLE player_data (
  fight_id VARCHAR(50),
  player_name VARCHAR(50),
  char_name VARCHAR(50),
  specialization VARCHAR(50),
  total_dps INT,
  power_dps INT,
  condi_dps INT,
  crit_percent FLOAT,
  above90_percent FLOAT,
  downed_count INT, 
  dead_at INT,
  subgroup INT,
  wep1 VARCHAR(50),
  wep2 VARCHAR(50),
  wep3 VARCHAR(50),
  wep4 VARCHAR(50),
  gear_stats VARCHAR(50),
  dmg_taken INT,
  PRIMARY KEY (fight_id, char_name)
);

CREATE TABLE buff_data (
  fight_id VARCHAR(50),
  char_name VARCHAR(50),
  PRIMARY KEY (fight_id, char_name)
);

CREATE TABLE boss_attacks (
  entry_id INT AUTO_INCREMENT PRIMARY KEY,
  fight_id VARCHAR(50),
  skill_name VARCHAR(50),
  damage_dealt INT,
  hits_percast FLOAT
);

CREATE TABLE team_info (
  team_id INT AUTO_INCREMENT PRIMARY KEY,
  team_name VARCHAR(50),
  team_code VARCHAR(50),
  player1 VARCHAR(50),
  player2 VARCHAR(50),
  player3 VARCHAR(50),
  player4 VARCHAR(50),
  player5 VARCHAR(50),
  player6 VARCHAR(50),
  player7 VARCHAR(50),
  player8 VARCHAR(50),
  player9 VARCHAR(50),
  player10 VARCHAR(50)
);


INSERT INTO team_info VALUES(
  1, 
  'Potatos', 
  'dhuumisdead',
  'blitzcreek.7019', 
  'Elestian.6134', 
  'MereLynn.9625', 
  'Vice.3164', 
  'Evil Andrex.8160', 
  'Burkid.4178', 
  'LucianTheAngelic.7054', 
  'PlasmaPunch.6281', 
  'kratox.3675', 
  'tunacom.6958');
```