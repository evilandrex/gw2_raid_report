# gw2_raid_report
Andrexia's raid report!

## Database setup
```SQL
CREATE DATABASE raid_report;
USE raid_report;

CREATE TABLE encounter_data (
  fight_id VARCHAR(50) PRIMARY KEY,
  team VARCHAR(50),
  boss VARCHAR(50),
  sucess INT,
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
  player10 VARCHAR(50)
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
  PRIMARY KEY (fight_id, char_name)
);

CREATE TABLE buff_data (
  fight_id VARCHAR(50),
  char_name VARCHAR(50),
  PRIMARY KEY (fight_id, char_name)
);

```