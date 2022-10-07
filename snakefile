import os

somno_paths = os.listdir("data/raw/somno_data/somno_analyses_data/")
zm_files = os.listdir("data/raw/screens_zmachine/")

# master rule

###

rule targets:
  input:
    "data/processed/somno_sleep_profiles.tsv",
    "data/processed/zm_scores.tsv"

###

rule get_somno_sleep_profiles:
  input:
    r_script = "code/get_sleep_profiles.R",
    relia = expand("data/raw/somno_data/somno_analyses_data/{id}/Sleep Profile Reliability.txt", id=somno_paths),
    status = expand("data/raw/somno_data/somno_analyses_data/{id}/Sleep Profile.txt", id=somno_paths)
  output:
    "data/processed/somno_sleep_profiles.tsv"
  shell:
    "{input.r_script}"

rule get_all_zm_data:
  input:
    r_script = "code/join_zm_data.R",
    data = expand("data/raw/screens_zmachine/{zm_files}", zm_files=zm_files)
  output:
    "data/processed/zm_scores.tsv"
  shell:
    "{input.r_script}"
