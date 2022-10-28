import os

somno_paths = os.listdir("data/raw/somno_data/somno_analyses_data/")

zm_files = os.listdir("data/raw/screens_zmachine/")

my_cwa = os.listdir("data/raw/my_study_acc_data/cwa/")

# filenames for ALL screens cwa files
screens_bsl = os.listdir("/media/esbenlykke/My Passport/screens_all_cwa_files/baseline")
screens_fup = os.listdir("/media/esbenlykke/My Passport/screens_all_cwa_files/followup")

# screens baseline and followup samples
SAMPLE = ["baseline", "followup"]

path_bsl = "/media/esbenlykke/My Passport/screens_cwa_children/baseline/"
path_fup = "/media/esbenlykke/My Passport/screens_cwa_children/followup/"

# file lists for rule prepare_screens_cwa
screens_children_cwa_bsl = os.listdir("/media/esbenlykke/My Passport/screens_cwa_children/baseline")
screens_children_cwa_fup = os.listdir("/media/esbenlykke/My Passport/screens_cwa_children/followup")

# create file lists for "rule cp_children_cwa" 
list_file_bsl = open("data/processed/bsl_children_cwa.txt", "r")
list_cwa_bsl = list_file_bsl.read().splitlines()
list_file_fup = open("data/processed/fup_children_cwa.txt", "r")
list_cwa_fup = list_file_fup.read().splitlines()

bsl_children_paths = [path_bsl + str(x) for x in list_cwa_bsl]
fup_children_paths = [path_fup + str(x) for x in list_cwa_fup]

### MASTER RULE

rule targets:
  input:
    "data/processed/somno_sleep_profiles.tsv",
    "data/processed/zm_scores.tsv",
    "data/processed/bsl_children_cwa.txt",
    "data/processed/fup_children_cwa.txt",
    expand("{path}", path = bsl_children_paths),
    expand("{path}", path = fup_children_paths),
    "data/processed/acc_temp_psg_study.feather",
    "data/processed/acc_temp_screens_baseline.feather",
    "data/processed/acc_temp_screens_followup.feather",
    "data/processed/screens_baseline.parquet",
    "data/processed/screens_followup.parquet",
    "data/processed/somno_acc.parquet",
    "data/processed/screens_all_baseline.parquet",
    "data/processed/screens_all_followup.parquet",
    "data/processed/yasa_preds.tsv"
    

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

rule make_list_cwa_children:
  input:
    r_script = "code/list_children_cwa_files.R",
    bsl_info = "data/participant_info/screens_baseline_info.xlsx",
    fup_info = "data/participant_info/screens_followup_info.xlsx",
    bsl_cwa_files = expand("/media/esbenlykke/My Passport/screens_all_cwa_files/baseline/{id}", id = screens_bsl),
    fup_cwa_files = expand("/media/esbenlykke/My Passport/screens_all_cwa_files/followup/{id}", id = screens_fup)
  output:
    bsl_children = "data/processed/bsl_children_cwa.txt",
    fup_children = "data/processed/fup_children_cwa.txt"
  shell:
    "{input.r_script}"

rule cp_children_cwa:
  input:
    bsl_cp_from = expand("/media/esbenlykke/My Passport/screens_all_cwa_files/baseline/{id}", id = screens_bsl),
    fup_cp_from = expand("/media/esbenlykke/My Passport/screens_all_cwa_files/followup/{id}", id = screens_fup),
    bsl_text = "data/processed/bsl_children_cwa.txt",
    fup_text = "data/processed/fup_children_cwa.txt",
    script = "code/move_children_cwa.sh"
  output:
    cp_to_bsl = expand("{path}", path = bsl_children_paths),
    cp_to_fup = expand("{path}", path = fup_children_paths)
  shell:
    "{input.script}"

rule prepare_my_cwa:
  input:
    r_script = "code/aggregate_cwa_to_feather_psg_study.R",
    data = expand("data/raw/my_study_acc_data/cwa/{id}", id = my_cwa)
  params:
    epoch_length = 5,
    dest = "data/processed/acc_temp_psg_study.feather",
    cwa_path = "data/raw/my_study_acc_data/cwa",
    cores = 5
  output:
    temp("data/processed/acc_temp_psg_study.feather")
  shell:
    """
    {input.r_script} \
    {params.epoch_length} \
    {params.dest} \
    {params.cwa_path} \
    {params.cores}
    """
    
rule bsl_cwa_to_feather:
  input:
    bash_script = "code/split_files.sh",
    r_script = "code/aggregate_cwa_to_feather_screens.R",
    files = expand("/media/esbenlykke/My Passport/screens_cwa_children/baseline/{id}", id = screens_children_cwa_bsl)
  params:
    input_dir = "/media/esbenlykke/My\ Passport/screens_cwa_children/baseline",
    epoch_length = 5,
    dest = "~/sleep_study/data/processed/acc_temp_screens_baseline.feather"
    # num_cores = 1
  output:
    protected("data/processed/acc_temp_screens_baseline.feather")
  shell:
    """
    {input.bash_script} \
    {params.input_dir} \
    {params.epoch_length} \
    {params.dest}
    """

rule fup_cwa_to_feather:
  input:
    bash_script = "code/split_files.sh",
    r_script = "code/aggregate_cwa_to_feather_screens.R",
    iles = expand("/media/esbenlykke/My Passport/screens_cwa_children/followup/{id}", id = screens_children_cwa_fup)
  params:
    input_dir = "/media/esbenlykke/My\ Passport/screens_cwa_children/followup",
    epoch_length = 5,
    dest = "~/sleep_study/data/processed/acc_temp_screens_followup.feather"
    # num_cores = 1
  output:
    protected("data/processed/acc_temp_screens_followup.feather")
  shell:
    """
    {input.bash_script} \
    {params.input_dir} \
    {params.epoch_length} \
    {params.dest}
    """

rule join_info_bsl:
  input:
    r_script = "code/join_participant_info.R",
    feather = "data/processed/acc_temp_screens_baseline.feather",
    info = "data/participant_info/screens_baseline_info.xlsx"
  output:
    dest = temp("data/processed/screens_baseline.parquet")
  shell:
    """
    {input.r_script} {input.feather} {input.info} {output.dest}
    """
    
rule join_info_fup:
  input:
    r_script = "code/join_participant_info.R",
    feather = "data/processed/acc_temp_screens_followup.feather",
    info = "data/participant_info/screens_followup_info.xlsx"
  output:
    dest = temp("data/processed/screens_followup.parquet")
  shell:
    """
    {input.r_script} {input.feather} {input.info} {output.dest}
    """

rule somno_join_acc:
  input:
    "data/processed/acc_temp_psg_study.feather",
    "data/processed/somno_sleep_profiles.tsv"
  output:
    "data/processed/somno_acc.parquet"
  shell:
    "code/somno_join_acc.R"

rule zm_join_acc_bsl:
  input:
    r_script = "code/zm_join_sceens_acc.R",
    zm = "data/processed/zm_scores.tsv",
    acc = "data/processed/screens_baseline.parquet"
  params:
    dest = "data/processed/screens_all_baseline.parquet"
  output:
    "data/processed/screens_all_baseline.parquet"
  shell:
    "{input.r_script} {input.acc} {params.dest}"

rule zm_join_acc_fup:
  input:
    r_script = "code/zm_join_sceens_acc.R",
    zm = "data/processed/zm_scores.tsv",
    acc = "data/processed/screens_followup.parquet"
  params:
    dest = "data/processed/screens_all_followup.parquet"
  output:
    "data/processed/screens_all_followup.parquet"
  shell:
    "{input.r_script} {input.acc} {params.dest}"

EDF = os.listdir("data/raw/somno_data/somno_edf/")

rule get_yasa_preds:
  input:
    py_script = "code/yasa_sleep_staging.py",
    r_script = "code/combine_yasa_preds.R",
    edf = expand("data/raw/somno_data/somno_edf/{id}", id = EDF)
  output:
    "data/processed/yasa_preds.tsv"
  shell:
    """
    code/yasa_sleep_staging.py && code/combine_yasa_preds.R && rm data/raw/somno_data/somno_edf/*csv
    """
