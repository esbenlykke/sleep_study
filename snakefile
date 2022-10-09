import os
import glob

somno_paths = os.listdir("data/raw/somno_data/somno_analyses_data/")

zm_files = os.listdir("data/raw/screens_zmachine/")

my_cwa = glob.glob("data/raw/my_study_acc_data/cwa/" + ".cwa", recursive=False)

# files list for rule make_list_cwa_children and rule cp_children_cwa (latter rule currently not working)
screens_bsl = glob.glob("/media/esbenlykke/My Passport/screens_all_cwa_files/baseline" + "cwa", recursive = False)
screens_fup = glob.glob("/media/esbenlykke/My Passport/screens_all_cwa_files/followup" + "cwa", recursive = False)

# file lists for rule prepare_screens_cwa
screens_children_cwa_bsl = os.listdir("/media/esbenlykke/My Passport/screens_cwa_children/baseline")
screens_children_cwa_fup = os.listdir("/media/esbenlykke/My Passport/screens_cwa_children/followup")

# create file lists for "rule cp_children_cwa" 
# TODO in the works
list_file_bsl = open("data/processed/bsl_children_cwa.txt", "r")
list_cwa_bsl = list_file_bsl.readlines()
list_file_fup = open("data/processed/fup_children_cwa.txt", "r")
list_cwa_fup = list_file_fup.readlines()

path_bsl = "/media/esbenlykke/My Passport/screens_cwa_children/baseline/"
path_fup = "/media/esbenlykke/My Passport/screens_cwa_children/followup/"

bsl_lines = [path_bsl + str(x) for x in list_cwa_bsl]
fup_lines = [path_fup + str(x) for x in list_cwa_fup]

cwa_list_bls = []

for element in bsl_lines:
    cwa_list_bls.append(element.strip())

cwa_list_fup = []

for element in fup_lines:
    cwa_list_fup.append(element.strip())

# screens baseline and followup samples
SAMPLE = ["bls", "fup"]

### MASTER RULE

rule targets:
  input:
    "data/processed/somno_sleep_profiles.tsv",
    "data/processed/zm_scores.tsv",
    "data/processed/acc_temp_psg_study.feather",
    "data/processed/bsl_children_cwa.txt",
    "data/processed/fup_children_cwa.txt",
    # expand("{sample}/{id}", id = cwa_list_bls, sample = SAMPLE), # targets for rule cp_children_cwa (currently not working)
    "data/processed/acc_temp_psg_study.feather",
    expand("data/processed/acc_temp_screens_{sample}.feather", sample = SAMPLE)
    

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

rule prepare_my_cwa:
  input:
    r_script = "code/downsample_and_concatenate_cwa.R",
    data = expand("data/raw/my_study_acc_data/cwa/{id}", id = my_cwa)
  params:
    epoch_length = 5,
    dest = "data/processed/acc_temp_psg_study.feather",
    cwa_path = "data/raw/my_study_acc_data/cwa"
  output:
    "data/processed/acc_temp_psg_study.feather"
  shell:
    "{input.r_script} {params.epoch_length} {params.dest} {params.cwa_path}"

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


# TODO rule cp_children_cwa does not work!

rule cp_children_cwa:
  input:
    bsl_cwa_files = expand("/media/esbenlykke/My Passport/screens_all_cwa_files/baseline/{id}", id = screens_bsl),
    fup_cwa_files = expand("/media/esbenlykke/My Passport/screens_all_cwa_files/followup/{id}", id = screens_fup),
    cp_bsl = "data/processed/bsl_children_cwa.txt",
    cp_fup = "data/processed/fup_children_cwa.txt",
    script = "code/move_children_cwa.sh"
  output:
    cp_to_bsl = expand("{path}", path = cwa_list_bls),
    cp_to_fup = expand("{path}", path = cwa_list_fup)
  shell:
    "{input.script}"

rule prepare_screens_cwa:
  input:
    r_script = "code/downsample_and_concatenate_cwa.R",
    data = expand("/media/esbenlykke/My Passport/screens_cwa_children/{sample}/{id}", id = screens_children_cwa_bsl, sample = SAMPLE)
  params:
    epoch_length = 5,
    dest = expand("data/processed/acc_temp_screens_{sample}.feather", sample = SAMPLE),
    cwa_path = expand("/media/esbenlykke/My Passport/screens_cwa_children/{sample}/", sample = SAMPLE)
  output:
    expand("data/processed/acc_temp_screens_{sample}.feather", sample = SAMPLE)
  shell:
    "{input.r_script} {params.epoch_length} {params.dest} {params.cwa_path}"
