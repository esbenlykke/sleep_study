import mne
import yasa
import numpy as np
import pandas as pd
import os
import pathlib
import os
import glob

# path = input("Path to .edf files: ")
# path = "C:/Users/eskovgaard/Desktop/sleep/data/raw/edf/test"
path = "C:/Users/eskovgaard/Desktop/sleep/data/raw/somno_edf"

# new_dir_name = input('Path to output folder: ')
# new_dir = pathlib.Path(
#     '/Users/eskovgaard/Desktop/sleep/data/', new_dir_name)
# new_dir.mkdir(parents=True, exist_ok=True)


files = glob.glob(path + "/*.edf")

for f in files:

    print(f)

    raw = mne.io.read_raw_edf(f, preload=True, verbose=True)

    #raw.drop_channels(['ECG', 'Activity', 'Accu', 'Position', 'SpO2', 'Pleth', 'Imped.', 'Puls'])

    raw.resample(100)
    sf = raw.info['sfreq']
    print("resampled to", sf, "hz")

    sls = yasa.SleepStaging(raw, eeg_name="C3:A2", eog_name="EOGl:A2")

    y_pred = sls.predict()

    confidence = sls.predict_proba().max(1)

    df_pred = pd.DataFrame({'Stage': y_pred, 'Confidence': confidence})
    print("Predictions from", sls, "put into dataframe")

    new_file = f.replace(".edf", ".csv")

    df_pred.to_csv(new_file)
    print("Predictions saved to", new_file)
    # work in progress!!!
