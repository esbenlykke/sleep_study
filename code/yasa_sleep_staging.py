#!/usr/bin/env python3

import mne
import yasa
import numpy as np
import pandas as pd
import glob

path = "data/raw/somno_data/somno_edf/"

files = glob.glob(path + "/*.edf")

for f in files:

    print(f)

    raw = mne.io.read_raw_edf(f, preload=True, verbose=True)

    #raw.drop_channels(['ECG', 'Activity', 'Accu', 'Position', 'SpO2', 'Pleth', 'Imped.', 'Puls'])

    raw.resample(100)
    sf = raw.info['sfreq']
    print("resampled to", sf, "hz")

    sls = yasa.SleepStaging(raw, eeg_name="C3", eog_name="EOGl")

    y_pred = sls.predict()

    confidence = sls.predict_proba().max(1)

    df_pred = pd.DataFrame({'stage': y_pred, 'confidence': confidence, 'start': raw.info['meas_date']})
    print("Predictions from", sls, "put into dataframe")

    new_file = f.replace(".edf", ".csv")

    df_pred.to_csv(new_file)
    print("Predictions saved to", new_file)
