# -*- coding: utf-8 -*-
"""ECG processing code.

This code uses-an ensemble approach, calculating R-Peaks on both the raw
and processed ECG signal, using different processing algorithm and sample frequencies.

The detected R-Peaks from these various configurations are then used to calculate the
R-peak agreement. This R-peak agreement can be seen as a proxy for signal-quality and
is thus used to determine VALID/INVALID regions.

After this signal quality proxy, these (IN)VALID regions are then used for extracting
valid R-peaks, which can then on their end be used to determine R-R delta's and HRV.

"""
__author__ = "Jonas Van Der Donckt"

import traceback
from datetime import timedelta
from typing import List

import neurokit2 as nk
import pandas as pd

# -------------------------------- TIME SERIES --------------------------------
from tsflex.processing import (
    SeriesProcessor,
    SeriesPipeline,
)
from .dataframes import (
    groupby_consecutive,
    time_based_outer_merge,
)
from scipy import signal


# ------------------------------ ECG PROCESSING ---------------------------------
# Helper methods which are used in the processing pipeline

def mean_resample(series: pd.Series, resample_rate, new_name=None) -> pd.Series:
    """Use pandas its mean downsampling"""
    res_series = series.resample(resample_rate).mean()
    if new_name is not None:
        return res_series.rename(new_name)
    return res_series


def scipy_downsample(series: pd.Series, ratio: int, new_name=None) -> pd.Series:
    """Resample using scipy.signal its resample method."""
    res_series = pd.Series(
        data=signal.resample(series.values, int(len(series) / ratio)),
        index=series.index[::ratio][: int(len(series) / ratio)],
    )
    if new_name is not None:
        return res_series.rename(new_name)
    return res_series


def clean_raw_ecg(raw_ecg: pd.Series, sampling_rate, method="neurokit") -> pd.Series:
    """Clean the ECG signal with Neurokit2."""
    return pd.Series(
        nk.ecg_clean(raw_ecg.values, sampling_rate=sampling_rate, method=method),
        index=raw_ecg.index,
    ).rename(f"{raw_ecg.name}_cleaned_{method}")


def find_r_peaks(
        ecg_cleaned: pd.Series, sampling_rate, method="neurokit") -> List[pd.Series]:
    """Find R-peaks"""
    try:
        r_peaks = nk.ecg_findpeaks(
            ecg_cleaned.values, sampling_rate=sampling_rate, method=method
        )["ECG_R_Peaks"]
    except:
        traceback.print_exc()
        s = pd.Series(name=f"ECG_R_Peaks_{ecg_cleaned.name}_{method}")
        s.index = pd.to_datetime(s).dt.tz_localize("Europe/Brussels")
        return [s]

    return [
        pd.Series(
            ecg_cleaned.iloc[r_peaks].values,
            index=ecg_cleaned.index[r_peaks],
        ).rename(f"ECG_R_Peaks_{ecg_cleaned.name}_{method}")
    ]


def filter_peaks(
        detected_r_peaks: pd.Series, n_peaks, q=0.1, use_std=True, min_ok_threshold=400
) -> List[pd.Series]:
    """Due to saturation error, we might detect peaks that aren't occurring, this
    processing code alleviates this issue."""
    r = detected_r_peaks.rolling(n_peaks, center=True)
    threshold = r.quantile(quantile=q)
    if use_std:
        threshold -= r.std()
    detected_r_peaks = detected_r_peaks[
        (detected_r_peaks > threshold) | (detected_r_peaks > min_ok_threshold)
        ].copy()
    return [
        detected_r_peaks,
        threshold.copy().rename(detected_r_peaks.name + "_threshold"),
    ]


def merge_ecg_r_peaks_series(*ecg_series) -> pd.Series:
    df_m = None
    for s in ecg_series:
        s_idx = s.index.to_series().rename(s.name)
        if len(s_idx) == 0:
            continue

        if df_m is None:
            df_m = s_idx.to_frame()
        else:
            df_m = time_based_outer_merge(
                df_m, s, tolerance=pd.Timedelta(f"{int(1000 / 200 * 5)}ms")
            )

    df_m["r_peak_agreement"] = df_m.notna().sum(axis=1) / len(ecg_series)
    return df_m["r_peak_agreement"]


def process_r_peak_agreement(
        agreement: pd.Series, ecg_sqi_series: pd.Series, ratio_threshold=0.6
) -> pd.Series:
    """"""
    df_m = pd.merge_asof(
        agreement,
        ecg_sqi_series,
        right_index=True,
        left_index=True,
        direction="nearest",
    )
    processed_peaks_bool = pd.Series(
        index=agreement.index, data=df_m[ecg_sqi_series.name].values
    ).rename("ECG_R_Peaks_processed")

    # set to false where a low ratio
    processed_peaks_bool.loc[agreement <= ratio_threshold] = False
    return processed_peaks_bool


def compute_ecg_sqi(
        series: pd.Series,
        min_threshold,
        max_threshold,
        min_numb_consecutive: int,
        margin_s: float,
        resample_period_s: float = 0.5,
) -> pd.Series:
    # the series in which the clipped output + margin will be stored
    valid_mask = pd.Series(
        index=pd.date_range(
            start=series.index[0],
            end=series.index[-1],
            freq=f"{resample_period_s}S",
        ),
        data=True,
    ).rename(f"{series.name}_quality_mask")
    margin = pd.Timedelta(seconds=margin_s)

    # mask: true if outside of clipping values
    clipped_mask = (series < min_threshold) | (series >= max_threshold)
    for _, r in groupby_consecutive(clipped_mask).iterrows():
        if r[series.name] and r["n_consecutive"] >= min_numb_consecutive:
            valid_mask[r.start - margin: r.end + margin] = False
    return valid_mask


# -------- Hyper params
if True:
    mean_resample_fs_list = [
        ("ECG_mean_500Hz", 500),
        ("ECG_mean_200Hz", 200),
    ]

    scipy_resample_fs_list = [
        ("ECG_scipy_500Hz", 500),
        ("ECG_scipy_200Hz", 200),
    ]

    sig_fs_list = [("ECG", 1000), *mean_resample_fs_list, *scipy_resample_fs_list]

    # available r-peak_methods =
    #   "neurokit",pantompkins1985", "hamilton2002", "christov2004", "gamboa2008",
    #   "elgendi2010", "engzeemod2012", "martinez2003", "rodrigues2021", "promac",
    sig_fs_rpeakmethod_list = [
        ("ECG", 1000, "neurokit"),
        ("ECG_cleaned_biosppy", 1000, "martinez"),
        # ("ECG_cleaned_biosppy", 1000, "gamboa"),
        ("ECG_scipy_200Hz", 200, "neurokit"),
        ("ECG_mean_200Hz", 200, "neurokit"),
        ("ECG_scipy_200Hz_cleaned_biosppy", 200, "neurokit"),
        ("ECG_mean_200Hz_cleaned_biosppy", 200, "neurokit"),
        ("ECG_scipy_500Hz", 500, "martinez"),
        ("ECG_mean_500Hz", 500, "neurokit"),
        ("ECG_scipy_500Hz_cleaned_biosppy", 500, "martinez"),
        ("ECG_mean_500Hz_cleaned_biosppy", 500, "neurokit"),
    ]

ecg_pipeline = SeriesPipeline(
    processors=[
        # ------------ 1. Resampling the series
        *[
            SeriesProcessor(
                mean_resample,
                "ECG",
                resample_rate=timedelta(microseconds=1e6 / fs),
                new_name=name,
            )
            for name, fs in mean_resample_fs_list
        ],
        *[
            SeriesProcessor(
                scipy_downsample, "ECG", ratio=int(1000 / fs), new_name=name
            )
            for name, fs in scipy_resample_fs_list
        ],
        # ------------ 2. Cleaning the series
        *[
            SeriesProcessor(
                series_names=series_name,
                function=clean_raw_ecg,
                sampling_rate=fs,
                method="biosppy",
                # others: "neurokit", "pantompkins", "hamilton", "elgendi", "engzeemod"
            )
            for series_name, fs in sig_fs_list
        ],
        SeriesProcessor(
            series_names="ECG",
            function=compute_ecg_sqi,
            min_threshold=-3000,
            max_threshold=3000,
            min_numb_consecutive=11,
            margin_s=1,
            resample_period_s=0.5,
        ),
        # ------------ 3. Finding peaks
        *[
            SeriesProcessor(
                series_names=series_names,
                function=find_r_peaks,
                sampling_rate=fs,
                method=method,
            )
            for series_names, fs, method in sig_fs_rpeakmethod_list
        ],
        # ----- 3.1 Filtering the peaks (based on amplitude)
        SeriesProcessor(
            series_names=[
                "_".join(["ECG_R_Peaks", name, suffix])
                for name, _, suffix in sig_fs_rpeakmethod_list
            ],
            function=filter_peaks,
            n_peaks=1001,
            q=0.4,
            use_std=True,
            min_ok_threshold=400
        ),  # -> todo maybe apply this for each step
        SeriesProcessor(
            series_names=tuple(
                [
                    "_".join(["ECG_R_Peaks", name, suffix])
                    for name, _, suffix in sig_fs_rpeakmethod_list
                ]
            ),
            function=merge_ecg_r_peaks_series,
        ),
        SeriesProcessor(
            series_names=tuple(["r_peak_agreement", "ECG_quality_mask"]),
            function=process_r_peak_agreement,
            ratio_threshold=0.6,
        ),
    ]
)


# ---------------------------------- The visualization code base -------------------
def process_ecg(ecg_raw: pd.Series) -> pd.DataFrame:
    out: List[pd.Series] = ecg_pipeline.process(data=ecg_raw)
    dfs = []
    for c in out:
        if c.name == "ECG_R_Peaks_processed":
            r_peaks = c.dropna()
            rr = (
                    r_peaks.index.to_series()
                    .diff()[1:]
                    .dt.total_seconds()
                    .rename("RR_interval_ms")
                    * 1000
            )
            hrv = (
                    r_peaks.index.to_series()
                    .diff()
                    .diff()[2:]
                    .dt.total_seconds()
                    .rename("HRV_ms")
                    * 1000
            )

            time_agreement_mask = (r_peaks.shift(1) & r_peaks).values
            hrv_agreement_mask = (r_peaks.shift(2) & r_peaks.shift(1) & r_peaks).values
            # add the rr-interval and the hrv
            dfs.append(rr[time_agreement_mask[1:]].copy())
            dfs.append(hrv[hrv_agreement_mask[2:]].copy())
        elif c.name in ["r_peak_agreement"]:
            dfs.append(c)
    df_rr = pd.concat(dfs, ignore_index=False, axis=1)
    return df_rr
