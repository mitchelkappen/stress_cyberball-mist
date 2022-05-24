from scipy import signal
import pandas as pd
import neurokit2 as nk

from typing import List
import traceback

from tsflex.processing import SeriesPipeline, SeriesProcessor, dataframe_func


from cybb_mist.dataframes import groupby_consecutive, time_based_outer_merge


# ------------------------------ ECG PROCESSING ---------------------------------
# Helper methods
if True:

    def mean_resample(series: pd.Series, resample_rate, new_name=None) -> pd.Series:
        res_series = series.resample(resample_rate).mean()
        if new_name is not None:
            return res_series.rename(new_name)
        return res_series

    def scipy_downsample(series: pd.Series, ratio: int, new_name=None) -> pd.Series:
        res_series = pd.Series(
            data=signal.resample(series.values, int(len(series) / ratio)),
            index=series.index[::ratio][: int(len(series) / ratio)],
        )
        if new_name is not None:
            return res_series.rename(new_name)
        return res_series

    def clean_raw_ecg(
        raw_ecg: pd.Series, sampling_rate, method="neurokit"
    ) -> pd.Series:
        return pd.Series(
            nk.ecg_clean(raw_ecg.values, sampling_rate=sampling_rate, method=method),
            index=raw_ecg.index,
        ).rename(f"{raw_ecg.name}_cleaned_{method}")

    def find_peaks(
        ecg_cleaned: pd.Series, sampling_rate, method="neurokit"
    ) -> List[pd.Series]:
        try:
            r_peaks = nk.ecg_findpeaks(
                ecg_cleaned.values, sampling_rate=sampling_rate, method=method
            )["ECG_R_Peaks"]
        except:
            traceback.print_exc()
            s = pd.Series(name=f"ECG_R_Peaks_{ecg_cleaned.name}_{method}")
            s.index = pd.to_datetime(s).dt.tz_localize("Europe/Brussels")
            return s.to_frame()

        return [
            pd.Series(
                ecg_cleaned.iloc[r_peaks].values,
                index=ecg_cleaned.index[r_peaks],
            ).rename(f"ECG_R_Peaks_{ecg_cleaned.name}_{method}")
        ]

    def filter_peaks(
        detected_r_peaks: pd.Series, n_peaks, q=0.2, use_std=True
    ) -> pd.Series:
        r = detected_r_peaks.rolling(n_peaks, center=True)
        threshold = r.quantile(quantile=q)
        if use_std:
            threshold -= r.std()
        # print(detected_r_peaks.name, "threshold:", threshold.describe())
        detected_r_peaks = detected_r_peaks[
            (detected_r_peaks > threshold) | (detected_r_peaks > 0.45)
        ].copy()
        return [
            detected_r_peaks,
            threshold.copy().rename(detected_r_peaks.name + "_threshold"),
        ]

    def merge_ecg_r_peaks_series(*ecg_series) -> pd.Series:
        df_m = None
        for s in ecg_series:
            if len(s) == 0:
                continue

            if df_m is None:
                df_m = s.to_frame()
            else:
                df_m = time_based_outer_merge(
                    df_m, s, tolerance=pd.Timedelta(f"{int(1000 / 200 * 5)}ms")
                )
        # print("R peak agreeement", df_m.shape)
        df_m["r_peak_agreement"] = df_m.notna().sum(axis=1) / len(ecg_series)
        # todo -> add post processing based on time diff
        return df_m["r_peak_agreement"]

    def process_r_peak_agreement(
        agreement: pd.Series, ecg_sqi_series: pd.Series, ratio_threshold=0.6
    ) -> pd.Series:
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

        # mask: true if outside of clipping values*-
        clipped_mask = (series < min_threshold) | (series >= max_threshold)
        for _, r in groupby_consecutive(clipped_mask).iterrows():
            if r[series.name] and r["n_consecutive"] >= min_numb_consecutive:
                valid_mask[r.start - margin : r.end + margin] = False
        return valid_mask


# -------- Hyper params
if True:
    mean_resample_fs_list = [
        # ("ECG_mean_500Hz", 500),
        # ("ECG_mean_200Hz", 200),
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
        # ("ECG_cleaned_biosppy", 1000, "martinez"),
        ("ECG_cleaned_biosppy", 1000, "neurokit"),
        # ("ECG_cleaned_biosppy", 1000, "gamboa"),
        ("ECG_scipy_200Hz", 200, "neurokit"),
        # TODO -> uncomment these below
        # ("ECG_mean_200Hz", 200, "neu"),
        ("ECG_scipy_200Hz_cleaned_biosppy", 200, "neurokit"),
        # ("ECG_mean_200Hz_cleaned_biosppy", 200, "neurokit"),
        ("ECG_scipy_500Hz", 500, "neurokit"),
        # ("ECG_mean_500Hz", 500, "neurokit"),
        ("ECG_scipy_500Hz_cleaned_biosppy", 500, "neurokit"),
        # ("ECG_mean_500Hz_cleaned_biosppy", 500, "neurokit"),
    ]

ecg_pipeline = SeriesPipeline(
    processors=[
        # ------------ 1. Resampling the series
        *[
            SeriesProcessor(
                mean_resample,
                "ECG",
                resample_rate=pd.Timedelta(microseconds=1e6 / fs),
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
            min_threshold=-3,
            max_threshold=3,
            min_numb_consecutive=11,
            margin_s=1,
            resample_period_s=0.5,
        ),
        # ------------ 3. Finding peaks
        *[
            SeriesProcessor(
                series_names=series_names,
                function=find_peaks,
                sampling_rate=fs,
                method=method,
            )
            for series_names, fs, method in sig_fs_rpeakmethod_list
        ],
        # ----- 3.1 Filtering the peaks (based on amplitude)
        # This filters the peaks signal and returns a new threshold signal -> which is 
        # that filter it's threshold.
        SeriesProcessor(
            series_names=[
                "_".join(["ECG_R_Peaks", name, suffix])
                for name, _, suffix in sig_fs_rpeakmethod_list
            ],
            function=filter_peaks,
            n_peaks=1001,  # TODO -> use quantile based peak approach to determine this
            q=0.4,
            use_std=True
            # threshold
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
