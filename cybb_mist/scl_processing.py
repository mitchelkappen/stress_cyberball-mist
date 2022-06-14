import pandas as pd
import numpy as np
from scipy import signal
from typing import List

from tsflex.processing import (
    SeriesPipeline,
    SeriesProcessor,
    dataframe_func,
    get_processor_logs,
)

import sys

sys.path.append("..")
from cybb_mist.dataframes import arr_to_repetitive_count

# CONFIG

FS = 10


# The GSR signal itself (artefacts)
if True:

    def low_pass_filter(
        s: pd.Series,
        order: int = 5,
        f_cutoff: int = 1,
        fs: int = None,
        output_name="filter",
    ) -> np.ndarray:
        if fs is None:  # determine the sample frequency
            fs = 1 / pd.Timedelta(pd.infer_freq(s.index)).total_seconds()
        b, a = signal.butter(
            N=order, Wn=f_cutoff / (0.5 * fs), btype="lowpass", output="ba", fs=FS
        )
        # the filtered output has the same shape as sig.values
        s = s.dropna()
        return pd.Series(
            index=s.index, data=signal.filtfilt(b=b, a=a, x=s.values).astype(np.float32)
        ).rename(output_name)

    def high_pass_filter(
        s: pd.Series,
        order: int = 5,
        f_cutoff: int = 1,
        fs: int = None,
        output_name="filter",
    ) -> np.ndarray:
        if fs is None:  # determine the sample frequency
            fs = 1 / pd.Timedelta(pd.infer_freq(s.index)).total_seconds()
        b, a = signal.butter(
            N=order, Wn=f_cutoff / (0.5 * fs), btype="highpass", output="ba", fs=FS
        )
        # the filtered output has the same shape as sig.values
        s = s.dropna()
        return pd.Series(
            index=s.index, data=signal.filtfilt(b=b, a=a, x=s.values).astype(np.float32)
        ).rename(output_name)

    def lost_sqi(
        eda_series: pd.Series,
        fs,
        window_s=3,
        min_sig_threshold=0.04,  # in uS
        min_ok_ratio: float = 0.9,
        output_name: str = "eda_lost_SQI",
    ) -> pd.Series:
        # A signal is deemed lost if its value is below `min_sig_threshold` uS.
        # For each window the ratio of non-lost `ok_values` versus window_size is
        # then calculated.
        # If this ratio is above 0.9, the signal is classified as of bad quality.
        w_size = window_s * fs
        w_size += w_size % 2 - 1
        ok_sum = (eda_series >= min_sig_threshold).rolling(w_size, center=True).sum()
        return (ok_sum >= w_size * min_ok_ratio).rename(output_name)

    def delta_sqi(
        eda_series: pd.Series,
        noise_series: pd.Series,
        noise_threshold=0.1,
        min_delta_threshold=0.02,
        max_increase=0.25,  # the ratio which a signal can increase in a second
        max_decrease=0.1,  # the ratio which a signal can decrease in a second
        fs=FS,
        window_s: int = 1,
        output_name: str = "eda_delta_SQI",
    ) -> pd.Series:
        eda_med = eda_series.rolling(fs * window_s, center=True).median()
        increase_threshold = np.maximum(
            min_delta_threshold, eda_med * max_increase / fs
        )
        decrease_threshold = (
            np.maximum(min_delta_threshold, eda_med * max_decrease / fs) * -1
        )
        delta = eda_series.diff()
        valid_decrease = (
            # and slow decrease | harsher decrease but rather low noise
            (delta >= decrease_threshold)
            | (delta >= 2 * decrease_threshold) & (noise_series <= noise_threshold / 2)
        )
        # we will more severly focus on decrease masks
        decrease_mask = (
            valid_decrease.rolling(6 * fs - 1).min().shift(-(3 * fs + 1)).astype(bool)
        )
        valid_delta = (
            # either slow increase | harsher increase but rather low noise!
            ((delta <= 0.5 * increase_threshold) & ~decrease_mask)
            | (delta <= increase_threshold) & decrease_mask
            | (delta <= 2 * increase_threshold)
            & decrease_mask
            & (noise_series <= noise_threshold / 2)
        ) & valid_decrease
        return valid_delta.rename(output_name)

    def slope_sqi(
        s: pd.Series, max_increase=0.25, max_decrease=0.1, output_name="eda_slope_SQI"
    ) -> List[pd.Series]:
        slope = s.shift(-1) / s
        slope_sqi = (slope >= 1 - max_decrease) & (slope <= 1 + max_increase)
        return [slope.rename("slope"), slope_sqi.rename(output_name)]

    def normalized_noise(
        s: pd.Series, s_filtered: pd.Series, output_name: str, precision=0.03
    ):
        return ((s - s_filtered) / (s_filtered + precision)).rename(output_name)

    def threshold_sqi(s: pd.Series, output_name, max_thresh=None, min_thresh=None):
        sqi = pd.Series(index=s.index, data=True)
        if max_thresh is not None:
            sqi &= s <= max_thresh
        if min_thresh is not None:
            sqi &= s >= min_thresh
        return sqi.rename(output_name)

    def sqi_and(*series, output_name: str = "EDA_SQI"):
        sqi = None
        for s in series:
            if sqi is None:
                sqi = s.rename(output_name)
                continue
            if len(sqi) == len(s):
                sqi &= s
            else:
                raise ValueError
                # sqi = pd.merge_asof(
                #     sqi, s, left_index=True, right_index=True, direction="nearest"
                # )[output_name]

        return sqi

    def sqi_smoothen(
        sqi: pd.Series,
        fs,
        window_s=5,
        min_ok_ratio=0.75,
        center=True,
        output_name="EDA_SQI_smoothend",
    ):
        # todo -> duplication of method abouve
        w_size = window_s * fs
        w_size += 1 if w_size % 2 == 0 and center else 0
        ok_sum = sqi.rolling(w_size, center=center).sum()
        return (sqi & ((ok_sum / w_size) >= min_ok_ratio)).rename(output_name)

    def interpolate_sqi(
        eda: pd.Series, valid: pd.Series, fs, output_name, max_interpolate_s=30
    ):
        eda_cleaned: pd.Series = eda.rename(output_name)
        eda_cleaned[~valid] = None

        eda_cleaned = eda_cleaned.interpolate(
            "time",
            limit=max_interpolate_s * fs,
            limit_area="inside",
            limit_direction="forward",
        )

        # remove the interpolations which end in the NaN regions
        eda_cleaned[
            ~valid & (arr_to_repetitive_count(valid) > max_interpolate_s * fs)
        ] = None
        return eda_cleaned

    def filter_duration(eda: pd.Series, fs, min_valid_len_s, output_name):
        min_n_samples = fs * min_valid_len_s

        eda_filtered = eda.rename(output_name)
        eda_notna = eda_filtered.notna()
        eda_filtered[
            eda_notna & (arr_to_repetitive_count(eda_notna) < min_n_samples)
        ] = None
        return eda_filtered


# GSR decomposition
if True:

    def tonic_eda(
        eda_cleaned: pd.Series,
        fs: int,
        q: float,
        smoothen_window_s: float,
    ) -> pd.Series:
        w_size = int(fs * smoothen_window_s)
        w_size += w_size % 2 - 1  # ensure that the window size is odd
        return (
            eda_cleaned.rolling(w_size + (w_size % 2 - 1), center=True)
            .quantile(quantile=q)
            .rolling(w_size, center=True)
            .mean()
            .rename(eda_cleaned.name + "_tonic")
        )

    def phasic(
        eda, eda_tonic, normalized_noise, noise_factor=1, output_name="EDA_Phasic"
    ):
        return (
            (
                (eda - eda_tonic - noise_factor * (0.3 + eda_tonic) * normalized_noise)
                # / (0.3 + eda_tonic)
            )
            .clip(lower=0)
            .rename(output_name)
        )

    def find_peaks_scipy(phasic: pd.Series, fs: int, **kwargs) -> pd.DataFrame:
        base_kwargs = dict(distance=fs, prominence=0.015, wlen=fs * 120)
        # no hight
        base_kwargs.update(kwargs)
        s_fp, s_fp_d = signal.find_peaks(phasic, **base_kwargs)
        df_peak = pd.DataFrame(s_fp_d)
        df_peak.index = phasic.index[s_fp]
        df_peak["SCR_Peaks_scipy"] = 1
        df_peak["left_bases"] = (s_fp - df_peak["left_bases"]) / fs
        df_peak["right_bases"] = (df_peak["right_bases"] - s_fp) / fs
        df_peak["peak_heights"] = phasic.iloc[s_fp]
        df_peak = df_peak.rename(
            columns={
                "left_bases": "SCR_RiseTime",
                "right_bases": "SCR_RecoveryTime",
                "peak_heights": "SCR_Amplitude",
            }
        )
        # print(df_peak)
        return df_peak

    @dataframe_func
    def remove_false_positives(
        df_scr,
        min_rise_time_s=0.5,
        min_recovery_time_s=0.5,
        max_rise_time_s=14,
        max_recovery_time_s=100,
        min_scr_amplitude=0.03,
        min_phasic_noise_ratio=5,
    ) -> pd.DataFrame:
        assert min_rise_time_s < max_rise_time_s
        assert min_recovery_time_s < max_recovery_time_s

        s_scr: pd.Series = df_scr["SCR_Peaks_scipy"].copy()  # not that loosely coupled
        # op zich zou dit vervangen kunnen worden door generieke thresholding functies
        s_scr.loc[df_scr["SCR_RiseTime"] < min_rise_time_s] = 0
        s_scr.loc[df_scr["SCR_RiseTime"] > max_rise_time_s] = 0

        s_scr.loc[df_scr["SCR_RecoveryTime"] < min_recovery_time_s] = 0
        s_scr.loc[df_scr["SCR_RecoveryTime"] > max_recovery_time_s] = 0

        # additional acc & amplitude related processing (compared to vic's processing)
        s_scr.loc[df_scr["SCR_Amplitude"] < min_scr_amplitude] = 0

        s_scr.loc[df_scr["phasic_noise_ratio"] < min_phasic_noise_ratio] = 0

        return s_scr[s_scr != 0].rename(s_scr.name + "_reduced").to_frame()


# -------------------------- The processing pipelines
gsr_processing_pipeline = SeriesPipeline(
    processors=[
        # EDA - filtering & slope SQI
        # Apply a 2Hz low-pass filter
        SeriesProcessor(
            low_pass_filter, "EDA", fs=FS, f_cutoff=2, output_name="EDA_lf_2Hz"
        ),
        SeriesProcessor(
            slope_sqi,
            "EDA_lf_2Hz",
            max_increase=0.25,
            max_decrease=0.12,
            output_name="EDA_slope_SQI",
        ),
        #
        # EDA - noise processing
        # First the normalized noise is calcualted after which the rolling abs-mean
        # is taken
        SeriesProcessor(
            normalized_noise, tuple(["EDA", "EDA_lf_2Hz"]), output_name="noise"
        ),
        SeriesProcessor(
            lambda x: x.abs()
            .rolling(
                5 * FS + ((5 * FS) % 2 - 1), center=True
            )  # checks that the window is odd
            .mean()
            .rename("noise_mean_2s"),
            "noise",
        ),
        SeriesProcessor(
            # TODO: was 0.1
            threshold_sqi,
            "noise_mean_2s",
            max_thresh=0.02,
            output_name="EDA_noise_SQI",
        ),
        # EDA - low sig values & large delta's
        SeriesProcessor(
            lost_sqi,
            "EDA",
            fs=FS,
            min_sig_threshold=0.05,
            window_s=5,
            output_name="EDA_lost_SQI",
        ),
        SeriesProcessor(
            delta_sqi,
            tuple(["EDA", "noise_mean_2s"]),
            noise_threshold=0.3,
            min_delta_threshold=0.04,
            max_increase=0.25,  # the ratio which a signal can increase in a second
            max_decrease=0.1,
            fs=FS,
            window_s=1,
            output_name="EDA_delta_SQI",
        ),
        # Calculate the total EDA SQI by applying a logical AND operation
        SeriesProcessor(
            sqi_and,
            tuple(["EDA_lost_SQI", "EDA_delta_SQI", "EDA_noise_SQI", "EDA_slope_SQI"]),
            output_name="EDA_SQI",
        ),
        SeriesProcessor(
            sqi_smoothen,
            "EDA_SQI",
            output_name="EDA_SQI_smoothend",
            fs=FS,
            window_s=5,
            center=True,
            min_ok_ratio=0.6,
        ),
        # Interpolate the SQI
        SeriesProcessor(
            interpolate_sqi,
            tuple(["EDA", "EDA_SQI_smoothend"]),
            fs=FS,
            max_interpolate_s=10,
            output_name="raw_cleaned",
        ),
        # Remove to short sessions
        SeriesProcessor(
            filter_duration,
            "raw_cleaned",
            output_name="raw_cleaned_duration_filter",
            fs=FS,
            min_valid_len_s=60,
        ),
        SeriesProcessor(
            low_pass_filter,
            "raw_cleaned_duration_filter",
            fs=FS,
            f_cutoff=2,
            output_name="EDA_lf_cleaned",
        ),
    ]
)

scr_processing_pipeline = SeriesPipeline(
    processors=[
        # Try to estimate the tonic component of the EDA
        SeriesProcessor(
            tonic_eda, "EDA_lf_cleaned", fs=FS, smoothen_window_s=45, q=0.025
        ),
        SeriesProcessor(
            low_pass_filter,
            "EDA_lf_cleaned_tonic",
            fs=FS,
            f_cutoff=0.05,
            output_name="EDA_lf_cleaned_tonic_lf",
        ),
        # Decompose into a tonic and phasic component & find peakds
        SeriesProcessor(
            phasic,
            tuple(["EDA_lf_cleaned", "EDA_lf_cleaned_tonic_lf", "noise_mean_2s"]),
            noise_factor=0,
        ),
        SeriesProcessor(tonic_eda, "EDA_Phasic", fs=FS, smoothen_window_s=10, q=0.05),
        SeriesProcessor(
            low_pass_filter,
            "EDA_Phasic_tonic",
            f_cutoff=0.1,
            fs=FS,
            output_name="EDA_Phasic_tonic_lf",
        ),
        SeriesProcessor(
            lambda noise_rms, eda_tonic, eda_phasic, eda_phasic_tonic: (
                (
                    # TODO -> this is not true
                    # note, we do not want to center the meen, we want to
                    # calculate this on the prior values
                    (eda_phasic - eda_phasic_tonic)
                    .fillna(0)
                    .rolling(5 * FS, center=True)
                    .mean()
                    / (noise_rms.clip(lower=0.002) * eda_tonic.clip(lower=0.5))
                ).clip(lower=0, upper=20)
            ).rename("phasic_noise_ratio"),
            series_names=tuple(
                [
                    "noise_mean_2s",
                    "EDA_lf_cleaned_tonic",
                    "EDA_Phasic",
                    "EDA_Phasic_tonic_lf",
                ]
            ),
        ),
        SeriesProcessor(
            lambda eda_phasic, eda_phasic_tonic: (
                # TODO -> this is not true
                # note, we do not want to center the meen, we want to
                # calculate this on the prior values
                (eda_phasic - eda_phasic_tonic).fillna(0)
            ).rename("EDA_Phasic_hf"),
            series_names=tuple(
                [
                    "EDA_Phasic",
                    "EDA_Phasic_tonic",
                ]
            ),
        ),
        # TODO -> denoise EDA phasic in another processing step
        SeriesProcessor(
            find_peaks_scipy,
            "EDA_Phasic",
            fs=FS,
            distance=FS,
            rel_height=0.1,
            prominence=0.02,
            wlen=FS * 60,
        ),
        # Filter the peaks
        SeriesProcessor(
            remove_false_positives,
            tuple(
                [
                    "SCR_RiseTime",
                    "SCR_Peaks_scipy",
                    "SCR_RecoveryTime",
                    "SCR_Amplitude",
                    "phasic_noise_ratio",
                ]
            ),
            min_rise_time_s=0.5,
            min_recovery_time_s=0.5,
            max_rise_time_s=100,
            max_recovery_time_s=100,
            min_scr_amplitude=0.015,
            min_phasic_noise_ratio=7.5,
        ),
    ]
)


# ------------------------- PIPELINES WRAPPERS
def process_gsr_pipeline(
    df_scl: pd.Series, use_scr_pipeline=True, print_timings=False
) -> List[pd.Series]:
    if use_scr_pipeline:
        tot_pipeline = SeriesPipeline(
            [gsr_processing_pipeline, scr_processing_pipeline]
        )
    else:
        tot_pipeline = SeriesPipeline([gsr_processing_pipeline])

    kwargs = {}
    if print_timings:
        kwargs = {"logging_file_path": "gsr_processing.log"}

    df_processed: pd.DataFrame = tot_pipeline.process(
        data=df_scl,
        return_all_series=True,
        drop_keys=["ACC_x", "ACC_y", "ACC_z"],
        return_df=True,
        **kwargs
    )

    if print_timings:
        df_logs = get_processor_logs("gsr_processing.log")
        df_logs["duration %"] = 100 * df_logs["duration"] / df_logs["duration"].sum()
        df_logs["duration"] = df_logs["duration"].dt.total_seconds().round(3)
        print("-" * 80)
        df_logs = df_logs.drop(columns=["log_time"])
        try:
            from IPython.display import display

            display(df_logs)
        except:
            print(df_logs)
        print("-" * 80)

    return df_processed
