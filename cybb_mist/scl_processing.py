import pandas as pd
import numpy as np
from scipy import signal
import neurokit2 as nk
from typing import List, Dict

from tsflex.processing import (
    SeriesPipeline,
    SeriesProcessor,
    dataframe_func,
    get_processor_logs,
)

import sys

sys.path.append("..")
from cybb_mist.dataframes import arr_to_repetitive_count


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
            N=order, Wn=f_cutoff / (0.5 * fs), btype="lowpass", output="ba", fs=fs
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
        w_size += 1 if w_size % 2 == 0 else 0
        ok_sum = (eda_series >= min_sig_threshold).rolling(w_size, center=True).sum()
        return ((ok_sum / w_size) >= min_ok_ratio).rename(output_name)

    def delta_sqi(
        eda_series: pd.Series,
        noise_series: pd.Series,
        noise_threshold=0.15,
        min_delta_threshold=0.04,
        max_increase=0.25,  # the ratio which a signal can increase in a second
        max_decrease=0.1,  # the ratio which a signal can decrease in a second
        fs=4,
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
        valid_delta = (
            # either slow increase | harsher increase but rather low noise!
            (delta <= increase_threshold)
            | (delta <= 2 * increase_threshold) & (noise_series <= noise_threshold)
        ) & (
            # and slow decrease | harsher decrease but rather low noise
            (delta >= decrease_threshold)
            | (delta >= 2 * decrease_threshold) & (noise_series <= noise_threshold / 2)
        )
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
        sqi = pd.Series(index=s.index, data=True, copy=True)
        if max_thresh is not None:
            sqi &= s <= max_thresh
        if min_thresh is not None:
            sqi = sqi & (s >= min_thresh)
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
                sqi = pd.merge_asof(
                    sqi, s, left_index=True, right_index=True, direction="nearest"
                )[output_name]

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
        smoothen_window_s: float,
    ) -> pd.Series:
        return (
            eda_cleaned.rolling(int(fs * smoothen_window_s), center=True)
            .quantile(quantile=0.05)
            .rolling(int(fs * smoothen_window_s), center=True)
            .mean()
            .rename(eda_cleaned.name + "_tonic")
        )

    def phasic(eda, eda_tonic, noise, output_name="EDA_Phasic"):
        return (eda - eda_tonic - noise).clip(lower=0).rename(output_name)

    def find_peaks(
        phasic: pd.Series, fs, method="neurokit", min_amplitude=0.03
    ) -> pd.DataFrame:
        peak_signal, _ = nk.eda_peaks(
            phasic,
            sampling_rate=fs,
            amplitude_min=min_amplitude,
            method=method,
        )
        return peak_signal.set_index(phasic.index).rename(
            columns={"SCR_Peaks": f"SCR_Peaks_{method}"}
        )

    @dataframe_func
    def remove_false_positives(
        df_scr,
        min_rise_time_s=1,
        min_recovery_time_s=1,
        max_rise_time_s=7,
        max_recovery_time_s=7,
        min_scr_amplitude=0.03,
    ) -> pd.DataFrame:
        assert min_rise_time_s < max_rise_time_s
        assert min_recovery_time_s < max_recovery_time_s
        scr_cols = [
            "SCR_RiseTime",
            "SCR_Peaks_neurokit",  # not that loosely coupled
            "SCR_RecoveryTime",
            "SCR_Amplitude",
        ]
        # op zich zou dit vervangen kunnen worden door generieke thresholding functies
        df_scr.loc[df_scr["SCR_RiseTime"] < min_rise_time_s, scr_cols] = 0
        df_scr.loc[df_scr["SCR_RiseTime"] > max_rise_time_s, scr_cols] = 0

        # df_scr.loc[df_scr["SCR_RecoveryTime"] < min_recovery_time_s, scr_cols] = 0
        # TODO -> RECOVERY
        df_scr.loc[df_scr["SCR_RecoveryTime"] > max_recovery_time_s, scr_cols] = 0

        # additional acc & amplitude related processing (compared to vic's processing)
        df_scr.loc[df_scr["SCR_Amplitude"] < min_scr_amplitude, scr_cols] = 0

        return df_scr.add_suffix("_reduced")


# -------------------------- The processing pipelines
gsr_processing_pipeline = SeriesPipeline(
    processors=[
        # EDA - filtering & slope SQI
        SeriesProcessor(low_pass_filter, "EDA", fs=10, output_name="EDA_lf_1Hz"),
        SeriesProcessor(slope_sqi, "EDA_lf_1Hz", max_increase=0.25, max_decrease=0.12),
        # EDA - noise processing
        SeriesProcessor(
            normalized_noise, tuple(["EDA", "EDA_lf_1Hz"]), output_name="noise"
        ),
        # TODO -> check if must be odd
        SeriesProcessor(
            lambda x: x.abs().rolling(9, center=True).sum().rename("noise_area_1s"),
            "noise",
        ),
        SeriesProcessor(
            threshold_sqi, "noise_area_1s", max_thresh=0.2, output_name="eda_noise_SQI"
        ),
        # EDA - low sig values & large delta's
        SeriesProcessor(lost_sqi, "EDA", fs=10, min_sig_threshold=0.05, window_s=5),
        SeriesProcessor(
            delta_sqi,
            tuple(["EDA", "noise_area_1s"]),
            noise_threshold=0.15,
            min_delta_threshold=0.04,
            max_increase=0.3,  # the ratio which a signal can increase in a second
            max_decrease=0.15,
            fs=10,
            window_s=1,
            output_name="eda_delta_SQI",
        ),
        # Calculate the total EDA SQI by applying a logical AND operation
        SeriesProcessor(
            sqi_and,
            tuple(["eda_lost_SQI", "eda_delta_SQI", "eda_noise_SQI", "eda_slope_SQI"]),
            output_name="EDA_SQI",
        ),
        SeriesProcessor(
            sqi_smoothen,
            "EDA_SQI",
            output_name="EDA_SQI_smoothend",
            fs=10,
            window_s=5,
            center=True,
            min_ok_ratio=0.6,
        ),
        # Interpolate the SQI
        SeriesProcessor(
            interpolate_sqi,
            tuple(["EDA", "EDA_SQI_smoothend"]),
            fs=10,
            max_interpolate_s=10,
            output_name="raw_cleaned",
        ),
        # Remove to short sessions
        SeriesProcessor(
            filter_duration,
            "raw_cleaned",
            output_name="raw_cleaned_duration_filter",
            fs=10,
            min_valid_len_s=60,
        ),
        SeriesProcessor(
            low_pass_filter,
            "raw_cleaned_duration_filter",
            fs=10,
            output_name="EDA_lf_cleaned",
        ),
    ]
)

scr_processing_pipeline = SeriesPipeline(
    processors=[
        # Try to estimate the tonic component of the EDA
        SeriesProcessor(
            tonic_eda,
            "EDA_lf_cleaned",
            fs=10,
            smoothen_window_s=15,
        ),
        SeriesProcessor(
            low_pass_filter,
            "EDA_lf_cleaned_tonic",
            fs=10,
            f_cutoff=0.1,
            output_name="EDA_lf_cleaned_tonic_lf",
        ),
        # Decompose into a tonic and phasic component & find peakds
        SeriesProcessor(
            phasic,
            tuple(["EDA_lf_cleaned", "EDA_lf_cleaned_tonic_lf", "noise_area_1s"]),
        ),
        # TODO -> denoise EDA phasic in another processing step
        SeriesProcessor(find_peaks, "EDA_Phasic", fs=10, method="neurokit"),
        # Filter the peaks
        SeriesProcessor(
            remove_false_positives,
            tuple(
                [
                    "SCR_RiseTime",
                    "SCR_Peaks_neurokit",
                    "SCR_RecoveryTime",
                    "SCR_Amplitude",
                ]
            ),
            min_rise_time_s=1,
            min_recovery_time_s=1,
            max_rise_time_s=7,
            max_recovery_time_s=7,
            min_scr_amplitude=0.03,
        ),
    ]
)


# ------------------------- PIPELINES WRAPPERS
def process_gsr_pipeline(
    df_scl: pd.Series, use_scr_pipeline=True
) -> List[pd.Series]:
    if use_scr_pipeline:
        tot_pipeline = SeriesPipeline(
            [gsr_processing_pipeline, scr_processing_pipeline]
        )
    else:
        tot_pipeline = SeriesPipeline([gsr_processing_pipeline])

    # series_list: List[pd.Series]
    df_processed: pd.DataFrame = tot_pipeline.process(
        data=df_scl,
        return_all_series=True,
        drop_keys=["ACC_x", "ACC_y", "ACC_z"],
        return_df=True,
        logging_file_path="gsr_processing.log",
    )

    print(get_processor_logs("gsr_processing.log"))
    print("-" * 80)
    print(df_processed)
    df_processed.to_parquet("/users/jonvdrdo/jonas/data/processed_gsr.parquet")
    return df_processed
