"""Parsing methods for several data files."""

from pathlib import Path
import pandas as pd


def parse_marker_file(marker_path: Path) -> pd.DataFrame:
    """Parse the marker file.

    The marker files are constructed by the trigger logs from the ECG/physio EDF file.

    Parameters
    ----------
    marker_path : Path
        The path of the marker files.

    Returns
    -------
    pd.DataFrame
        The parsed DataFrame.
    """
    tz = "europe/brussels"
    df_marker = pd.read_csv(marker_path)
    for c in ["timestamp"]:
        df_marker[c] = pd.to_datetime(df_marker[c]).dt.tz_convert(tz)
    return df_marker


def parse_timeline_file(timeline_path: Path) -> pd.DataFrame:
    """Parse the just-read in timeline file.

    The timeline-files are constructed from the Eprime xml logs, gathered on the PC
    (on which the participant are executing the raven matrix task.)

    Parameters
    ----------
    timeline_path : Path
        The path of the Eprime timeline file.

    Returns
    -------
    pd.DataFrame
        The parsed timeline DataFrame.

    """
    tz = "europe/brussels"
    df_timeline = pd.read_csv(timeline_path)
    for c in ["t_start", "t_stop"]:
        df_timeline[c] = pd.to_datetime(df_timeline[c]).dt.tz_convert(tz)

    # 2. assert whether the start-time of the timeline file is sorted
    df_timeline = df_timeline.sort_values(by=["t_start", "Trial"])
    assert df_timeline["t_start"].is_monotonic_increasing

    # 3. Add some additional columns
    df_timeline["prev_Phase"] = df_timeline.Phase.shift(1)
    df_timeline["RESP"] = df_timeline.RESP.shift(1)  # slide response
    df_timeline["CRESP"] = df_timeline.CRESP.shift(1)  # correct slide response
    df_timeline["answered_in_time"] = df_timeline.RESP.notna()
    df_timeline["answered_correctly"] = df_timeline.RESP == df_timeline.CRESP
    df_timeline[
        "delay_slide_feedback"
    ] = df_timeline.t_start - df_timeline.t_stop.shift(1)

    return df_timeline


def add_condition_mask(df: pd.DataFrame) -> pd.DataFrame:
    """Adds a new `condition` column to the passed DataFrame, which indicates whether
    the given feedback resides from the `control` or `negative` block.

    Parameters
    ----------
    df : pd.DataFrame
        The passed dataframe to which a the `condition` column will be added.

    Returns
    -------
    pd.DataFrame
        The a view of `df`, with an added `condition` column

    """
    df['condition'] = None
    df.loc[df.fileNum.isin([1, 2, 3]), 'condition'] = 'control'  # neutral
    df.loc[df.fileNum.isin([5, 6, 7]), 'condition'] = 'negative'  # stress
    return df
