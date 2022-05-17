"""Various utilities for DAtaFrame-based operations."""

import pandas as pd
import numpy as np

from typing import Union


def time_based_outer_merge(
        a: Union[pd.Series, pd.DataFrame],
        b: Union[pd.Series, pd.DataFrame],
        tolerance: pd.Timedelta) -> pd.DataFrame:
    """

    Parameters
    ----------
    b:  Union[pd.Series, pd.DataFrame]
        time-indexed series
    a:  Union[pd.Series, pd.DataFrame]
        time-indexed series
    tolerance:
        The time tolerance for the merge

    Returns
    -------
    The outer merged pd.DataFrame

    """
    # share the name of the
    time_index_name: str = a.index.name
    time_index_name_b = b.index.name
    b.index.rename(time_index_name, inplace=True)

    # create a new unique row-index on which the outer merge will be performed
    b = b.reset_index().assign(idx=np.arange(len(b)))

    out = (
        pd.merge_asof(
            a.reset_index(drop=False),
            # just merge with the time index and unique index of b
            b[[time_index_name, "idx"]],
            on=time_index_name,
            direction='nearest',
            tolerance=tolerance,
        )
            # merge back on row number
            .merge(b, on="idx", how="outer")
            # fill the time
            .assign(
            **{time_index_name: lambda x: x[f"{time_index_name}_x"].fillna(
                x[f"{time_index_name}_y"])}
        )
            # set the index back
            .set_index(time_index_name)
            .drop([f"{time_index_name}_x", f"{time_index_name}_y", "idx"], axis=1)
            .sort_index()
    )
    # fix the pass by reference
    b.index.rename(time_index_name_b, inplace=True)
    return out


def groupby_consecutive(
        df: Union[pd.Series, pd.DataFrame], col_name: str = None
) -> pd.DataFrame:
    """Merges consecutive `column_name` values in a single dataframe.

    This is especially useful if you want to represent sparse data in a more
    compact format.

    Parameters
    ----------
    df : Union[pd.Series, pd.DataFrame]
        Must be time-indexed!
    col_name : str, optional
        If a dataFrame is passed, you will need to specify the `col_name` on which
        the consecutive-grouping will need to take place.

    Returns
    -------
    pd.DataFrame
        A new `DataFrame` view, with columns:
        [`start`, `end`, `n_consecutive`, `col_name`], representing the
        start- and end-time of the consecutive range, the number of consecutive samples,
        and the col_name's consecutive values.

    """
    if type(df) == pd.Series:
        col_name = df.name
        df = df.to_frame()

    assert col_name in df.columns

    df_cum = (
        (df[col_name].diff(1) != 0)
            .astype("int")
            .cumsum()
            .rename("value_grp")
            .to_frame()
    )
    df_cum["sequence_idx"] = df.index
    df_cum[col_name] = df[col_name]

    df_grouped = pd.DataFrame(
        {
            "start": df_cum.groupby("value_grp")["sequence_idx"].first(),
            "end": df_cum.groupby("value_grp")["sequence_idx"].last(),
            "n_consecutive": df_cum.groupby("value_grp").size(),
            col_name: df_cum.groupby("value_grp")[col_name].first(),
        }
    ).reset_index(drop=True)
    df_grouped["next_start"] = df_grouped.start.shift(-1).fillna(df_grouped["end"])
    return df_grouped
