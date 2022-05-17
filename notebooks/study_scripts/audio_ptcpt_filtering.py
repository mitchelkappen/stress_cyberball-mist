"""Code to filter participants based on audio quality measures."""

import pandas as pd

_to_be_removed = {
    # Remove due to bad audio quality
    1: [1, 3],
    2: None,
    3: [0, 8],
    5: [8],
    6: [7],
    8: [8],
    9: [3],
    17: [6],
    24: None,
    25: None,
    29: [2, 4, 8],
    31: [2],
    35: [0, 3, 5, 6, 7],
    36: [8],
    45: None,
    48: [5],
    53: None,
    60: [0],
    61: [7],
    62: None,
    71: None,
    72: None,
    73: None,
    74: None,
    82: None,
}


def add_audio_mask(df: pd.DataFrame) -> pd.DataFrame:
    """Add a new bool column `bad_audio` which indicates whether the audio-quality of
    the corresponding feedback mask is bad (True) or not (False)

    Parameters
    ----------
    df : pd.DataFrame
        The DataFrame on which the `bad_audio` column will be added.

    Returns
    -------
    pd.DataFrame
        A view of df.
    """
    df["bad_audio"] = False
    for ptcpt_id, f_ids in _to_be_removed.items():
        if f_ids is None:  # remove all the data for that participant
            df["bad_audio"] |= df.participantNum == ptcpt_id
        else:  # only remove the corresponding file id's for that participant
            df["bad_audio"] |= (
                df.participantNum == ptcpt_id
            ) & df.fileNum.isin(f_ids)
    return df
