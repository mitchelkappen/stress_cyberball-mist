import pandas as pd


trigger_mapping = {
    # Cyberball
    # "cybb": {
        1: "Introduction start",
        10: "Start rest baseline (eyes closed)",
        11: "End rest baseline (eyes closed)",
        12: "Start rest baseline (eyes open)",
        13: "End rest baseline (eyes open)",
        5: "Start cyberball control",
        6: "End cyberball control",
        20: "Start rest control (eyes closed)",
        21: "Start rest control (eyes closed)",
        22: "Start rest control (eyes open)",
        23: "End rest control (eyes open)",
        15: "Start cyberball stress",
        16: "End cyberball stress",
        30: "Start rest stress (eyes closed)",
        31: "Start rest stress (eyes closed)",
        32: "Start rest stress (eyes open)",
        33: "End rest stress (eyes open)",
        2: "end",
    # },
    # Mist
    # "mist": {
        51: "Introduction start",
        60: "Start rest baseline (eyes closed)",
        61: "End rest baseline (eyes closed)",
        62: "Start rest baseline (eyes open)",
        63: "End rest baseline (eyes open)",
        55: "Start MIST control",
        56: "End MIST control",
        70: "Start rest control (eyes closed)",
        71: "Start rest control (eyes closed)",
        72: "Start rest control (eyes open)",
        73: "End rest control (eyes open)",
        65: "Start MIST stress",
        66: "End MIST stress",
        80: "Start rest stress (eyes closed)",
        81: "Start rest stress (eyes closed)",
        82: "Start rest stress (eyes open)",
        83: "End rest stress (eyes open)",
        52: "end"
    # }
}

trigger_color_mapping = {
    "Introduction start": "dimgrey",
    "end": "dimgrey",

    "rest baseline (eyes closed)": "lightgreen",
    "rest baseline (eyes open)": "green",

    "cyberball control": "orange",
    "MIST control": "orange",
    "rest control (eyes closed)": "lightgreen",
    "rest control (eyes open)": "green",

    "cyberball stress": "orangered",
    "MIST stress": "orangered",
    "rest stress (eyes closed)": "lightgreen",
    "rest stress (eyes open)": "green",
}

def parse_trigger_series(df_trigger: pd.Series) -> pd.DataFrame:
    df_trigger = df_trigger[df_trigger > 0].map(
        lambda x: trigger_mapping.get(x, x)).reset_index().rename(columns={'timestamp': 't_start'}
    )
    df_trigger['t_end'] = df_trigger.t_start.shift(-1)
    df_trigger.loc[~df_trigger['trigger'].str.lower().str.startswith('start '), 't_end'] = None
    df_trigger = df_trigger[~df_trigger['trigger'].str.lower().str.startswith('end ')]

    return df_trigger
