import uuid
from pathlib import Path
from typing import Dict, List, Tuple

import dash
import dash_bootstrap_components as dbc
from dash import dcc, html

from datetime import datetime

import pandas as pd
import plotly.graph_objs as go
from dash_utils import _update_user_widget
from functional import seq
from plotly_resampler import FigureResampler
from dash.dependencies import Input, Output, State
from dash import Dash
from plotly.subplots import make_subplots
from trace_updater import TraceUpdater

from tsflex.processing import SeriesPipeline, SeriesProcessor, dataframe_func
from scipy import signal
import traceback
import numpy as np
from copy import copy

import sys

sys.path.append("../")
from cybb_mist.visualizations import draw_line, draw_rectangle
from cybb_mist.path_conf import interim_data_dir
from cybb_mist.trigger_mapping import (
    trigger_mapping,
    trigger_color_mapping,
    parse_trigger_series,
)
from cybb_mist.dataframes import time_based_outer_merge, groupby_consecutive
from cybb_mist.ecg_processing import ecg_pipeline
from cybb_mist.scl_processing import process_gsr_pipeline
import neurokit2 as nk

# global figure object
# Future work: use a singleton object for this ...
fig_dict: Dict[str, FigureResampler] = {}  # and use uuid for this

# server = flask.Flask(__name__)
app = Dash(__name__, external_stylesheets=[dbc.themes.LUX])

# each list is a type
name_folders_list = [
    {
        "cybb-mist": {"user_folder": interim_data_dir},
    },
]


# --------------------------------- Visualization ---------------------------------
# visualization helper method
def plot_ecg_cols(f, df_s, row_idx):
    ecg_rpeak_colors = [
        "mediumseagreen",
        "slateblue",
        "lightsalmon",
        "dimgray",
        "orangered",
        "lightgreen",
        "hotpink",
        "gold",
        "turquoise",
    ]
    ecg_rpeak_color_idx = 0

    for col in sorted(set(df_s.columns.values).difference(["index"])):
        if not len(df_s[col].dropna()):
            continue

        print(col, df_s[col].dtype)  # df_s[col])

        if col.startswith("ECG_R_Peaks_") and not col.endswith("_threshold"):

            # raw_symbols = SymbolValidator().values[:5]
            # import random

            f.add_trace(
                trace=go.Scattergl(
                    x=[],
                    y=[],
                    name=col,
                    mode="markers",
                    marker_size=15,
                    marker_color=ecg_rpeak_colors[ecg_rpeak_color_idx],
                    # marker_symbol=random.choice(raw_symbols),
                    legendgroup="_".join(col.split("_")[:-1]),
                ),
                hf_x=df_s[col].dropna().index,
                hf_y=df_s[col].dropna().astype(float),
                max_n_samples=4000,
                limit_to_view=True,
                row=row_idx,
                col=1,
            )

            # ecg -> r peaks -> add the timedelta's on another axis
            r_peaks = df_s[col].dropna()
            time_diff = r_peaks.index.to_series().diff()[1:].dt.total_seconds()
            hrv = r_peaks.index.to_series().diff().diff()[2:].dt.total_seconds()

            if col == "ECG_R_Peaks_processed":
                # slice based on the agreement bool
                agreement_mask = (r_peaks.shift(1) & r_peaks).values
                hrv_agreement_mask = (
                    r_peaks.shift(2) & r_peaks.shift(1) & r_peaks
                ).rename("SQI")

                time_diff = time_diff[agreement_mask[1:]]
                hrv = hrv[hrv_agreement_mask.values[2:]]
                f.add_trace(
                    trace=go.Scattergl(
                        x=[],
                        y=[],
                        name=f"HRV - {col}",
                        mode="markers",
                        marker_color=ecg_rpeak_colors[ecg_rpeak_color_idx],
                        marker_size=10,
                        legendgroup=col + "_hrv",
                    ),
                    hf_x=hrv.index,
                    hf_y=hrv.values,
                    max_n_samples=1000,
                    limit_to_view=True,
                    row=row_idx + 1,
                    col=1,
                )

                # add secondary background based on hrv agreement mask
                df_grouped = groupby_consecutive(hrv_agreement_mask)
                # print("df_grouped", df_grouped)
                df_grouped["SQI"] = df_grouped["SQI"].map(bool)
                df_grouped["good_sqi"] = df_grouped["SQI"].map(int)
                df_grouped["bad_sqi"] = (~df_grouped["SQI"]).map(int)
                for sqi_col, col_or in [
                    ("good_sqi", "#2ca02c"),
                    ("bad_sqi", "#d62728"),
                ]:
                    f.add_trace(
                        go.Scattergl(
                            x=df_grouped["start"],
                            y=df_grouped[sqi_col],
                            mode="lines",
                            line_width=0,
                            fill="tozeroy",
                            fillcolor=col_or,
                            opacity=0.1 if "good" in sqi_col else 0.2,
                            line_shape="hv",
                            name=sqi_col,
                            showlegend=False,
                        ),
                        limit_to_view=False,
                        secondary_y=True,
                        row=row_idx + 1,
                        col=1,
                    )

            f.add_trace(
                trace=go.Scattergl(
                    x=[],
                    y=[],
                    name=f"r-peaks time diff - {col}",
                    mode="markers",
                    marker_color=ecg_rpeak_colors[ecg_rpeak_color_idx],
                    marker_size=10,
                    legendgroup="_".join(col.split("_")[:-1]),
                ),
                hf_x=time_diff.index,
                hf_y=time_diff.values,
                max_n_samples=1000,
                limit_to_view=True,
                row=row_idx + 1,
                col=1,
            )

            ecg_rpeak_color_idx = (ecg_rpeak_color_idx + 1) % len(ecg_rpeak_colors)
            continue

        # elif col.startswith('ECG_res'):
        # fs_ecg_sig = 200
        # mp_approx = stumpy.scrump(df_s[col].dronna(), m=3*fs_ecg_sig, percentage=0.01, pre_scrump=True, s=None)
        # fig.add_trace(
        #     trace=go.Scattergl(x=[], y=[], name=f'matrix profile approximation - {col}'),
        #     max_n_samples=number_of_samples,
        #     # NOTE this slicing is correct!
        #     # see -> https://stumpy.readthedocs.io/en/latest/Tutorial_STUMPY_Basics.html#Find-a-Motif-Using-STUMP
        #     hf_x=df_s[col].dropna().index[:len(mp_approx)],
        #     hf_y=mp_approx,
        #     cut_points_to_view=True,
        #     row=row_idx + 1,
        #     col=1,
        # )
        # note -> no continue as we still will display the orig data

        f.add_trace(
            trace=go.Scattergl(
                x=[],
                y=[],
                name=col,
                legendgroup="ECG_R_Peaks_" + col,
                mode="lines+ markers" if col == "r_peak_agreement" else "lines",
                line_shape="hvh" if col == "r_peak_agrement" else "linear",
            ),
            max_n_samples=1000,
            hf_x=df_s[col].dropna().index,
            hf_y=df_s[col].dropna().astype(np.float32),
            limit_to_view=True,
            row=row_idx
            + int(
                col.startswith("ECG_SQI_")
                or col in ["r_peak_agreement", "ECG_quality_mask"]
            ),
            secondary_y=bool(col in ["r_peak_agreement", "ECG_quality_mask"]),
            col=1,
        )


def plot_gsr_cols(f, df_s, row_idx):
    secondary_y_cols = ["EDA_SQI_smoothend"]
    skip_cols = [
        "eda_lost_SQI",
        "EDA_delta_SQI",
        "eda_delta_SQI",
        "eda_large_delta_SQI",
        "eda_slope_SQI",
        "eda_noise_SQI",
        # "EDA_lf_1Hz",
        "raw_cleaned",
        "raw_cleaned_duration_filter",
        # "EDA_lf_cleaned_tonic",
        # TODO
        # "SCR_Amplitude",
        # "SCR_RiseTime",
        # "SCR_RecoveryTime",
        # "SCR_Peaks_neurokit",
        # "noise_area_1s",
        # "EDA_SQI_smoothend",
        "slope",
        # "noise",
        "SCR_RecoveryTime_reduced_acc",
    ]
    additional_row_cols = [
        "normalized_noise",
        "slope",
        "ACC_std",
        "noise",
        "noise_mean_1s",
        "noise_mean_2s",
        "acc_energy",
        # TODO added these
        "EDA_lost_SQI",
        "EDA_delta_SQI",
        "eda_delta_SQI",
        "eda_large_delta_SQI",
        "EDA_slope_SQI",
        "EDA_noise_SQI",
        "SCR_Amplitude",
        "SCR_RiseTime",
        "SCR_RecoveryTime",
        "phasic_noise_ratio",
    ]
    cols = sorted(set(df_s.columns.values).difference(["index"]))
    for col in cols:
        print(f"{col}" + "-" * 30)
        if df_s[col].dtype == "bool":
            df_s[col] = df_s[col].astype("uint8")
        if col in skip_cols:
            continue

        if not len(df_s[col].dropna()):
            print(col, "is all na")

        if col.lower() in [
            "gsr",
            "gsr1us",
            "gsr2us",
            "scr_recoverytime_reduced",
            "scr_risetime_reduced",
            # "eda_sqi_smoothened",
            "scr_amplitude_reduced",
            "scr_recovery",
            "scr_onsets",
        ]:
            continue
        elif col == "EDA":
            f.add_trace(
                trace=go.Scattergl(
                    x=[],
                    y=[],
                    name=col,
                    visible="legendonly",
                    line_color="gray",
                ),
                row=row_idx,
                col=1,
                max_n_samples=1000,
                hf_x=df_s.index,
                hf_y=df_s[col],
            )
            continue
        elif col == "EDA_SQI" or col == "EDA_SQI_tot":
            df_grouped = groupby_consecutive(df_s[col])
            df_grouped[col] = df_grouped[col].map(bool)
            df_grouped["good_sqi"] = df_grouped[col].map(int)
            df_grouped["bad_sqi"] = (~df_grouped[col]).map(int)
            print(df_grouped)
            for sqi_col, col_or in [
                ("good_sqi", "#2ca02c"),
                ("bad_sqi", "#d62728"),
            ]:
                f.add_trace(
                    go.Scattergl(
                        x=df_grouped["start"],
                        y=df_grouped[sqi_col],
                        mode="lines",
                        line_width=0,
                        fill="tozeroy",
                        fillcolor=col_or,
                        opacity=0.1 if "good" in sqi_col else 0.2,
                        line_shape="hv",
                        name=sqi_col,
                        showlegend=False,
                    ),
                    max_n_samples=len(df_grouped),
                    limit_to_view=False,
                    secondary_y=True,
                    row=row_idx,
                    col=1,
                )
            continue
            # NOTE: if you comment out the continue -> the EDA_SQI will also
            # be displayed
        elif col.startswith("SCR_Peaks_"):
            df_peaks = df_s[df_s[col] == 1]
            if not len(df_peaks):
                print(f"\tno peaks {df_peaks.shape} col={col}")
                continue

            f.add_trace(
                trace=go.Scattergl(
                    x=df_peaks.index,
                    y=df_peaks["EDA"],  # * 1.05,
                    visible="legendonly",
                    mode="markers",
                    marker_symbol="cross",
                    marker_size=15,
                    # marker_color="red",
                    name=col,
                ),
                limit_to_view=True,
                max_n_samples=1000,
                row=row_idx,
                col=1,
            )
            continue
        elif col in ["GSR", "GSR1uS"]:
            f.add_trace(
                trace=go.Scattergl(x=[], y=[], name=col, visible="legendonly"),
                row=row_idx,
                col=1,
                max_n_samples=1000,
                hf_x=df_s.index,
                hf_y=df_s[col],
            )
            continue
        elif (
            col.startswith("SCR_Onset")
            # or col.startswith("SCR_Recovery")
            # or col.startswith("SCR_Rise")
            or col.startswith("SCR_Height")
            # or col.startswith("EDA_Phasic")
            or col.startswith("SCR_Peaks")
            or col in ["SCR_Recovery", "EDA_SQI"]
            # or (
            #     col.startswith("SCR_Amplitude")
            #     and not col.endswith("reduced_acc")
            # )
        ):
            continue
        elif (  # additional row cols
            col.startswith("ACC_std")
            or col.startswith("EDA_Phasic")
            or col.startswith("SCR_RecoveryT")
            or col.startswith("SCR_Rise")
            or col.startswith("SCR_")
            # or col.startswith("EDA_SQI_")
            or col.startswith("SCR_Amplitude")
            or col in additional_row_cols
        ):
            f.add_trace(
                trace=go.Scattergl(x=[], y=[], name=col, visible="legendonly"),
                row=row_idx + 1,
                col=1,
                max_n_samples=1000,
                hf_x=df_s.index,
                hf_y=df_s[col],
            )
            continue
        f.add_trace(
            trace=go.Scattergl(
                x=[],
                y=[],
                name=col,
                # mode="lines+markers",
                visible="legendonly",
            ),
            row=row_idx,
            col=1,
            # todo -> fix hard-coded nbr of samples
            secondary_y=col in secondary_y_cols,
            max_n_samples=1000,
            limit_to_view=True,
            # orig_x=df_sensor[col].dropna().index,
            # orig_y=df_sensor[col].dropna(),
            hf_x=df_s[col].index,
            hf_y=df_s[col],
        )


def plot_multi_sensors(
    har_checklist,
    folder_user_subfolder_list: List[Tuple[str, str, str, str, str, List[str]]],
    session_id,
    paradigm,
    number_of_samples,
) -> go.Figure:
    global fig_dict

    har_checklist = [h.strip() for h in har_checklist]

    # create the figures
    prev_rows: List[str] = []
    if "show timeline" in har_checklist:
        prev_rows += ["timeline"]


    print(folder_user_subfolder_list)
    folder, user, subfolder = folder_user_subfolder_list[0]
    sensors_names = (
        seq(sorted(list(Path(folder).joinpath(user).glob(f"*{paradigm}.parquet"))))
        .map(lambda x: x.name)
        .to_list()
    )
    print(sensors_names)
    i = 0
    for sensor in copy(sensors_names):
        if sensor.split("_")[0].lower() == "ecg" and "process ECG" in har_checklist:
            sensors_names.insert(i + 1, "ECG - R peaks")
            i += 2
        elif sensor.split("_")[0].lower() == "scl" and "process GSR" in har_checklist:
            sensors_names.insert(i + 1, "GSR - SCR")
            i += 2
        else:
            i += 1

    # TODO -> add parse headache method
    total_rows: int = len(prev_rows) + len(sensors_names)
    print(sensors_names, flush=True)
    kwargs = {"vertical_spacing": 0.15 / total_rows} if total_rows >= 2 else {}

    specs = [
        [{"secondary_y": False}] * len(prev_rows)
        + [
            {
                "secondary_y": bool(
                    sensor.split("_")[0] in ["ECG - R peaks", "SCL", "GSR - SCR"]
                )
            }
            for sensor in sensors_names
        ]
    ]

    fig = FigureResampler(
        make_subplots(
            rows=total_rows,
            cols=1,
            shared_xaxes=True,
            specs=np.reshape(specs, (-1, 1)).tolist(),
            subplot_titles=prev_rows + sensors_names,
            **kwargs,
        ),
    )
    fig.update_layout(template="plotly_white")
    fig_dict[session_id] = fig

    fig.update_layout(height=min(1000, 400 * total_rows))
    fig.update_layout(title=f"Multi sensor overview - {paradigm}", title_x=0.5)

    # 0. Visualize the mBrain timeline
    legend_names = []
    if "timeline" in prev_rows:
        df_trigger = parse_trigger_series(
            pd.read_parquet(Path(folder).joinpath(user, f"SCL_{paradigm}.parquet"))[
                "trigger"
            ]
        )
        start, end = df_trigger.t_start.iloc[0], df_trigger.t_start.iloc[-1]

        for _, r in df_trigger.iterrows():
            # print(dict(r))
            # print(r.trigger)
            name = (
                r.trigger.lstrip("Start").strip()
                if r.trigger.startswith("Start")
                else r.trigger
            )

            if isinstance(r.t_end, pd.Timestamp):
                try:
                    fig.add_trace(
                        draw_rectangle(
                            t_start=r.t_start,
                            t_end=max(r.t_start + pd.Timedelta(seconds=6), r.t_end),
                            row=1,
                            opacity=0.5,
                            text=name,
                            name=name,
                            marker_color=trigger_color_mapping[name],
                            showlegend=False,
                        ),
                        limit_to_view=False,
                        row=1,
                        col=1,
                    )
                except:
                    print(r.t_stop)
            else:
                fig.add_trace(
                    draw_rectangle(
                        t_start=r.t_start,
                        t_end=r.t_start + pd.Timedelta(seconds=6),
                        row=1,
                        text=name,
                        name=name,
                        opacity=0.6,
                        marker_color=trigger_color_mapping[name],
                        showlegend=False,
                    ),
                    limit_to_view=False,
                    row=1,
                    col=1,
                )
    else:
        df_trigger = parse_trigger_series(
            pd.read_parquet(Path(folder).joinpath(user, f"SCL_{paradigm}.parquet"))[
                "trigger"
            ]
        )
        start, end = df_trigger.t_start.iloc[0], df_trigger.t_start.iloc[-1]

    # 1. Visualize the sensor data
    row_idx = 1 + len(prev_rows)
    for sensor in sorted(Path(folder).joinpath(user).glob(f"*{paradigm}.parquet")):
        df_sensor = pd.read_parquet(sensor).drop(columns=["trigger"])[start:end]

        for c in df_sensor.columns:
            if c == "ECG" and "process ECG" in har_checklist:
                df_sensor = ecg_pipeline.process(
                    df_sensor["ECG"],
                    return_df=True,
                    logging_file_path="ecg_processing_logs.log",
                )
                # .drop(columns=["ECG"]),
                plot_ecg_cols(fig, df_sensor, row_idx)
                row_idx += 1
            elif c == "SCL" and "process GSR" in har_checklist:
                df_sensor = process_gsr_pipeline(df_sensor["SCL"].rename("EDA"))
                plot_gsr_cols(fig, df_sensor, row_idx)
                row_idx += 1
            else:
                fig.add_trace(
                    go.Scattergl(name=c),
                    hf_x=df_sensor.index,
                    hf_y=df_sensor[c],
                    row=row_idx,
                    col=1,
                )
        row_idx += 1

    return fig


dark_figure = go.Figure()
dark_figure.update_layout(template="plotly_white")

# --------------------------------- DASH code base ---------------------------------
def serve_layout() -> dbc.Container:
    """Constructs the app's layout.

    Returns
    -------
    A Container withholding the layout.
    """
    session_id = str(uuid.uuid4())
    return dbc.Container(
        [
            # todo look into:
            # https://stackoverflow.com/questions/62732631/how-to-collapsed-sidebar-in-
            # dash-plotly-dash-bootstrap-components
            dbc.Container(
                html.H1("Plotly-Resampler cyberball mist dashboard"),
                style={"textAlign": "center"},
            ),
            # will be used for session storage
            html.Div(session_id, id="session-id", style={"display": "none"}),
            # is used to detect close events
            html.Div(id="listener"),
            # duc.Unload(id="page-listener"),
            html.Hr(),
            dbc.Row(
                [
                    dbc.Col(
                        dbc.Card(
                            [
                                dbc.Form(
                                    [
                                        html.Br(),
                                        dcc.Checklist(
                                            id="har-checklist",
                                            options=[
                                                " show timeline",
                                                " process ECG",
                                                " process GSR",
                                            ],
                                        ),
                                        html.Br(),
                                        dbc.Label("paradigm:"),
                                        dcc.Dropdown(
                                            id=f"paradigm-selector",
                                            options=["mist", "cybb"],
                                            value="mist",
                                            clearable=False,
                                        ),
                                    ]
                                ),
                                html.Br(),
                            ]
                            + [
                                dbc.Card(
                                    [
                                        dbc.Form(
                                            [
                                                dbc.Label("folder"),
                                                dcc.Dropdown(
                                                    id=f"folder-selector{i}",
                                                    options=[
                                                        {
                                                            "label": l,
                                                            "value": str(
                                                                f["user_folder"]
                                                            ),
                                                        }
                                                        for (
                                                            l,
                                                            f,
                                                        ) in name_folders.items()
                                                    ],
                                                    clearable=True,
                                                ),
                                                # a div with stored 'folder_name',"sub_folder";...
                                                html.Div(
                                                    ";".join(
                                                        [
                                                            str(f["user_folder"])
                                                            + ","
                                                            + str(
                                                                f.get("sub_folder", "")
                                                            )
                                                            for (
                                                                _,
                                                                f,
                                                            ) in name_folders.items()
                                                        ]
                                                    ),
                                                    id=f"subfolder{i}",
                                                    style={"display": "none"},
                                                ),
                                                dbc.Label("User:"),
                                                dcc.Dropdown(
                                                    id=f"user-selector{i}",
                                                    options=[],
                                                    clearable=False,
                                                ),
                                                # dbc.Label("Sensors:"),
                                                # dcc.Dropdown(
                                                #     id=f"sensor-selector{i}",
                                                #     options=[],
                                                #     multi=True,
                                                # ),
                                            ]
                                        ),
                                    ],
                                    color="primary",
                                    outline=True,
                                )
                                for i, name_folders in enumerate(name_folders_list, 1)
                            ]
                            + [
                                dbc.Form(
                                    [
                                        html.Br(),
                                        dbc.Button(
                                            "Run interact",
                                            id="plot-button",
                                            color="primary",
                                            style={"textAlign": "center"},
                                        ),
                                    ],
                                    style={"textAlign": "center"},
                                ),
                            ],
                            body=True,
                        ),
                        md=2,
                    ),
                    dbc.Col(
                        [
                            dcc.Graph(
                                id="resampled-graph",
                                figure=FigureResampler(dark_figure),
                            ),
                            TraceUpdater(
                                id="trace-updater",
                                gdID="resampled-graph",
                                sequentialUpdate=False,
                            ),
                        ],
                        md=10,
                    ),
                ],
                align="center",
            ),
        ],
        fluid=True,
    )


for id, _ in enumerate(name_folders_list, 1):
    app.callback(
        Output(f"user-selector{id}", "options"),
        [Input(f"folder-selector{id}", "value")],
    )(_update_user_widget)


app.layout = serve_layout()


# --- update the figure
# the list sum operationflattens the list
# TODO -> this is not loosely coupled
selector_states = list(
    sum(
        [
            (
                State(f"folder-selector{i}", "value"),
                State(f"user-selector{i}", "value"),
                State(f"subfolder{i}", "children"),
            )
            for i in range(1, len(name_folders_list) + 1)
        ],
        (),
    )
)


@app.callback(
    Output("trace-updater", "updateData"),
    [
        Input("resampled-graph", "relayoutData"),
        State("session-id", "children"),
    ],
)
def update_graph(relayoutdata: dict, session_id: str):
    global fig_dict
    fr: FigureResampler = fig_dict.get(session_id)
    if fr is None or relayoutdata is None:
        raise dash.exceptions.PreventUpdate()
    return fr.construct_update_data(relayoutdata)


@app.callback(
    Output("resampled-graph", "figure"),
    [
        Input("plot-button", "n_clicks"),
        State("har-checklist", "value"),
        State("session-id", "children"),
        State("paradigm-selector", "value"),
        *selector_states,
    ],
)
def plot_or_update_graph(n_clicks, har_checklist, session_id, paradigm, *folder_list):
    print(paradigm)
    har_checklist = [] if har_checklist is None else har_checklist
    global fig_dict
    if session_id not in fig_dict.keys():
        fig_dict[session_id] = None  # just create an empty placeholder

    it = iter(folder_list)
    folder_user_subfolder_list = []
    for folder, user, subfolder in zip(it, it, it):
        if not all((folder, user, subfolder)):
            continue
        else:
            folder_user_subfolder_list.append((folder, user, subfolder))

    ctx = dash.callback_context
    if (
        len(ctx.triggered)
        and "plot-button" in ctx.triggered[0]["prop_id"]
        and len(folder_user_subfolder_list)
    ):
        return plot_multi_sensors(
            har_checklist=har_checklist,
            paradigm=paradigm,
            folder_user_subfolder_list=folder_user_subfolder_list,
            session_id=session_id,
            number_of_samples=1000,
        )
    return dash.no_update


# + tags=[]
if __name__ == "__main__":
    app.run_server(port=8066, host="0.0.0.0", debug=True)
