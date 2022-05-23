# -*- coding: utf-8 -*-
"""
"""

from pathlib import Path
from typing import Dict, Union, List

import dash_bootstrap_components as dbc
from dash import dcc, html
from dash.dependencies import Input, Output, State
from functional import seq

__author__ = "Jonas Van Der Donckt"


def folder_user_date_sensor_selector(
    app,
    name_folders_dict: Dict[str, Union[Path, str]],
) -> dbc.Card:
    """Constructs a folder user date selector

    Creates a `dbc.Card` component which can be

    Parameters
    ----------
    app:
        The dash application.
    name_folders: Dict[str, Union[Path, str]]
         A dict with key, the display-key and values the correspondign path.

    Returns
    -------
    A bootstrap card component
    """
    selector = dbc.Card(
        [
            dbc.Form(
                [
                    dbc.Label("folder"),
                    dcc.Dropdown(
                        id="folder-selector",
                        options=[
                            {"label": l, "value": str(f)}
                            for (l, f) in name_folders_dict.items()
                        ],
                        clearable=False,
                    ),
                ]
            ),
            dbc.Form(
                [
                    dbc.Label("user"),
                    dcc.Dropdown(
                        id="user-selector",
                        options=[],
                        clearable=False,
                    ),
                ]
            ),
            dbc.Form(
                [
                    dbc.Label("date"),
                    dcc.Dropdown(id="date-selector", options=[], clearable=False),
                ]
            ),
            dbc.Form(
                [
                    dbc.Label("Sensors"),
                    dcc.Dropdown(id="sensor-selector", options=[], multi=True),
                ]
            ),
            dbc.Form(
                [
                    dbc.Button("Run interact", id="plot-button", color="primary"),
                ]
            ),
        ],
        body=True,
    )
    _register_selection_callbacks(app=app)
    return selector


def _create_subfolder_dict(
    subfolder_str, inter_folder_sep=";", intra_folder_subfolder_sep=","
) -> dict:
    folder_subfolder_dict = {}
    for folder_subfolder in subfolder_str.split(inter_folder_sep):
        if len(folder_subfolder.split(intra_folder_subfolder_sep)) == 2:
            f, sub_f = folder_subfolder.split(",")
            folder_subfolder_dict[f] = sub_f
    return folder_subfolder_dict


def _update_user_widget(folder: Path, reverse=True):
    if folder is None:
        return []
    return [
        {"label": username, "value": username}
        for username in sorted(
            set(
                list(
                    seq(Path(folder).iterdir())
                    .filter(lambda x: x.is_dir())
                    .map(lambda x: x.name)
                )
            ),
            reverse=reverse
        )
    ]


def _set_default_value(available_options):
    if available_options is not None and len(available_options):
        return available_options[0]["value"]
    return None


def _update_date_widget(user: str, folder: Path, subfolder: str, reverse=True):
    if folder is None or user is None:
        return []

    date_str_options = sorted(
        list(
            set(
                seq(
                    Path(folder)
                    .joinpath(user)
                    .joinpath(_create_subfolder_dict(subfolder).get(folder, ""))
                    .iterdir()
                ).map(lambda x: "_".join(x.name.split(".")[0].split("_")[-3:]))
            )
        ),
        reverse=reverse,
    )
    return [{"label": date_str, "value": date_str} for date_str in date_str_options]


def _update_shared_date_widget(*user_folder_subfolders):
    # todo -> ook nog ergens de prev options -> nvm we kunnen dees altijd opnieuw berekenen
    sub_folders = user_folder_subfolders[-len(user_folder_subfolders) // 3 :]
    it = iter(user_folder_subfolders)
    day_options = []
    for user, folder, sub_folder in zip(it, it, sub_folders):
        if user is not None and folder is not None:
            folder_user_opts = list(
                set(
                    seq(
                        Path(folder)
                        .joinpath(user)
                        .joinpath(_create_subfolder_dict(sub_folder).get(folder, ""))
                        .iterdir()
                    ).map(lambda x: "_".join(x.name.split(".")[0].split("_")[-3:]))
                )
            )
            print(Path(folder).name, folder_user_opts)
            day_options.append(folder_user_opts)

    # calculate the intersection
    if len(day_options) > 1:
        inters_options = set(day_options[0])
        for options in day_options[1:]:
            inters_options = inters_options.intersection(options)
        day_options = sorted(inters_options, reverse=True)
    elif len(day_options) == 1:
        day_options = sorted(day_options[0], reverse=True)

    return [{"label": date_str, "value": date_str} for date_str in day_options]


def _update_sensor_widget(user, date, folder, sub_folders=""):
    if folder is None or user is None or date is None:
        return []

    # print("sub_folder", sub_folders)
    folder_subfolder_dict = _create_subfolder_dict(sub_folders)
    print("get subfolder\n", folder_subfolder_dict.get(folder, ""))
    sensors = sorted(
        list(
            seq(
                Path(folder)
                .joinpath(user)
                .joinpath(folder_subfolder_dict.get(folder, ""))
                .glob(f"*{date}*")
            ).map(lambda x: '_'.join(x.name.split("_")[:-3])),
        ),
        reverse=True,
    )
    return [{"label": sensor_str, "value": sensor_str} for sensor_str in sensors]


def _register_selection_callbacks(app, ids=None, same_date=False):
    if ids is None:
        ids = [""]

    # TODO -> simplify logic
    date_selector = "date-selector"
    if same_date:
        user_folder_inputs_subfolder_states = list(
            sum(
                [
                    (
                        Input(f"user-selector{_id}", "value"),
                        Input(f"folder-selector{_id}", "value"),
                    )
                    for _id in ids
                ],
                (),
            )
        )
        # states must come after subfolders -> hence the weird order of not grouping
        user_folder_inputs_subfolder_states += (
            [State(f"subfolder{_id}", "children") for _id in ids],
        )

        app.callback(
            Output(date_selector, "options"), *user_folder_inputs_subfolder_states
        )(_update_shared_date_widget)

    for id in ids:
        if not same_date:
            date_selector = f"date-selector{id}"

        app.callback(
            Output(f"user-selector{id}", "options"),
            [Input(f"folder-selector{id}", "value")],
        )(_update_user_widget)

        if not same_date:
            app.callback(
                Output(date_selector, "options"),
                [
                    Input(f"user-selector{id}", "value"),
                    State(f"folder-selector{id}", "value"),
                    State(f"subfolder{id}", "children"),
                ],
            )(_update_date_widget)

        app.callback(
            Output(f"sensor-selector{id}", "options"),
            [
                Input(f"user-selector{id}", "value"),
                Input(date_selector, "value"),
                State(f"folder-selector{id}", "value"),
                State(f"subfolder{id}", "children"),
            ],
        )(_update_sensor_widget)

    app.callback(
        Output(date_selector, "value"),
        [Input(date_selector, "options")],
    )(_set_default_value)


def multiple_folder_user_date_sensor_selector(
    app, name_folders_list: List[Dict[str, dict]]
) -> dbc.Card:
    """Constructs a folder user date selector

    Creates a `dbc.Card` component which can be

    Parameters
    ----------
    app:
        The dash application.
    name_folders_list:List[Dict[str, Union[Path, str]]]
    TODO
         A dict with key, the display-key and values the correspondign path.

    Returns
    -------
    A bootstrap card component
    """
    selector = dbc.Card(
        [
            dbc.Card(
                [
                    dbc.FormGroup(
                        [
                            dbc.Label("folder"),
                            dcc.Dropdown(
                                id=f"folder-selector{i}",
                                options=[
                                    {"label": l, "value": str(f["user_folder"])}
                                    for (l, f) in name_folders.items()
                                ],
                                clearable=False,
                            ),
                            # a div with stored 'folder_name',"sub_folder";...
                            html.Div(
                                ";".join(
                                    [
                                        str(f["user_folder"])
                                        + ","
                                        + str(f.get("sub_folder", ""))
                                        for (_, f) in name_folders.items()
                                    ]
                                ),
                                id=f"subfolder{i}",
                                style={"display": "none"},
                            ),
                            dbc.Label("user"),
                            dcc.Dropdown(
                                id=f"user-selector{i}",
                                options=[],
                                clearable=False,
                            ),
                            dbc.Label("date"),
                            dcc.Dropdown(
                                id=f"date-selector{i}", options=[], clearable=False
                            ),
                            dbc.Label("Sensors"),
                            dcc.Dropdown(
                                id=f"sensor-selector{i}", options=[], multi=True
                            ),
                        ]
                    ),
                ]
            )
            for i, name_folders in enumerate(name_folders_list, 1)
        ]
        + [
            dbc.Card(
                dbc.FormGroup(
                    [
                        dbc.Button("Run interact", id="plot-button", color="primary"),
                    ]
                ),
            )
        ],
        body=True,
    )

    _register_selection_callbacks(app=app, ids=range(1, len(name_folders_list) + 1))
    return selector


def multiple_folder_user_same_date_sensor_selector(
    app, name_folders_list: List[Dict[str, dict]]
) -> dbc.Card:
    """Constructs a folder user date selector

    Creates a `dbc.Card` component which can be

    Parameters
    ----------
    app:
        The dash application.
    name_folders_list:List[Dict[str, Union[Path, str]]]
    TODO
         A dict with key, the display-key and values the corresponding path.

    Returns
    -------
    A bootstrap card component
    """
    selector = dbc.Card(
        [
            dbc.Form(
                [
                    dbc.Label("date", style={"textAlign": "center"}),
                    dcc.Dropdown(id="date-selector", options=[], clearable=False),
                ]
            )
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
                                    {"label": l, "value": str(f["user_folder"])}
                                    for (l, f) in name_folders.items()
                                ],
                                clearable=True,
                            ),
                            # a div with stored 'folder_name',"sub_folder";...
                            html.Div(
                                ";".join(
                                    [
                                        str(f["user_folder"])
                                        + ","
                                        + str(f.get("sub_folder", ""))
                                        for (_, f) in name_folders.items()
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
                            dbc.Label("Sensors:"),
                            dcc.Dropdown(
                                id=f"sensor-selector{i}", options=[], multi=True
                            ),
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
    )

    _register_selection_callbacks(
        app=app, ids=range(1, len(name_folders_list) + 1), same_date=True
    )
    return selector


def multiple_folder_user_date_range_sensor_selector(
    app, name_folders_list: List[Dict[str, dict]]
) -> dbc.Card:
    """Constructs a folder user date selector

    Creates a `dbc.Card` component which can be

    Parameters
    ----------
    app:
        The dash application.
    name_folders_list:List[Dict[str, Union[Path, str]]]
    TODO
         A dict with key, the display-key and values the corresponding path.

    Returns
    -------
    A bootstrap card component
    """
    selector = dbc.Card(
        [
            dbc.Form(
                [
                    dbc.Label("date", style={"textAlign": "center"}),
                    dcc.Dropdown(id="start-date", options=[], clearable=False),
                    dcc.Dropdown(id="end-date", options=[], clearable=False),
                ]
            )
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
                                    {"label": l, "value": str(f["user_folder"])}
                                    for (l, f) in name_folders.items()
                                ],
                                clearable=True,
                            ),
                            # a div with stored 'folder_name',"sub_folder";...
                            html.Div(
                                ";".join(
                                    [
                                        str(f["user_folder"])
                                        + ","
                                        + str(f.get("sub_folder", ""))
                                        for (_, f) in name_folders.items()
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
                            dbc.Label("Sensors:"),
                            dcc.Dropdown(
                                id=f"sensor-selector{i}", options=[], multi=True
                            ),
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
    )

    _register_selection_callbacks(
        app=app, ids=range(1, len(name_folders_list) + 1), same_date=True
    )
    return selector
