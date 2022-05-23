# -*- coding: utf-8 -*-
"""
    ****************
    visualizations.py
    ****************
    
    Created at 03/02/22        
"""
__author__ = 'Jonas Van Der Donckt'

import os

import pandas as pd
import plotly.graph_objs as go
from pathlib import Path
from typing import List, Union


def figs_to_html(figs: List[go.Figure], html_path: Union[Path, str], append=False, 
                 include_plotlyjs=True):
    """Save a list of figures in a single HTML file.

    :param figs: A list of plotly figures
    :param html_path: the HTML path where the figure will be saved
    :param append:
    """
    if not isinstance(html_path, Path):
        html_path = Path(html_path)

    if not html_path.parent.exists():
        os.makedirs(html_path.parent)

    with open(html_path, "a" if append else "w") as f:
        for fig in figs:
            f.write(fig.to_html(full_html=False, include_plotlyjs=include_plotlyjs))



def draw_rectangle(
        t_start: pd.Timestamp, t_end: pd.Timestamp, row: int = 1, **kwargs
) -> go.Scatter:
    """Use a go.Scatter object to draw a rectangle.

    :param t_start: The start time of the rectangle
    :param t_end: The end time of the rectangle
    :param row: The row on which the rectangle will be drawn
        INDEX starts at 1!!
    :param kwargs: Additional arguments which will be passed to the Scatter function.

    :return: The drawn rectangle

    See Also
    --------
    https://plotly.com/python-api-reference/generated/plotly.graph_objects.Scatter.html
    for more info.

    """
    assert t_start <= t_end
    row = max(1, row)
    return go.Scatter(
        x=[t_start, t_start, t_end, t_end, t_start],
        y=[row - 0.9, row - 0.1, row - 0.1, row - 0.9, row - 0.9],
        fill="toself",
        **kwargs,
    )


def draw_line(timestamp: pd.Timestamp, row: int = 1, **kwargs) -> go.Scatter:
    """Use a go.Scatter object to draw a rectangle.

    :param timestamp: The timestamp where the line will be drawn
    :param row: The row on which the rectangle will be drawn
        INDEX starts at 1!!
    :param kwargs: Additional arguments which will be passed to the Scatter function

    :return: The drawn rectangle

    See Also
    --------
    https://plotly.com/python-api-reference/generated/plotly.graph_objects.Scatter.html
    for more info.

    """
    row = max(1, row)
    return go.Scatter(x=[timestamp, timestamp], y=[row - 1 + 0.1, row - 0.1], **kwargs)
